// Copyright (c) Microsoft. All rights reserved.
// Licensed under the MIT license. See LICENSE file in the project root for full license information.
namespace Bond.TypeProvider.DesignTime

open System
open System.Reflection
open System.Collections.Generic
open System.IO
open ProviderImplementation.ProvidedTypes
open FSharp.Core.CompilerServices
open Bond
open Bond.Protocols
open Bond.IO.Unsafe

[<TypeProvider>]
type public BondTypeProvider(cfg : TypeProviderConfig) =
  inherit TypeProviderForNamespaces()

  // TODO: Is there a better way to enable quotation deserialization to find Bond.dll types?  This seems brittle.
  static let asms = [typeof<IProtocolWriter>.Assembly]
  static do
    AppDomain.CurrentDomain.add_AssemblyResolve (fun _ ev ->
         printfn "%s" ev.Name
         match asms |> List.tryFind (fun asm -> asm.FullName = ev.Name) with
         | Some asm -> asm
         | None -> null)

  let memo f =
      let d = Dictionary(HashIdentity.Structural)
      fun x y ->
          if not <| d.ContainsKey (x,y) then
              d.[(x,y)] <- f x y
          d.[(x,y)]

  let runtimeAssembly = typeof<BondTypeProvider>.Assembly
  let ns = "Bond.TypeProvider"
  let ctxt = ProviderImplementation.ProvidedTypesContext.Create(cfg)
  let schemaTy = ctxt.ProvidedTypeDefinition(runtimeAssembly, ns, "SchemaTypeProvider", Some typeof<obj>)
  let filename = ctxt.ProvidedStaticParameter("FilePath", typeof<string>)
  let protTy = ctxt.ProvidedStaticParameter("Protocol", typeof<ProtocolType>) // TODO former default: , int ProtocolType.MARSHALED_PROTOCOL)

  let helpText = """<summary>Typed representation of a Bond schema</summary>
                    <param name='FilePath'>Bond SchemaDef location</param>
                    <param name='Protocol'>Schema serialization protocol (marshalled by default)</param>"""

  do schemaTy.AddXmlDoc helpText


  /// the SchemaDef that we will reflect as provided types
  let (|SchemaContents|) (contents : obj array) =
      match contents with
      | [| (:? string as filename); (:? ProtocolType as prot) |] ->
        let uri =
          if Uri.IsWellFormedUriString(filename, UriKind.Relative) then
            Uri(Path.Combine(Path.Combine(cfg.ResolutionFolder, filename)))
          else // note: this just works for full paths even without leading "file:///"
            Uri(filename)

        // Deserialize the SchemaDef from the Uri using the relevant ITaggedProtocolReader
        use strm =
          if uri.IsFile then
            File.OpenRead(uri.LocalPath) :> Stream
          else
            let client = new System.Net.WebClient()
            let content = client.DownloadData(uri)
            new MemoryStream(content) :> _
        if prot = ProtocolType.MARSHALED_PROTOCOL then
          Unmarshal<SchemaDef>.From(new InputStream(strm))
        else
          let rdr : ITaggedProtocolReader =
              match prot with
              | ProtocolType.COMPACT_PROTOCOL -> upcast new CompactBinaryReader<InputStream>(new InputStream(strm))
              | ProtocolType.FAST_PROTOCOL -> upcast new FastBinaryReader<InputStream>(new InputStream(strm))
              | p -> failwithf "Unrecognized protocol : %A" p
          Deserialize<SchemaDef>.From(rdr)
      | f -> failwithf "unexpected arguments %A" f

  do schemaTy.DefineStaticParameters([filename; protTy;], memo (fun tyName (SchemaContents schemaContents) ->
    /// maps struct IDs to provided type
    let provTys = Dictionary()
    let tp = TP(provTys)

    /// maps struct i => Tuple<...>
    let tupTys = Dictionary<uint16,Lazy<Type>>()

    /// transitive closure of related struct indices
    let containerTy = ctxt.ProvidedTypeDefinition(runtimeAssembly, ns, tyName, Some typeof<obj>)

    containerTy.AddMembers(
        schemaContents.structs
        |> Seq.toList
        |> List.mapi (fun i st -> (i,st))
        |> List.filter (fun (_,st) -> st.base_def = null) // we don't currently support inheritance
        |> List.map (fun (i,st) ->
            let tp2 = TP2(schemaContents, uint16 i, tupTys, tp)
            let reprTy = lazy
              tp2.FieldsFor
              |> List.map (fun fieldInfo -> tp.TypeForBondType fieldInfo.fieldType |> fst)
              |> Array.ofList
              |> Reflection.FSharpType.MakeTupleType

            tupTys.[uint16 i]  <- reprTy
            let stTy = ctxt.ProvidedTypeDefinition(st.metadata.name, Some typeof<obj>)
            provTys.[uint16 i] <- stTy

            stTy.AddMembersDelayed(fun () ->
                let props =
                  tp2.FieldsFor |> List.mapi (fun idx fieldInfo ->
                      let (_,ty) = tp.TypeForBondType fieldInfo.fieldType
                      ctxt.ProvidedProperty(fieldInfo.metadata.name, ty,
                                            getterCode = fun [this] -> QExpr.TupleGet(QExpr.Coerce(this, tupTys.[uint16 i].Value), idx)) :> MemberInfo)
                let createInstance args =
                    List.zip args tp2.FieldsFor
                    |> List.map (fun (arg:Quotations.Expr, f) ->
                        if f.defaultValue = null then
                            // <@ if (arg :> obj) = null then defaultExpr else arg @>
                            // Note that we can't use the generic equality test at the actual arg type or it will throw a null reference exception, thanks to F#'s non-nullable type checking
                            QExpr.IfThenElse(<@ %%QExpr.Coerce(arg, typeof<obj>) = null @>, f.defaultExpr, arg)
                        else
                            arg)
                    |> TP2.NewTuple

                props @ [ctxt.ProvidedConstructor(
                            [for (:? PropertyInfo as pi), fi in Seq.zip props tp2.FieldsFor -> ProvidedParameter(pi.Name, pi.PropertyType, optionalValue = fi.defaultValue)],
                            invokeCode = createInstance)
                         ctxt.ProvidedMethod("DeserializeFrom",
                                             [ctxt.ProvidedParameter("reader", typeof<ITaggedProtocolReader>)],
                                             stTy, // isStaticMethod = true,
                                             invokeCode = fun [rdr] -> QExpr.LetRecursive(tp2.TaggedDeserializers.Value, QExpr.Application(QExpr.Var tp2.TaggedDeserializerVars.Value.[uint16 i], rdr)))
                         ctxt.ProvidedMethod("DeserializeFrom",
                                             [ProvidedParameter("reader", typeof<IUntaggedProtocolReader>)],
                                             stTy, // IsStaticMethod = true,
                                             invokeCode = fun [rdr] -> QExpr.LetRecursive(tp2.UntaggedDeserializers.Value, QExpr.Application(QExpr.Var tp2.UntaggedDeserializerVars.Value.[uint16 i], rdr)))
                         ctxt.ProvidedMethod("SerializeTo",
                                             [ProvidedParameter("writer", typeof<IProtocolWriter>)],
                                             typeof<unit>,
                                             invokeCode = fun [this;wrtr] -> QExpr.LetRecursive(tp2.Serializers.Value, QExpr.Application(
                                                                                                                          QExpr.Application(QExpr.Var tp2.SerializerVars.Value.[uint16 i], wrtr),
                                                                                                                          QExpr.Coerce(this, tupTys.[uint16 i].Value))))])
            stTy))
    containerTy))

  do base.AddNamespace(ns, [schemaTy])
