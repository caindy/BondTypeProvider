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

type private fieldInfo = { id : uint16; metadata : Metadata; defaultExpr : Quotations.Expr; defaultValue : obj; fieldType : TypeDef }

/// Convert SchemaDefs (and their components) to equivalent quotation literals
module private SchemaQuotation =
  /// Given a function for converting list elements to quotations and a list of values, generate a quoted list
  let rec private qList f = function
  | [] -> <@ [] @>
  | x::xs -> <@ %(f x) :: %(qList f xs) @>

  /// Given a dictionary, generate a quotation building an equivalent dictionary
  let private quoteDict d =
      <@ Dictionary(dict %(d |> List.ofSeq |> qList (fun (KeyValue(k,v)) -> <@ k, v @>))) @>

  /// Given a Variant, produce an equivalent quotation
  let quoteVariant (v:Variant) =
      if v.nothing then
          <@ Variant(nothing = true) @>
      elif v.int_value <> 0L then
          let v = v.int_value in <@ Variant(int_value = v) @>
      elif v.uint_value <> 0uL then
          let v = v.uint_value in <@ Variant(uint_value = v) @>
      elif v.double_value <> 0.0 then
          let v = v.double_value in <@ Variant(double_value = v) @>
      elif v.string_value <> "" then
          let v = v.string_value in <@ Variant(string_value = v) @>
      elif v.wstring_value <> "" then
          let v = v.wstring_value in <@ Variant(wstring_value = v) @>
      else
          <@ Variant() @>

  /// Given a Metadata, produce an equivalent quotation
  let quoteMetadata (m : Metadata) =
      let qn = m.qualified_name
      let nm = m.name
      let md = m.modifier
      <@ Metadata(qualified_name = qn, name = nm, modifier = md, default_value = %(quoteVariant m.default_value), attributes = %(quoteDict m.attributes)) @>


[<TypeProvider>]
type public BondTypeProvider(cfg : TypeProviderConfig) =
    inherit TypeProviderForNamespaces()

    // TODO: Is there a better way to enable quotation deserialization to find Bond.dll types?  This seems brittle.
    static let asms = [typeof<IProtocolWriter>.Assembly]
    static do System.AppDomain.CurrentDomain.add_AssemblyResolve(fun _ ev -> printfn "%s" ev.Name; match asms |> List.tryFind (fun asm -> asm.FullName = ev.Name) with | Some asm -> asm | None -> null)

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
    let protTy = ctxt.ProvidedStaticParameter("Protocol", typeof<ProtocolType>) // , int ProtocolType.MARSHALED_PROTOCOL)

    let helpText = """<summary>Typed representation of a Bond schema</summary>
                      <param name='FilePath'>Bond SchemaDef location</param>
                      <param name='Protocol'>Schema serialization protocol (marshalled by default)</param>"""

    do schemaTy.AddXmlDoc helpText
    do schemaTy.DefineStaticParameters([filename; protTy;], memo (fun tyName [| :? string as filename; :? int as prot; |] ->
        let uri =
            if Uri.IsWellFormedUriString(filename, UriKind.Relative) then
                Uri(Path.Combine(Path.Combine(cfg.ResolutionFolder, filename)))
            else // note: this just works for full paths even without leading "file:///"
                Uri(filename)

        /// the SchemaDef that we will reflect as provided types
        let schemaContents =
            // Deserialize the SchemaDef from the Uri using the relevant ITaggedProtocolReader
            use strm =
                if uri.IsFile then
                    File.OpenRead(uri.LocalPath) :> Stream
                else
                    let client = new System.Net.WebClient()
                    let content = client.DownloadData(uri)
                    new MemoryStream(content) :> _
            let def =
                if enum prot = ProtocolType.MARSHALED_PROTOCOL then
                    Unmarshal<SchemaDef>.From(new InputStream(strm))
                else
                    let rdr : ITaggedProtocolReader =
                        match enum prot with
                        | ProtocolType.COMPACT_PROTOCOL -> upcast new CompactBinaryReader<InputStream>(new InputStream(strm))
                        | ProtocolType.FAST_PROTOCOL -> upcast new FastBinaryReader<InputStream>(new InputStream(strm))
                        | p -> failwithf "Unrecognized protocol : %A" p
                    Deserialize<SchemaDef>.From(rdr)
            def

        /// maps struct IDs to provided type
        let provTys = Dictionary()
        let typeForBondType = TP.typeForBondType provTys

        /// maps struct i => Tuple<...>
        let tupTys = Dictionary()

        /// transitive closure of related struct indices
        let structsFrom (s : SchemaDef) idx =
            let rec structComponents (t : TypeDef) =
                if t.id = BondDataType.BT_STRUCT then Set.singleton t.struct_def
                elif t.element = null then Set.empty
                else
                    match t.id with
                    | BondDataType.BT_LIST | BondDataType.BT_SET ->
                        structComponents t.element
                    | BondDataType.BT_MAP ->
                        Set.union (structComponents t.element) (structComponents t.key)
                    | BondDataType.BT_STRUCT ->
                        Set.singleton t.struct_def
                    | _ -> Set.empty
            let rec loop seen idx =
                let frontier =
                    s.structs.[int idx].fields
                    |> Seq.map (fun f -> structComponents f.``type``)
                    |> Set.unionMany
                if Set.isSubset frontier seen then seen
                else
                    frontier - seen
                    |> Set.fold loop (Set.union seen frontier)
            loop Set.empty idx


        let containerTy = ctxt.ProvidedTypeDefinition(runtimeAssembly, ns, tyName, None)

        /// Gets the list of (field ID, metadata, default value (expression), field type) for each field in the nth type
        let fieldsFor i =
            [for f in schemaContents.structs.[i].fields ->
                { id = f.id; metadata = f.metadata; defaultExpr = TP.defaultExpr provTys f.``type`` f.metadata.default_value; defaultValue = TP.defaultValue f.``type`` f.metadata.default_value; fieldType = f.``type``}]
            |> List.sortBy (fun fi -> fi.id)

        containerTy.AddMembers(
            schemaContents.structs
            |> Seq.toList
            |> List.mapi (fun i st -> (i,st))
            |> List.filter (fun (_,st) -> st.base_def = null) // we don't currently support inheritance
            |> List.map (fun (i,st) ->

                let reprTy =
                    lazy
                        fieldsFor i |> List.map (fun fieldInfo -> typeForBondType fieldInfo.fieldType |> fst)
                        |> Array.ofList
                        |> Reflection.FSharpType.MakeTupleType

                let stTy = ctxt.ProvidedTypeDefinition(st.metadata.name, Some(typeof<obj>))
                provTys.[uint16 i] <- stTy
                tupTys.[uint16 i] <- lazy reprTy.Value

                stTy.AddMembersDelayed(fun () ->
                    let props =
                      fieldsFor i |> List.mapi (fun idx fieldInfo ->
                          let (_,ty) = typeForBondType fieldInfo.fieldType
                          ctxt.ProvidedProperty(fieldInfo.metadata.name, ty,
                                                getterCode = fun [this] -> QExpr.TupleGet(QExpr.Coerce(this, tupTys.[uint16 i].Value), idx)) :> MemberInfo)
                    let unitVal = QExpr.Value(null, typeof<unit>)

                    let rec mkFnTy (dom::tys) =
                        let rng =
                            match tys with
                            | [rng] -> rng
                            | l -> mkFnTy l
                        Reflection.FSharpType.MakeFunctionType(dom, rng)

                    let relatedStructs =
                        structsFrom schemaContents (uint16 i)
                        |> Set.add (uint16 i)

                    let taggedDeserializerVars =
                        relatedStructs
                        |> Seq.map (fun i -> i, Quotations.Var(sprintf "tagged_read%i" i, mkFnTy [typeof<ITaggedProtocolReader>; tupTys.[i].Value]))
                        |> dict

                    let untaggedDeserializerVars =
                        relatedStructs
                        |> Seq.map (fun i -> i, Quotations.Var(sprintf "untagged_read%i" i, mkFnTy [typeof<IUntaggedProtocolReader>; tupTys.[i].Value]))
                        |> dict

                    let serializerVars =
                        relatedStructs
                        |> Seq.map (fun i -> i, Quotations.Var(sprintf "write%i" i, mkFnTy [typeof<IProtocolWriter>; tupTys.[i].Value; typeof<unit>]))
                        |> dict

                    let allSequential = function
                        | [] -> <@@ () @@>
                        | xs -> List.reduce (fun e1 e2 -> QExpr.Sequential(e1, <@@ %%e2 : unit @@>)) xs

                    let serializers =
                        [for (KeyValue(idx, serializerVar)) in serializerVars ->
                            let writeVarExprs = [for i in relatedStructs ->
                                                    i,
                                                    fun wrtr e ->
                                                        QExpr.Application(QExpr.Application(QExpr.Var serializerVars.[i], wrtr), QExpr.Coerce(e, tupTys.[i].Value))] |> dict
//                          let write (ipw : IProtocolWriter) =
//                              ipw.WriteStructBegin(structMeta)
//
//                              if tupGet 1 <> def then
//                                  ipw.WriteFieldBegin(BondDataType.BT_INT32, 1, valueMeta)
//                                  ipw.WriteInt32(tupGet n)
//                                  ipw.WriteFieldEnd()
//                              else
//                                  ipw.WriteFieldOmitted(BondDataType.BT_INT32, 1, valueMeta)
//
//                              if (tupGet 2).Count <> 0 then
//                                  ipw.WriteFieldBegin(BondDataType.BT_LIST, 2, childrenMeta)
//                                  // loop
//                                  ipw.WriteFieldEnd()
//                              else
//                                  ipw.WriteFieldOmitted(BondDataType.BT_INT32, 2, childrenMeta)
//
//                              ipw.WriteStructEnd(false)
//                          write wrtr


                            let writer = Quotations.Var("wrtr", typeof<IProtocolWriter>)
                            let value = Quotations.Var("value", tupTys.[idx].Value)
                            let serializerExpr =
                                QExpr.Lambda(writer,
                                    QExpr.Lambda(value,
                                        let writer = QExpr.Cast<IProtocolWriter>(QExpr.Var writer)
                                        let write = Quotations.Var("write", typeof<IProtocolWriter -> unit>)
                                        QExpr.Let(
                                            write,
                                            (let ipw = Quotations.Var("ipw", typeof<IProtocolWriter>)
                                             QExpr.Lambda(ipw,
                                                let ipw = QExpr.Cast<IProtocolWriter>(QExpr.Var ipw)

                                                QExpr.Sequential(
                                                    let writeBegin = <@ (%ipw).WriteStructBegin((%SchemaQuotation.quoteMetadata schemaContents.structs.[int idx].metadata)) @>
                                                    let writeFields =
                                                        fieldsFor (int idx)
                                                        |> List.mapi (fun idx fieldInfo ->
                                                                        let id = fieldInfo.id
                                                                        let elt = QExpr.TupleGet(QExpr.Var value, idx)

                                                                        let bondTyIsContainer = function
                                                                        | BondDataType.BT_LIST | BondDataType.BT_SET | BondDataType.BT_MAP -> true
                                                                        | _ -> false

                                                                        let cond =  // val <> default  (or val.Count <> 0)
                                                                            if not (bondTyIsContainer fieldInfo.fieldType.id) then
                                                                                let (Quotations.Patterns.Call(_,neq,[_;_])) = <@ 1 <> 2 @>
                                                                                QExpr.Call(neq.GetGenericMethodDefinition().MakeGenericMethod(fieldInfo.defaultExpr.Type), [elt; fieldInfo.defaultExpr])
                                                                            else
                                                                                // not {List,Set,Map}.isEmpty
                                                                                let (Quotations.Patterns.Call(None,m,[_])) =
                                                                                    match fieldInfo.fieldType.id with
                                                                                    | BondDataType.BT_LIST -> <@ List.isEmpty [] @>
                                                                                    | BondDataType.BT_SET -> <@ Set.isEmpty Set.empty @>
                                                                                    | BondDataType.BT_MAP -> <@ Map.isEmpty Map.empty @>
                                                                                <@@ not (%%QExpr.Call(m.GetGenericMethodDefinition().MakeGenericMethod(elt.Type.GetGenericArguments()), [elt])) @@>

                                                                        let bondTy = fieldInfo.fieldType.id
                                                                        let writeField =
                                                                            <@ (%ipw).WriteFieldBegin(bondTy, id, %SchemaQuotation.quoteMetadata fieldInfo.metadata)
                                                                               (%%TP.writerForBondType provTys fieldInfo.fieldType ipw elt writeVarExprs)
                                                                               (%ipw).WriteFieldEnd() @>
                                                                        if fieldInfo.metadata.modifier <> Modifier.Optional then
                                                                            // if the field is required, always write it
                                                                            writeField
                                                                        else
                                                                            // otherwise, perform default check to see if we must write it
                                                                            <@ if %%cond then
                                                                                    %writeField
                                                                                else
                                                                                    (%ipw).WriteFieldOmitted(bondTy, id, %SchemaQuotation.quoteMetadata fieldInfo.metadata) @>)
                                                    writeBegin :: writeFields |> List.reduce (fun q1 q2 -> <@ %q1; %q2 @>),
                                                    <@ (%ipw).WriteStructEnd() @>))),

                                            let write = QExpr.Cast<IProtocolWriter->unit>(QExpr.Var write)
                                            <@ (%write) %writer @>)))
                            serializerVar, serializerExpr]

                    let NewTuple_ (expr : Quotations.Expr list) =
                        // BUGBUG: QExpr.NewTuple does not create a Tuple when called with one argument, this appear to be F# compiler bug
                        let tupTy = [| for e in expr -> e.Type |]
                                    |> Reflection.FSharpType.MakeTupleType
                        if expr.Length > 1 then
                            QExpr.NewTuple(expr)
                        else
                            QExpr.NewObject(tupTy.GetConstructors().[0], expr)

                    let structFieldVarsAndVals idx =
                        fieldsFor (int idx)
                        |> List.map (fun fieldInfo ->
                            let e = fieldInfo.defaultExpr
                            let refTy = typedefof<_ ref>.MakeGenericType(e.Type)
                            let (Quotations.Patterns.Call(None,refGet,[_])) = <@ !(ref 0) @>
                            let var = Quotations.Var(fieldInfo.metadata.name, refTy)
                            var, QExpr.NewRecord(refTy, [e]), QExpr.Call(refGet.GetGenericMethodDefinition().MakeGenericMethod(e.Type), [QExpr.Var var]))
                        |> List.toArray

                    // inline fieldswitch instead of having it be a function
                    let simplify e =
                        let rec simplify = function
                        | Quotations.Patterns.Application(Quotations.Patterns.Lambda(v,e), Quotations.Patterns.Var v') ->
                            true, e.Substitute(fun v'' -> if v'' = v then Some(QExpr.Var v') else None) |> simplify |> snd
                        | Quotations.Patterns.Application(f,b) as e ->
                            let sf, ef = simplify f
                            let sb, eb = simplify b
                            if sf || sb then true, QExpr.Application(ef, eb) |> simplify |> snd
                            else false, e
                        | Quotations.ExprShape.ShapeLambda(v,b) as e ->
                            let sb, eb = simplify b
                            sb, if sb then QExpr.Lambda(v, eb) else e
                        | Quotations.ExprShape.ShapeCombination(o, l) as e ->
                            let l' = List.map simplify l
                            if List.exists fst l' then true, Quotations.ExprShape.RebuildShapeCombination(o, List.map snd l') |> simplify |> snd
                            else false, e
                        | Quotations.ExprShape.ShapeVar _ as e -> false, e
                        simplify e |> snd

                    let taggedDeserializers =
                        [for (KeyValue(idx, deserializerVar)) in taggedDeserializerVars ->
                            let makeVarExprs = [for i in relatedStructs ->
                                                    i,
                                                    fun rdr ->
                                                        QExpr.Application(QExpr.Var taggedDeserializerVars.[i], rdr)] |> dict

//                          rdr.ReadStructBegin()
//                          let rec loop() =
//                              let ty,id = Helpers.ReadFieldBegin rdr
//                              if ty <> BondDataType.BT_STOP && ty <> BondDataType.BT_STOP_BASE then
//                                  if id = 1us then
//                                      readValue()
//                                  elif id = 2us then
//                                      readChildren()
//                                  else
//                                      rdr.Skip(ty)
//                                  rdr.ReadFieldEnd()
//                                  loop()
//                          loop()


// TODO: need to handle required fields:
//          add throwing "else" blocks to "if not ..."
//          add bitarray writing within loop and checking after

                            let reader = Quotations.Var("rdr", typeof<ITaggedProtocolReader>)

                            let fieldVarsAndVals = structFieldVarsAndVals idx

                            let expr =
                                let reader = QExpr.Cast<ITaggedProtocolReader>(QExpr.Var reader)

                                let readFieldFns =
                                    fieldsFor (int idx)
                                    |> List.mapi (fun fldIdx fieldInfo ->
                                        let fn = Quotations.Var(sprintf "read_%s" fieldInfo.metadata.name, typeof<unit->unit>)
                                        let read = TP.taggedReader provTys fieldInfo.fieldType reader makeVarExprs
                                        fn, QExpr.Lambda(Quotations.Var("_", typeof<unit>),
                                                let (Quotations.Patterns.Call(None,refSet,[_;_])) = <@ ref 0 := 0 @>
                                                let (var,_,_) = fieldVarsAndVals.[fldIdx]
                                                QExpr.Call(refSet.GetGenericMethodDefinition().MakeGenericMethod(fst (typeForBondType fieldInfo.fieldType)), [QExpr.Var var; read])))
                                    |> List.toArray

                                let fieldSwitch =
                                    let tyVar = Quotations.Var("ty", typeof<BondDataType>)
                                    let idVar = Quotations.Var("id", typeof<uint16>)
                                    QExpr.Lambda(tyVar,
                                        QExpr.Lambda(idVar,
                                            let fn =
                                                fieldsFor (int idx)
                                                |> List.mapi (fun fldIdx fieldInfo ->
                                                    fun ty id' next ->
                                                        let id = fieldInfo.id
                                                        // TODO: throw exception if expected and actual type differ?
                                                        QExpr.IfThenElse(<@ %id' = id @>, <@ (%%QExpr.Var (fst readFieldFns.[fldIdx])) () : unit @>, next))
                                                |> List.fold (fun e f ty id -> f ty id (e ty id)) (fun ty _ -> <@@ (%reader).Skip(%ty) @@>)
                                            fn (tyVar |> QExpr.Var |> QExpr.Cast) (idVar |> QExpr.Var |> QExpr.Cast)))

                                let body =
                                    <@@ (%reader).ReadStructBegin()
                                        let rec loop() =
                                            let ty,id = RuntimeHelpers.ReadFieldBegin %reader
                                            if ty <> BondDataType.BT_STOP && ty <> BondDataType.BT_STOP_BASE then
                                                (%%fieldSwitch) ty id
                                                (%reader).ReadFieldEnd()
                                                loop()
                                        loop()
                                        (%reader).ReadStructEnd() @@>

                                readFieldFns
                                |> Array.fold (fun b (v,e) -> QExpr.Let(v,e,b)) body
                                |> simplify

                            let deserializerExpr =
                                QExpr.Lambda(reader,
                                    fieldVarsAndVals
                                    |> Array.fold (fun e (var,def,_) -> QExpr.Let(var, def, e)) (QExpr.Sequential(expr, NewTuple_(fieldVarsAndVals |> Array.map (fun (_,_,getVal) -> getVal) |> List.ofArray))))

                            deserializerVar, deserializerExpr]


                    let untaggedDeserializers =
                        [for (KeyValue(idx,deserializerVar)) in untaggedDeserializerVars ->
                            let makeVarExprs = [for i in relatedStructs ->
                                                    i,
                                                    fun rdr ->
                                                        QExpr.Application(QExpr.Var untaggedDeserializerVars.[i], rdr)] |> dict

                            let reader = Quotations.Var("rdr", typeof<IUntaggedProtocolReader>)

                            let fieldVarsAndVals = structFieldVarsAndVals idx

                            let expr =
                                let reader = QExpr.Cast<IUntaggedProtocolReader>(QExpr.Var reader)

                                let readFieldFns =
                                    fieldsFor (int idx)
                                    |> List.mapi (fun fldIdx fieldInfo ->
                                        let fn = Quotations.Var(sprintf "read_%s" fieldInfo.metadata.name, typeof<unit->unit>)
                                        let read = TP.untaggedReader provTys fieldInfo.fieldType reader makeVarExprs
                                        fn, QExpr.Lambda(Quotations.Var("_", typeof<unit>),
                                                let (Quotations.Patterns.Call(None,refSet,[_;_])) = <@ ref 0 := 0 @>
                                                let (var,_,_) = fieldVarsAndVals.[fldIdx]
                                                QExpr.Call(refSet.GetGenericMethodDefinition().MakeGenericMethod(fst (typeForBondType fieldInfo.fieldType)), [QExpr.Var var; read])))
                                    |> List.toArray

                                let body =
                                    [for (v,_) in readFieldFns ->
                                        <@@ if not ((%reader).ReadFieldOmitted()) then
                                                (%%QExpr.Var v) ()  @@>]
                                    |> allSequential

                                readFieldFns
                                |> Array.fold (fun b (v,e) -> QExpr.Let(v,e,b)) body

                            let deserializerExpr =
                                QExpr.Lambda(reader,
                                    fieldVarsAndVals
                                    |> Array.fold (fun e (var,def,_) -> QExpr.Let(var, def, e)) (QExpr.Sequential(expr, NewTuple_(fieldVarsAndVals |> Array.map (fun (_,_,getVal) -> getVal) |> List.ofArray))))

                            deserializerVar, deserializerExpr]

                    let createInstance args =
                        List.zip args (fieldsFor i)
                        |> List.map (fun (arg:Quotations.Expr, f) ->
                            if f.defaultValue = null then
                                // <@ if (arg :> obj) = null then defaultExpr else arg @>
                                // Note that we can't use the generic equality test at the actual arg type or it will throw a null reference exception, thanks to F#'s non-nullable type checking
                                QExpr.IfThenElse(<@ %%QExpr.Coerce(arg, typeof<obj>) = null @>, f.defaultExpr, arg)
                            else
                                arg)
                        |> NewTuple_

                    props @ [ctxt.ProvidedConstructor(
                                [for (:? PropertyInfo as pi), fi in Seq.zip props (fieldsFor i) -> ProvidedParameter(pi.Name, pi.PropertyType, optionalValue = fi.defaultValue)],
                                invokeCode = createInstance)
                             ctxt.ProvidedMethod("DeserializeFrom",
                                                 [ctxt.ProvidedParameter("reader", typeof<ITaggedProtocolReader>)],
                                                 stTy, // isStaticMethod = true,
                                                 invokeCode = fun [rdr] -> QExpr.LetRecursive(taggedDeserializers, QExpr.Application(QExpr.Var taggedDeserializerVars.[uint16 i], rdr)))
                             ctxt.ProvidedMethod("DeserializeFrom",
                                                 [ProvidedParameter("reader", typeof<IUntaggedProtocolReader>)],
                                                 stTy, // IsStaticMethod = true,
                                                 invokeCode = fun [rdr] -> QExpr.LetRecursive(untaggedDeserializers, QExpr.Application(QExpr.Var untaggedDeserializerVars.[uint16 i], rdr)))
                             ctxt.ProvidedMethod("SerializeTo",
                                                 [ProvidedParameter("writer", typeof<IProtocolWriter>)],
                                                 typeof<unit>,
                                                 invokeCode = fun [this;wrtr] -> QExpr.LetRecursive(serializers, QExpr.Application(
                                                                                                                              QExpr.Application(QExpr.Var serializerVars.[uint16 i], wrtr),
                                                                                                                              QExpr.Coerce(this, tupTys.[uint16 i].Value))))])
                stTy))
        containerTy))
    do base.AddNamespace(ns, [schemaTy])
