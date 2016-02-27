#I __SOURCE_DIRECTORY__
#load "./ProvidedTypes.fs"
#load "./ProvidedTypesTesting.fs"
#load "./AssemblyReader.fs"
#load "./AssemblyReaderReflection.fs"
#load "./ProvidedTypesContext.fs"
#r "./bin/Debug/Bond.dll"
#r "./bin/Debug/Bond.IO.dll"


open ProviderImplementation.ProvidedTypes
open ProviderImplementation.ProvidedTypesTesting

#load "TypeProvider.fs"
#load "TP2.fs"
#load "./BondTypeProvider.DesignTime.fs"
open Bond.TypeProvider.DesignTime
open System.Reflection
let cfg = Testing.MakeSimulatedTypeProviderConfig(__SOURCE_DIRECTORY__, typeof<BondTypeProvider>.Assembly.FullName, []) // CompilerServices.TypeProviderConfig(fun _ -> true)
let b = new BondTypeProvider(cfg) :> FSharp.Core.CompilerServices.ITypeProvider
let tpns =
  b.GetNamespaces()
  |> Seq.find (fun t -> t.NamespaceName = "Bond.TypeProvider")

let schemaTPType = tpns.ResolveTypeName("SchemaTypeProvider")
let schemaTPParams = b.GetStaticParameters(schemaTPType)

let staticParameterValues = 
  [| for x in schemaTPParams -> 
      (match x.Name with 
      | "FilePath" -> box "../../tests/BondTypeProvider.Tests/unittest.schema.Nullable"
      | "Protocol" -> box Bond.ProtocolType.MARSHALED_PROTOCOL
      | _ -> box x.RawDefaultValue) |]

let schemaTP = b.ApplyStaticArguments(schemaTPType, [|"MyScript"; "SchemaTypeProvider"|], staticParameterValues)

