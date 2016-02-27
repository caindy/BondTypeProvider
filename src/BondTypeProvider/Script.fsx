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
(*
let system = typeof<obj>.Assembly
let cfg = Testing.MakeSimulatedTypeProviderConfig(__SOURCE_DIRECTORY__, typeof<BondTypeProvider>.Assembly.FullName, [system.FullName])
let b = new BondTypeProvider(cfg) :> TypeProviderForNamespaces
let tpns =
  b.Namespaces
  |> Seq.find (fun (name, t) -> name = "Bond.TypeProvider")
  |> snd
  |> Seq.head
  *)
let bondTpAsm = typeof<BondTypeProvider>.Assembly.FullName
let args = [|box "../../tests/BondTypeProvider.Tests/unittest.schema.Nullable";
             box Bond.ProtocolType.MARSHALED_PROTOCOL |]
let refs = Targets.FSharpCore40Ref
let system = typeof<obj>.Assembly
let instance = Testing.GenerateProvidedTypeInstantiation(__SOURCE_DIRECTORY__, bondTpAsm, [refs], BondTypeProvider, args)
//let instance = tpns.MakeParametricType("SchemaTypeProvider", args)
instance |> Testing.FormatProvidedType