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

#load "./QuotationsHelper.fs"
#load "./BondTypeProvider.DesignTime.fs"
open Bond.TypeProvider.DesignTime
open System.Reflection
let cfg = Testing.MakeSimulatedTypeProviderConfig(__SOURCE_DIRECTORY__, typeof<BondTypeProvider>.Assembly.FullName, []) // CompilerServices.TypeProviderConfig(fun _ -> true)
let b = new BondTypeProvider(cfg)
b.Namespaces
