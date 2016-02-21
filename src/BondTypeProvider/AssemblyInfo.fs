namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("BondTypeProvider")>]
[<assembly: AssemblyProductAttribute("BondTypeProvider")>]
[<assembly: AssemblyDescriptionAttribute("an F# Type Provider of Bond types")>]
[<assembly: AssemblyVersionAttribute("1.0")>]
[<assembly: AssemblyFileVersionAttribute("1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "1.0"
