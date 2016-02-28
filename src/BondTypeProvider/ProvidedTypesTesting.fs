﻿// --------------------------------------------------------------------------------------
// Helpers for writing type providers
// ----------------------------------------------------------------------------------------------

namespace ProviderImplementation.ProvidedTypesTesting

open System
open System.Collections.Generic
open System.Reflection
open System.IO
open System.Text
open Microsoft.FSharp.Core.CompilerServices
open Microsoft.FSharp.Core.Printf
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Reflection
open ProviderImplementation.ProvidedTypes

/// Simulate a real host of TypeProviderConfig 
type internal DllInfo(path: string) = 
    member x.FileName = path

/// Simulate a real host of TypeProviderConfig 
type internal TcImports(bas: TcImports option, dllInfos: DllInfo list) =
    member x.Base = bas
    member x.DllInfos = dllInfos


type internal Testing() = 

    /// Simulates a real instance of TypeProviderConfig 
    static member MakeSimulatedTypeProviderConfig (resolutionFolder: string, runtimeAssembly: string, runtimeAssemblyRefs: string list) =

        let cfg = new TypeProviderConfig(fun _ -> false)
        let (?<-) cfg prop value =
            let ty = cfg.GetType()
            match ty.GetProperty(prop,BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic) with 
            | null -> ty.GetField(prop,BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic).SetValue(cfg, value)|> ignore
            | p -> p.GetSetMethod(nonPublic = true).Invoke(cfg, [| box value |]) |> ignore
        cfg?ResolutionFolder <- resolutionFolder
        cfg?RuntimeAssembly <- runtimeAssembly
        cfg?ReferencedAssemblies <- Array.zeroCreate<string> 0

        // Fake an implementation of SystemRuntimeContainsType the shape expected by AssemblyResolver.fs.
        let dllInfos = [yield DllInfo(runtimeAssembly); for r in runtimeAssemblyRefs do yield DllInfo(r)]
        let tcImports = TcImports(Some(TcImports(None,[])),dllInfos)
        let systemRuntimeContainsType = (fun (_s:string) -> if tcImports.DllInfos.Length = 1 then true else true)
        cfg?systemRuntimeContainsType <- systemRuntimeContainsType 

        //Diagnostics.Debugger.Launch() |> ignore
        Diagnostics.Debug.Assert(cfg.GetType().GetField("systemRuntimeContainsType",BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance) <> null)
        Diagnostics.Debug.Assert(systemRuntimeContainsType.GetType().GetField("tcImports",BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance) <> null)
        Diagnostics.Debug.Assert(typeof<TcImports>.GetField("dllInfos",BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance) <> null)
        Diagnostics.Debug.Assert(typeof<TcImports>.GetProperty("Base",BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance) <> null)
        Diagnostics.Debug.Assert(typeof<DllInfo>.GetProperty("FileName",BindingFlags.NonPublic ||| BindingFlags.Public ||| BindingFlags.Instance) <> null)

        cfg

    /// Simulates a real instance of TypeProviderConfig and then creates an instance of the last
    /// type provider added to a namespace by the type provider constructor
    static member GenerateProvidedTypeInstantiation (resolutionFolder: string, runtimeAssembly: string, runtimeAssemblyRefs: string list, typeProviderForNamespacesConstructor, args) =
        printfn "TESTING: Generating one type, resolutionFolder = %s, runtimeAssembly = %s, runtimeAssemblyRefs = %A, args = %A" resolutionFolder runtimeAssembly runtimeAssemblyRefs args

        let cfg = Testing.MakeSimulatedTypeProviderConfig (resolutionFolder, runtimeAssembly, runtimeAssemblyRefs) 

        let typeProviderForNamespaces = typeProviderForNamespacesConstructor cfg :> TypeProviderForNamespaces

        let providedTypeDefinition = typeProviderForNamespaces.Namespaces |> Seq.last |> snd |> Seq.last

        match args with
        | [||] -> providedTypeDefinition
        | args ->
            let typeName =
                if providedTypeDefinition.IsErased then
                    providedTypeDefinition.Name + (args |> Seq.map (fun s -> ",\"" + (if s = null then "" else s.ToString()) + "\"") |> Seq.reduce (+))
                else
                    // The type name ends up quite mangled in the dll output if we combine the name using static parameters, so for generated types we don't do that
                    providedTypeDefinition.Name 
            providedTypeDefinition.MakeParametricType(typeName, args)

    /// Returns a string representation of the signature (and optionally also the body) of all the
    /// types generated by the type provider up to a certain depth and width
    /// If ignoreOutput is true, this will still visit the full graph, but it will output an empty string to be faster
    static member FormatProvidedType (t: ProvidedTypeDefinition, ?signatureOnly, ?ignoreOutput, ?maxDepth, ?maxWidth, ?useQualifiedNames) = 

        let signatureOnly = defaultArg signatureOnly false
        let ignoreOutput = defaultArg ignoreOutput false
        let maxDepth = defaultArg maxDepth 10
        let maxWidth = defaultArg maxWidth 100
        let useQualifiedNames = defaultArg useQualifiedNames false

        let knownNamespaces = 
            [ t.Namespace
              "Microsoft.FSharp.Core"
              "Microsoft.FSharp.Core.Operators"
              "Microsoft.FSharp.Collections"
              "Microsoft.FSharp.Control"
              "Microsoft.FSharp.Text" ]
            |> Set.ofSeq

        let pending = new Queue<_>()
        let visited = new HashSet<_>()

        let add t =
            if visited.Add t then
                pending.Enqueue t

        let fullName (t: Type) =
            let fullName = 
                if useQualifiedNames && not (t :? ProvidedTypeDefinition) then
                    t.AssemblyQualifiedName 
                else t.Namespace + "." + t.Name
            if fullName.StartsWith "FSI_" then
                fullName.Substring(fullName.IndexOf('.') + 1)
            else
                fullName

        let rec toString useFullName (t: Type) =

            let hasUnitOfMeasure = t.Name.Contains("[")

            let innerToString (t: Type) =
                match t with
                | t when t = typeof<bool> -> "bool"
                | t when t = typeof<obj> -> "obj"
                | t when t = typeof<int> -> "int"
                | t when t = typeof<int64> -> "int64"
                | t when t = typeof<float> -> "float"
                | t when t = typeof<float32> -> "float32"
                | t when t = typeof<decimal> -> "decimal"
                | t when t = typeof<string> -> "string"
                | t when t = typeof<Void> -> "()"
                | t when t = typeof<unit> -> "()"
                | t when t.IsArray -> (t.GetElementType() |> toString useFullName) + "[]"
                | :? ProvidedTypeDefinition as t ->
                    add t
                    t.Name.Split(',').[0]
                | t when t.IsGenericType ->
                    let args =
                        if useFullName then
                            t.GetGenericArguments() 
                            |> Seq.map (if hasUnitOfMeasure then (fun t -> t.Name) else toString useFullName)
                        else
                            t.GetGenericArguments() 
                            |> Seq.map (fun _ -> "_")
                    if FSharpType.IsTuple t then
                        String.concat " * " args
                    elif t.Name.StartsWith "FSharpFunc`" then
                        "(" + (String.concat " -> " args) + ")"
                    else 
                        let args = String.concat "," args
                        let name, reverse = 
                            match t with
                            | t when hasUnitOfMeasure -> toString useFullName t.UnderlyingSystemType, false
                            // Short names for some known generic types 
                            | t when not useQualifiedNames && t.GetGenericTypeDefinition().Name = typeof<int seq>.GetGenericTypeDefinition().Name -> "seq", true
                            | t when not useQualifiedNames && t.GetGenericTypeDefinition().Name = typeof<int list>.GetGenericTypeDefinition().Name -> "list", true
                            | t when not useQualifiedNames && t.GetGenericTypeDefinition().Name = typeof<int option>.GetGenericTypeDefinition().Name -> "option", true
                            | t when not useQualifiedNames && t.GetGenericTypeDefinition().Name = typeof<int ref>.GetGenericTypeDefinition().Name -> "ref", true
                            | t when not useQualifiedNames && t.Name = "FSharpAsync`1" -> "Async", true
                            // Short names for types in F# namespaces
                            | t when not useQualifiedNames && knownNamespaces.Contains t.Namespace -> t.Name, false
                            | t -> (if useFullName then fullName t else t.Name), false
                        let name = name.Split('`').[0]
                        if reverse then
                            args + " " + name 
                        else
                            name + "<" + args + ">"
                // Short names for types in F# namespaces
                | t when not useQualifiedNames && knownNamespaces.Contains t.Namespace -> t.Name
                // Short names for generic parameters
                | t when t.IsGenericParameter -> t.Name
                | t -> if useFullName then fullName t else t.Name

            let rec warnIfWrongAssembly (t:Type) =
                match t with
                | :? ProvidedTypeDefinition -> ""
                | t when t.IsGenericType -> defaultArg (t.GetGenericArguments() |> Seq.map warnIfWrongAssembly |> Seq.tryFind (fun s -> s <> "")) ""
                | t when t.IsArray -> warnIfWrongAssembly <| t.GetElementType()
                | t -> if not t.IsGenericParameter && t.Assembly = Assembly.GetExecutingAssembly() then " [DESIGNTIME]" else ""

            if ignoreOutput then
                ""
            elif hasUnitOfMeasure || t.IsGenericParameter || t.DeclaringType = null then
                innerToString t + (warnIfWrongAssembly t)
            else
                (toString useFullName t.DeclaringType) + "+" + (innerToString t) + (warnIfWrongAssembly t)

        let toSignature (parameters: ParameterInfo[]) =
            if parameters.Length = 0 then
                "()"
            else
                parameters 
                |> Seq.map (fun p -> p.Name + ":" + (toString true p.ParameterType))
                |> String.concat " -> "

        let printExpr expr =

            let sb = StringBuilder ()
            let print (str:string) = sb.Append(str) |> ignore
        
            let getCurrentIndent() =
                let lastEnterPos = sb.ToString().LastIndexOf('\n')
                if lastEnterPos = -1 then sb.Length + 4 else sb.Length - lastEnterPos - 1

            let breakLine indent = 
                print "\n"
                print (new String(' ', indent))

            let isBigExpression = function
            | Let _ | NewArray _ | NewTuple _ -> true
            | _ -> false

            let inline getAttrs attrName m = 
                ( ^a : (member GetCustomAttributesData : unit -> IList<CustomAttributeData>) m)
                |> Seq.filter (fun attr -> attr.Constructor.DeclaringType.Name = attrName) 

            let inline hasAttr attrName m = 
                not (Seq.isEmpty (getAttrs attrName m))

            let rec printSeparatedByCommas exprs = 
                match exprs with
                | [] -> ()
                | e::es ->
                    printExpr false true e
                    for e in es do
                        print ", "
                        printExpr false true e
                     
            and printCall fromPipe printName (mi:MethodInfo) args = 
                if fromPipe && List.length args = 1 then
                    printName()
                elif not (hasAttr "CompilationArgumentCountsAttribute" mi) then
                    printName()
                    match args with
                    | [] -> print "()"
                    | arg::args ->
                        print "("
                        let indent = getCurrentIndent()
                        printExpr false true arg
                        for arg in args do
                            print ", "
                            if isBigExpression arg then
                                breakLine indent
                            printExpr false true arg
                        print ")"
                else
                    print "("
                    printName()
                    for arg in args do
                      print " "
                      printExpr false true arg
                    print ")"

            and printExpr fromPipe needsParens = function
                | Call (instance, mi, args) ->
                    if mi.Name = "GetArray" && mi.DeclaringType.FullName = "Microsoft.FSharp.Core.LanguagePrimitives+IntrinsicFunctions" then
                        printExpr false true args.Head
                        print ".["
                        printExpr false true args.Tail.Head
                        print "]"
                    elif mi.DeclaringType.IsGenericType && mi.DeclaringType.GetGenericTypeDefinition().Name = typeof<int option>.GetGenericTypeDefinition().Name then
                        if args.IsEmpty then 
                            match instance with
                            | None -> print "None"
                            | Some instance -> 
                                printExpr false true instance
                                print "."
                                print <| mi.Name.Substring("get_".Length)
                        else 
                          print "Some "
                          printExpr false true args.Head
                    elif mi.Name.Contains "." && not args.IsEmpty then
                        // instance method in type extension
                        let printName() = 
                            printExpr false true args.Head
                            print "."
                            print (mi.Name.Substring(mi.Name.IndexOf '.' + 1))
                        printCall fromPipe printName mi args.Tail
                    elif mi.Attributes &&& MethodAttributes.SpecialName = MethodAttributes.SpecialName && mi.Name.StartsWith "get_" && args.IsEmpty then
                        // property get
                        match instance with
                        | Some expr -> printExpr false true expr
                        | None -> print (toString false mi.DeclaringType)
                        print "."
                        print <| mi.Name.Substring("get_".Length)
                    elif mi.Name = "op_PipeRight" && args.Length = 2 then
                        printExpr false false args.Head
                        print " |> "
                        match args.Tail.Head with
                        | Lambda (_, (Call(_,_,_) as call)) -> printExpr true false call
                        | _ as expr -> printExpr false false expr
                    else
                        let printName() =
                            match instance with
                            | Some expr -> printExpr false true expr
                            | None -> print (toString false mi.DeclaringType)
                            print "."
                            print mi.Name
                        let isOptional (arg:Expr, param:ParameterInfo) =
                            hasAttr "OptionalArgumentAttribute" param
                            && arg.ToString() = "Call (None, get_None, [])"
                        let args = 
                            mi.GetParameters()
                            |> List.ofArray 
                            |> List.zip args
                            |> List.filter (not << isOptional)
                            |> List.map fst                        
                        printCall fromPipe printName mi args
                | Let (var1, TupleGet (Var x, 1), Let (var2, TupleGet (Var y, 0), body)) when x = y ->
                    let indent = getCurrentIndent()
                    bprintf sb "let %s, %s = %s" var2.Name var1.Name x.Name
                    breakLine indent
                    printExpr false false body
                | Let (var, value, body) ->
                    let indent = getCurrentIndent()
                    let usePattern = sprintf "IfThenElse(TypeTest(IDisposable,Coerce(%s,Object)),Call(Some(Call(None,UnboxGeneric,[Coerce(%s,Object)])),Dispose,[]),Value(<null>))" var.Name var.Name
                    let body = 
                        match body with
                        | TryFinally (tryExpr, finallyExpr) when finallyExpr.ToString().Replace("\n", null).Replace(" ", null) = usePattern ->
                            bprintf sb "use %s = " var.Name
                            tryExpr
                        | _ -> 
                            if var.IsMutable then
                                bprintf sb "let mutable %s = " var.Name
                            else
                                bprintf sb "let %s = " var.Name
                            body
                    match value with 
                    | Let _ -> 
                        breakLine (indent + 4)
                        printExpr false false value
                    | _ -> printExpr false false value
                    breakLine indent
                    printExpr false false body
                | Value (null, _) ->
                    print "null"
                | Value (value, typ) when typ = typeof<string> && (value :?> string).Contains("\\") ->
                    bprintf sb "@%A" value
                | Value (value, _) ->
                    bprintf sb "%A" value
                | Var (var) ->
                    print var.Name
                | NewObject (ci, args) ->
                    let getSourceConstructFlags (attr:CustomAttributeData) =
                        let arg = attr.ConstructorArguments
                                  |> Seq.filter (fun arg -> arg.ArgumentType.Name = "SourceConstructFlags") 
                                  |> Seq.head
                        arg.Value :?> int
                    let compilationMappings = getAttrs "CompilationMappingAttribute" ci.DeclaringType
                    if not (Seq.isEmpty compilationMappings) && (getSourceConstructFlags (Seq.head compilationMappings)) = int SourceConstructFlags.RecordType then
                        print "{ "
                        let indent = getCurrentIndent()
                        let recordFields = FSharpType.GetRecordFields(ci.DeclaringType)
                        args |> List.iteri (fun i arg ->
                            if i > 0 then
                                breakLine indent
                            print recordFields.[i].Name
                            print " = "
                            printExpr false false arg)
                        print " }"
                    else
                        print "(new "
                        print (toString false ci.DeclaringType)
                        print "("
                        printSeparatedByCommas args
                        print "))"
                | NewDelegate (typ, vars, expr) ->
                    print "new "
                    print (toString false typ)
                    match expr with
                    | Var v when not vars.IsEmpty && vars.Tail.IsEmpty && vars.Head = v -> print "(id)"
                    | _ ->
                        let indent = getCurrentIndent()
                        if vars.IsEmpty then
                            print "(fun () -> "
                        else
                            print "(fun"
                            for var in vars do
                                bprintf sb " (%s:%s)" var.Name (toString false var.Type)
                            print " -> "
                        if isBigExpression expr then
                            breakLine (indent + 4)
                            printExpr false false expr
                        else
                            printExpr false false expr
                    print ")"
                | NewTuple (exprs) ->
                    if needsParens then print "("
                    let indent = getCurrentIndent()
                    printExpr false true exprs.Head
                    for e in exprs.Tail do
                        print ","
                        breakLine indent
                        printExpr false true e
                    if needsParens then print ")"
                | NewArray (_, exprs) ->
                    if exprs.Length = 0 then print "[| |]"
                    else
                        print "[| "
                        let indent = getCurrentIndent()
                        printExpr false true exprs.Head
                        for e in exprs.Tail do
                            breakLine indent
                            printExpr false true e
                        print " |]"
                | Coerce (expr, typ) ->
                    print "("
                    printExpr false false expr
                    print " :> "
                    print (toString false typ)
                    print ")"
                | TupleGet (expr, index) ->
                    print "(let "
                    let rec getTupleLength (typ:Type) =
                        let length = typ.GetGenericArguments().Length
                        if length = 0 then // happens in the Apiary provider                            
                            let typeNameSuffix = typ.Name.Substring(typ.Name.IndexOf('`') + 1)
                            typeNameSuffix.Substring(0, typeNameSuffix.IndexOf('[')) |> Int32.Parse
                        else
                            let lastItem = typ.GetGenericArguments() |> Seq.last
                            if lastItem.Name.StartsWith "Tuple`"
                            then length + getTupleLength lastItem - 1
                            else length
                    let tupleLength = getTupleLength expr.Type
                    let varName = "t" + (string (index + 1))
                    for i in 0..tupleLength-1 do
                        if i = index then
                            print varName
                        else
                            print "_"
                        if i <> tupleLength-1 then
                            print ","
                    print " = "
                    printExpr false false expr
                    print (" in " + varName + ")")
                | expr -> print (expr.ToString())

            printExpr false false expr
            sb.ToString()

        let sb = StringBuilder ()

        let print (str: string) =
            if not ignoreOutput then
                sb.Append(str) |> ignore
        
        let println() =
            if not ignoreOutput then
                sb.AppendLine() |> ignore
              
        let printMember (memberInfo: MemberInfo) =        

            let print str =
                print "    "                
                print str
                println()

            let getMethodBody (m: ProvidedMethod) = 
                seq { if not m.IsStatic then yield (ProvidedTypeDefinition.EraseType m.DeclaringType)
                      for param in m.GetParameters() do yield (ProvidedTypeDefinition.EraseType param.ParameterType) }
                |> Seq.map (fun typ -> Expr.Value(null, typ))
                |> Array.ofSeq
                |> m.GetInvokeCodeInternal false

            let getConstructorBody (c: ProvidedConstructor) = 
                if c.IsImplicitCtor then Expr.Value(()) else
                seq { for param in c.GetParameters() do yield (ProvidedTypeDefinition.EraseType param.ParameterType) }
                |> Seq.map (fun typ -> Expr.Value(null, typ))
                |> Array.ofSeq
                |> c.GetInvokeCodeInternal false

            let printExpr x = 
                if not ignoreOutput then
                    let rec removeParams x = 
                      match x with
                      | Let (_, Value(null, _), body) -> removeParams body
                      | _ -> x
                    let formattedExpr = printExpr (removeParams x)
                    print formattedExpr
                    println()

            let printObj x = 
                if ignoreOutput then 
                    ""
                else 
                    sprintf "\n%O\n" x

            let getName (m:MemberInfo) = 
                if memberInfo.Name.Contains(" ") then
                    "``" + m.Name + "``"
                else
                    m.Name

            match memberInfo with

            | :? ProvidedConstructor as cons -> 
                if not ignoreOutput then
                    print <| "new : " + 
                             (toSignature <| cons.GetParameters()) + " -> " + 
                             (toString true memberInfo.DeclaringType)
                if not signatureOnly then
                    cons |> getConstructorBody |> printExpr

            | :? ProvidedLiteralField as field -> 
                let value = 
                    if signatureOnly then ""
                    else field.GetRawConstantValue() |> printObj
                if not ignoreOutput then
                    print <| "val " + (getName field) + ": " + 
                             (toString true field.FieldType) + 
                             value
                         
            | :? ProvidedProperty as prop -> 
                if not ignoreOutput then
                    print <| (if prop.IsStatic then "static " else "") + "member " + 
                             (getName prop) + ": " + (toString true prop.PropertyType) + 
                             " with " + (if prop.CanRead && prop.CanWrite then "get, set" else if prop.CanRead then "get" else "set")
                if not signatureOnly then
                    if prop.CanRead then
                        getMethodBody (prop.GetGetMethod() :?> ProvidedMethod) |> printExpr
                    if prop.CanWrite then
                        getMethodBody (prop.GetSetMethod() :?> ProvidedMethod) |> printExpr

            | :? ProvidedMethod as m ->
                if m.Attributes &&& MethodAttributes.SpecialName <> MethodAttributes.SpecialName then
                    if not ignoreOutput then
                        print <| (if m.IsStatic then "static " else "") + "member " + 
                        (getName m) + ": " + (toSignature <| m.GetParameters()) + 
                        " -> " + (toString true m.ReturnType)
                    if not signatureOnly then
                        m |> getMethodBody |> printExpr

            | _ -> ()

        add t

        let currentDepth = ref 0

        while pending.Count <> 0 && !currentDepth <= maxDepth do
            let pendingForThisDepth = new List<_>(pending)
            pending.Clear()
            let pendingForThisDepth = 
                pendingForThisDepth
                |> Seq.sortBy (fun m -> m.Name)
                |> Seq.truncate maxWidth
            for t in pendingForThisDepth do
                //Disabled because not working on Mono
                //for attr in t.GetCustomAttributesData() do
                //     print <| (sprintf "[<%A>]" attr).Replace("Microsoft.FSharp.Core.", null).Replace("CompilerServices.", null).Replace("Attribute(", "(")
                //     println()
                match t with
                | t when FSharpType.IsRecord t-> "record "
                | t when FSharpType.IsModule t -> "module "
                | t when t.IsValueType -> "struct "
                | t when t.IsClass && t.IsSealed && t.IsAbstract -> "static class "
                | t when t.IsClass && t.IsAbstract -> "abstract class "
                | t when t.IsClass -> "class "
                | _ -> ""
                |> print
                print (toString true t)
                if t.BaseType <> null && t.BaseType <> typeof<obj> then
                    print " : "
                    print (toString true t.BaseType)
                println()
                t.GetMembers() 
                |> Seq.sortBy (fun m -> m.Name)
                |> Seq.iter printMember
                println()
            currentDepth := !currentDepth + 1
    
        sb.ToString()


module internal Targets = 

    let private (++) a b = System.IO.Path.Combine(a,b)
    
    let runningOnMono = Type.GetType("Mono.Runtime") <> null
    let runningOnMac = 
        (Environment.OSVersion.Platform = PlatformID.MacOSX)
        || (Environment.OSVersion.Platform = PlatformID.Unix) && Directory.Exists("/Applications") && Directory.Exists("/System") && Directory.Exists("/Users") && Directory.Exists("/Volumes")
    let runningOnLinux = 
        (Environment.OSVersion.Platform = PlatformID.Unix) && not runningOnMac

    // Assumes OSX
    let monoRoot = 
        Path.GetFullPath(Path.Combine(System.Runtime.InteropServices.RuntimeEnvironment.GetRuntimeDirectory(),".."))
        //match System.Environment.OSVersion.Platform with 
        //| System.PlatformID.MacOSX -> "/Library/Frameworks/Mono.framework/Versions/Current/lib/mono"
        //| System.PlatformID.MacOSX -> "/Library/Frameworks/Mono.framework/Versions/Current/lib/mono"
        //| _ -> 

    let referenceAssembliesPath = 
        (if runningOnMono then monoRoot else Environment.GetFolderPath Environment.SpecialFolder.ProgramFilesX86)
        ++ "Reference Assemblies" 
        ++ "Microsoft" 

    let private fsharpPortableAssembliesPath fsharp profile = 
         match fsharp, profile with 
         | "3.1", 47 -> referenceAssembliesPath ++ "FSharp" ++ ".NETPortable" ++ "2.3.5.1" ++ "FSharp.Core.dll"
         | "3.1", 7 -> referenceAssembliesPath ++ "FSharp" ++ ".NETCore" ++ "3.3.1.0" ++ "FSharp.Core.dll"
         | "3.1", 78 -> referenceAssembliesPath ++ "FSharp" ++ ".NETCore" ++ "3.78.3.1" ++ "FSharp.Core.dll"
         | "3.1", 259 -> referenceAssembliesPath ++ "FSharp" ++ ".NETCore" ++ "3.259.3.1" ++ "FSharp.Core.dll"
         | "4.0", 47 -> referenceAssembliesPath ++ "FSharp" ++ ".NETPortable" ++ "3.47.4.0" ++ "FSharp.Core.dll"
         | "4.0", 7 -> referenceAssembliesPath ++ "FSharp" ++ ".NETCore" ++ "3.7.4.0" ++ "FSharp.Core.dll"
         | "4.0", 78 -> referenceAssembliesPath ++ "FSharp" ++ ".NETCore" ++ "3.78.4.0" ++ "FSharp.Core.dll"
         | "4.0", 259 -> referenceAssembliesPath ++ "FSharp" ++ ".NETCore" ++ "3.259.4.0" ++ "FSharp.Core.dll"
         | _ -> failwith "unimplemented portable profile"

    let private fsharpAssembliesPath fsharp = 
        match fsharp with 
        | "3.1" -> 
            if runningOnMono then monoRoot ++ "gac" ++ "FSharp.Core" ++ "4.3.1.0__b03f5f7f11d50a3a"
            else referenceAssembliesPath ++ "FSharp" ++ ".NETFramework" ++ "v4.0" ++ "4.3.1.0"
        | "4.0" -> 
            if runningOnMono then monoRoot ++ "gac" ++ "FSharp.Core" ++ "4.4.0.0__b03f5f7f11d50a3a"
            else referenceAssembliesPath ++ "FSharp" ++ ".NETFramework" ++ "v4.0" ++ "4.4.0.0"
        | _ -> failwith "unimplemented portable profile"

    let private net45AssembliesPath = 
        if runningOnMono then monoRoot ++ "4.5"
        else referenceAssembliesPath ++ "Framework" ++ ".NETFramework" ++ "v4.5" 

    let private portableAssembliesPath profile = 
        let portableRoot = if runningOnMono then monoRoot ++ "xbuild-frameworks" else referenceAssembliesPath ++ "Framework"
        match profile with 
        | 47 -> portableRoot ++ ".NETPortable" ++ "v4.0" ++ "Profile" ++ "Profile47" 
        | 7 -> portableRoot ++ ".NETPortable" ++ "v4.5" ++ "Profile" ++ "Profile7" 
        | 78 -> portableRoot ++ ".NETPortable" ++ "v4.5" ++ "Profile" ++ "Profile78" 
        | 259 -> portableRoot ++ ".NETPortable" ++ "v4.5" ++ "Profile" ++ "Profile259" 
        | _ -> failwith "unimplemented portable profile"

    let private portableCoreFSharpRefs fsharp profile = 
        [ for asm in [ "System.Runtime"; "mscorlib"; "System.Collections"; "System.Core"; "System"; "System.Globalization"; "System.IO"; "System.Linq"; "System.Linq.Expressions"; 
                       "System.Linq.Queryable"; "System.Net"; "System.Net.NetworkInformation"; "System.Net.Primitives"; "System.Net.Requests"; "System.ObjectModel"; "System.Reflection"; 
                       "System.Reflection.Extensions"; "System.Reflection.Primitives"; "System.Resources.ResourceManager"; "System.Runtime.Extensions"; 
                       "System.Runtime.InteropServices.WindowsRuntime"; "System.Runtime.Serialization"; "System.Threading"; "System.Threading.Tasks"; "System.Xml"; "System.Xml.Linq"; "System.Xml.XDocument";
                       "System.Runtime.Serialization.Json"; "System.Runtime.Serialization.Primitives"; "System.Windows" ] do 
             yield portableAssembliesPath profile ++ asm + ".dll"
          yield fsharpPortableAssembliesPath fsharp profile ]

    let DotNet45Refs = [net45AssembliesPath ++ "mscorlib.dll"; net45AssembliesPath ++ "System.Xml.dll"; net45AssembliesPath ++ "System.Core.dll"; net45AssembliesPath ++ "System.Xml.Linq.dll"; net45AssembliesPath ++ "System.dll" ]
    let FSharpCoreRef fsharp = fsharpAssembliesPath fsharp ++ "FSharp.Core.dll"
    let DotNet45FSharpRefs fsharp = [ yield! DotNet45Refs; yield FSharpCoreRef fsharp ]
    let Portable47FSharpRefs fsharp = [portableAssembliesPath 47 ++ "mscorlib.dll"; portableAssembliesPath 47 ++ "System.Xml.Linq.dll"; fsharpPortableAssembliesPath fsharp 47]

    let DotNet45FSharp31Refs = DotNet45FSharpRefs "3.1"
    let Portable47FSharp31Refs = Portable47FSharpRefs "3.1"
    let Portable7FSharp31Refs = portableCoreFSharpRefs "3.1" 7
    let Portable78FSharp31Refs = portableCoreFSharpRefs "3.1" 78
    let Portable259FSharp31Refs = portableCoreFSharpRefs "3.1" 259

    let FSharpCore40Ref = FSharpCoreRef "4.0"
    let DotNet45FSharp40Refs = DotNet45FSharpRefs "4.0"
    let Portable7FSharp40Refs = portableCoreFSharpRefs "4.0" 7
    let Portable78FSharp40Refs = portableCoreFSharpRefs "4.0" 78
    let Portable259FSharp40Refs = portableCoreFSharpRefs "4.0" 259

    let supportsFSharp40 = (try File.Exists FSharpCore40Ref with _ -> false) 
    // Some tests disabled on Linux for now because the standard packages don't come with F# PCL FSharp.Core.dll for this profile
    let hasPortableFSharpCoreDLLs = not runningOnLinux