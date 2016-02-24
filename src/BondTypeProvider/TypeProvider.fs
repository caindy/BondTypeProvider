namespace Bond.TypeProvider.DesignTime

open System
open System.Collections.Generic
open Bond
open Bond.Protocols
open ProviderImplementation.ProvidedTypes

type   QExpr = Quotations.Expr
module QPat  = Quotations.Patterns
 
// Hard to create quotations calling methods without arguments, so wrap instead
module RuntimeHelpers =
  let ReadFieldBegin(reader : ITaggedProtocolReader) =
      reader.ReadFieldBegin()

  let ReadContainerBegin(reader : ITaggedProtocolReader) : _*_ =
      reader.ReadContainerBegin()

  let ReadKeyValueContainerBegin(reader : ITaggedProtocolReader) : _*_*_ =
      reader.ReadContainerBegin()

[<RequireQualifiedAccess>]
module internal TP =
  open FSharp.Quotations
  open Patterns
  type internal Helper =
    /// <summary>Makes a quotation of the form
    /// <code>
    ///   for v in s do
    ///     body
    /// </code>
    /// </summary>
    static member MakeFor<'a,'b when 'a :> seq<'b>>(v, s, body) =
        let q = <@@ for el in (%%s : 'a) do
                        %%body @@>
        // replace 'el' variable with v, leaving everything else the same
        // we know that q is of the form 
        //     let inputSequence = s
        //     let enumerator = inputSequence.GetEnumerator()
        //     try
        //         while enumerator.MoveNext() do
        //             let el = enumerator.Current
        //             ...
        //     finally
        //         ...
        let (Let(v0, e0, Let(v1, e1, TryFinally(WhileLoop(e2, Let(v3, e3, b3)), e4)))) = q
        Expr.Let(v0,e0,Expr.Let(v1,e1,Expr.TryFinally(Expr.WhileLoop(e2, Expr.Let(v,e3,b3)), e4)))

  module Expr =
    /// <summary>Makes a quotation of the form
    /// <code>
    ///   for v in s do
    ///     body</code></summary>
    let internal For(v:Quotations.Var, s:Quotations.Expr, b:Quotations.Expr) =
        let seqTy = s.Type
        let eltTy = seqTy.GetInterface("System.Collections.Generic.IEnumerable`1").GetGenericArguments().[0]
        typeof<Helper>.GetMethod("MakeFor", enum 0xffffffff).MakeGenericMethod(seqTy, eltTy).Invoke(null, [|v;s;b|]) :?> Expr

  /// gets the representation type and full (unerased) type corresponding to the given TypeDef
  let typeForBondType (provTys : IDictionary<uint16, ProvidedTypeDefinition>) (t' : TypeDef) =
    let rec typeForBondType' (t : TypeDef) =
      match t.id with
      | BondDataType.BT_BOOL ->    typeof<bool>   , typeof<bool>
      | BondDataType.BT_DOUBLE ->  typeof<float>  , typeof<float>
      | BondDataType.BT_FLOAT ->   typeof<float32>, typeof<float32>
      | BondDataType.BT_INT16 ->   typeof<int16>  , typeof<int16>
      | BondDataType.BT_INT32 ->   typeof<int>    , typeof<int>
      | BondDataType.BT_INT64 ->   typeof<int64>  , typeof<int64>
      | BondDataType.BT_INT8  ->   typeof<sbyte>  , typeof<sbyte>
      | BondDataType.BT_UINT16 ->  typeof<uint16> , typeof<uint16>
      | BondDataType.BT_UINT32 ->  typeof<uint32> , typeof<uint32>
      | BondDataType.BT_UINT64 ->  typeof<uint64> , typeof<uint64>
      | BondDataType.BT_UINT8  ->  typeof<byte>   , typeof<byte>
      | BondDataType.BT_STRING ->  typeof<string> , typeof<string>
      | BondDataType.BT_WSTRING -> typeof<string> , typeof<string>
      | BondDataType.BT_STRUCT -> typeof<obj>, upcast provTys.[t.struct_def]
      | BondDataType.BT_LIST ->
          let mkTy t =
              let tyDef = typedefof<_ list>
              tyDef.MakeGenericType([|t|])
          let repr, full = typeForBondType' t.element
          mkTy repr, mkTy full
      | BondDataType.BT_MAP ->
          let mkTy k v =
              let tyDef = typedefof<Map<_,_>>
              tyDef.MakeGenericType([|k;v|])
          let krepr, kfull = typeForBondType' t.key
          let vrepr, vfull = typeForBondType' t.element
          mkTy krepr vrepr, mkTy kfull vfull
      | BondDataType.BT_SET ->
          let mkTy t =
              let tyDef = typedefof<Set<_>>
              tyDef.MakeGenericType([|t|])
          let repr, full = typeForBondType' t.element
          mkTy repr, mkTy full
      | BondDataType.BT_STOP
      | BondDataType.BT_STOP_BASE
      | BondDataType.BT_UNAVAILABLE
      | _ as ty -> failwith (sprintf "Unexpected BondDataType: %A" ty)
    typeForBondType' t'
  let rec defaultValue (t:TypeDef) (v:Variant) =
      match t.id with
      | BondDataType.BT_BOOL ->   v.uint_value <> 0UL       |> box
      | BondDataType.BT_DOUBLE -> v.double_value            |> box
      | BondDataType.BT_FLOAT ->  v.double_value |> float32 |> box
      | BondDataType.BT_INT16 ->  v.int_value |> int16      |> box
      | BondDataType.BT_INT32 ->  v.int_value |> int        |> box
      | BondDataType.BT_INT64 ->  v.int_value               |> box
      | BondDataType.BT_INT8  ->  v.int_value |> sbyte      |> box
      | BondDataType.BT_UINT16 -> v.uint_value |> uint16    |> box
      | BondDataType.BT_UINT32 -> v.uint_value |> uint32    |> box
      | BondDataType.BT_UINT64 -> v.uint_value |> uint64    |> box
      | BondDataType.BT_UINT8  -> v.uint_value |> byte      |> box
      | BondDataType.BT_STRING -> v.string_value            |> box
      | BondDataType.BT_WSTRING -> v.wstring_value          |> box
      | BondDataType.BT_STRUCT
      | BondDataType.BT_LIST
      | BondDataType.BT_SET
      | BondDataType.BT_MAP    -> null
      | BondDataType.BT_STOP
      | BondDataType.BT_STOP_BASE
      | BondDataType.BT_UNAVAILABLE
      | _ as ty -> failwith (sprintf "Unexpected BondDataType: %A" ty)

  let rec defaultExpr (provTys : IDictionary<uint16, ProvidedTypeDefinition>) (t:TypeDef) (v:Variant) =
      match t.id with
      | BondDataType.BT_BOOL ->   let b = v.uint_value <> 0UL       in <@@ b @@>
      | BondDataType.BT_DOUBLE -> let f = v.double_value            in <@@ f @@>
      | BondDataType.BT_FLOAT ->  let f = v.double_value |> float32 in <@@ f @@>
      | BondDataType.BT_INT16 ->  let i = v.int_value |> int16      in <@@ i @@>
      | BondDataType.BT_INT32 ->  let i = v.int_value |> int        in <@@ i @@>
      | BondDataType.BT_INT64 ->  let i = v.int_value               in <@@ i @@>
      | BondDataType.BT_INT8  ->  let i = v.int_value |> sbyte      in <@@ i @@>
      | BondDataType.BT_UINT16 -> let i = v.uint_value |> uint16    in <@@ i @@>
      | BondDataType.BT_UINT32 -> let i = v.uint_value |> uint32    in <@@ i @@>
      | BondDataType.BT_UINT64 -> let i = v.uint_value |> uint64    in <@@ i @@>
      | BondDataType.BT_UINT8  -> let i = v.uint_value |> byte      in <@@ i @@>
      | BondDataType.BT_STRING -> let s = v.string_value            in <@@ s @@>
      | BondDataType.BT_WSTRING -> let s = v.wstring_value          in <@@ s @@>
      | BondDataType.BT_STRUCT -> <@@ null : obj @@>
      | BondDataType.BT_LIST ->
          let (ty,_) = typeForBondType provTys t
          QExpr.NewUnionCase(Reflection.FSharpType.GetUnionCases ty |> Array.find (fun uc -> uc.Name = "Empty"), [])
      | BondDataType.BT_SET ->
          let (elTy,_) = typeForBondType provTys t.element
          let (Quotations.Patterns.Call(None,emptyMethod,[])) = <@ Set.empty @>
          QExpr.Call(emptyMethod.GetGenericMethodDefinition().MakeGenericMethod(elTy), [])
      | BondDataType.BT_MAP ->
          let (keyTy,_) = typeForBondType provTys t.key
          let (elTy,_) = typeForBondType provTys t.element
          let (Quotations.Patterns.Call(None,emptyMethod,[])) = <@ Map.empty @>
          QExpr.Call(emptyMethod.GetGenericMethodDefinition().MakeGenericMethod(keyTy,elTy), [])
      | BondDataType.BT_STOP
      | BondDataType.BT_STOP_BASE
      | BondDataType.BT_UNAVAILABLE
      | _ as ty -> failwith (sprintf "Unexpected BondDataType: %A" ty)


  let zeroCreate ty = match <@ Array.zeroCreate 0 @> with | QPat.Call(None,zeroCreate,[_]) -> zeroCreate.GetGenericMethodDefinition().MakeGenericMethod([|ty|])
  let setArray ty = match <@ [| |].[0] <- 0 @> with | QPat.Call(None,setArray,[_;_;_]) -> setArray.GetGenericMethodDefinition().MakeGenericMethod([|ty|])
  let toList ty = match <@ Array.toList [| |] @> with | QPat.Call(None,toList,[_]) -> toList.GetGenericMethodDefinition().MakeGenericMethod([|ty|])
  let setOfArray ty = match <@ Set.ofArray [| |] @> with | QPat.Call(None,ofArray,[_]) -> ofArray.GetGenericMethodDefinition().MakeGenericMethod([|ty|])
  let mapOfArray keyRep elRep = match <@ Map.ofArray [| |] @> with | QPat.Call(None,ofArray,[_]) -> ofArray.GetGenericMethodDefinition().MakeGenericMethod([|keyRep; elRep|])

  let readContainer ct elRep readEl readEnd =
      // produces a quotation like:
      //    <@ let arr = Array.zeroCreate<_> (int ct)
      //       for i = 0 to ct - 1 do
      //           arr.[i] <- readEl
      //       readEnd
      //       arr@>
      let arrRep = (elRep : Type).MakeArrayType()
      let arr = Quotations.Var("arr", arrRep)
      let i = Quotations.Var("i", typeof<int>)
      QExpr.Let(arr, QExpr.Call(zeroCreate elRep, [ QExpr.Var ct ]),
          QExpr.Sequential(
              QExpr.ForIntegerRangeLoop(i, <@ 0 @>, <@ (%%(QExpr.Var ct) : int) - 1 @>,
                  QExpr.Call(setArray elRep, [QExpr.Var arr; QExpr.Var i; readEl])),
              QExpr.Sequential(
                  readEnd, QExpr.Var arr)))

  let readKeyValueContainer ct keyRep valRep readKey readVal readEnd =
      // produces a quotation like:
      //    <@ let arr = Array.zeroCreate<_*_> (int ct)
      //       for i = 0 to ct - 1 do
      //           arr.[i] <- readKey, readVal
      //       readEnd
      //       arr @>
      let tupRep = Reflection.FSharpType.MakeTupleType([|keyRep; valRep|])
      let arrRep = tupRep.MakeArrayType()
      let arr = Quotations.Var("arr", arrRep)
      let i = Quotations.Var("i", typeof<int>)
      QExpr.Let(arr, QExpr.Call(zeroCreate tupRep, [ QExpr.Var ct ]),
          QExpr.Sequential(
              QExpr.ForIntegerRangeLoop(i, <@ 0 @>, <@ (%%(QExpr.Var ct) : int) - 1 @>,
                  QExpr.Call(setArray tupRep, [QExpr.Var arr; QExpr.Var i; QExpr.NewTuple [readKey; readVal]])),
              QExpr.Sequential(
                  readEnd, QExpr.Var arr)))


  /// Given a TypeDef, an expression for an IUntaggedProtocolReader and a dictionary mapping struct IDs to readers,
  /// produces an expression that reads the corresponding type of value from a reader
  // TODO: Use ReadHelper to make this more flexible (e.g. integral promotion)
  let rec untaggedReader (provTys : IDictionary<uint16, ProvidedTypeDefinition>) (t:TypeDef) : Quotations.Expr<IUntaggedProtocolReader> -> IDictionary<uint16,_> -> _ =
      match t.id with
      | BondDataType.BT_BOOL ->    fun rdr _ -> <@@ (%rdr).ReadBool() @@>
      | BondDataType.BT_DOUBLE ->  fun rdr _ -> <@@ (%rdr).ReadDouble() @@>
      | BondDataType.BT_FLOAT ->   fun rdr _ -> <@@ (%rdr).ReadFloat() @@>
      | BondDataType.BT_INT16 ->   fun rdr _ -> <@@ (%rdr).ReadInt16() @@>
      | BondDataType.BT_INT32 ->   fun rdr _ -> <@@ (%rdr).ReadInt32() @@>
      | BondDataType.BT_INT64 ->   fun rdr _ -> <@@ (%rdr).ReadInt64() @@>
      | BondDataType.BT_INT8  ->   fun rdr _ -> <@@ (%rdr).ReadInt8() @@>
      | BondDataType.BT_UINT16 ->  fun rdr _ -> <@@ (%rdr).ReadUInt16() @@>
      | BondDataType.BT_UINT32 ->  fun rdr _ -> <@@ (%rdr).ReadUInt32() @@>
      | BondDataType.BT_UINT64 ->  fun rdr _ -> <@@ (%rdr).ReadUInt64() @@>
      | BondDataType.BT_UINT8  ->  fun rdr _ -> <@@ (%rdr).ReadUInt8() @@>
      | BondDataType.BT_STRING ->  fun rdr _ -> <@@ (%rdr).ReadString() @@>
      | BondDataType.BT_WSTRING -> fun rdr _ -> <@@ (%rdr).ReadWString() @@>
      | BondDataType.BT_STRUCT ->  fun rdr d -> d.[t.struct_def] rdr
      | BondDataType.BT_LIST
      | BondDataType.BT_SET ->
              // produces a quotation like:
              //    <@ let ct,_ = rdr.ReadContainerBegin()
              //       let arr = Array.zeroCreate<_> (int ct)
              //       for i = 0 to ct - 1 do
              //           arr.[i] <- read rdr
              //       rdr.ReadContainerEnd()
              //       Array.toList/Set.ofArray arr @>
              let elRep, _ = typeForBondType provTys t.element
              let convertArray = if t.id = BondDataType.BT_LIST then toList elRep else setOfArray elRep
              fun rdr provFns ->
                  let read = untaggedReader provTys t.element rdr provFns
                  let ct = Quotations.Var("ct", typeof<int>)
                  QExpr.Let(ct, <@@ (%rdr).ReadContainerBegin() @@>,
                      QExpr.Call(convertArray,
                          [readContainer ct elRep read <@@ (%rdr).ReadContainerEnd() @@>]))

      | BondDataType.BT_MAP ->
              // produces a quotation like:
              //    <@ let ct,_,_ = rdr.ReadContainerBegin()
              //       let arr = Array.zeroCreate<_*_> (int ct)
              //       for i = 0 to ct - 1 do
              //           arr.[i] <- readKey rdr, readVal rdr
              //       rdr.ReadContainerEnd()
              //       Map.ofArray arr @>
              let elRep, _ = typeForBondType provTys t.element
              let keyRep, _ = typeForBondType provTys t.key
              fun rdr provFns ->
                  let readKey = untaggedReader provTys t.key rdr provFns
                  let readEl = untaggedReader provTys t.element rdr provFns
                  let ct = Quotations.Var("ct", typeof<int>)
                  QExpr.Let(ct, <@@ (%rdr).ReadContainerBegin() @@>,
                      QExpr.Call(mapOfArray keyRep elRep,
                          [readKeyValueContainer ct keyRep elRep readKey readEl <@@ (%rdr).ReadContainerEnd() @@>]))

      | BondDataType.BT_STOP
      | BondDataType.BT_STOP_BASE
      | BondDataType.BT_UNAVAILABLE
      | _ as ty -> failwith (sprintf "Unexpected BondDataType: %A" ty)

  /// Given a TypeDef, an expression for an ITaggedProtocolReader and a dictionary mapping struct IDs to readers,
  /// produces an expression that reads the corresponding type of value from a reader
  // TODO: Use ReadHelper to make this more flexible (e.g. integral promotion)
  let rec taggedReader (provTys : IDictionary<uint16, ProvidedTypeDefinition>) (t:TypeDef) : Quotations.Expr<ITaggedProtocolReader> -> IDictionary<uint16,_> -> _ =
      match t.id with
      | BondDataType.BT_BOOL ->    fun rdr _ -> <@@ (%rdr).ReadBool() @@>
      | BondDataType.BT_DOUBLE ->  fun rdr _ -> <@@ (%rdr).ReadDouble() @@>
      | BondDataType.BT_FLOAT ->   fun rdr _ -> <@@ (%rdr).ReadFloat() @@>
      | BondDataType.BT_INT16 ->   fun rdr _ -> <@@ (%rdr).ReadInt16() @@>
      | BondDataType.BT_INT32 ->   fun rdr _ -> <@@ (%rdr).ReadInt32() @@>
      | BondDataType.BT_INT64 ->   fun rdr _ -> <@@ (%rdr).ReadInt64() @@>
      | BondDataType.BT_INT8  ->   fun rdr _ -> <@@ (%rdr).ReadInt8() @@>
      | BondDataType.BT_UINT16 ->  fun rdr _ -> <@@ (%rdr).ReadUInt16() @@>
      | BondDataType.BT_UINT32 ->  fun rdr _ -> <@@ (%rdr).ReadUInt32() @@>
      | BondDataType.BT_UINT64 ->  fun rdr _ -> <@@ (%rdr).ReadUInt64() @@>
      | BondDataType.BT_UINT8  ->  fun rdr _ -> <@@ (%rdr).ReadUInt8() @@>
      | BondDataType.BT_STRING ->  fun rdr _ -> <@@ (%rdr).ReadString() @@>
      | BondDataType.BT_WSTRING -> fun rdr _ -> <@@ (%rdr).ReadWString() @@>
      | BondDataType.BT_STRUCT ->  fun rdr d -> d.[t.struct_def] rdr
      | BondDataType.BT_LIST
      | BondDataType.BT_SET ->
              // produces a quotation like:
              //    <@ let ct,_ = Helpers.ReadContainerBegin(rdr)
              //       let arr = Array.zeroCreate<_> (int ct)
              //       for i = 0 to ct - 1 do
              //           arr.[i] <- read rdr
              //       rdr.ReadContainerEnd()
              //       Array.toList/Set.ofArray arr @>
              let elRep, _ = typeForBondType provTys t.element 
              let convertArray = if t.id = BondDataType.BT_LIST then toList elRep else setOfArray elRep
              fun rdr provFns ->
                  let read = taggedReader provTys t.element rdr provFns
                  let ct = Quotations.Var("ct", typeof<int>)
                  QExpr.Let(ct, QExpr.TupleGet(<@@ RuntimeHelpers.ReadContainerBegin %rdr @@>, 0),
                      QExpr.Call(convertArray,
                          [readContainer ct elRep read <@@ (%rdr).ReadContainerEnd() @@>]))

      | BondDataType.BT_MAP ->
              // produces a quotation like:
              //    <@ let ct,_,_ = Helpers.ReadKeyValueContainerBegin(rdr)
              //       let arr = Array.zeroCreate<_*_> (int ct)
              //       for i = 0 to ct - 1 do
              //           arr.[i] <- readKey rdr, readVal rdr
              //       rdr.ReadContainerEnd()
              //       Map.ofArray arr @>
              let elRep, _ = typeForBondType provTys t.element
              let keyRep, _ = typeForBondType provTys t.key
              fun rdr provFns ->
                  let readKey = taggedReader provTys t.key rdr provFns
                  let readEl = taggedReader provTys t.element rdr provFns
                  let ct = Quotations.Var("ct", typeof<int>)
                  QExpr.Let(ct, QExpr.TupleGet(<@@ RuntimeHelpers.ReadKeyValueContainerBegin %rdr @@>, 0),
                      QExpr.Call(mapOfArray keyRep elRep,
                          [readKeyValueContainer ct keyRep elRep readKey readEl <@@ (%rdr).ReadContainerEnd() @@>]))

      | BondDataType.BT_STOP
      | BondDataType.BT_STOP_BASE
      | BondDataType.BT_UNAVAILABLE
      | _ as ty -> failwith (sprintf "Unexpected BondDataType: %A" ty)

  /// Given a TypeDef, an expression representing an IProtocolWriter, and an expression representing the value to write, produces an expression for writing the value
  let rec writerForBondType (provTys : IDictionary<uint16, ProvidedTypeDefinition>) (t:TypeDef) : Quotations.Expr<IProtocolWriter> -> _ -> IDictionary<uint16,_> -> _ =
      match t.id with
      | BondDataType.BT_BOOL ->    fun wrtr e _ -> <@@ (%wrtr).WriteBool(%%e) @@>
      | BondDataType.BT_DOUBLE ->  fun wrtr e _ -> <@@ (%wrtr).WriteDouble(%%e) @@>
      | BondDataType.BT_FLOAT ->   fun wrtr e _ -> <@@ (%wrtr).WriteFloat(%%e) @@>
      | BondDataType.BT_INT16 ->   fun wrtr e _ -> <@@ (%wrtr).WriteInt16(%%e) @@>
      | BondDataType.BT_INT32 ->   fun wrtr e _ -> <@@ (%wrtr).WriteInt32(%%e) @@>
      | BondDataType.BT_INT64 ->   fun wrtr e _ -> <@@ (%wrtr).WriteInt64(%%e) @@>
      | BondDataType.BT_INT8  ->   fun wrtr e _ -> <@@ (%wrtr).WriteInt8(%%e) @@>
      | BondDataType.BT_UINT16 ->  fun wrtr e _ -> <@@ (%wrtr).WriteUInt16(%%e) @@>
      | BondDataType.BT_UINT32 ->  fun wrtr e _ -> <@@ (%wrtr).WriteUInt32(%%e) @@>
      | BondDataType.BT_UINT64 ->  fun wrtr e _ -> <@@ (%wrtr).WriteUInt64(%%e) @@>
      | BondDataType.BT_UINT8  ->  fun wrtr e _ -> <@@ (%wrtr).WriteUInt8(%%e) @@>
      | BondDataType.BT_STRING ->  fun wrtr e _ -> <@@ (%wrtr).WriteString(%%e) @@>
      | BondDataType.BT_WSTRING -> fun wrtr e _ -> <@@ (%wrtr).WriteWString(%%e) @@>
      | BondDataType.BT_STRUCT ->  fun wrtr e d -> d.[t.struct_def] wrtr e
      | BondDataType.BT_LIST ->
          // produces a quotation like:
          //    <@ wrtr.WriteContainerBegin(l.Count, dataType)
          //       for e in l do
          //           write wrtr e
          //       wrtr.WriteContainerEnd() @>
          let lstRep, _ = typeForBondType provTys t
          let elRep, _ = typeForBondType provTys t.element
          let wrtrGen = writerForBondType provTys t.element
          let lenProp = lstRep.GetProperty("Length")
          fun wrtr lstExpr d ->
              let write e = wrtrGen wrtr e d
              QExpr.Sequential(
                  let dataType = t.element.id
                  <@@ (%wrtr).WriteContainerBegin((%%QExpr.PropertyGet(lstExpr, lenProp) : int), dataType) @@>,
                  QExpr.Sequential(
                      let e = Quotations.Var("e", elRep)
                      Expr.For(e, lstExpr, write (QExpr.Var e)),
                      <@@ (%wrtr).WriteContainerEnd() @@>))
      | BondDataType.BT_SET ->
          // produces a quotation like:
          //    <@ wrtr.WriteContainerBegin(s.Count, dataType)
          //       for e in s do
          //           write wrtr e
          //       wrtr.WriteContainerEnd() @>
          let setRep, _ = typeForBondType provTys t
          let elRep, _ = typeForBondType provTys t.element
          let wrtrGen = writerForBondType provTys t.element
          let lenProp = setRep.GetProperty("Count")
          fun wrtr setExpr d ->
              let write e = wrtrGen wrtr e d
              QExpr.Sequential(
                  let dataType = t.element.id
                  <@@ (%wrtr).WriteContainerBegin((%%QExpr.PropertyGet(setExpr, lenProp) : int), dataType) @@>,
                  QExpr.Sequential(
                      let e = Quotations.Var("e", elRep)
                      Expr.For(e, setExpr, write (QExpr.Var e)),
                      <@@ (%wrtr).WriteContainerEnd() @@>))

      | BondDataType.BT_MAP ->
          // produces a quotation like:
          //    <@ wrtr.WriteContainerBegin(s.Count, keyDataType, elDataType)
          //       for e in s do
          //           write wrtr e
          //       wrtr.WriteContainerEnd() @>
          let dictRep, _ = typeForBondType provTys t
          let elRep, _ = typeForBondType provTys t.element
          let keyRep, _ = typeForBondType provTys t.key
          let elWrtrGen = writerForBondType provTys t.element
          let keyWrtrGen = writerForBondType provTys t.key
          let lenProp = dictRep.GetProperty("Count")
          fun wrtr dictExpr d ->
              let writeKey e = keyWrtrGen wrtr e d
              let writeEl e = elWrtrGen wrtr e d
              QExpr.Sequential(
                  let elType = t.element.id
                  let keyType = t.key.id
                  <@@ (%wrtr).WriteContainerBegin((%%QExpr.PropertyGet(dictExpr, lenProp) : int), keyType, elType) @@>,
                  QExpr.Sequential(
                      let itemType = typedefof<KeyValuePair<_,_>>.MakeGenericType(keyRep, elRep)
                      let kvp = Quotations.Var("kvp", itemType)
                      Expr.For(kvp, dictExpr, QExpr.Sequential(writeKey (QExpr.PropertyGet(QExpr.Var kvp, itemType.GetProperty("Key"))), writeEl (QExpr.PropertyGet(QExpr.Var kvp, itemType.GetProperty("Value"))))),
                      <@@ (%wrtr).WriteContainerEnd() @@>))

      | BondDataType.BT_STOP
      | BondDataType.BT_STOP_BASE
      | BondDataType.BT_UNAVAILABLE
      | _ as ty -> failwith (sprintf "Unexpected BondDataType: %A" ty)

