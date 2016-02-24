namespace Bond.TypeProvider.DesignTime

open System
open System.Collections.Generic
open Bond
open Bond.Protocols

type internal fieldInfo = {
  id : uint16; metadata : Metadata;
  defaultExpr : Quotations.Expr; defaultValue : obj; fieldType : TypeDef }

/// Convert SchemaDefs (and their components) to equivalent quotation literals
module private SchemaQuotation =
  /// Given a function for converting list elements to quotations and a list of values,
  /// generate a quoted list
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

type internal TP2 (s : SchemaDef, idx : uint16, tupTys : Dictionary<uint16,Lazy<Type>>, tp : TP) =
  let relatedStructs =
    let rec loop seen idx =
      let frontier =
        s.structs.[int idx].fields
        |> Seq.map (fun f -> structComponents f.``type``)
        |> Set.unionMany
      if Set.isSubset frontier seen then seen
      else
        frontier - seen
        |> Set.fold loop (Set.union seen frontier)
    and structComponents (t : TypeDef) =
      match t.element with
      | null -> Set.empty
      | _ ->
      match t.id with
      | BondDataType.BT_STRUCT -> Set.singleton t.struct_def
      | BondDataType.BT_LIST | BondDataType.BT_SET ->
        structComponents t.element
      | BondDataType.BT_MAP ->
        Set.union (structComponents t.element) (structComponents t.key)
      | _ -> Set.empty
    loop Set.empty idx |> Set.add idx

  /// Gets the list of (field ID, metadata, default value (expression), field type) for each field
  /// in the nth type
  let fieldsFor i =
    [ for f in s.structs.[i].fields ->
        { id = f.id; metadata = f.metadata
          defaultExpr  = tp.DefaultExpr  f.``type`` f.metadata.default_value
          defaultValue = tp.DefaultValue f.``type`` f.metadata.default_value
          fieldType = f.``type`` }]
    |> List.sortBy (fun fi -> fi.id)

  static let rec mkFnTy (dom :: tys) =
    let rng =
      match tys with
      | [rng] -> rng
      | l -> mkFnTy l
    Reflection.FSharpType.MakeFunctionType(dom, rng)

  let serializerVars =
    relatedStructs
    |> Seq.map (fun i -> i, Quotations.Var(sprintf "write%i" i, mkFnTy [typeof<IProtocolWriter>; tupTys.[i].Value; typeof<unit>]))
    |> dict

  static let _lambda var body = QExpr.Lambda (var, body)
  static let _application var body = QExpr.Application (var, body)
  static let _let var letexpr body = QExpr.Let (var, letexpr, body)
  static let _sequential first second = QExpr.Sequential(first, second)
  let serializers =
    [ for (KeyValue(idx, serializerVar)) in serializerVars ->
        let writeVarExprs =
          [ for i in relatedStructs ->
             i, fun wrtr e ->
                let coerce = QExpr.Coerce(e, tupTys.[i].Value)
                let var = QExpr.Var serializerVars.[i]
                _application <| _application var coerce <| wrtr ]
          |> dict
        let write  = Quotations.Var("write", typeof<IProtocolWriter -> unit>)
        let writer = Quotations.Var("wrtr",  typeof<IProtocolWriter>)
        let value  = Quotations.Var("value", tupTys.[idx].Value)
        let ipw    = Quotations.Var("ipw",   typeof<IProtocolWriter>)

        let writeBody =
          let writer = QExpr.Cast<IProtocolWriter>(QExpr.Var writer)
          let write = QExpr.Cast<IProtocolWriter->unit>(QExpr.Var write)
          <@ (%write) %writer @>

        let writeExpr =
          let ipw = QExpr.Cast<IProtocolWriter>(QExpr.Var ipw)
          let writeStruct =
            let writeBegin = <@ (%ipw).WriteStructBegin((%SchemaQuotation.quoteMetadata s.structs.[int idx].metadata)) @>
            let mapFields idx fieldInfo =
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
                   (%%tp.WriterForBondType fieldInfo.fieldType ipw elt writeVarExprs) ((%ipw).WriteFieldEnd()) @>
              if fieldInfo.metadata.modifier <> Modifier.Optional then
                 // if the field is required, always write it
                 writeField
              else
                 // otherwise, perform default check to see if we must write it
                 <@ if %%cond then
                         %writeField
                     else
                         (%ipw).WriteFieldOmitted(bondTy, id, %SchemaQuotation.quoteMetadata fieldInfo.metadata) @>
            let writeFields = fieldsFor (int idx) |> List.mapi mapFields
            writeBegin :: writeFields |> List.reduce (fun q1 q2 -> <@ %q1; %q2 @>)
          _sequential writeStruct <@ (%ipw).WriteStructEnd() @>

        let serializerExpr =
          _lambda writer << _lambda value <|
          _let write (_lambda ipw writeExpr) writeBody

        serializerVar, serializerExpr]

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

  static let NewTuple_ (expr : Quotations.Expr list) =
    // BUGBUG: QExpr.NewTuple does not create a Tuple when called with one argument, this appear to be F# compiler bug
    let tupTy = [| for e in expr -> e.Type |]
                |> Reflection.FSharpType.MakeTupleType
    if expr.Length > 1 then
      QExpr.NewTuple(expr)
    else
      QExpr.NewObject(tupTy.GetConstructors().[0], expr)

  let taggedDeserializerVars =
    relatedStructs
    |> Seq.map (fun i -> i, Quotations.Var(sprintf "tagged_read%i" i, mkFnTy [typeof<ITaggedProtocolReader>; tupTys.[i].Value]))
    |> dict

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
                      let read = tp.TaggedReader fieldInfo.fieldType reader makeVarExprs
                      fn, QExpr.Lambda(Quotations.Var("_", typeof<unit>),
                              let (Quotations.Patterns.Call(None,refSet,[_;_])) = <@ ref 0 := 0 @>
                              let (var,_,_) = fieldVarsAndVals.[fldIdx]
                              QExpr.Call(refSet.GetGenericMethodDefinition().MakeGenericMethod(fst (tp.TypeForBondType fieldInfo.fieldType)), [QExpr.Var var; read])))
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
  let untaggedDeserializerVars =
    relatedStructs
    |> Seq.map (fun i -> i, Quotations.Var(sprintf "untagged_read%i" i, mkFnTy [typeof<IUntaggedProtocolReader>; tupTys.[i].Value]))
    |> dict

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
                      let read = tp.UntaggedReader fieldInfo.fieldType reader makeVarExprs
                      fn, QExpr.Lambda(Quotations.Var("_", typeof<unit>),
                              let (Quotations.Patterns.Call(None,refSet,[_;_])) = <@ ref 0 := 0 @>
                              let (var,_,_) = fieldVarsAndVals.[fldIdx]
                              QExpr.Call(refSet.GetGenericMethodDefinition().MakeGenericMethod(fst (tp.TypeForBondType fieldInfo.fieldType)), [QExpr.Var var; read])))
                  |> List.toArray

              let allSequential = function
                  | [] -> <@@ () @@>
                  | xs -> List.reduce (fun e1 e2 -> QExpr.Sequential(e1, <@@ %%e2 : unit @@>)) xs

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

  member __.TaggedDeserializerVars = taggedDeserializerVars
  member __.TaggedDeserializers = taggedDeserializers
  member __.UntaggedDeserializerVars = untaggedDeserializerVars
  member __.UntaggedDeserializers = untaggedDeserializers
  member __.SerializerVars = serializerVars
  member __.Serializers = serializers
  member __.FieldsFor = fieldsFor <| int idx

  static member NewTuple e = NewTuple_ e
