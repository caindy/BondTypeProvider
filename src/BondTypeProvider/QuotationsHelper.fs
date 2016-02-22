namespace Bond.TypeProvider.Quotations

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
