#I "../packages/Bond.Core.CSharp/lib/net45"
#r "Bond.dll"
#r "Bond.IO.dll"

open System
open System.IO
let marshaledSchemaFile = FileStream("unittest.schema.All", FileMode.Create)
open Bond.IO.Unsafe
let out = OutputStream(marshaledSchemaFile)
let 




