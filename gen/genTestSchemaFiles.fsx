#I "../packages/Bond.Core.CSharp/lib/net45"
#I "../packages/Bond.Runtime.CSharp/lib/net45"
#r "Bond.dll"
#r "Bond.JSON.dll"
#r "Bond.IO.dll"
#r "System.IO"
#r "System.Runtime"

open System
open System.IO
let marshaledSchemaFile = FileStream("unittest.schema.All", FileMode.Create)
open Bond.IO.Unsafe
let out = OutputStream(marshaledSchemaFile)
open Bond.Protocols
type Writer = CompactBinaryWriter<OutputStream>
let writer = new Writer(out);
open Bond
let jsonSchemaFile = FileStream("unittest.schema.All.json", FileMode.Open)
let jsonReader = SimpleJsonReader(jsonSchemaFile)
let schema = Deserialize<SchemaDef>.From(jsonReader)
Serialize.To<Writer,Bond.SchemaDef>(writer, schema);
out.Flush();
marshaledSchemaFile.Position = 0L;
let input = new InputStream(marshaledSchemaFile)
let reader = new CompactBinaryReader<InputStream>(input)
let schemaDef = Deserialize<SchemaDef>.From(reader)
let schema' = new RuntimeSchema(schemaDef)
ignore schema'