#I "../packages/Bond.Core.CSharp/lib/net45"
#I "../packages/Bond.Runtime.CSharp/lib/net45"
#I "../packages/Newtonsoft.Json/lib/net45"
#r "Bond.dll"
#r "Bond.JSON.dll"
#r "Bond.IO.dll"
#r "System.IO"
#r "System.Runtime"

open System.IO
let local path = Path.Combine(__SOURCE_DIRECTORY__, path)

open Bond.Protocols
open Bond

// read json runtime schema
let jsonSchemaFile = FileStream(local "UnitTest.All.json", FileMode.Open)
let jsonReader = SimpleJsonReader(jsonSchemaFile)
let schema = Deserialize<SchemaDef>.From(jsonReader)

// generate marshaled runtime schema file
let outputFilePath = local "unittest.schema.All"
let marshaledSchemaFile = FileStream(outputFilePath, FileMode.Create)
open Bond.IO.Unsafe
let out = OutputStream(marshaledSchemaFile)
let writer = CompactBinaryWriter<OutputStream>(out);
Serialize.To(writer, schema);
out.Flush();
marshaledSchemaFile.Position <- 0L;

// verify
let input = new InputStream(marshaledSchemaFile)
let reader = new CompactBinaryReader<InputStream>(input)
let schemaDef = Deserialize<SchemaDef>.From(reader)
let schema' = new RuntimeSchema(schemaDef)
marshaledSchemaFile.Dispose()

// move to test folder
File.Move(outputFilePath, local "../tests/BondTypeProvider.Tests/unittest.schema.All")