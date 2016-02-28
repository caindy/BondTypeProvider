using System;
using unittest.schema;
using Bond;
using Bond.IO.Unsafe;
using Bond.Protocols;
using System.IO;

namespace GenerateTestAssests
{
  class MainClass
  {
	private static Stream WriteBinaryFile<T>(T o) {
		var binFile = new FileStream ("unittest.schema.SingleField", FileMode.CreateNew);
		var output = new OutputStream (binFile);
		var writer = new CompactBinaryWriter<OutputStream>(output);
		Serialize.To(writer, o);
		output.Flush();
		binFile.Position = 0;
		return binFile;
	}
	private static Stream MarshalBinary<T>(T o) {
		var binFile = new FileStream ("unittest.schema.SingleField", FileMode.CreateNew);
		var output = new OutputStream (binFile);
		var writer = new CompactBinaryWriter<OutputStream>(output);
		Marshal.To(writer, o);
		output.Flush();
		binFile.Position = 0;
		return binFile;
	}
	private static Stream WriteJsonFile(SingleField o) {
		var writer = new StreamWriter("unittest.schema.SingleField.json", false);
		var jsonWriter = new SimpleJsonWriter(writer);
		Serialize.To(writer, o);
		jsonWriter.Flush();
		return writer.BaseStream;
	}

	static void RoundTripSingleFieldSchema() {
		var binFile = new FileStream ("unittest.schema.SingleField", FileMode.CreateNew);
		var output = new OutputStream (binFile);
		var writer = new CompactBinaryWriter<OutputStream>(output);
		// Get runtime schema for type Example and serialize SchemaDef
		Serialize.To(writer, Schema<SingleField>.RuntimeSchema.SchemaDef);
		output.Flush();
		binFile.Position = 0;
		var input = new InputStream(binFile);
		var reader = new CompactBinaryReader<InputStream>(input);
		var schemaDef = Deserialize<SchemaDef>.From(reader);
		var schema = new RuntimeSchema(schemaDef);
	}

    public static void Main (string[] args) {
			/*
				var src = new SingleField() { name = "test" };
				var bin = MarshalBinary(src);
				var input = new InputStream(bin);
				var dst = Unmarshal<SingleField>.From(input);
				System.Diagnostics.Debug.Assert(dst.name == src.name);
			*/
			RoundTripSingleFieldSchema();
    }
	private static T ReadJson<T>(Stream s)
		{
			var rdr = new SimpleJsonReader(s);
			return Deserialize<T>.From(rdr);
		}

	private static T ReadBinary<T>(Stream s)
	{
	  var input = new InputStream(s);
	  var reader = new CompactBinaryReader<InputStream>(input);
	  return Deserialize<T>.From(reader);
	}
  }
}
