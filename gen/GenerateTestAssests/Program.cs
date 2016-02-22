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
    public static void Main (string[] args)
    {
      var src = new SingleField() { name = "test" };
      var binFile = new FileStream ("unittest.schema.SingleField", FileMode.CreateNew);
      var output = new OutputStream (binFile);
      var writer = new CompactBinaryWriter<OutputStream>(output);
      Serialize.To(writer, src);

      output.Flush();
      binFile.Position = 0;

      var input = new InputStream(binFile);
      var reader = new CompactBinaryReader<InputStream>(input);

       var dst = Deserialize<SingleField>.From(reader);
      System.Diagnostics.Debug.Assert(dst.name == src.name);
    }
  }
}
