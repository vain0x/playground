using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.EntityClassGenerator.CSharpWriters
{
    public interface ITextWritable
    {
        void Write(TextWriter writer);
    }

    public static class TextWritableExtension
    {
        public static void WriteToFile(this ITextWritable @this, FileInfo file)
        {
            using (var stream = file.OpenWrite())
            using (var writer = new StreamWriter(stream))
            {
                stream.SetLength(0);
                @this.Write(writer);
            }
        }

        public static string GetString(this ITextWritable @this)
        {
            using (var writer = new StringWriter())
            {
                @this.Write(writer);
                return writer.ToString();
            }
        }
    }
}
