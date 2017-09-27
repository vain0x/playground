using System;
using System.Collections;
using System.Collections.Generic;
using System.ComponentModel;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Net;
using System.Net.Http;
using System.Net.Mail;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using System.Web;
using Effort;

namespace VainZero.Sandbox
{
    public sealed class Program
    {
        public void Run()
        {
            var data = new Effort.Extra.ObjectData(Effort.Extra.TableNamingStrategy.EntityName);
            for (var i = 0; i < 10; i++)
            {
                data.Table<codes>("codes").Add(new codes()
                {
                    value_name = "a",
                    value = i,
                });
                data.Table<codes>();
            }
            var loader = new Effort.Extra.ObjectDataLoader(data);

            using (var connection = DbConnectionFactory.CreateTransient(loader))
            {
                using (var context = new MyDbContext(connection))
                {
                    foreach (var code in context.codes)
                    {
                        Console.WriteLine($"{code.value_name} = {code.value}");
                    }
                }
            }
        }

        public static void Main(string[] args)
        {
            new Program().Run();
        }
    }
}
