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
            using (var connection = DbConnectionFactory.CreateTransient())
            {
                using (var context = new MyDbContext(connection))
                {
                    for (var i = 0; i < 10; i++)
                    {
                        context.codes.Add(new codes()
                        {
                            value_name = "a",
                            value = i,
                        });
                    }

                    context.SaveChanges();
                }

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
