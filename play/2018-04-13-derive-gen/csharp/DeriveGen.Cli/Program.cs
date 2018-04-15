using System;
using System.Collections.Generic;
using System.Dynamic;
using System.Linq;
using Utf8Json;

namespace DeriveGen.Cli
{
    public static class JsonSerializerHelper
    {
        static System.Text.Encoding UTF8 = new System.Text.UTF8Encoding();

        public static dynamic DeserializeDynamic(string json)
        {
            return DeserializeDynamic(UTF8.GetBytes(json));
        }

        public static dynamic DeserializeDynamic(byte[] jsonBytes)
        {
            dynamic Go(ref JsonReader reader)
            {
                var token = reader.GetCurrentJsonToken();
                switch (token)
                {
                    case JsonToken.BeginObject:
                        {
                            dynamic obj = new ExpandoObject();
                            var c = 0;
                            while (reader.ReadIsInObject(ref c))
                            {
                                var key = reader.ReadPropertyName();
                                var value = Go(ref reader);
                                ((IDictionary<string, object>)obj).Add(key, value);
                            }
                            return obj;
                        }
                    case JsonToken.BeginArray:
                        {
                            var items = new List<object>();
                            var c = 0;
                            while (reader.ReadIsInArray(ref c))
                            {
                                items.Add(Go(ref reader));
                            }
                            return items.ToArray();
                        }
                    case JsonToken.Number:
                        return reader.ReadDouble();
                    case JsonToken.String:
                        return reader.ReadString();
                    case JsonToken.True:
                    case JsonToken.False:
                        return reader.ReadBoolean();
                    case JsonToken.Null:
                        reader.ReadIsNull();
                        return null;
                    default:
                        throw new Exception($"Unexpected JSON token: ${token}.");
                }
            }

            {
                var reader = new Utf8Json.JsonReader(jsonBytes);
                return Go(ref reader);
            }
        }
    }

    public static class DotLiquidHelper
    {
        static DotLiquid.Hash NewDict()
        {
            return new DotLiquid.Hash((self, name) =>
            {
                throw new Exception($"Field '{name}' missing in env [{string.Join(", ", self.Keys)}].");
            });
        }

        static object CreateEnvCore(ref JsonReader reader)
        {
            var token = reader.GetCurrentJsonToken();
            switch (token)
            {
                case JsonToken.BeginObject:
                    {
                        var obj = NewDict();
                        var c = 0;
                        while (reader.ReadIsInObject(ref c))
                        {
                            var key = reader.ReadPropertyName();
                            var value = CreateEnvCore(ref reader);
                            obj.Add(key, value);
                        }
                        return obj;
                    }
                case JsonToken.BeginArray:
                    {
                        var items = new List<object>();
                        var c = 0;
                        while (reader.ReadIsInArray(ref c))
                        {
                            items.Add(CreateEnvCore(ref reader));
                        }
                        return items.ToArray();
                    }
                case JsonToken.Number:
                    return reader.ReadDouble();
                case JsonToken.String:
                    return reader.ReadString();
                case JsonToken.True:
                case JsonToken.False:
                    return reader.ReadBoolean();
                case JsonToken.Null:
                    reader.ReadIsNull();
                    return null;
                default:
                    throw new Exception($"Unexpected JSON token: ${token}.");
            }
        }

        public static DotLiquid.Hash CreateEnv<T>(T model)
        {
            var jsonBytes = Utf8Json.JsonSerializer.Serialize(model);
            var reader = new Utf8Json.JsonReader(jsonBytes);
            return (DotLiquid.Hash)CreateEnvCore(ref reader);
        }
    }

    public sealed class NamespaceModel
    {
        public string NamespaceName;
        public ClassModel[] Classes;
    }

    public sealed class ClassModel
    {
        public string Modifiers = "public";
        public string ClassName;
        public string Description;

        public FieldModel[] Fields;
        public MethodModel[] Methods;
    }

    public sealed class FieldModel
    {
        public string Modifiers = "public";
        public string Type;
        public string FieldName;
        public string Description;
        public string Accessors;
    }

    public sealed class MethodModel
    {
        public string Modifiers = "public";
        public string Type;
        public string MethodName;
        public string Description;

        public ParameterModel[] Parameters;
        public string[] Statements;
    }

    public sealed class ParameterModel
    {
        public string Type;
        public string Name;
    }

    sealed class Program
    {
        public static string JsonSample => @"{
    `DeriveGen.Samples`: {
        `Person`: {
            `derive`: `record`,
            `fields`: {
                `Name`: {
                    `type`: `string`
                },
                `Age`: {
                    `type`: `int`
                }
            }
        },
        `Box`: {
            `derive`: `record`,
            `fields`: {
                `Value`: {
                    `type`: `string`
                }
            }
        }
    }
}".Replace("`", "\"");

        public static string Template => @"
/// <auto-generated />
{%- for ns in Model -%}

namespace {{ ns.NamespaceName }}
{
    {%- for class in ns.Classes -%}
    {%- if class != ns.Classes.first -%}

    {%- endif -%}
    {{ class.Modifiers }} partial class {{ class.ClassName }}
    {
        {%- for field in class.Fields -%}
        {{ field.Modifiers }} {{ field.Type }} {{ field.FieldName }} {{- field.AutoAccessors }};
        {%- endfor -%}

        {%- for method in class.Methods -%}
        {{ method.Modifiers }} {{ method.Type }}{% if method.MethodName != """" %} {{ method.MethodName }}{% endif %}(
            {%- for parameter in method.Parameters -%}
            {{ parameter.Type }} {{ parameter.Name }} {%- if parameter != method.Parameters.last %},{% endif %}
            {%- endfor -%}
        )
        {
            {%- for statement in method.Statements -%}
            {{ statement }}
            {%- endfor -%}
        }
        {%- endfor -%}
    }
    {%- endfor -%}
}
{%- endfor -%}
".Replace("`", "\"").TrimStart();

        static string ToLowerCamelCase(string name)
        {
            var c = name[0];
            if (c == char.ToLower(c))
            {
                return "_" + name;
            }
            else
            {
                return char.ToLower(c).ToString() + name.Substring(1);
            }
        }

        NamespaceModel[] CreateModel(dynamic conf)
        {
            ClassModel CreateClassModel(string className, dynamic design)
            {
                var d = (IDictionary<string, object>)design;
                var fieldMetas = (IDictionary<string, object>)(d.TryGetValue("fields", out var fields1) ? fields1 : new Dictionary<string, object>());
                var fields = new List<FieldModel>();
                foreach (var (fieldName, fieldMeta) in fieldMetas)
                {
                    var type = ((dynamic)fieldMeta).type;
                    fields.Add(new FieldModel()
                    {
                        FieldName = fieldName,
                        Type = (string)type,
                    });
                }

                var methods = new List<MethodModel>();
                methods.Add(new MethodModel()
                {
                    Type = className,
                    MethodName = "",
                    Parameters = fields.Select(field => new ParameterModel()
                    {
                        Name = ToLowerCamelCase(field.FieldName),
                        Type = field.Type,
                    }).ToArray(),
                    Statements = fields.Select(field => $"{field.FieldName} = {ToLowerCamelCase(field.FieldName)};").ToArray(),
                });

                return new ClassModel()
                {
                    ClassName = className,
                    Fields = fields.ToArray(),
                    Methods = methods.ToArray(),
                };
            }

            return ((IDictionary<string, object>)conf).Select(kv => new NamespaceModel()
            {
                NamespaceName = kv.Key,
                Classes = ((IDictionary<string, object>)kv.Value).Select(p => CreateClassModel(p.Key, p.Value)).ToArray(),
            }).ToArray();
        }

        string Render(string json, string templateSource)
        {
            dynamic conf = JsonSerializerHelper.DeserializeDynamic(json);
            var model = new { Model = CreateModel(conf) };

            var template = DotLiquid.Template.Parse(templateSource);
            return template.Render(DotLiquidHelper.CreateEnv(model));
        }

        void RunSample()
        {
            Console.WriteLine(Render(JsonSample, Template));
        }

        private static void Main(string[] args)
        {
            new Program().RunSample();
        }
    }
}
