using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;

namespace VainZero.Sandbox
{
    [Table("codes")]
    public partial class codes
    {
        [Key]
        [Column("id")]
        public long id { get; set; }

        [Column("category")]
        public int category { get; set; }

        [Column("category_name")]
        [StringLength(20)]
        public string category_name { get; set; }

        [Column("value")]
        public int value { get; set; }

        [Column("value_name")]
        [StringLength(20)]
        public string value_name { get; set; }
    }
}
