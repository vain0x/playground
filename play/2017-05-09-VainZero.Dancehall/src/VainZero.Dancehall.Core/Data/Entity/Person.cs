using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.Dancehall.Data.Entity
{
    [Table("persons")]
    public class Person
    {
        [Key]
        [Column]
        [DatabaseGenerated(DatabaseGeneratedOption.None)]
        public long Id { get; set; }

        [Column]
        public string Name { get; set; }
    }
}
