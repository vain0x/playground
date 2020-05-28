using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MicroStream.Data.Entity
{
    public class Sequence
    {
        [Key]
        [Column]
        public long Id { get; set; }
    }
}
