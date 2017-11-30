using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.Dancehall.Data.Entity
{
    [Table("works_tags")]
    public class WorkTagPair
    {
        [Key]
        [Column(Order = 0)]
        public long WorkId { get; set; }

        [Key]
        [Column(Order = 1)]
        public long TagId { get; set; }
    }
}
