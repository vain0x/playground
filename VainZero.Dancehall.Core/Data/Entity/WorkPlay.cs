using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.Dancehall.Data.Entity
{
    [Table("work_plays")]
    public class WorkPlay
    {
        [Key]
        [Column]
        [DatabaseGenerated(DatabaseGeneratedOption.Computed)]
        public long Id { get; set; }

        [Column]
        [Index]
        public long WorkId { get; set; }

        [Column]
        public DateTime DateTime { get; set; }

        [Column]
        public WorkPlayResult Result { get; set; }
    }

    public enum WorkPlayResult
    {
        Complete,
        Skip,
    }
}
