using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.Dancehall.Data.Entity
{
    [Table("work_parties")]
    public class WorkParty
    {
        [Key]
        [Column]
        [DatabaseGenerated(DatabaseGeneratedOption.None)]
        public long Id { get; set; }

        [Column]
        [Index]
        public long WorkId { get; set; }

        [Column]
        public long PersonId { get; set; }

        [Column]
        public long WorkPartyTypeId { get; set; }
    }
}
