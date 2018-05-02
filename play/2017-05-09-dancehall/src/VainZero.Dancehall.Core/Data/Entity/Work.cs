using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.Dancehall.Data.Entity
{
    [Table("works")]
    public class Work
    {
        [Key]
        [Column]
        [DatabaseGenerated(DatabaseGeneratedOption.None)]
        public long Id { get; set; }

        [Column]
        [Required]
        [StringLength(1024)]
        public string Title { get; set; }

        /// <summary>
        /// The date when the work is officially released, the month, year, or unknown (null).
        /// </summary>
        [Column]
        public DateTime? ReleaseDate { get; set; }

        [Column]
        public DateTime RegistrationDate { get; set; }

        [Column]
        [Required]
        [StringLength(1024)]
        public string FilePath { get; set; }
    }
}
