using System;
using System.Collections.Generic;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace VainZero.Dancehall.Data.Entity
{
    [Table("playlist_items")]
    public class PlaylistItem
    {
        [Key]
        [Column]
        [DatabaseGenerated(DatabaseGeneratedOption.None)]
        public long Id { get; set; }

        [Column]
        public long WorkId { get; set; }

        [Column]
        public double Order { get; set; }
    }
}
