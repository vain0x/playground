using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MicroStream.Data.Entity
{
    public class MastodonAuth
    {
        [Key]
        [Column]
        [DatabaseGenerated(DatabaseGeneratedOption.None)]
        public long Id { get; set; }

        [Column]
        [StringLength(255)]
        public string Instance { get; set; }

        [Column]
        [StringLength(255)]
        public string UserName { get; set; }

        [Column]
        [StringLength(1024)]
        public string AccessToken { get; set; }
    }
}
