using System;
using System.ComponentModel.DataAnnotations;
using System.ComponentModel.DataAnnotations.Schema;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace MicroStream.Data.Entity
{
    public class TwitterAuth
    {
        [Key]
        [Column]
        [DatabaseGenerated(DatabaseGeneratedOption.None)]
        public long Id { get; set; }

        [Column]
        [StringLength(255)]
        public string UserName { get; set; }

        [Column]
        [StringLength(1024)]
        public string AccessToken { get; set; }

        [Column]
        [StringLength(1024)]
        public string AccessTokenSecret { get; set; }

        public static TwitterAuth
            Create(
                long id,
                string userName,
                string accessToken,
                string accessTokenSecret
            ) =>
            new TwitterAuth()
            {
                Id = id,
                UserName = userName,
                AccessToken = accessToken,
                AccessTokenSecret = accessTokenSecret,
            };
    }
}
