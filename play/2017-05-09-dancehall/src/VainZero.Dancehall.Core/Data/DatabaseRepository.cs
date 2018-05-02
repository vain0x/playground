using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using VainZero.Dancehall.Data.Entity;

namespace VainZero.Dancehall.Data
{
    public sealed class DatabaseRepository
        : IRepository
    {
        AppDbContext Connect()
        {
            return new AppDbContext();
        }
    }
}
