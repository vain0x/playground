using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc;
using Microsoft.AspNetCore.Mvc.RazorPages;
using Microsoft.EntityFrameworkCore;
using ZelgaAnc.Models.Data;

namespace ZelgaAnc.Pages
{
    public class IndexModel : PageModel
    {
        private readonly AppDbContext _db;

        public IReadOnlyList<Customer> Customers { get; set; }

        [TempData]
        public string Message { get; set; }

        public async Task OnGetAsync()
        {
            Customers = await _db.Customers.AsNoTracking().ToArrayAsync();
        }

        public async Task<IActionResult> OnPostDeleteAsync(long id)
        {
            var contact = await _db.Customers.FindAsync(id);
            if (contact == null) return RedirectToPage();

            _db.Customers.Remove(contact);
            await _db.SaveChangesAsync();
            return RedirectToPage();
        }

        public IndexModel(AppDbContext db)
        {
            _db = db;
        }
    }
}
