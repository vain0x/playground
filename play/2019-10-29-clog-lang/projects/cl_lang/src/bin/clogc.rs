fn main() {
    let entry_path = std::env::args()
        .skip(1)
        .next()
        .expect("Expect an argument <entry-path>");
    cl_lang::run(&entry_path)
}
