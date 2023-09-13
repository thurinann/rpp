fn main() {
    if let Err(e) = rpp_build::build() {
        panic!("{e}");
    }
}
