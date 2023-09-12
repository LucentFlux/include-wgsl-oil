mod dir1;
mod dir2;

mod shader {
    include_wgsl_oil::include_wgsl_oil! {"general.wgsl"}
}

fn main() {
    println!("General source: {}", shader::SOURCE);
}
