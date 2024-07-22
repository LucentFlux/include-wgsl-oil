#[include_wgsl_oil::include_wgsl_oil("main.wgsl")]
mod main_shader {}

fn main() {
    println!("Main source: {}", main_shader::SOURCE);
}
