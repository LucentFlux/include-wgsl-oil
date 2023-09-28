#[include_wgsl_oil::include_wgsl_oil("vertex_shader.wgsl")]
mod vertex_shader {}
#[include_wgsl_oil::include_wgsl_oil("fragment_shader.wgsl")]
mod fragment_shader {}

fn main() {
    println!("Vertex source: {}", vertex_shader::SOURCE);
    println!("Fragment source: {}", fragment_shader::SOURCE);
}
