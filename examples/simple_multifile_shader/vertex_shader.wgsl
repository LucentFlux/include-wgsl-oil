#import shared.wgsl as Shared

@vertex
fn vs_main(
    @builtin(vertex_index) in_vertex_index: u32,
) -> Shared::VertexOutput {
    // Able to access the `VertexOutput` data type in the `Shared` module.
    var out: Shared::VertexOutput;
    
    // Able to access the `mean` function in the `Shared` module.
    let x = Shared::mean(f32(in_vertex_index), 1.0);
    let y = Shared::mean(f32(in_vertex_index), 2.0);
    
    out.clip_position = vec4<f32>(x, y, 0.0, 1.0);
    return out;
}