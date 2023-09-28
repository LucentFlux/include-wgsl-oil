struct VertexOutput {
    @builtin(position) clip_position: vec4<f32>,
}

fn mean(a: f32, b: f32) -> f32 {
    return (a + b) / 2.0;
}
