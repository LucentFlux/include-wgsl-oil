// Import using relative imports
#import shared/shared.wgsl as Shared
// Could also import using absolute imports with `shaders/shared/shared.wgsl`

// Able to access the `VertexOutput` data type in the `Shared` module.
@fragment
fn fs_main(in: Shared::VertexOutput) -> @location(0) vec4<f32> {
    // Able to access the `mean` function in the `Shared` module.
    let red = Shared::mean(0.1, 0.4);

    return vec4<f32>(red, 0.2, 0.1, 1.0);
}