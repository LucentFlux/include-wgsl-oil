#import utils.wgsl::{MyStruct, my_fn}

fn baz(a: f32, b: f32, c: f32) -> f32 {
    return my_fn(MyStruct(vec3(a, b, c)));
}
