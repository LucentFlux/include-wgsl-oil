struct MyStruct {
    foo: vec3<f32>,
}

fn my_fn(
    bar: MyStruct,
) -> f32 {
    return bar.foo.y + 1.0;
}