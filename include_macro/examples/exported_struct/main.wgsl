// Structs can be `exported` to rust using the @export tag, allowing it to be used
// in rust code.
@export struct MyStruct {
    foo: u32,
    bar: f32,
    bax: vec3<f32>,
}