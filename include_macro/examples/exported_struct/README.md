# Exporting structs to Rust
Structs can be exported from a `wgsl` shader into your Rust code using the `@export` tag to the
struct definition:

```wgsl
@export struct MyStruct {
    foo: u32,
    bar: f32,
    bax: vec3<f32>,
}
```

Structs exported from `wgsl` modules can use `encase` and/or `glam` for better integration
with your Rust code. Generated code will only use these crates if the corresponding features
are enabled:

In your `Cargo.toml` file:
```toml
include-wgsl-oil = {version = ..., features = ["encase", "glam"]}
encase = {version = ..., features = ["glam"]}
glam = "0.24"
```