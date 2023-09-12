#[doc = include_str!("README.md")]
use encase;
use glam;

mod shader {
    include_wgsl_oil::include_wgsl_oil! {"main.wgsl"}
}

type MyStruct = shader::types::MyStruct;

fn main() {
    let _ = MyStruct {
        foo: 12,
        bar: -18.5,
        bax: glam::vec3(1.0, 0.0, 0.0),
    };
}
