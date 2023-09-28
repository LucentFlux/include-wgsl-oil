#[include_wgsl_oil::include_wgsl_oil("definitions.wgsl")]
mod definitions_shader {}
type ExportedStruct = definitions_shader::types::MyFooStruct;

fn main() {
    let my_shader_data = ExportedStruct {
        field1: 12.0,
        field2: 8,
        array_field: [[1, 2, 3], [-1, -2, -3], [1, -2, 3], [-1, 2, -3]],
    };

    println!("my_data: {my_shader_data:?}")
}
