# Include WGSL-OIL
![crates.io](https://img.shields.io/crates/v/include-wgsl-oil.svg)
![crates.io](https://img.shields.io/crates/l/include-wgsl-oil.svg)

A comprehensive Rust integration WGSL preprocessor, with support for imports, Rust code generation and more.

# Motivation

WGSL is, and always will be, a minimal language focussing on compatability across many systems. Much like JavaScript/EMCAScript, as more platforms support WebGPU, the rate at which features can be added to WGSL will decrease in favor of backwards compatability with older systems. This library aims to be a complete pre-processor solution which presents a selection of quality of life features to the programmer (you!) which map cleanly onto the features offered by WGSL. 

This extended version of WGSL offered by this library is called "WGSL-OIL", where OIL is taken from the [`naga-oil`](https://crates.io/crates/naga-oil) crate and stands for Organised Integration Library. A specification of the additions made to the WGSL specifications can be found in the [`SPECIFICATION.md`](https://github.com/LucentFlux/include-wgsl-oil/blob/main/SPECIFICATION.md) file. While this library uses the `naga-oil` crate for imports and definitions, it offers many features above those added by `naga-oil`.

# Features

- Shader errors reported at compile time

- Bonus syntax added to wgsl by the `naga-oil` preprocessor like `include`s and method overriding.

- Types, constants and globals can be exposed to Rust using the [`naga-to-tokenstream`](https://crates.io/crates/naga-to-tokenstream) crate, allowing Rust code to refer to things in the shader without duplicating code.

- Support for `glam` and `encase` with the corresponding feature flags.

- Support for wgsl minification using the [`wgsl-minifier`](https://crates.io/crates/wgsl-minifier) crate with the `minify` feature flag, further reducing startup time.

A complete list of features can be found in the [`SPECIFICATION.md`](https://github.com/LucentFlux/include-wgsl-oil/blob/main/SPECIFICATION.md) file.

# Getting started

Including a shader module follows similar syntax to the Rust `include_str` or `include_bytes` macros, where a path is given relative to the containing folder of the Rust file that the macro is invoked from. However the `include_wgsl_oil` macro generates a large number of constants, so it is recommended to invoke the macro inside a module:

```rust ignore
#[include_wgsl_oil("path/to/shader.wgsl")]
mod my_shader;

// The preprocessed sourcecode constant string can be found in the `SOURCE` constant at the root of the module:
println!("shader source: {}", my_shader::SOURCE); 
```

# Includes

Shader includes are processed relative to the root of the crate source folder, and shaders may only include other shaders that are 'below' them in the directory tree. This forms an implicit inheritence hierarchy which prevents recursive imports.

For example, if your crate directory structure looks like the following:

```text
my-crate/
├─ src/
│  ├─ submodule/
│  │  ├─ special_shader.wgsl
│  │  ├─ mod.rs
│  ├─ general_shader.wgsl
│  ├─ main.rs
├─ Cargo.toml
```

Then `special_shader.wgsl` is able to include `general_shader.wgsl`:

`general_shader.wgsl`:
```wgsl
fn foo() {
    ...
}
```

`special_shader.wgsl`:
```wgsl
#include general_shader.wgsl as GeneralShader

fn bar() {
    GeneralShader::foo();
}
```

# Exported Types and Constants

Structs and constants defined in your shader can be exported as equivalent Rust structs and constants using the `@export` tag before the definition you want to export.

For structs, each of the fields of the struct must be representable, for example by enabling the `glam` feature to represent vectors and matrices, and then your struct definition must be prepended with an `@export` tag, as follows:

```wgsl
@export struct MyStruct {
    foo: u32,
    bar: i32
}
```

Then, in your Rust file, you can do the following:

```rust ignore
#[include_wgsl_oil("path/to/shader.wgsl")]
mod my_shader;

let my_instance = my_shader::types::MyStruct {
    foo: 12,
    bar: -7
};
```

The `encase` feature on this crate makes every exported struct derive `encase::ShaderType`. Note that this may invalidate exported structs, as some types (such as `bool`s) cannot be encoded with `encase`, however it is assumed that the only structs that you would want to export are structs that your program shares between host and GPU, and so should be encodable.

# Preprocessor

A C/C++ style preprocessor is run on your shader, allowing you to `#define` variables and then enable or disable portions of code using `#if`, `#ifdef`, `#ifndef`, `#else` or `#endif` directives.

```wgsl
#define BIG_NUMBER false

fn get_number() -> f32 {
    #if BIG_NUMBER
        return 999.0;
    #else
        return 0.999;
    #endif
}
```

# Definitions

The following definitions are added to pass information from Rust to your shaders:

- `__DEBUG` is defined as `true` if your project is being built in debug mode, else it is `false`.
- `__TARGET_WEB` is defined as `true` if your build target architecture is one of `wasm32`, `wasm64` or `asmjs`, else it is `false`.

So in your shaders included by this crate, you can do something like the following:
```wgsl
#if __DEBUG
    do_something_debug();
#endif
```

# And more

A complete list of features can be found in the [`SPECIFICATION.md`](https://github.com/LucentFlux/include-wgsl-oil/blob/main/SPECIFICATION.md) file.
