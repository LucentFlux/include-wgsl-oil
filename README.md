# Include WGSL-oil
[![crates.io](https://img.shields.io/crates/v/include-wgsl-oil.svg)](https://crates.io/crates/include-wgsl-oil)
[![crates.io](https://img.shields.io/crates/l/include-wgsl-oil.svg)](https://github.com/LucentFlux/include-wgsl-oil/blob/main/LICENSE)

Provides a macro for including a wgsl file, using the [`naga-oil`](https://crates.io/crates/naga-oil) preprocessor at compile time.

# Motivation

Running the `naga-oil` preprocessor at runtime increases startup time for apps, and also leads to shader errors only being reported at runtime. Running it at compile time allows the preprocessor cost to be spent before the user runs the app, and allows the Rust compiler to report shader errors at compile time.

This crate also uses [`naga-to-tokenstream`](https://crates.io/crates/naga-to-tokenstream) to expose a bunch of information about the types, constants and globals within a shader to Rust. This allows for de-duplication of code, where a constant from a shader module can be used as a Rust constant, and structs defined in shaders to be used as Rust structs.

# Features

- Shader errors are reported at compile time

- Bonus syntax added to wgsl by the `naga-oil` preprocessor like `include`s and method overriding.

- Types, constants and globals are exposed to Rust, allowing code to refer to things in the shader without duplicating code.

- Support for `glam` and `encase` with the corresponding feature flags.

- Support for wgsl minification using the [`wgsl-minifier`](https://crates.io/crates/wgsl-minifier) crate with the `minify` feature flag, further reducing startup time.

# Getting started

Including a shader module follows similar syntax to the Rust `include_str` or `include_bytes` macros, where a path is given relative to the containing folder of the Rust file that the macro is invoked from. However the `include_wgsl_oil` macro generates a large number of objects, so it is instead invoked as an attribute to a module that you would like it to populate with shader information:

```rust ignore
#[include_wgsl_oil::include_wgsl_oil("path/to/shader.wgsl")]
mod my_shader {}

// The preprocessed sourcecode constant string can be found in the `SOURCE` constant at the root of the module:
println!("shader source: {}", my_shader::SOURCE); 
```

# Imports

Shader imports are processed both relative to the importing file, and relative to the root of the crate source folder, and shaders may import any other shaders so long as there is no circular dependency on imports between files.

For example, if your crate directory structure looks like the following:

```text
my-crate/
├─ src/
│  ├─ submodule/
│  │  ├─ subsubmodule/
│  │  │  ├─ extra_special_shader.wgsl
│  │  ├─ special_shader.wgsl
│  │  ├─ mod.rs
│  ├─ general_shader.wgsl
│  ├─ main.rs
├─ Cargo.toml
```

Then `special_shader.wgsl` is able to include `general_shader.wgsl` with either of the following lines:

```text
#import general_shader.wgsl as GeneralShader
#import ../general_shader.wgsl as GeneralShader

GeneralShader::foo();
```

And `extra_special_shader.wgsl` is able to include `special_shader.wgsl` with either of the following lines:

```text
#import ../special_shader.wgsl as SpecialShader
#import submodule/special_shader.wgsl as SpecialShader

SpecialShader::foo();
```

# Exported Types

Structs defined in your shader can be exported as an equivalent Rust struct. To do this, each of the fields of the struct must be representable, for example by enabling the `glam` feature to represent vectors and matrices, and then your struct definition must be prepended with an `@export` tag, as follows:

```wgsl
@export struct MyStruct {
    foo: u32,
    bar: i32
}
```

Then, in your Rust file, you can do the following:

```rust ignore
#[include_wgsl_oil::include_wgsl_oil("path/to/shader.wgsl")]
mod my_shader { }

let my_instance = my_shader::types::MyStruct {
    foo: 12,
    bar: -7
};
```

The `encase` feature on this crate makes every exported struct derive `encase::ShaderType`. Note that this may invalidate exported structs, as some types (such as `bool`s) cannot be encoded with `encase`, however it is assumed that the only structs that you would want to export are structs that your program shares between host and GPU, and so should be encodable.

# Definitions

The following definitions are added to pass information from Rust to your shaders:

- `__DEBUG` is defined as `true` iff your project is being built in debug mode.

So in your shaders included by this crate, you can do something like the following:
```wgsl
#if __DEBUG
    do_something_debug();
#endif
```

# Generated Items

For a full list of the items generated when including a module with this macro, see the [`naga-to-tokenstream`](https://crates.io/crates/naga-to-tokenstream) documentation. 
