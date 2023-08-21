# Include WGSL-oil
![crates.io](https://img.shields.io/crates/v/include-wgsl-oil.svg)
![crates.io](https://img.shields.io/crates/l/include-wgsl-oil.svg)

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

Including a shader module follows similar syntax to the Rust `include_str` or `include_bytes` macros, where a path is given relative to the containing folder of the Rust file that the macro is invoked from. However the `include_wgsl_oil` macro generates a large number of constants, so it is recommended to invoke the macro inside a module:

```rust ignore
mod my_shader {
    include_wgsl_oil::include_wgsl_oil! {"path/to/shader.wgsl"}
}

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

Then `special_shader.wgsl` is able to include `general_shader.wgsl`, and would do so with the following first line:

```text
#include general_shader.wgsl as GeneralShader

GeneralShader::foo();
```

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
