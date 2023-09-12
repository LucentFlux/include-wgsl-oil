# EWGSL Specification

EWGSL is an extended form of WGSL. As such, this specification extends on the [WGSL Specification](https://www.w3.org/TR/WGSL/), and any file that is a valid WGSL shader is also a valid EWGSL shader. EWGSL is designed to be transpiled to WGSL.

# Motivation

WGSL is, and always will be, a minimalistic compatability-oriented interchange shader language. It must be this to be successful - many different vendors will implement WebGPU across many different systems, and adding features to WGSL both threatens application support on older systems, and risks reducing the universality offered. 

This fact means that WGSL will never include features that are desired by programmers. WGSL will never have import/include/use statements, as this would require that WebGPU engines implement a far more complex API, without actually increasing the expressability of the language. Similarly, WGSL will never have a preprocessor, templating, or imports and exports to and from the host language. However programmers all desire these features. 

EWGSL is designed to be a one-stop shop for all of the quality of life features that you would want from a shader language, while still transpiling to WGSL to offer the same incredible level of cross platform support. This document serves as a specification for all of the features offered by EWGSL, however unlike the WGSL specification this document is not intended to be re-implemented by vendors. Instead, the library contained in the repository with this document is the reference implementation of everything required to use EWGSL, as the translation to WGSL is done before being handed to a WebGPU engine.

# Multi-File Shaders

Identifiers in EWGSL are permitted to contain `'::'`. EWGSL allows shader code to be split across many files, termed modules, and either imported using a `use` statement, or referred to using a path as an identifier, much like that of Rust:

```regex
ewgsl_ident :
    path_segment ( `'::'` path_segment)*

path_segment :
   ident | `'super'` | `'crate'`

use_tree :
      ewgsl_ident ( `'{'` (use_tree ( `','` use_tree )* `','`? )? `'}'` )?

use_declaration :
   `'use'` use_tree `';'`
```

The `crate` path segment resolves to the root of your project, e.g. the `src/` directory or the `examples/[example_name]` subdirectory. The `super` path segment is similar to the `..` file path segment, in that it refers to the parent directory of the path so far.

Use statements resolve to a set of paths to items, relative to the file that contains them. These statements must be at the top of a file, before any other items such as constants or functions. Use statements must refer to shader files with a `wgsl` extension.

### Example
The following statement imports a shader located at `dir1/foo.wgsl` from the root, as well as the items named `fn1` and `const3` from a shader located at `../bar.wgsl` relative to the file containing the `use` statements:
```wgsl
use crate::dir1::foo;
use super::bar::{fn1, const3};
```

## Import Semantics

EWGSL effectively creates headers for each file before resolving use statements, meaning that two files may mutually import content from each other. However recursion is still not permitted within function bodies, or in constant expression evaluation, even if the co-recursive expressions are in different files.

When resolving imports:
 - Each shader file in the project is scanned to find any definitions, and the identifiers and types of those definitions are recorded.
 - For each definition, a set of immediate dependencies are calculated. Expressions and statements depend on their inputs, functions depend on everything that their bodies depend on, etc. This forms a dependency graph across all defined objects.
 - This graph is checked to be a DAG. Any cycles in the dependency graph indicate recursion in resulting functions or expressions, which is disallowed and results in a compile error.
 - The DAG is traversed such that all dependencies are traversed before the objects that depend on them. 
 - Each traversed object is emitted, with definition names mangled depending on the module name, and referenced object names mapped to match the mangled versions.

# Overrides

A limited form overrides is available, where an importing shader may override an imported function, and as such any references to the base function within the imported shader will instead refer to the overrided version.

```regex
virtual_function_header :
    `virtual` function_header
```

```regex
override_function_header :
    `'override'` `'fn'` ident `'::'` ident `'('` param_list ? `')'` ( `'->'` attribute * template_elaborated_ident ) ?
```

*see definitions of the items referred to in the above definitions in [Section 10.1](https://www.w3.org/TR/WGSL/#function-declaration-sec) of the WGSL specification.

### Example

In base shader:
```wgsl
virtual fn point_light(world_position: vec3<f32>) -> vec3<f32> { ... }
```

In importing shader:
```wgsl
#import lighting.wgsl as Lighting

override fn Lighting::point_light (world_position: vec3<f32>) -> vec3<f32> { ... }
```

## Override Semantics

Override function definitions cause all calls to the original function in the entire shader scope to be replaced by calls to the new function, with the exception of calls within the override function itself.

It is a compile error for the function signature of the override to not match the base function.

Overrides can be specified at any point in the final shader's import tree, and multiple overrides can be applied to the same function. In this way a chain of overrides can be applied for a given top-level shader, and many different top level shaders can have different override chains.

Different overrides of the same function can be specified in different import branches. The final override stack will be ordered based on the first occurrence of the override in the import tree (using a depth first search).

For example, given:
 - a module `A` containing a function `f`,
 - a module `B` that imports `A`, and contains an override function `A::f`,
 - a module `C` that imports `A` and `B`, and contains an override function `A::f`,

Then `B` and `C` both specify an override for `A::f`, with the following behaviour: 
 - the override function `A::f` declared in module `B` may call `A::f` within its body.
 - the override function `A::f` declared in module `C` may call `A::f` within its body, and the call will be redirected to `B::f`.
 - any calls to `A::f` within `C`, or modules which import `C` (importing within modules `A` or `B` outside of `A::f`) will be redirected to `C::f`.
 - else, if `C` is not present, any calls to `A::f` within `B`, or modules which import `B` (importing within module `A` outside of `A::f`) will be redirected to `B::f`.
 - else, if `B` is not present, any calls to `A::f` within `A`, or modules which import `A` (importing within module `A` outside of `A::f`) will not be redirected.

### Diamond Inheritance

Imports into a module/shader are processed in order, and are processed before the body of the module, so there is no way to import a module containing an override and inject a call into the override stack prior to that imported override. You can instead create two modules each containing an override and import them into a parent module/shader to order them as required.

For example, given:
 - a module `A` containing a function `f`,
 - a module `B` that imports `A`, and contains an override function `A::f`,
 - a module `C` that imports `A`, and contains an override function `A::f`,
 - a module `D` that imports both `B` and `C`,

Then calls to `A::f` from within `D` will depend on the import order of `B` and `C`. If `B` is imported before `C`:
 - calls to `A::f` within the override function `A::f` declared in module `B` will not be redirected.
 - calls to `A::f` within the override function `A::f` declared in module `C` will be redirected to `B::f`.
 - calls to `A::f` within `D`, or modules which import `D` (importing within modules `A`, `B` or `C` outside of `A::f`) will be redirected to `C::f`.

# Conditional Preprocessor and Preprocessor Variables

EWGSL has a preprocessor reminiscent of the C/C++ preprocessor. Values or flags can be `#define`d, and then portions of code can be enabled or disabled using `#if`, `#ifdef`, `#ifndef`, `#else` and `#endif` directives.

Directives allow portions of code to be ommitted or included conditionally. For example, the following code will return either `999.0` or `0.999`, depending on whether the `BIG_NUMBER` preprocessor variable is defined.

```wgsl
#define BIG_NUMBER true

fn get_number() -> f32 {
    #ifdef BIG_NUMBER
        return 999.0;
    #else
        return 0.999;
    #endif
}
```

Directives are resolved before the shader is handed off to WebGPU, so they use useful for enabling or disabling portions of code that require extensions or features to use. They also allow you to write generic shader modules which are specialised by their importing top level modules.

The `#define` directive takes a preprocessor variable name, and an optional value, and assigns the value to the name. The value must be either a signed or unsigned integer, or a boolean. If no value is specified, a value of `true` is instead used.

`#define` directives may only be present in a top-level shader module, and must come before all other non-comment items in the shader's source. It is an error for a shader to import a child shader that has `#define` statements. 

The `#ifdef` directive enables the code in its body when the given name exists in the input binding set (regardless of value). The `#ifndef` directive does the reverse.

The `#if` directive requires a preprocessor variable name, an operator, and a value for comparison. The operator must be one of ``==`, `!=`, `>=`, `>`, `<`, `<=`, and the value must be an integer literal if comparing to a signed or unsigned integer, or `true` or `false` if comparing to a boolean.

Preprocessor variables can also be used in the shader source with `#{SHADER_DEF}`, and will be substituted for their value.

The preprocessor branching directives (`#ifdef`, `#ifndef` and `#if`) can be followed by an `#else` directive to create more complex control flow:

```wgsl
fn get_number() -> f32 {
    #ifdef BIG_NUMBER
        return 999.0;
    #else if USER_NUMBER > 1
        return f32(#{USER_NUMBER})
    #else
        return 0.999;
    #endif
}
```

## Provided Preprocessor Variables

Some preprocessor variables are defined by default, and carry information about the environment in which the shader is being compiled:

- `__DEBUG` is defined as `true` if your project is being built in debug mode, else it is `false`.
- `__TARGET_WEB` is defined as `true` if your build target architecture is one of `wasm32`, `wasm64` or `asmjs`, else it is `false`.


