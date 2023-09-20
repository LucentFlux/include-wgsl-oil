# EWGSL
![crates.io](https://img.shields.io/crates/v/ewgsl.svg)
![crates.io](https://img.shields.io/crates/l/ewgsl.svg)

#### A comprehensive extension of WGSL, with support for imports, host language code generation and more.

This crate provides the parser of the EWGSL language. It also provides the following optional features which can be enabled with feature flags:

- `eq` implements `PartialEq` and `Eq` for `ParsedModule`s. 
- `span_erasure` allows you to call `erase_spans` on `ParsedModule`s, stripping all span information. This is useful in conjunction with the `eq` flag to check if two modules are equivalent once parsed, ignoring whitespace and comments. 

# Motivation

WGSL is, and always will be, a minimal language focussing on compatability across many systems. Much like JavaScript/EMCAScript, as more platforms support WebGPU, the rate at which features can be added to WGSL will decrease in favor of backwards compatability with older systems. This library aims to be a complete pre-processor solution which presents a selection of quality of life features to the programmer (you!) which map cleanly onto the features offered by WGSL. 
