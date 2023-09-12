# EWGSL
![crates.io](https://img.shields.io/crates/v/ewgsl.svg)
![crates.io](https://img.shields.io/crates/l/ewgsl.svg)

A comprehensive Rust integration WGSL preprocessor, with support for imports, Rust code generation and more.

# Motivation

WGSL is, and always will be, a minimal language focussing on compatability across many systems. Much like JavaScript/EMCAScript, as more platforms support WebGPU, the rate at which features can be added to WGSL will decrease in favor of backwards compatability with older systems. This library aims to be a complete pre-processor solution which presents a selection of quality of life features to the programmer (you!) which map cleanly onto the features offered by WGSL. 
