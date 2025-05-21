#[cfg(any(target_arch = "aarch64", target_arch = "arm64ec"))]
pub mod aarch64;
#[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
pub mod x86;
