use std::env;
use std::fs::{self, File};
use std::io::prelude::*;
use std::path::{Path, PathBuf};

fn main() {
    v2_macros();
    v2_simd_macros();
    println!("cargo:rerun-if-changed=build.rs");
}

fn v2_macros() {
    let mut base: PathBuf = env::var_os("OUT_DIR").unwrap().into();
    base.push("src");

    fs::create_dir_all(&base)
        .unwrap_or_else(|e| panic!("Could not create directory {}: {}", base.display(), e));

    base.push("v2_macros.rs");

    let mut f = File::create(&base)
        .unwrap_or_else(|e| panic!("Could not create {}: {}", base.display(), e));

    v2_macros_bytes(&mut f, &base);
}

fn v2_macros_bytes(f: &mut File, base: &Path) {
    let arms: String = (1..=16)
        .map(|max| {
            let args: Vec<_> = (0..max).map(|i| format!("$b{:02}:expr", i)).collect();
            let args = args.join(", ");

            let arg_values: Vec<_> = (0..max).map(|i| format!("$b{:02} as u8", i)).collect();

            let mut array = arg_values.clone();
            array.extend((max..16).map(|_| String::from("0")));
            let array = array.join(", ");

            let closure_body: Vec<_> = arg_values.iter().map(|n| format!("{} == c", n)).collect();
            let closure = format!("|c| {}", closure_body.join(" || "));

            format!(
                "({}) => ($crate::v2::Bytes::new([{}], {}, {}));\n",
                args, array, max, closure
            )
        })
        .collect();

    write!(
        f,
        r#"
#[macro_export]
macro_rules! bytes {{
{}}}
"#,
        arms
    ).unwrap_or_else(|e| panic!("Could not write {}: {}", base.display(), e));
}

fn v2_simd_macros() {
    let mut base: PathBuf = env::var_os("OUT_DIR").unwrap().into();
    base.push("src");
    base.push("v2");

    fs::create_dir_all(&base)
        .unwrap_or_else(|e| panic!("Could not create directory {}: {}", base.display(), e));

    base.push("simd_macros.rs");

    let mut f = File::create(&base)
        .unwrap_or_else(|e| panic!("Could not create {}: {}", base.display(), e));

    let arms: String = (1..=16)
        .map(|max| {
            let args: Vec<_> = (0..max).map(|i| format!("$b{:02}:expr", i)).collect();
            let args = args.join(", ");

            let mut array: Vec<_> = (0..max).map(|i| format!("$b{:02}", i)).collect();
            array.extend((max..16).map(|_| String::from("0")));
            let array = array.join(", ");

            format!(
                "({}) => ($crate::v2::simd::Fast::new([{}], {}));\n",
                args, array, max
            )
        })
        .collect();

    write!(
        &mut f,
        r#"
#[allow(unused_macros)]
macro_rules! fast {{
{}}}
"#,
        arms
    ).unwrap_or_else(|e| panic!("Could not write {}: {}", base.display(), e));
}
