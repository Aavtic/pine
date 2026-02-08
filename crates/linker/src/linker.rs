use std::{io::Error, process::Output};
use utils::get_all_files_in_dir;

use inkwell::{
    module::Module,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, FileType},
    OptimizationLevel
};

pub struct ObjectCompiler;
pub struct ObjectLinker;

impl ObjectCompiler {
    pub fn compile_module(module: &Module, name: &str) {
        const OPTIMIZATION_LEVEL: OptimizationLevel = OptimizationLevel::Aggressive;
        const RELOC_MODE: RelocMode = RelocMode::PIC;
        const CODE_MODEL: CodeModel = CodeModel::Large;

        Target::initialize_all(&InitializationConfig::default());
        let target_triple = TargetMachine::get_default_triple();
        let target = Target::from_triple(&target_triple).unwrap();
        let target_machine = target.create_target_machine(
            &target_triple,
            "generic",
            "",
            OPTIMIZATION_LEVEL,
            RELOC_MODE,
            CODE_MODEL
        ).unwrap();

        let output_name = format!("{}.o", name);
        let output_path = std::path::Path::new(&output_name);
        target_machine.write_to_file(module, FileType::Object, output_path).unwrap();
    }
}

impl ObjectLinker {
    pub fn link(module_name: &str, output: &str, runtime_objects: Option<Vec<String>>) -> Result<Output, Error> {
        let mut output_path = output.to_string();
        if cfg!(windows) && !output.contains(".exe") {
            output_path = format!("{}.exe", output_path);
        }

        if let Some(runtime_objs) = runtime_objects {
            let input = format!("{}.o", module_name);
            let linker_output = std::process::Command::new("cc")
                .arg(input.clone())
                .args(runtime_objs)
                .arg("-o")
                .arg(output_path)
                .output();

            std::fs::remove_file(input).expect("Unable to delete object file");
            linker_output
        } else {
            let input = format!("{}.o", module_name);
            let linker_output = std::process::Command::new("cc")
                .arg(input.clone())
                .arg("-o")
                .arg(output_path)
                .output();

            std::fs::remove_file(input).expect("Unable to delete object file");
            linker_output
        }
    }

    // output is only used to identify if it is windows or linux
    pub fn compile_runtime(module_name: &str) -> Option<Vec<String>> {
        let runtime_path = "./runtime/c/";
        let runtime_files = get_all_files_in_dir(runtime_path);

        if runtime_files.len() == 0 { return None }

        let mut object_files = Vec::new();

        for runtime_file in runtime_files {
            let out_file = std::path::Path::new(&runtime_file).file_name().unwrap().to_str().unwrap();
            let output = format!("{}_{}", module_name, out_file.replace(".c", ".o"));
            std::process::Command::new("cc")
                .arg("-c")
                .arg(runtime_file)
                .arg("-o")
                .arg(output.clone())
                .output().unwrap();
            object_files.push(output);
        }

        return Some(object_files);
    }
}

