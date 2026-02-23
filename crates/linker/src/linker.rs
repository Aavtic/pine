use std::{io::Error, process::Output};
use std::path::{Path, PathBuf}; 
use utils::get_all_files_in_dir;
use config::stdlib;

use inkwell::{
    module::Module,
    targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine, FileType},
    OptimizationLevel
};

pub struct ObjectCompiler;
pub struct ObjectLinker;

impl ObjectCompiler {
    pub fn compile_module(module: &Module, name: &str, output_dir: &Path) -> PathBuf {
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

        let file_name = Path::new(name)
            .file_name()
            .unwrap()
            .to_str()
            .unwrap();
        let output_name = if file_name.contains(".alp") {
            file_name.replace(".alp", ".o")
        } else {
            format!("{}.o", file_name)
        };

        let output_path = output_dir.join(std::path::Path::new(&output_name));
        target_machine.write_to_file(module, FileType::Object, &output_path).unwrap();
        return output_path;
    }
}

impl ObjectLinker {
    pub fn link(output: &str, object_files: Vec<PathBuf>, runtime_objects: Option<Vec<String>>) -> Result<std::process::ExitStatus, Error> {
        let mut output_path = output.to_string();
        if cfg!(windows) && !output.contains(".exe") {
            output_path = format!("{}.exe", output_path);
        }

        let mut obj_files: Vec<String> = object_files.iter().map(|obj| obj.to_str().unwrap().to_string()).collect();

        if let Some(runtime_objs) = runtime_objects {
            obj_files.extend(runtime_objs);
        }

        //let input = format!("{}.o", module_name);
        let linker_output = std::process::Command::new("cc")
            .args(obj_files.clone())
            .arg("-o")
            .arg(output_path)
            .status();

        for obj_file in obj_files {
            std::fs::remove_file(obj_file).expect("Unable to delete object file");
        }

        linker_output
        

        //} else {
        //    let input = format!("{}.o", module_name);
        //    let linker_output = std::process::Command::new("cc")
        //        .arg(input.clone())
        //        .arg("-o")
        //        .arg(output_path)
        //        .output();
        //
        //    //std::fs::remove_file(input).expect("Unable to delete object file");
        //    linker_output
        //}
    }

    // output is only used to identify if it is windows or linux
    pub fn compile_runtime(module_name: &str) -> Option<Vec<String>> {
        let runtime_path = stdlib::runtime_path();
        let runtime_files = get_all_files_in_dir(runtime_path.to_str().unwrap());

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

