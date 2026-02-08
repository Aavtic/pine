use std::fs;
use std::fs::OpenOptions;
use std::path::PathBuf;
use std::io;
use std::io::Write;


pub fn read_from_file(file_name: &PathBuf) -> Result<String, std::io::Error> {
    fs::read_to_string(file_name)
}

pub fn get_file_ptr(file_name: PathBuf) -> io::Result<fs::File> {
    fs::File::create(file_name.clone()).unwrap();
    OpenOptions::new()
        .write(true)
        .append(true)
        .create(true) 
        .open(file_name)
}

pub fn append_to_file(file_ptr: &mut fs::File, data: &str) -> Result<(), std::io::Error> {
    writeln!(file_ptr, "{}", data)
}

pub fn get_all_files_in_dir(dir_path: &str) -> Vec<String> {
    let mut result = Vec::new();
    // read_dir returns an iterator, which can be looped through
    for entry in fs::read_dir(dir_path).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        
        // Optional: filter to only show files, not directories or other types
        if path.is_file() {
            result.push(path.to_str().unwrap().to_string());
        }
    }

    return result;
}
