use std::fs;
use std::fs::OpenOptions;
use std::path::PathBuf;
use std::io;
use std::io::Write;


pub fn read_from_file(file_name: PathBuf) -> Result<String, std::io::Error> {
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
