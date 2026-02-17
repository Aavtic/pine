pub fn stdlib_path() -> std::path::PathBuf {
    let exe = std::env::current_exe().expect("Cannot get compiler path");
    exe
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .join("lib")
        .join("stdlib")
}
