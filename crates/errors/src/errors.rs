use std::fmt;

#[derive(Debug, Clone)]
pub struct ErrorMsg {
    file_name: String,
    package_name: String,

    message: String,
    line_no: usize,
    column_no: usize,
}

impl fmt::Display for ErrorMsg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, r#"Error {}
at {}:{}:{} in package: {}"#, self.message, self.file_name, self.line_no, self.column_no, self.package_name)
    }
}


pub trait IntoError {
    fn create_error(self) -> ErrorMsg;
}

pub trait ErrorCreation {
    fn add_line(&mut self, line_no: usize) -> &mut ErrorMsg;
    fn add_column(&mut self, column_no: usize) -> &mut ErrorMsg;
    fn add_file_name(&mut self, file_name: &str) -> &mut ErrorMsg;
    fn add_package_name(&mut self, package_name: &str) -> &mut ErrorMsg;
}

impl ErrorMsg {
    pub fn new() -> Self {
        return Self {
            file_name: String::new(),
            package_name: String::new(),

            message: String::new(),
            line_no: 0,
            column_no: 0,
        }
    }

    pub fn from(message: String) -> Self {
        return Self {
            file_name: String::new(),
            package_name: String::new(),

            message,
            line_no: 0,
            column_no: 0,
        }

    }

    pub fn message(error_message: String, line_no: usize, column_no: usize, file_name: String, package_name: String) -> String {
        format!(r#"Error {error_message}
at {file_name}:{line_no}:{column_no} in package: {package_name}"#)
    }
}

//impl DescError for ErrorMsg {}

impl IntoError for String {
    fn create_error(self) -> ErrorMsg {
        return ErrorMsg::from(self)
    }

}

impl ErrorCreation for ErrorMsg {
    fn add_line(&mut self, line: usize) -> &mut ErrorMsg {
        self.line_no = line;
        return self;
    }

    fn add_column(&mut self, column_no: usize) -> &mut ErrorMsg {
        self.column_no = column_no;
        return self;
    }

    fn add_file_name(&mut self, file_name: &str) -> &mut ErrorMsg {
        self.file_name = file_name.to_string();
        return self;
    }

    fn add_package_name(&mut self, package_name: &str) -> &mut ErrorMsg {
        self.package_name= package_name.to_string();
        return self;
    }
}
