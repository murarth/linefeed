use std::env::{home_dir, var_os};
use std::path::PathBuf;

pub fn env_init_file() -> Option<PathBuf> {
    var_os("INPUTRC").map(PathBuf::from)
}

pub fn system_init_file() -> Option<PathBuf> {
    Some(PathBuf::from("/etc/inputrc"))
}

pub fn user_init_file() -> Option<PathBuf> {
    home_dir().map(|p| p.join(".inputrc"))
}
