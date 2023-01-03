use prost_types::FileDescriptorSet;
use std::ffi::OsStr;
use std::fs;
use std::io::{Error, Result};
use std::path::Path;

mod parsers;

#[derive(Debug, Default)]
pub struct Config {}

impl Config {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn parse(
        &mut self,
        protos: &[impl AsRef<Path>],
        includes: &[impl AsRef<Path>],
    ) -> Result<FileDescriptorSet> {
        let mut file = Vec::new();
        for proto in protos {
            let data = fs::read(proto.as_ref()).map_err(|e| {
                Error::new(
                    e.kind(),
                    format!("unable to open proto file: {:?}, OS: {}", proto.as_ref(), e),
                )
            })?;
            let mut file_descriptor = parsers::parse(data)?;
            file_descriptor.name = proto
                .as_ref()
                .file_name()
                .and_then(OsStr::to_str)
                .map(String::from);
            if let Some(ref package) = file_descriptor.package {
                for service in &mut file_descriptor.service {
                    for mut method in &mut service.method {
                        method.input_type = method.input_type.as_ref().map(|name| {
                            if name.starts_with('.') {
                                name.into()
                            } else {
                                format!(".{package}.{name}")
                            }
                        });
                        method.output_type = method.output_type.as_ref().map(|name| {
                            if name.starts_with('.') {
                                name.into()
                            } else {
                                format!(".{package}.{name}")
                            }
                        });
                    }
                }
            }
            file.push(file_descriptor);
        }
        // TODO parse imports
        Ok(FileDescriptorSet { file })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use prost::Message;

    #[test]
    fn parse_smoke_test() {
        let buf = fs::read("src/fixtures/smoke_test/file_descriptor").unwrap();
        let mut fds = FileDescriptorSet::decode(&*buf).unwrap();
        fds.file[0].source_code_info = None;
        fds.file[0].syntax = Some(String::from("proto2"));
        let fds = dbg!(fds);
        let file_descriptor_set = Config::new()
            .parse(&["src/fixtures/smoke_test/smoke_test.proto"], &["src"])
            .unwrap();
        assert_eq!(file_descriptor_set, fds);
    }
}
