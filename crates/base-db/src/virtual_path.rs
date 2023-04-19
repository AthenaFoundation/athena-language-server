use std::{
    borrow::Borrow,
    path::{Path, PathBuf},
};

#[derive(Clone, Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct VirtualFilePathBuf(PathBuf);

#[derive(Debug, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[repr(transparent)]
pub struct VirtualFilePath(Path);

impl VirtualFilePath {
    pub fn join(&self, other: &VirtualFilePath) -> VirtualFilePathBuf {
        VirtualFilePathBuf(self.0.join(&other.0))
    }

    pub fn as_str(&self) -> &str {
        self.0.to_str().unwrap()
    }

    pub fn parent(&self) -> Option<&VirtualFilePath> {
        self.0.parent().map(VirtualFilePath::from_path)
    }

    fn from_path(path: &Path) -> &VirtualFilePath {
        // SAFETY: VirtualFilePath is a repr(transparent) wrapper around Path
        unsafe { &*(path as *const Path as *const VirtualFilePath) }
    }
}

impl std::ops::Deref for VirtualFilePathBuf {
    type Target = VirtualFilePath;

    fn deref(&self) -> &Self::Target {
        self.as_path()
    }
}

impl VirtualFilePathBuf {
    pub const PREFIX: &'static str = "virtual:";

    pub fn assert(pth: String) -> VirtualFilePathBuf {
        VirtualFilePathBuf::try_from(pth).unwrap()
    }

    pub fn as_path(&self) -> &VirtualFilePath {
        VirtualFilePath::from_path(self.0.as_path())
    }

    pub fn components(&self) -> impl Iterator<Item = std::path::Component> {
        self.0.components()
    }
}

impl TryFrom<String> for VirtualFilePathBuf {
    type Error = ();

    fn try_from(value: String) -> Result<Self, Self::Error> {
        VirtualFilePathBuf::try_from(value.as_str())
    }
}

impl TryFrom<&str> for VirtualFilePathBuf {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value.starts_with(VirtualFilePathBuf::PREFIX) {
            Ok(VirtualFilePathBuf(PathBuf::from(
                value.strip_prefix(VirtualFilePathBuf::PREFIX).unwrap(),
            )))
        } else {
            Err(())
        }
    }
}

impl ToOwned for VirtualFilePath {
    type Owned = VirtualFilePathBuf;

    fn to_owned(&self) -> Self::Owned {
        VirtualFilePathBuf(self.0.to_owned())
    }
}

impl Borrow<VirtualFilePath> for VirtualFilePathBuf {
    fn borrow(&self) -> &VirtualFilePath {
        self.as_path()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parent() {
        let path = VirtualFilePathBuf::try_from("virtual:/foo/bar").unwrap();
        assert_eq!(path.parent().unwrap().as_str(), "/foo");
    }

    #[test]
    fn test_join() {
        let path = VirtualFilePathBuf::assert("virtual:/foo/bar".into());
        let child = VirtualFilePathBuf::assert("virtual:baz".into());
        assert_eq!(path.join(&child).as_str(), "/foo/bar/baz");
    }
}
