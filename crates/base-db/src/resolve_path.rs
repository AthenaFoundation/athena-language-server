use std::{path::Path, str::FromStr};

use paths::{AbsPath, AbsPathBuf};

use crate::{virtual_path::VirtualFilePath, FileId, SourceDatabase, VfsPath, VirtualFilePathBuf};

trait FileExists {
    fn file_exists(&self, db: &dyn SourceDatabase) -> bool;
}

impl FileExists for &AbsPath {
    fn file_exists(&self, db: &dyn SourceDatabase) -> bool {
        db.in_mem_contents(self).is_some() || std::fs::metadata(self).is_ok()
    }
}

impl FileExists for &VirtualFilePath {
    fn file_exists(&self, db: &dyn SourceDatabase) -> bool {
        db.get_virtual_file((*self).to_owned()).is_some()
    }
}

impl FileExists for VfsPath {
    fn file_exists(&self, db: &dyn SourceDatabase) -> bool {
        match self {
            VfsPath::Real(path) => path.as_path().file_exists(db),
            VfsPath::Virtual(path) => path.as_path().file_exists(db),
        }
    }
}

trait PathLike: Sized {
    type JoinPath<'a>: ?Sized;
    type Owned;

    fn parent(&self) -> Option<Self>;

    fn join(&self, other: &Self::JoinPath<'_>) -> Self::Owned;
}

impl<'p> PathLike for &'p AbsPath {
    type JoinPath<'a> = Path;
    type Owned = AbsPathBuf;

    fn parent(&self) -> Option<Self> {
        AbsPath::parent(self)
    }

    fn join(&self, other: &Self::JoinPath<'static>) -> AbsPathBuf {
        AbsPath::join(self, other)
    }
}

impl<'p> PathLike for &'p VirtualFilePath {
    type JoinPath<'a> = VirtualFilePath;
    type Owned = VirtualFilePathBuf;

    fn parent(&self) -> Option<Self> {
        <VirtualFilePath>::parent(self)
    }

    fn join(&self, other: &VirtualFilePath) -> VirtualFilePathBuf {
        (**self).join(other)
    }
}

impl PathLike for VfsPath {
    type JoinPath<'a> = VfsPathComponent<'a>;
    type Owned = VfsPath;

    fn parent(&self) -> Option<Self> {
        match self {
            VfsPath::Real(path) => path.parent().map(|it| it.to_owned().into()),
            VfsPath::Virtual(path) => path.parent().map(|it| it.to_owned().into()),
        }
    }

    fn join(&self, other: &Self::JoinPath<'_>) -> Self {
        match (self, other) {
            (VfsPath::Real(a), VfsPathComponent::Real(b)) => a.join(b).into(),
            (VfsPath::Real(_), VfsPathComponent::Virtual(_))
            | (VfsPath::Virtual(_), VfsPathComponent::Real(_)) => {
                panic!("Cannot join real and virtual paths")
            }
            (VfsPath::Virtual(a), VfsPathComponent::Virtual(b)) => a.join(b).into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum VfsPathComponent<'a> {
    Real(&'a Path),
    Virtual(&'a VirtualFilePath),
}

impl<'a> VfsPathComponent<'a> {
    fn matches(&self, path: &VfsPath) -> bool {
        matches!(
            (self, path),
            (VfsPathComponent::Real(_), VfsPath::Real(_))
                | (VfsPathComponent::Virtual(_), VfsPath::Virtual(_))
        )
    }
}

impl<'a> From<&'a VirtualFilePath> for VfsPathComponent<'a> {
    fn from(v: &'a VirtualFilePath) -> Self {
        Self::Virtual(v)
    }
}

impl<'a> From<&'a Path> for VfsPathComponent<'a> {
    fn from(v: &'a Path) -> Self {
        Self::Real(v)
    }
}

#[must_use]
fn find_file(db: &dyn SourceDatabase, from: FileId, path: VfsPathComponent<'_>) -> Option<FileId> {
    eprintln!("find_file({:?}, {:?})", from, path);
    let current_file = db.lookup_intern_path(from);
    let dir = current_file.parent()?;
    let candidate = dir.join(&path);
    if candidate.file_exists(db) {
        eprintln!("yuh");
        let file_id = db.intern_path(candidate);
        return Some(file_id);
    }
    let roots = db.roots();
    for root in roots.iter() {
        let comp: VfsPathComponent<'_> = path.clone();
        if !comp.matches(root) {
            continue;
        }
        let path = root.join(&comp);
        if path.file_exists(db) {
            let file_id = db.intern_path(path);
            return Some(file_id);
        }
    }
    None
}

pub(super) fn resolve_file_path_query(
    db: &dyn SourceDatabase,
    from: FileId,
    path: String,
) -> Option<FileId> {
    eprintln!("resolve_file_path_query({:?}, {:?})", from, path);
    if let Ok(virt) = VirtualFilePathBuf::try_from(&*path) {
        if virt.is_absolute() {
            eprintln!("absolute");
            if virt.as_path().file_exists(db) {
                return Some(db.intern_path(virt.into()));
            }
        }
        return find_file(db, from, virt.as_path().into());
    }

    let path = std::path::PathBuf::from_str(path.as_str()).ok()?;

    if path.is_absolute() {
        let path = AbsPathBuf::assert(path);
        if std::fs::metadata(path.as_path()).is_ok() {
            let file_id = db.intern_path(path.into());
            return Some(file_id);
        }
        None
    } else {
        let from = db.lookup_intern_path(from);
        let from = match from {
            VfsPath::Real(path) => path,
            VfsPath::Virtual(_) => return None,
        };

        let path = from.parent().unwrap().join(&path);
        if std::fs::metadata(path.as_path()).is_ok() {
            let file_id = db.intern_path(path.into());
            return Some(file_id);
        }
        None
    }
}

#[cfg(test)]
mod test {
    use core::fmt;
    use std::sync::Arc;

    use test_utils::FxHashSet;

    use crate::{FileWatcher, VirtualFileDatabase};

    use super::*;

    #[derive(Default)]
    #[salsa::database(crate::SourceDatabaseStorage, crate::VirtualFileDatabaseStorage)]
    struct TestDb {
        virtual_files: FxHashSet<VirtualFilePathBuf>,
        storage: salsa::Storage<Self>,
    }

    impl TestDb {
        fn add_file(&mut self, path: &str, s: &str) -> FileId {
            let path = virt(path);
            let contents = contents(s);
            let file_id = self.intern_path(path.clone().into());
            self.add_virtual_file(path, contents);
            file_id
        }

        fn change_roots<'a>(&mut self, roots: impl IntoIterator<Item = &'a str>) {
            let roots = roots.into_iter().map(|it| virt(it).into()).collect();

            self.set_roots(roots);
        }
    }

    impl crate::FileWatcher for TestDb {
        fn did_change_file(&mut self, _file_id: FileId) {
            unimplemented!()
        }

        fn in_mem_contents(&self, _path: &AbsPath) -> Option<std::sync::Arc<String>> {
            unimplemented!()
        }

        fn set_in_mem_contents(&mut self, _path: AbsPathBuf, _contents: std::sync::Arc<String>) {
            unimplemented!()
        }

        fn add_virtual_file(&mut self, path: VirtualFilePathBuf, contents: std::sync::Arc<String>) {
            self.virtual_files.insert(path.clone());
            self.set_virtual_file_contents(path, contents);
        }

        fn get_virtual_file(&self, path: VirtualFilePathBuf) -> Option<std::sync::Arc<String>> {
            eprintln!("get_virtual_file({path:?}) : {:?}", self.virtual_files);
            if self.virtual_files.contains(&path) {
                eprintln!("contains!");
                Some(self.virtual_file_contents(path))
            } else {
                None
            }
        }
    }

    impl fmt::Debug for TestDb {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.debug_struct("TestDb")
                .field("virtual_files", &self.virtual_files)
                .finish()
        }
    }

    impl salsa::Database for TestDb {}

    fn virt(path: &str) -> VirtualFilePathBuf {
        VirtualFilePathBuf::assert(path.into())
    }

    fn contents(s: &str) -> Arc<String> {
        Arc::new(s.into())
    }

    #[test]
    fn test_resolve_file_path() {
        let mut db = TestDb::default();
        db.change_roots(["virtual:/home"]);
        let a = db.add_file("virtual:/foo/baz", "domain A");
        let b = db.add_file("virtual:/foo/bar", "domain B");

        assert_eq!(db.resolve_file_path(a, "virtual:bar".into()), Some(b));
    }
}
