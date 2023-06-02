use base_db::FileId;

use super::Get;
use std::fmt::{self, Write};

type WriteTarget<'a> = &'a mut (dyn fmt::Write + 'a);

pub(crate) fn tab(f: WriteTarget<'_>, n: usize) -> fmt::Result {
    let s = "    ";
    for _ in 0..n {
        write!(f, "{s}")?;
    }

    Ok(())
}

pub struct DebugDumper<'a> {
    pub hir: &'a super::FileHir,
    #[allow(unused)]
    source_map: &'a super::SourceMap,
    f: &'a mut dyn fmt::Write,
    indent: usize,

    last: u8,
    db: &'a dyn super::HirNewDatabase,
}

impl<'a> DebugDumper<'a> {
    pub(crate) fn get<G: Get>(&self, g: G) -> <G as Get>::Output<'a> {
        g.get(self.hir)
    }

    pub(crate) fn debug_dump<D: DebugDump>(&mut self, d: D) -> fmt::Result {
        d.debug_dump(self)
    }

    pub(crate) fn dump<G: Get>(&mut self, g: G) -> fmt::Result
    where
        <G as Get>::Output<'a>: DebugDump,
    {
        self.debug_dump(self.get(g))
    }

    pub(crate) fn tab(&mut self) -> fmt::Result {
        if self.indent == 0 {
            return Ok(());
        }
        tab(self.f, self.indent)
    }

    pub(crate) fn indent(&mut self) {
        self.indent += 1;
    }

    pub(crate) fn dedent(&mut self) {
        self.indent -= 1;
    }

    pub(crate) fn indented<R>(&mut self, func: impl FnOnce(&mut Self) -> R) -> R {
        self.indent();
        let r = func(self);
        self.dedent();
        r
    }

    pub(crate) fn comma_separated<T>(
        &mut self,
        values: impl IntoIterator<Item = T>,
        mut func: impl FnMut(&mut Self, T) -> fmt::Result,
    ) -> fmt::Result {
        for (i, value) in values.into_iter().enumerate() {
            if i != 0 {
                write!(self, ", ")?;
            }
            func(self, value)?;
        }

        Ok(())
    }

    pub(crate) fn dump_file(&mut self, file_id: FileId) -> fmt::Result {
        let s = self.db.debug_dump(file_id);
        self.write_str(&s)
    }
}

impl<'a> DebugDumper<'a> {
    pub(crate) fn new(
        hir: &'a super::FileHir,
        source_map: &'a super::SourceMap,
        f: &'a mut dyn fmt::Write,
        db: &'a dyn super::HirNewDatabase,
    ) -> Self {
        Self {
            hir,
            source_map,
            f,
            indent: 0,
            last: b'\0',
            db,
        }
    }
}

impl fmt::Write for DebugDumper<'_> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        let mut lines = s.split_inclusive('\n');
        if let Some(first) = lines.next() {
            if self.last == b'\n' {
                self.tab()?;
            }
            self.f.write_str(first)?;
        }
        for l in lines {
            self.tab()?;
            self.f.write_str(l)?;
        }
        if s.len() > 0 {
            self.last = s.as_bytes().last().copied().unwrap();
        }
        Ok(())
    }
}

pub trait DebugDump {
    fn debug_dump(&self, dd: &mut DebugDumper) -> fmt::Result;
}

impl<T> DebugDump for &T
where
    T: DebugDump,
{
    fn debug_dump(&self, dd: &mut DebugDumper) -> fmt::Result {
        (*self).debug_dump(dd)
    }
}
