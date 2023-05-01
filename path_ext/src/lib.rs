//! A trait with implementation that extends [`std::path::Path`
//! ](https://doc.rust-lang.org/std/path/struct.Path.html).

use std::fs::{self, read_dir, symlink_metadata, ReadDir};
use std::path::{Path, PathBuf};
use std::time::SystemTime;

/// The iterator returned by [`canonical_entries()`](fn.canonical_entries).
///
#[derive(Debug)]
pub struct CanonicalEntries(ReadDir);

impl Iterator for CanonicalEntries {
    type Item = PathBuf;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let entry = self.0.next();
            match entry {
                Some(result) => match result {
                    Ok(entry) => match entry.path().canonicalize() {
                        Ok(path) => return Some(path),
                        Err(_) => (),
                    },
                    Err(_) => (),
                },
                None => return None,
            }
        }
    }
}

/// The iterator returned by [`dir_entries()`
/// ](fn.dir_entries).
///
#[derive(Debug)]
pub struct DirEntries(ReadDir);

impl Iterator for DirEntries {
    type Item = PathBuf;
    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let entry = self.0.next();
            match entry {
                Some(result) => match result {
                    Ok(entry) => return Some(entry.file_name().into()),
                    Err(_) => (),
                },
                None => return None,
            }
        }
    }
}

/// Used by PathExt
///
#[derive(Clone, Copy, Debug, Eq, Hash, PartialEq)]
pub enum FileType {
    Directory,
    File,
    Symlink,
    Unknown,
}

/// Convenience:
/// - replace FileType
/// - Tweak some functions to suit the author's coding style.
///
/// Only intended to be implemented for Path.
///
pub trait PathExt
where
    Self: AsRef<Path>,
{
    /// Iterate over the entries in `self`, which must be a directory, as
    /// [canonicalized
    /// ](https://doc.rust-lang.org/std/path/struct.PathBuf.html#method.canonicalize)
    /// `PathBuf`s.
    ///
    /// ## Errors
    ///
    /// Forwards any filesystem errors.
    ///
    fn canonical_entries(&self) -> std::io::Result<CanonicalEntries>;

    /// [`fs::copy`](https://doc.rust-lang.org/std/fs/fn.copy.html), but also
    /// setting destination mtime and converting errors.
    ///
    fn copy(&self, destination: &Path) -> std::io::Result<()>;

    /// Essentially [`read_dir()`
    /// ](https://doc.rust-lang.org/std/fs/fn.read_dir.html). The differences are:
    /// - The type returned by the iterator is [`PathBuf`
    ///   ](https://doc.rust-lang.org/std/path/struct.PathBuf.html).
    /// - Errors during iteration are silently supressed.
    ///
    /// ## Errors
    ///
    /// Forwards any errors from [`read_dir()`
    /// ](https://doc.rust-lang.org/std/fs/fn.read_dir.html).
    ///
    fn dir_entries(&self) -> std::io::Result<DirEntries>;

    /// Essentially fs::symlink_metadata() and find out the file type. File
    /// system error => `FileType::Unknown`.
    ///
    fn file_type(&self) -> FileType;

    /// If `path` is a symlink, follow it recursively. Otherwise equivalent to
    /// [`to_path_buf()`
    /// ](https://doc.rust-lang.org/std/path/struct.Path.html#method.to_path_buf).
    ///
    /// Note that this implies that there is no error even if `self` does not
    /// refer to an accessible file system object.
    ///
    fn follow_links(&self) -> PathBuf;

    /// [`path::parent()`
    /// ](https://doc.rust-lang.org/std/path/struct.Path.html#method.parent),
    /// but converting `None` to an [`Error`](struct.Error.html).
    ///
    fn parent_or_error(&self) -> std::io::Result<&Path>;

    /// Use [`fs::symlink_metadata()`
    /// ](https://doc.rust-lang.org/std/fs/fn.symlink_metadata.html) to find
    /// the size and modification time.
    ///
    /// ## Errors
    ///
    /// Forwards any errors from [`symlink_metadata()`
    /// ](https://doc.rust-lang.org/std/fs/fn.symlink_metadata.html).
    ///
    fn size_mtime(&self) -> std::io::Result<(usize, i64)>;
}

impl PathExt for Path {
    fn follow_links(&self) -> PathBuf {
        let mut path = self.to_path_buf();
        loop {
            if path.is_symlink() {
                if let Ok(target) = path.read_link() {
                    path = target;
                    continue;
                }
            }
            break;
        }
        path
    }

    fn canonical_entries(&self) -> std::io::Result<CanonicalEntries> {
        Ok(CanonicalEntries(read_dir(self)?))
    }

    fn copy(&self, destination: &Path) -> std::io::Result<()> {
        use filetime::{set_file_mtime, FileTime};
        fs::copy(self, destination)?;
        set_file_mtime(
            destination,
            FileTime::from_last_modification_time(&symlink_metadata(self)?),
        )?;
        Ok(())
    }

    fn dir_entries(&self) -> std::io::Result<DirEntries> {
        Ok(DirEntries(read_dir(self)?))
    }

    fn file_type(&self) -> FileType {
        match symlink_metadata(self) {
            Ok(meta) => {
                if meta.is_dir() {
                    FileType::Directory
                } else if meta.is_file() {
                    FileType::File
                } else if meta.is_symlink() {
                    FileType::Symlink
                } else {
                    FileType::Unknown
                }
            }
            _ => FileType::Unknown,
        }
    }

    fn parent_or_error(&self) -> std::io::Result<&Path> {
        self.parent().ok_or_else(|| {
            std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("{} has no parent directory", self.display()),
            )
        })
    }

    fn size_mtime(&self) -> std::io::Result<(usize, i64)> {
        let meta = symlink_metadata(self)?;
        let mtime = meta
            .modified()?
            .duration_since(SystemTime::UNIX_EPOCH)
            .map_err(|e| {
                std::io::Error::new(std::io::ErrorKind::Other, e.to_string())
            })?
            .as_secs() as i64;
        Ok((meta.len() as usize, mtime))
    }
}

#[cfg(test)]
#[allow(unused_assignments)]
#[allow(unused_mut)]
#[allow(unused_variables)]
mod tests {
    use super::*;
    use std::fs::{create_dir_all, read, remove_dir_all, write};
    use std::os::unix::fs::symlink;
    use std::thread::sleep;
    use std::time::Duration;

    // (root, foo, bar, fil, sym) where sym -> anonymous -> fil
    fn create_files() -> (PathBuf, PathBuf, PathBuf, PathBuf, PathBuf) {
        let root =
            std::env::temp_dir().join(&ljumvall_test_utils::random_hex());
        let _ = create_dir_all(&root);
        let foo = root.join("foo");
        let _ = create_dir_all(&foo);
        //let foo = foo.canonicalize().unwrap();
        let fil = foo.join("fil.txt");
        let _ = write(&fil, "test");
        let bar = root.join("bar");
        let _ = create_dir_all(&bar);
        //let bar = bar.canonicalize().unwrap();
        let sym1 = foo.join("sym1");
        let _ = symlink(&fil, &sym1);
        let sym = bar.join("sym");
        let _ = symlink(&sym1, &sym);
        (root, foo, bar, fil, sym)
    }

    speculate2::speculate! {
        describe "impl PathExt for Path" {
            before {
                let (root, foo, bar, fil, sym) = create_files();
            }

            after {
                let _ = remove_dir_all(&root);
            }

            context "canonical_entries()" {
                it "iterates over canonicalized entries" {
                    let entries = root.canonical_entries()
                        .unwrap()
                        .collect::<Vec<_>>();
                    assert_eq!(entries.len(), 2);
                    assert!(entries.contains(&foo));
                    assert!(entries.contains(&bar));
                }

                it "resolves symlinks" {
                    assert_eq!(
                        bar.canonical_entries().unwrap().collect::<Vec<_>>(),
                        vec![fil],
                    );
                }

                it "XXX NOT TESTED supresses errors NOT TESTED XXX" {
                    // too much trouble provoking DirRead::next() to return
                    // Some(Err(_))
                }

                it "forwards file system errors" {
                    assert!(bar.join("no_dir").canonical_entries().is_err());
                }
            }

            context "copy()" {
                it "copies contents" {
                    let dest = root.join("dest.txt");
                    assert!(fil.copy(&dest).is_ok());
                    let copy = read(dest);
                    assert!(copy.is_ok());
                    let copy = copy.unwrap();
                    let copy = std::str::from_utf8(copy.as_slice());
                    assert!(copy.is_ok());
                    assert_eq!(copy.unwrap(), "test");
                }

                it "copies mtime" {
                    let dest = root.join("dest.txt");
                    // ensure mtime differs
                    sleep(Duration::from_millis(1000));
                    let _ = fs::copy(&fil, &dest);
                    let (_, raw_mtime) = dest.size_mtime().unwrap();
                    assert!(fil.copy(&dest).is_ok());
                    let (_, self_mtime) = fil.size_mtime().unwrap();
                    let (_, dest_mtime) = dest.size_mtime().unwrap();
                    assert!(raw_mtime > dest_mtime);
                    assert_eq!(dest_mtime, self_mtime);
                }
            }

            context "dir_entries()" {
                it "iterates over entries" {
                    let entries = root.dir_entries()
                        .unwrap()
                        .collect::<Vec<_>>();
                    assert_eq!(entries.len(), 2);
                    assert!(entries.contains(
                        &PathBuf::from(foo.file_name().unwrap())
                    ));
                    assert!(entries.contains(
                        &PathBuf::from(bar.file_name().unwrap())
                    ));
                }

                it "does not resolve symlinks" {
                    let entries =
                        bar.dir_entries().unwrap().collect::<Vec<_>>();
                    assert_eq!(
                        entries,
                        vec![PathBuf::from(sym.file_name().unwrap())],
                    );
                }

                it "forwards file system errors" {
                    assert!(bar.join("no_dir").dir_entries().is_err());
                }
            }

            context "file_type()" {
                it "returns File for files" {
                    assert_eq!(fil.file_type(), FileType::File);
                }

                it "returns Directory for directories" {
                    assert_eq!(foo.file_type(), FileType::Directory);
                }

                it "returns Symlink for symlinks" {
                    assert_eq!(sym.file_type(), FileType::Symlink);
                }

                it "returns Unknown for e.g. devices" {
                    assert_eq!(
                        PathBuf::from("/dev/tty").file_type(),
                        FileType::Unknown,
                    );
                }

                it "returns Unknown for e.g. inaccessible files" {
                    assert_eq!(
                        bar.join("no_dir").file_type(),
                        FileType::Unknown,
                    );
                }
            }

            context "follow_links()" {
                it "follows recursively" {
                    assert_eq!(sym.follow_links(), fil);
                }

                it "no symlink -> to_path_buf()" {
                    assert_eq!(fil.follow_links(), fil.to_path_buf());
                }

                it "ignores file system errors" {
                    let no_file = root.join("no_file");
                    assert_eq!(no_file.follow_links(), no_file.to_path_buf());
                }
            }

            context "parent_or_error()" {
                it "is equivalent to parent() if successful" {
                    assert_eq!(
                        fil.parent_or_error().unwrap(),
                        fil.parent().unwrap(),
                    );
                }

                it "returns Ok(_) for inaccessible file" {
                    let none = bar.join("none");
                    let result = none.parent_or_error();
                    assert!(result.is_ok());
                    assert_eq!(result.unwrap(), &bar);
                }

                it "returns error if no parent" {
                    let result = Path::new("/").parent_or_error();
                    assert!(result.is_err());
                    assert_eq!(
                        result.err().unwrap().to_string(),
                        "/ has no parent directory",
                    );
                }
            }

            context "size_mtime()" {
                it "returns size and mtime" {
                    let (size, mtime) = fil.size_mtime().unwrap();
                    assert_eq!(size, 4);
                    assert_eq!(
                        mtime,
                        symlink_metadata(&fil).unwrap()
                            .modified().unwrap()
                            .duration_since(SystemTime::UNIX_EPOCH).unwrap()
                            .as_secs() as i64,
                    );
                }

                it "uses symlink_metadata()" {
                    let (size, mtime) = sym.size_mtime().unwrap();
                    assert_eq!(
                        size,
                        symlink_metadata(&sym).unwrap().len() as usize,
                    );
                    assert_eq!(
                        mtime,
                        symlink_metadata(&sym).unwrap()
                            .modified().unwrap()
                            .duration_since(SystemTime::UNIX_EPOCH).unwrap()
                            .as_secs() as i64,
                    );
                }

                it "forwards file system errors" {
                    assert!(bar.join("no_dir").size_mtime().is_err());
                }
            }
        }
    }
}
