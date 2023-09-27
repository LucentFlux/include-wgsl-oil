use std::{ffi::OsStr, ops::Deref, path::PathBuf};

/// A PathBuf that is absolute, exists and points to a folder that is the root of a Rust module/test/example/executable.
pub(crate) struct AbsoluteRustRootPathBuf {
    inner: PathBuf,
}

impl AbsoluteRustRootPathBuf {
    /// Creates a new [`AbsoluteRustRootPathBuf`], panicking if any requirements aren't met.
    pub(crate) fn new(path: PathBuf) -> Self {
        assert!(
            path.is_dir(),
            "`{}` is not a directory - expected a Rust root directory",
            path.display()
        );
        assert!(path.is_absolute(), "`{}` is not absolute", path.display());

        Self { inner: path }
    }
}

impl Deref for AbsoluteRustRootPathBuf {
    type Target = PathBuf;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl std::fmt::Debug for AbsoluteRustRootPathBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

/// A PathBuf that is absolute, exists and points to a Rust file
pub(crate) struct AbsoluteRustFilePathBuf {
    inner: PathBuf,
}

impl AbsoluteRustFilePathBuf {
    /// Creates a new [`AbsoluteRustFilePathBuf`], panicking if any requirements aren't met.
    pub(crate) fn new(path: PathBuf) -> Self {
        assert!(
            path.is_file(),
            "`{}` is not a file - expected a `rs` file",
            path.display()
        );
        assert!(path.is_absolute(), "`{}` is not absolute", path.display());
        assert_eq!(
            path.extension(),
            Some(OsStr::new("rs")),
            "`{}` does not have a `.rs` extension",
            path.display()
        );

        Self { inner: path }
    }

    /// Given a path to a Rust file, gives a best guess to the source of a module containing that file. Uses the following logic:
    /// - If the parent folder of the file is a sibling of a `Cargo.toml` file, then the parent folder is the source root.
    /// - If the parent folder of the file is called `bin`, and that folder's parent is called `src` and is sibling to
    ///   a `Cargo.toml` file, then the parent folder is the source root.
    /// - For each parent folder of the file, if the folder is called `src`, contains a `lib.rs` file and is sibling to
    ///   a `Cargo.toml` file, then the parent folder is the source root.
    /// - For each parent folder of the file, if the folder contains a `main.rs` file and is sibling to
    ///   a `Cargo.toml` file, then the parent folder is the source root.
    /// - For each parent folder of the file, if the folder contains a `main.rs` file and that folder's parent is sibling to
    ///   a `Cargo.toml` file, then the parent folder is the source root.
    /// - For each parent folder of the file, if the folder contains a `main.rs` file. the folder's parent folder is called `bin`,
    ///   and that folder's parent is called `src` and is sibling to a `Cargo.toml` file, then the parent folder is the source root.
    pub(crate) fn get_source_rust_root(&self) -> Option<AbsoluteRustRootPathBuf> {
        let mut source_root = self.parent()?;

        let res = |source_root: &std::path::Path| {
            Some(AbsoluteRustRootPathBuf::new(source_root.to_path_buf()))
        };

        // If the parent folder of the file is a sibling of a `Cargo.toml` file
        let possible_cargo = source_root.parent()?.join("Cargo.toml");
        if possible_cargo.is_file() {
            return res(source_root);
        }

        // If the parent folder is called `bin`, and that folder's parent is called `src` and is sibling to a `Cargo.toml` file
        let possible_src = source_root.parent()?;
        let possible_cargo = possible_src.parent()?.join("Cargo.toml");
        if source_root.ends_with("bin") && possible_src.ends_with("src") && possible_cargo.is_file()
        {
            return res(source_root);
        }

        // For each parent folder of the file,
        loop {
            // If the folder is called `src`, contains a `lib.rs` file and is sibling to a `Cargo.toml` file
            let possible_lib = source_root.join("lib.rs");
            let possible_cargo = source_root.parent()?.join("Cargo.toml");
            if source_root.ends_with("src") && possible_lib.is_file() && possible_cargo.is_file() {
                return res(source_root);
            }

            // If the folder contains a `main.rs` file and is sibling to a `Cargo.toml` file
            let possible_main = source_root.join("main.rs");
            let possible_cargo = source_root.parent()?.join("Cargo.toml");
            if possible_main.is_file() && possible_cargo.is_file() {
                return res(source_root);
            }

            // If the folder contains a `main.rs` file and that folder's parent is sibling to a `Cargo.toml` file
            let possible_main = source_root.join("main.rs");
            let possible_cargo = source_root.parent()?.parent()?.join("Cargo.toml");
            if possible_main.is_file() && possible_cargo.is_file() {
                return res(source_root);
            }

            // If the folder contains a `main.rs` file. the folder's parent folder is called `bin`,
            // and that folder's parent is called `src` and is sibling to a `Cargo.toml` file
            let possible_main = source_root.join("main.rs");
            let possible_cargo = source_root.parent()?.parent()?.parent()?.join("Cargo.toml");
            if source_root.ends_with("src/bin")
                && possible_main.is_file()
                && possible_cargo.is_file()
            {
                return res(source_root);
            }

            source_root = source_root.parent()?;
        }
    }
}

impl Deref for AbsoluteRustFilePathBuf {
    type Target = PathBuf;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl std::fmt::Debug for AbsoluteRustFilePathBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}

/// A PathBuf that is absolute, exists and points to a WGSL file
#[derive(Hash, PartialEq, Eq, Clone)]
pub(crate) struct AbsoluteWGSLFilePathBuf {
    inner: PathBuf,
}

impl AbsoluteWGSLFilePathBuf {
    /// Creates a new [`AbsoluteWGSLFilePathBuf`], panicking if any requirements aren't met.
    pub(crate) fn new(path: PathBuf) -> Self {
        assert!(
            path.is_file(),
            "`{}` is not a file - expected a `wgsl` file",
            path.display()
        );
        assert!(path.is_absolute(), "`{}` is not absolute", path.display());
        assert_eq!(
            path.extension(),
            Some(OsStr::new("wgsl")),
            "`{}` does not have a `.wgsl` extension",
            path.display()
        );

        Self { inner: path }
    }
}

impl Deref for AbsoluteWGSLFilePathBuf {
    type Target = PathBuf;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl std::fmt::Debug for AbsoluteWGSLFilePathBuf {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.inner.fmt(f)
    }
}
