use std::{cmp::Ordering, error::Error, fmt, ops::*};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Loc {
    pub line: usize,
    pub col: usize,
}

impl Loc {
    pub fn new(line: usize, col: usize) -> Loc {
        Loc { line, col }
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: Loc,
    pub end: Loc,
}

impl Span {
    pub fn new(start: Loc, end: Loc) -> Self {
        Span { start, end }
    }
    pub fn sp<T>(self, data: T) -> Sp<T> {
        Sp { data, span: self }
    }
}

impl Sub for Span {
    type Output = Self;
    fn sub(self, other: Self) -> Self {
        Span::new(self.start.min(other.start), self.end.max(other.end))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Sp<T> {
    pub data: T,
    pub span: Span,
}

impl<T> Sp<T> {
    pub fn as_ref(&self) -> Sp<&T> {
        self.span.sp(self)
    }
    pub fn map<F, U>(self, f: F) -> Sp<U>
    where
        F: FnOnce(T) -> U,
    {
        self.span.sp(f(self.data))
    }
}

impl<T> PartialEq for Sp<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.data == other.data
    }
}

impl<T> Eq for Sp<T> where T: Eq {}

impl<T> PartialOrd for Sp<T>
where
    T: PartialOrd,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.data.partial_cmp(&other.data)
    }
}

impl<T> Ord for Sp<T>
where
    T: Ord,
{
    fn cmp(&self, other: &Self) -> Ordering {
        self.data.cmp(&other.data)
    }
}

impl<T> Deref for Sp<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        &self.data
    }
}

impl<T> DerefMut for Sp<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.data
    }
}

impl<T> fmt::Display for Sp<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} {}:{}",
            self.data, self.span.start.line, self.span.start.col
        )
    }
}

impl<T> Error for Sp<T> where T: Error {}

pub type SpResult<T, E> = Result<T, Sp<E>>;
