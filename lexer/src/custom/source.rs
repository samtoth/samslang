// Inspired by logos

use std::fmt::Debug;
use std::ops::Range;

pub trait Source {
    type Slice: ?Sized + PartialEq + Eq + Debug;

    /// Length of the source
    fn len(&self) -> usize;

    fn read<'a, Chunk>(&'a self, offset: usize) -> Option<Chunk>
    where
        Chunk: self::Chunk<'a>;

    /// Read a chunk of bytes into an array without doing bounds checks.
    unsafe fn read_unchecked<'a, Chunk>(&'a self, offset: usize) -> Chunk
    where
        Chunk: self::Chunk<'a>;

    fn slice(&self, range: Range<usize>) -> Option<&Self::Slice>;

    unsafe fn slice_unchecked(&self, range: Range<usize>) -> &Self::Slice;

    #[inline]
    fn find_boundary(&self, index: usize) -> usize {
        index
    }

    fn is_boundary(&self, index: usize) -> bool;
}

impl Source for str {
    type Slice = str;

    #[inline]
    fn len(&self) -> usize {
        self.len()
    }

    #[inline]
    fn read<'a, Chunk>(&'a self, offset: usize) -> Option<Chunk>
    where
        Chunk: self::Chunk<'a>,
    {
        if offset + (Chunk::SIZE - 1) < self.len() {
            Some(unsafe { Chunk::from_ptr(self.as_ptr().add(offset)) })
        } else {
            None
        }
    }

    #[inline]
    unsafe fn read_unchecked<'a, Chunk>(&'a self, offset: usize) -> Chunk
    where
        Chunk: self::Chunk<'a>,
    {
        Chunk::from_ptr(self.as_ptr().add(offset))
    }

    #[inline]
    fn slice(&self, range: Range<usize>) -> Option<&str> {
        self.get(range)
    }

    #[inline]
    unsafe fn slice_unchecked(&self, range: Range<usize>) -> &str {
        debug_assert!(
            range.start <= self.len() && range.end <= self.len(),
            "Reading out of bounds {:?} for {}!",
            range,
            self.len()
        );

        self.get_unchecked(range)
    }

    #[inline]
    fn find_boundary(&self, mut index: usize) -> usize {
        while !self.is_char_boundary(index) {
            index += 1;
        }

        index
    }

    #[inline]
    fn is_boundary(&self, index: usize) -> bool {
        self.is_char_boundary(index)
    }
}

pub trait Chunk<'source>: Sized + Copy + PartialEq + Eq {
    /// Size of the chunk being accessed in bytes.
    const SIZE: usize;

    /// Create a chunk from a raw byte pointer.
    unsafe fn from_ptr(ptr: *const u8) -> Self;
}

impl<'source> Chunk for char {
    const SIZE: usize = 1;

    #[inline]
    unsafe fn from_ptr(ptr: *const u8) -> Self {
        *ptr
    }
}
