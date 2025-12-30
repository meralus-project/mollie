use std::{
    fmt::Debug,
    hash::Hash,
    marker::PhantomData,
    ops::{Index, IndexMut},
};

use serde::Serialize;

#[macro_export]
macro_rules! new_idx_type {
    ($name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
        #[cfg_attr(feature = "serde", derive(serde::Serialize))]
        pub struct $name(usize);

        impl $crate::Idx for $name {
            fn new(idx: usize) -> Self {
                Self(idx)
            }

            fn index(self) -> usize {
                self.0
            }
        }
    };
}

pub trait Idx: Debug + Copy + PartialEq + Eq + Hash + 'static {
    fn new(idx: usize) -> Self;

    fn index(self) -> usize;
}

#[derive(Debug, Clone, Serialize)]
#[serde(bound = "T: Serialize")]
pub struct IndexBoxedSlice<I: Idx, T> {
    #[serde(flatten)]
    pub raw: Box<[T]>,
    _phantom: PhantomData<I>,
}

impl<I: Idx, T> IndexBoxedSlice<I, T> {
    pub const fn len(&self) -> usize {
        self.raw.len()
    }

    pub const fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    pub fn get(&self, index: I) -> Option<&T> {
        self.raw.get(index.index())
    }

    pub fn get_mut(&mut self, index: I) -> Option<&mut T> {
        self.raw.get_mut(index.index())
    }

    pub fn into_values(self) -> impl Iterator<Item = T> {
        self.raw.into_iter()
    }

    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.raw.iter()
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.raw.iter_mut()
    }

    #[allow(clippy::should_implement_trait)]
    pub fn into_iter(self) -> impl Iterator<Item = (I, T)> {
        self.raw.into_iter().enumerate().map(|(i, value)| (I::new(i), value))
    }

    pub fn iter(&self) -> impl Iterator<Item = (I, &T)> {
        self.raw.iter().enumerate().map(|(i, value)| (I::new(i), value))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (I, &mut T)> {
        self.raw.iter_mut().enumerate().map(|(i, value)| (I::new(i), value))
    }
}

impl<I: Idx, T> Default for IndexBoxedSlice<I, T> {
    fn default() -> Self {
        Self {
            raw: Box::default(),
            _phantom: PhantomData,
        }
    }
}

impl<I: Idx, T> Index<I> for IndexBoxedSlice<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        &self.raw[index.index()]
    }
}

impl<I: Idx, T> From<Box<[T]>> for IndexBoxedSlice<I, T> {
    fn from(value: Box<[T]>) -> Self {
        Self {
            raw: value,
            _phantom: PhantomData,
        }
    }
}

impl<I: Idx, T> From<Vec<T>> for IndexBoxedSlice<I, T> {
    fn from(value: Vec<T>) -> Self {
        value.into_boxed_slice().into()
    }
}

#[derive(Debug, Clone, Serialize)]
#[serde(bound = "T: Serialize")]
pub struct IndexVec<I: Idx, T> {
    #[serde(flatten)]
    pub raw: Vec<T>,
    _phantom: PhantomData<I>,
}

impl<I: Idx, T> IndexVec<I, T> {
    pub const fn new() -> Self {
        Self {
            raw: Vec::new(),
            _phantom: PhantomData,
        }
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            raw: Vec::with_capacity(capacity),
            _phantom: PhantomData,
        }
    }

    pub fn get(&self, index: I) -> Option<&T> {
        self.raw.get(index.index())
    }

    pub fn get_mut(&mut self, index: I) -> Option<&mut T> {
        self.raw.get_mut(index.index())
    }

    pub const fn len(&self) -> usize {
        self.raw.len()
    }

    pub const fn is_empty(&self) -> bool {
        self.raw.is_empty()
    }

    pub fn push(&mut self, value: T) {
        self.raw.push(value);
    }

    pub fn insert(&mut self, value: T) -> I {
        let index = I::new(self.raw.len());

        self.raw.push(value);

        index
    }

    pub fn into_values(self) -> impl Iterator<Item = T> {
        self.raw.into_iter()
    }

    pub fn values(&self) -> impl Iterator<Item = &T> {
        self.raw.iter()
    }

    pub fn values_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.raw.iter_mut()
    }

    #[allow(clippy::should_implement_trait)]
    pub fn into_iter(self) -> impl Iterator<Item = (I, T)> {
        self.raw.into_iter().enumerate().map(|(i, value)| (I::new(i), value))
    }

    pub fn iter(&self) -> impl Iterator<Item = (I, &T)> {
        self.raw.iter().enumerate().map(|(i, value)| (I::new(i), value))
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = (I, &mut T)> {
        self.raw.iter_mut().enumerate().map(|(i, value)| (I::new(i), value))
    }

    pub fn truncate(&mut self, len: usize) {
        self.raw.truncate(len);
    }

    pub fn into_boxed_slice(self) -> IndexBoxedSlice<I, T> {
        IndexBoxedSlice {
            raw: self.raw.into_boxed_slice(),
            _phantom: PhantomData,
        }
    }
}

impl<I: Idx, T> FromIterator<T> for IndexBoxedSlice<I, T> {
    fn from_iter<Iter: IntoIterator<Item = T>>(iter: Iter) -> Self {
        Self {
            raw: iter.into_iter().collect(),
            _phantom: PhantomData,
        }
    }
}

impl<I: Idx, T> FromIterator<T> for IndexVec<I, T> {
    fn from_iter<Iter: IntoIterator<Item = T>>(iter: Iter) -> Self {
        Self {
            raw: iter.into_iter().collect(),
            _phantom: PhantomData,
        }
    }
}

impl<I: Idx, T> Default for IndexVec<I, T> {
    fn default() -> Self {
        Self {
            raw: Vec::new(),
            _phantom: PhantomData,
        }
    }
}

impl<I: Idx, T> Index<I> for IndexVec<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        &self.raw[index.index()]
    }
}

impl<I: Idx, T> IndexMut<I> for IndexVec<I, T> {
    fn index_mut(&mut self, index: I) -> &mut Self::Output {
        &mut self.raw[index.index()]
    }
}

impl<I: Idx, T> From<Vec<T>> for IndexVec<I, T> {
    fn from(value: Vec<T>) -> Self {
        Self {
            raw: value,
            _phantom: PhantomData,
        }
    }
}
