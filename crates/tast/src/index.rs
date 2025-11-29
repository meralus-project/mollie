use std::{fmt::Debug, hash::Hash, marker::PhantomData};
use serde::Serialize;

#[macro_export]
macro_rules! new_idx_type {
    ($name:ident) => {
        #[derive(Debug, Serialize, Clone, Copy, PartialEq, Eq, Hash)]
        pub struct $name(usize);

        impl $crate::index::Idx for $name {
            fn new(idx: usize) -> Self {
                Self(idx)
            }

            fn index(self) -> usize {
                self.0
            }
        }
    }
}

pub trait Idx: Debug + Copy + PartialEq + Eq + Hash + 'static {
    fn new(idx: usize) -> Self;

    fn index(self) -> usize;
}

#[derive(Debug, Serialize)]
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

    pub const fn len(&self) -> usize {
        self.raw.len()
    }

    pub fn push(&mut self, value: T) {
        self.raw.push(value);
    }
}

impl<I: Idx, T> std::ops::Index<I> for IndexVec<I, T> {
    type Output = T;

    fn index(&self, index: I) -> &Self::Output {
        &self.raw[index.index()]
    }
}
