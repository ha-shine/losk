use ahash::AHasher;
use nohash::IsEnabled;
use std::hash::{Hash, Hasher};
use std::ops::{Deref, DerefMut};

#[derive(Clone, Debug, Eq)]
pub(crate) struct Hashed<T> {
    pub(crate) val: T,
    pub(crate) hash: u64,
}

impl<T> Hashed<T>
where
    T: Hash,
{
    pub(crate) fn new(val: T) -> Self {
        let mut hasher = AHasher::default();
        val.hash(&mut hasher);

        Hashed {
            val,
            hash: hasher.finish(),
        }
    }
}

impl<T> IsEnabled for Hashed<T> {}

impl<T> Hash for Hashed<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        state.write_u64(self.hash);
    }
}

impl<T> PartialEq for Hashed<T>
where
    T: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        self.hash == other.hash && self.val == other.val
    }
}

impl<T> Deref for Hashed<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.val
    }
}

impl<T> DerefMut for Hashed<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.val
    }
}
