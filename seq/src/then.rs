pub trait Then<T> {
    fn then(self, fun: impl FnOnce(T));
}

impl<T> Then<T> for Option<T> {
    #[inline(always)]
    #[cold]
    fn then(self, fun: impl FnOnce(T)) {
        if let Some(t) = self {
            fun(t)
        }
    }
}
