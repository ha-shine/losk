#[derive(Debug, Copy, Clone)]
pub(crate) struct Constant {
    pub(crate) index: u8,
}

#[derive(Debug, Copy, Clone)]
pub(crate) struct StackOffset {
    pub(crate) index: u8,
}

#[derive(Debug, Copy, Clone)]
pub(crate) struct JumpDist {
    pub(crate) dist: usize,
}
