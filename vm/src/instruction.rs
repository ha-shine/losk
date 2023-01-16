#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) struct Constant {
    pub(crate) index: u8,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) struct StackOffset {
    pub(crate) index: u8,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) struct JumpDist {
    pub(crate) dist: usize,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) struct ArgCount {
    pub(crate) count: usize,
}
