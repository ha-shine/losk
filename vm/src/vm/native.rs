use crate::vm::StackValue;
use std::time::{SystemTime, UNIX_EPOCH};
use crate::object::NativeValue;

pub(super) fn clock(_: &[StackValue]) -> Option<NativeValue> {
    let start = SystemTime::now();
    let since_epoch = start.duration_since(UNIX_EPOCH).unwrap();
    Some(NativeValue::Num(since_epoch.as_millis() as usize))
}
