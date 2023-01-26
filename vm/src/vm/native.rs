use std::time::{SystemTime, UNIX_EPOCH};

use crate::object::NativeValue;
use crate::vm::StackValue;

pub(super) fn clock(_: &[StackValue]) -> Option<NativeValue> {
    let start = SystemTime::now();
    let since_epoch = start.duration_since(UNIX_EPOCH).unwrap();
    Some(NativeValue::Num(since_epoch.as_secs_f64()))
}
