use crate::value::ConstantValue;
use crate::vm::StackValue;
use std::time::{SystemTime, UNIX_EPOCH};

pub(super) fn clock(_: &[StackValue]) -> Option<ConstantValue> {
    let start = SystemTime::now();
    let since_epoch = start.duration_since(UNIX_EPOCH).unwrap();
    Some(ConstantValue::Double(since_epoch.as_millis() as f64))
}
