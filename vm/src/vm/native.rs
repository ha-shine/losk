use crate::value::Value;
use crate::vm::StackValue;
use std::time::{SystemTime, UNIX_EPOCH};

pub(super) fn clock(_: &[StackValue]) -> Option<Value> {
    let start = SystemTime::now();
    let since_epoch = start.duration_since(UNIX_EPOCH).unwrap();
    Some(Value::Double(since_epoch.as_millis() as f64))
}
