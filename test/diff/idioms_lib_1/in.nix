{
  traceIf =
    # Predicate to check
    pred:
    # Message that should be traced
    msg:
    # Value to return
    x: if pred then trace msg x else x;
}
