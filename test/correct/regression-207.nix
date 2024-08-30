{
  rust-channel-of-version =
    assertEq (rustChannelOf { channel = "1.48.0"; }).rustc
      stable."1.48.0".rustc;
}
