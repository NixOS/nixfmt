let
  aLongVariableNameOrEvenDeepAttrsetGettingAssignedAnAppendedToLocalPath = ./.
    + "some/string/path/that/forces/a/wrap/to/happen";
  multiplePathConcatenationsWithLongNamesAndPathsThatDefinitelyWrap = ./base
    + "/first/very/long/path/component"
    + "/second/even/longer/path/component/that/continues";
in
"example"
