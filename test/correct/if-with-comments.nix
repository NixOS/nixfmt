v:

# check if v is int
if isInt v then
  # handle int v
  fromInt v
# comments here apply to the branch below, not to the value above
# check if v is bool
else if isBool v then
  # handle bool v
  fromBool v
# no idea what v could be
else
  # we give up
  error "iunno"
