rec /**/ {

  a = (((4)));
  a = (((a: b)));

  a = {a     =     1     ;};


  b = {a     =     1/*d*/;};


  c = {a     =/*c*/1     ;};
  d = {a     =/*c*/1/*d*/;};
  e = {a/*b*/=     1     ;};
  f = {a/*b*/=     1/*d*/;};
  h = {a/*b*/=/*c*/1     ;};
  i = {a/*b*/=/*c*/1/*d*/;};
  j = a:       {    b = 1           ;};
  k = a:       {    b = 1;     c = 2;};
  l = a: /*b*/ {    b = 1           ;};
  m = a: /*b*/ {    b = 1;     c = 2;};
  n = pkgs: { };
  o = { pkgs
  , ...
  }: { };

  a
  /*b*/
  =
    /*c*/
    1
    /*d*/
    ;

  p =
  aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa { }
  a;


}
