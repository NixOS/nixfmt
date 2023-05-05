[
  (with       b;       c)
  (with       b; /*b*/ c)
  (with /*a*/ b;       c)
  (with /*a*/ b; /*b*/ c)
  ( with b; cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc )
  ( with b;
  cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc )
  { a = with b; 1;}
  { a = with b; 1 + 1;}
  { a = with b; {c=1;};}
  { a = with b; {c=1; d=2; e=3;};}
  { a = with b;
      # comment
      1;
      }
  { a = with b;
      1;
      # comment
      }
 (with a; with b; with c; {a=1;})
 (with a; with b; with c; {a=1;b=2;})
 (with a; /* comment */ with b; with c; {a=1;b=2;})
  { a = with b;with b;with b;
      1;
      }
    {
      binPath =
    with pkgs;
    makeBinPath (
       [
         rsync
         util-linux]);}
]
