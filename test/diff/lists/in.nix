[
  []
  [
  ]
  [

  ]

  [ [

  ] ]

  [ "string"

  ]

  [ {
    # multiline
    foo = "bar";
    foo2 = "barbar";
  } ]
  { # List in attrset with comment

    imports0 = [];

    imports2 = [
      # ./disko.nix
      ./hardware-configuration.nix
    ];
    imports3 = [
      # comment
      ./disko.nix
      ./hardware-configuration.nix
    ];
  }
  [ (if foo then
    bar #multiline too
    else
    baz
  )]
  [ 1 ]

  [ 1
  ]

  [       b       d       ]
  [       b       d /*e*/ ]
  [       b /*c*/ d       ]
  [       b /*c*/ d /*e*/ ]
  [ /*a*/ b       d       ]
  [ /*a*/ b       d /*e*/ ]
  [ /*a*/ b /*c*/ d       ]
  [ /*a*/ b /*c*/ d /*e*/ ]

  [


    b


    d


  ]
  [


    /*a*/


    b


    /*c*/


    d


    /*e*/


  ]


  [ [ multi line ] ]
  [ [ [ singleton ] ] ]
  [ [ [ { } ] ] ]
  [ [ [ { } multiline ] ] ]
]
