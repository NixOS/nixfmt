/*nixfmt:disable*/
                      
                      
                      # Directive at the very start of the file 
/*nixfmt:enable*/
{
  # -- Operator chains --
  # Directive in the middle of an operator chain
  opChain =
    [ 1 2 3 ]
    ++
/*nixfmt:disable*/
    [ 4    5    6    7    8 ]
/*nixfmt:enable*/
    ++   [ 9 10 ]
  ;

  # Directive in a string concatenation chain
  strConcat =
    "line 1"
    +
/*nixfmt:disable*/
    "line   2"
    + "line   3"
/*nixfmt:enable*/
    +   "line 4";

  # -- List items --
  # Aligned list items (the most common real-world use case)
  fonts = [
/*nixfmt:disable*/
    "arial.ttf"        "arialbd.ttf"    "ariali.ttf"          "arialbi.ttf"     # Arial
    "comic.ttf"        "comicbd.ttf"    "comici.ttf"          "comicz.ttf"      # Comic Sans MS
    "consola.ttf"      "consolab.ttf"   "consolai.ttf"        "consolaz.ttf"    # Consolas
/*nixfmt:enable*/
  ];

  # -- Blank lines --
  # Multiple consecutive blank lines before a directive (should be collapsed)
  beforeBlank   =   1;



/*nixfmt:disable*/
  insideBlank    =    2;
/*nixfmt:enable*/

  # Multiple consecutive blank lines inside a disabled region (preserved as-is)
/*nixfmt:disable*/
  a    =    1;



  b    =    2;
/*nixfmt:enable*/
  afterBlank   =   3;

  # -- Nested attribute sets --
  # Directive spanning nested sets (force-expanded per spec)
  nested = {
    outer = {
/*nixfmt:disable*/
      inner    =    {
        nested   =   1;
        deep     =   2;
      };
/*nixfmt:enable*/
    };
  };

  # -- Niche cases --

  # Indented string with escape sequences near directive-like text.
  # Only disable (if the scanner misreads this, the rest of the file breaks)
  escapes = ''
    escaped quotes: '''
    escaped dollar: ''$
    escaped newline: ''\n
    /*nixfmt:disable*/
    this is string content, not a directive
  '';

  # This MUST be formatted (proves escapes didn't confuse the scanner)
  afterEscapes   =   1;

  # Consecutive interpolations
  multiInterp = ''${x}${y}${z}'';

  # This MUST be formatted
  afterMultiInterp   =   2;

  # Directive with tabs in leading/trailing whitespace
	/*nixfmt:disable*/
  tabbed    =    1;
	/*nixfmt:enable*/

  # This MUST be formatted
  afterTabbed   =   3;

  last   =   99;
}
