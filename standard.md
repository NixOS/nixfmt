# Standard Nix format

This document describes the standard Nix format, as first established [by RFC 166](https://github.com/NixOS/rfcs/blob/master/rfcs/0166-nix-formatting.md#standard-nix-format).

Controversial changes to this document must go through another RFC.
It is up to the Nix formatting team to decide when an RFC is necessary.

## Terms and definitions:

- **Brackets:** `[]`
- **Braces:** `{}`
- **Parentheses**: `()`
- **Expressions:**
  All syntax nodes that would be a syntactically correct Nix program on their own.
- **Terms:** The following expressions are called terms 
  - Variables, int, float, string, path, list, set, selection, all parenthesised expressions
  - As a rule of thumb: Expressions which can be used as list items (without parentheses)
- **Absorption:**
  A multiline expression can have an alternative layout depending on the context.
  In that case, it will start on the end of the current line instead of a new line,
  potentially saving a level of indentation of its content.
  ```nix
  {
    # The right-hand side of bindings is an example of a situation where absorption improves the style.
    absorbed = with bar; [
      1
      2
      3
    ];
    notAbsorbed =
      with bar; # Placing a comment here will force the non-absorbed, multiline layout.
      [
        1
        2
        3
      ];

    # In this case, absorption does not reduce the indentation level of the set.
    absorbed' =
      let
        qux = 1;
      in
      # { is absorbed
      bar: baz: {
        # <-- same level
      };

    notAbsorbed' =
      let
        qux = 1;
      in
      way:
      too:
      many:
      arguments:
      {
        # <-- same level
      };
  }
  ```
- **Absorbable Terms:**
  Attribute sets, lists, and multiline `''` strings are called absorbable terms. Parenthesized absorbable terms are absorbable terms again too.

## General

- The formatter should be as "pure" as possible, i.e. different input formats of the "same" code (same AST with comments) should result in the same output format.
  - The formatter may take the input formatting into account in some cases in order to preserve multi-line syntax elements (which would otherwise have been contracted by the rules).
- Line breaks may be added or removed, but empty lines must not be created. Single empty lines must be preserved, and consecutive empty lines must be collapsed into a single empty line.
  This allows the formatter to expand or compact multi-line expressions, while still allowing grouping of code.

  For example, formatting this code:
  ```nix
  [
    0 10

    (
      20 + 1
    )


    30
  ]
  ```

  turns into this:
  ```nix
  [
    0  # Line break added
    10

    (20 + 1) # Line breaks removed
             # Consecutive empty lines turned into a single empty line
    30
  ]
  ```

- Expressions of the same kind that can be treated as a sequence of expressions on the same level should be treated as such, even though they are technically parsed as a nested tree.
  - This applies to else-if chains, functions with multiple arguments, some operators, etc.
  - Example:
    ```nix
    # This is treated as a sequence of if-then-else chains, instead of indenting the second if as part of the else body
    if cond1 then
      foo
    else if cond2 then
      bar
    else
      baz
    ```

- Indentation should reflect the expression structure.
  Example:
  ```nix
  # Bad, the indentation misleads the user
  {
    foo = {
    bar = if
    baz == null then 10
      else 20
    ;
  }; }

  # Good
  {
    foo = {
      bar =
        if baz == null then
          10
        else
          20;
    };
  }
  ```

### Editor Config

This [editor config](https://editorconfig.org/) specifies the basic details about Nix files:

```editorconfig
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true
charset = utf-8
indent_style = space
```

### Single-line common ancestor expression rule

For any two (sub-)expressions that are fully on a common single line, their smallest common ancestor expression must also be on the same line.

**Example**

```nix
# Bad, expressions cond and foo are fully on the same line,
# but their smallest common ancestor expression is the entire if-then-else, which spans multiple lines
if cond then foo
else bar

# Okay, cond, foo and bar have the if-then-else as a common ancestor expression,
# which is also fully on the same line
if cond then foo else bar

# Bad, due to function application precedence, the smallest common ancestor expression
# of foo and bar is `foo || bar baz`, which spans two lines
foo || bar
  baz
```

**Rationale**

This rule has turned out to be very practical at catching code that could be potentially hard to understand or edit.

### Line length

- There should be a configurable _soft_ line length limit, limiting the number of characters on one line without counting the leading indentation.
  The default should be 100 characters.
- There may also be a configurable _hard_ line length limit, which includes the leading indentation.
- String-like values such as strings, paths, comments, urls, etc. may go over the hard line length limit.

### Indentation

- Two spaces must be used for each indentation level.
  - This may be revisited should Nix get proper support for [using tabs for indentation](https://github.com/NixOS/nix/issues/7834) in the future.
- No special care is taken to preserve vertical alignment in the AST or comments.
  - It is non-trivial to specify a rule for preserving vertical alignment, so this is out of scope for now, but could be reconsidered in the future.
  - Examples:
    ```nix
    {
      # Vertically aligned input like this..
      foo  = 10; # Foo
      b    = 10; # - b
      baz  = 10; #   - baz
      more = 10; #   - more

      # ..will get formatted like this.
      # The vertical alignment is not preserved.
      foo = 10; # Foo
      b = 10; # - b
      baz = 10; #   - baz
      more = 10; #   - more


      # Vertically aligned input like this..
      netbsd   = { execFormat = elf;     families = { inherit bsd; }; };
      none     = { execFormat = unknown; families = {              }; };

      # ..will get formatted like this.
      netbsd = {
        execFormat = elf;
        families = {
          inherit bsd;
        };
      };
      none = {
        execFormat = unknown;
        families = { };
      };


      # Vertically aligned input like this..
      optExecFormat =
        lib.optionalString (kernel.name == "netbsd" &&
                            gnuNetBSDDefaultExecFormat cpu != kernel.execFormat
                           )
                           kernel.execFormat.name;

      # ..will get formatted like this.
      optExecFormat = lib.optionalString (
        kernel.name == "netbsd" && gnuNetBSDDefaultExecFormat cpu != kernel.execFormat
      ) kernel.execFormat.name;
    }
    ```
- Increasing indentation levels must not be "skipped": On subsequent lines, indentation can only increase by at most one level, but may decrease arbitrarily many levels.
  - Examples:
    ```nix
    # Bad indentation
    buildInputs = [
        foo # <-- Not okay, increase by 2 levels
      ]
      ++ lib.optionals cond [
        bar
      ];

    # Okay indentation, subsequent lines at most one more level
    buildInputs = [
      foo
    ]
    ++ lib.optionals cond [
      bar
    ];

    # Bad indentation
    attribute = { args }: let
        foo = "bar"; # <-- Not okay, increase by 2 levels
      in
        foo;

    # Okay indentation
    attribute = { args }:
      let
        foo = "bar";
      in
        foo;

    # Bad indentation
    (callFunction {
        foo = "bar"; # <-- Not okay, increase by 2 levels
      }
      arg
    )
    # Okay indentation
    (callFunction
      {
        foo = "bar";
      }
      arg
    )

    # Okay indentation
    let
      x = {
        a = foo
          bar
          baz;
      }; # <-- The decrease by two levels here is okay, only increases are limited to one level
    in
    null
    ```

### Expansion of expressions

Unless stated otherwise, any expression that fits onto one single line must be trivially formatted as such.

For list elements, attributes, and function arguments, the following applies:

- If expanded into multiple lines, each item must be on its own line.
  - Grouping similar items together can be done by adding blank lines or comments between the groups instead.
  - This also applies to the first item, so e.g. `[ firstElement` in a multi line list is not allowed.
- Long sequences of items should be liberally expanded, even if they would fit onto one line character-wise.
  - The motivation is to keep the information per line manageable. Usually "number of elements" is a better metric for that than "line length".
  - The cutoff is usually determined empirically based on common usage patterns.

**Examples:**

```nix
{
  buildInputs = [
    foo
    bar
    baz

    somethingElse
  ];

  systemd.services = {
    foo = { };
    bar = { };
  };

  inherit
    lib
    foo
    bar
    baz
    ;
}
```

### Strings

- The kind of quotes used in strings (`"` vs `''`) must be preserved from the input.
- The non-interpolated string parts must be preserved from the input
  - E.g. changing `\t` to a tab character must not be done automatically

**Examples:**
```nix
# Kept as is
"foo \n\t ${bar} baz"
# This one too
''
  foo \n\t ${bar} baz
''

# Even if strings exceed the line length limit, no attempt to make it smaller is made
''
  This is a really long string that would not fit within the line length limit
''
```

#### Interpolations

- "Simple" interpolations must be rendered using the single-line format, regardless of the line's length.
  - Otherwise, the multiline formatting must be used
  - "simple" is implementation-defined and generally includes short expressions of low complexity.
    Multiline expressions are never "simple".
- If the interpolation is the first thing on the string line, then its contents may be absorbed.
  - Otherwise, the interpolation code must start on a new line

**Examples**:
```nix
# Short and simple
"foo \n\t ${bar} baz"

# Interpolation of simple or short code
# Good
throw ''Some very long error messages containing ${variables} and stuff''
# Bad
throw ''Some very long error messages containing ${
  variables
} and stuff''

''
  # Don't absorb interpolations if they don't start the line
  # Good
  some longer line ${
    some function [
      1
      2
    ]
  } baz
  # Bad
  some longer line ${some function [
    1
    2
  ]} baz

  # However, absorption is allowed here, since the interpolation starts a line
  ${other function (
    # with stuff
  )}
''
```

### Comments

- `/**` comments must be handled according to [RFC 0145: Doc comments](https://github.com/NixOS/rfcs/pull/145).
  - Specifically, the expression that the comment is attached to must be maintained by the formatter, as well as the resulting doc string.
- Empty comments may be deleted.
  - Often their only purpose is to vertically align lines, which is not allowed.
- Single-line `/*` comments must be converted to `#` comments.
- Single-line comments may be moved up or down a line to improve the layout.
- Anything after the first `#` of single-line comments must be preserved.
  - This allows the common pattern of prefixing many lines with `#` to comment them out, without the formatter trying to change anything.
- For multiline `/*` and `/**` comments:
  - Both `/*`/`/**` and `*/` start on a new line each, and are vertically aligned (i.e. have the same level of indentation).
  - The left-most line in between have one extra level of indentation (relative to the starting `/*`/`/**`).
    - Inner-comment indentation is preserved, in a similar way as for multiline strings.
  - Whitespace immediately after `/*`/`/**` and whitespace before `*/` may not be preserved.
  - Content after `/*`/`/**` on the same line may get shifted the next line.
  - Comments where all intermittent lines start with a `*` may have it stripped.
    - Otherwise, the non-whitespace content of each comment line must be preserved.

**Examples:**

Note that these examples show *allowed* transformations, which may or may not be applied by the formatter.

```nix
/* foo */
↓
# foo

/*bar    */
↓
# bar

function call ( # trailing comment
  body
)
↓
function call (
  # trailing comment
  body
)

if /* inline comment */ cond then
  true
else
  false
↓
# inline comment
if cond then
  true
else
  false

if cond /* inline comment */ then
  true
then
  false
↓
if
  cond # inline comment
then
  true
else
  false
  
/* foo */ ''
  bar
''
↓
# foo
''
  bar
''

/* Foo
   bar
   baz */
↓
/*
  Foo
  bar
  baz
*/

/* Foo
  bar
  baz

*/
↓
/*
  Foo
  bar
  baz
*/

/* Foo
 * bar
 */
↓
/*
  Foo
  bar
*/

# Some comment
#   Some preserved indentation
#This also stays as is
```

**Alternatives:**
- There are some ways of using multi-line comments to comment parts of strings that wouldn't otherwise support comments.
  For example in bash
  ```bash
  some-command \ # Some comment
    some-arg \ # Some comment
    another-arg
  ```
  is not valid.
  In Nix we have the ability to use string concatenation and inline comments to add comments between the arguments:
  ```nix
  ''
    some-command \
  '' + /* Some comment */ ''
    some-arg \
  '' + /* Some comment */ ''
    another-arg
  ''
  ```
  
  This style of the `+` operator for one isn't consistent with the rest of the formatting rules.
  
  Alternatively:
  ```nix
  some-command \ ${""/* Some comment */}
    some-arg \ ${"" /* Some comment */}
    another-arg
  ```

  But this is considered too hacky.
  

### Function application

In a function application chain, the first element is treated as the "function" and the remaining ones as "arguments".

- As many arguments as possible must be fit onto the first line.
- If there is at most one multi-line argument that can be absorbed and all other arguments before/after fit onto a single line respectively, then that multi-line argument is absorbed.
- Otherwise, the first argument not fitting onto the first line will start a new line with indentation, and all subsequent arguments will start on their own line as well.
  - All arguments that are not on the same line as the function must be indented by one level.
- If the last argument is parenthesized, the parentheses should get absorbed while its body is put on a new line with indentation.
  - Exception: If the last argument is parenthesized and its body contains an absorbable term, an alternative and more compact layout may be used instead: The body gets compacted and its term absorbed.
    - In this case, the inner term may be force-expanded.
    - This results in less indentation for many common Nix idioms.

**Examples:**

```nix
# All arguments fit onto the first line
function arg1 arg2

# The line length limit is reached, so the remaining arguments need to be on their own lines
function arg1 arg2 arg3
  arg4
  arg5

# The last argument is a multiline expression, so it doesn't fit on the first line,
# but it can still start on the first line
function arg1 arg2 {
  more = "things";
}

# The second argument doesn't fit on the first line, but it's not the last argument,
# so it needs to start on a new line
function arg1 {
  more = "things";
} arg3

# In this case, the remaining arguments after the second one woulnd't fit onto the line anymore, therefore start all of them on a new line
function arg1
  {
    more = "things";
  }
  arg3
  many
  long
  args

# Same with multiple multiline arguments
function
  {
    a = 1;
    b = 2;
  }
  {
    c = 1;
    d = 2;
  }

# Assume that the line length limit is here   ↓
# Good
concatMapString (s: "short string: ${s}") (
  attrsToList foo
)
# Good, this is also allowed
concatMapString (s: "short string: ${s}")
  (attrsToList foo)

# Bad: The first argument would have fit onto the first line
concatMapString (
  s: "short string: ${s}"
) (attrsToList foo)

# Good: The body of the last argument starts on a new line with indentation
pkgs.nixosTest (
  { pkgs }:
  {
    config = { };
  }
)

# Good: The last argument is parenthesised and contains a function declaration, the exception makes this have less lines
stdenv.mkDerivation (finalAttrs: {
  name = "...";
})
```

**Drawbacks**

- This style sometimes forces lists or attribute sets to start on a new line, with additional indentation of their items.

**Alternatives**

- Compacting multiline arguments like this:
  ```nix
  function arg1 {
    # stuff
  } arg3

  function {
    # ...
  } {
    # ...
  }
  ```
  - This violates the guideline of the indentation representing the expression structure, and thus reduces readability.

### Function declaration

- The body of the function must not be indented relative to its first arguments.
- A small number of ("simple") identifier arguments can be written onto the same line.
  - Otherwise they're each on their own line.
  - The body may get absorbed here
- Attribute set arguments must always start on a new line and they must not be mixed with identifier arguments.
  - If they have few attributes, the argument may be written on a single line
  - Otherwise each attribute must be on its own line with indentation, followed by a trailing comma.
- Due to structural similarity and for consistency reasons, attribute set arguments with a default value follow the same rules as [bindings](#bindings).

**Examples**

```nix
#1
name: value: name ++ value

#2 absorption
name: value: ''
  ${name} = ${value};
''

#3
name: value:
name
++ value
++ more stuff making the line too long

#4
{ pkgs }: pkgs.hello

#5
args@{
  some,
  argument,
  default ? value,
  ...
}:
{
  # body
}

#6
{ pkgs }:
name: value: 
{
  # body
}

#7: These would be over the line length limit on a single line
aaaa:
bbbb:
cccc: 
dddd:
eeee:
null

#8: @ patterns can also come afterwards
{ pkgs }@args: pkgs.hello
```

**Alternatives**

- Have leading commas for parameters in attribute set arguments, like currently done in Nixpkgs.

  - This makes attribute set arguments less likely to be confused with lists.
  - It's easier to see where arguments start and end.
  ```nix
  { some
  , arg
  }:
  
  args@{
    some
  , argument
    # Single line comment
  , commentedArgument
  , # Comment on the value
    # multiline comment
    default ? value
  , ...
  }:
  # ...
  ```

  Problems with this alternative:
  - Moving items around with this style may require editing lines.
  - Inconsistent with the [expression expansion guidelines](#expansion-of-expressions), which disallows forms like `{ some`; `some` should start on a new line instead.
  - This does not work well with leading `@` bindings.
  - It's unclear whether comments belong to the next or the previous argument.
  - The leading comma style was a lesser-evil workaround for the lack of trailing commas in the Nix language. Now that the language has this feature, there is no reason to keep it that way anymore.

### Operators

From the [list of operators](https://nixos.org/manual/nix/stable/language/operators.html#operators), this section focuses on binary operators.
Function application and attribute selection are not treated as an "operator" in the sense of this section, see [function application](#function-application) instead.

#### Non-chainable operators

Operators with no associativity are non-chainable.
Each invocation will always have exactly one left-hand side and one right-hand side.

The right-hand side must always be attached to the operator on the same line.
The operator must either be attached to the left-hand side as well, or start on a new line.

```nix
shortVariable == 42

stringLength (drvName (toString oldDependency))
== stringLength (drvName (toString newDependency))

some complicated calculation {
  # arguments
} == other stuff {
  # which may be multiline
}

some complicated calculation {
  # arguments
}
== "some very long string"
```

#### Chainable operators

Chained binary associative [operators](https://nixos.org/manual/nix/stable/language/operators.html#operators) with the same or monotonically decreasing precedence must be treated together as a single operator chain.

If an operator chain does not fit onto one line, it must be expanded such that every operator starts a new line:
- If the operand can also fit on the same line as the operator, it must be put there
- Otherwise, the operand must either be absorbed or start a new line with indentation

Operator chains in bindings may be compacted as long as all lines between the first and last one are indented.

**Examples**

```nix
# These chained associative operators have increasing precedence, so they're _not_ treated the same
foo
-> # <- The operator starts on a new line, but right operand is all of the below lines, they don't fit here, so indent
  bar
  ||
    baz
    && qux # <- The operand fits on this line

# These chained associative operators have decreasing precedence, so they're treated the same
foo
&& bar # <- All of these operands are just identifiers, they fit on the same line
|| baz # <- We shouldn't indent these lines, because it misleads into thinking that || binds stronger than &&
-> qux

[
  some
  flags
]
++ ( # <- Parenthesized expressions get absorbed
  foo
)
++ optionals condition [ # <- As are some multiline function applications
  more
  items
]
++
  runCommand name # <- Function application which cannot be absorbed start on a new line with indentation
    ''
      echo hi
    ''
    test

# In bindings we can use a more compact form as long as all in-between lines are indented.
{
  foo = bar // {
    x = 10;
    y = 20;
  } // baz;
}

# Bad, we can't use the more compact form because an intermediate line is not indented.
{
  foo = {
    x = 10;
    y = 20;
  } // bar // {
    z = 30;
    w = 40;
  };
}

# Good, this is the non-compact operator form
{
  foo = {
    x = 10;
    y = 20;
  }
  // bar
  // {
    z = 30;
    w = 40;
  };
}

# Good
{
  postPatch = ''
    patchShebangs .
  ''
  + lib.optionalString withFrei0r ''
    substituteInPlace libavfilter/vf_frei0r.c \
      --replace /usr/local/lib/frei0r-1 ${frei0r}/lib/frei0r-1
    substituteInPlace doc/filters.texi \
      --replace /usr/local/lib/frei0r-1 ${frei0r}/lib/frei0r-1
  '';

  configureFlags = [
    # *  Program flags
    (enableFeature buildFfmpeg "ffmpeg")
    (enableFeature buildFfplay "ffplay")
    (enableFeature buildFfprobe "ffprobe")
  ]
  ++ optionals withBin [ "--bindir=${placeholder "bin"}/bin" ]
  ++ [
    # ...
  ];
}
```

### if-then-else

- `if` and `else` keywords must always start on a new line.
- The `if` and `else` bodies must always be indented.
- If the condition does not fit onto one line, then it will start on the next line with indentation, and `then` will be on the start of the line following the condition.
- `else if` chains are treated as one long sequence, with no indentation creep on each step.
- `else if` chains must not be on a single line.

**Examples**

```nix
# Condition fits on one line
if builtins.length matches != 0 then
  { inherit path matches; }
else if path == /. then
  [
    1
    2
  ]
else
  go (dirOf path);

# Condition doesn't fit onto one line
if
  matches != null
  && builtins.length matches != 0
then
  { inherit path matches; }
else if path == /. then
  null
else
  go (dirOf path);
```

**Alternatives**

- The bodies could be absorbed in some cases, saving an indentation level:
  ```nix  
  #1a
  if builtins.length matches != 0 then {
    inherit path matches;
  } else if path == /. then [
    1
    2
  ] else
    go (dirOf path);
  ```
  - This results in inconsistent vertical start of the keywords, making the structure harder to follow
- Have the `then` on the start of the next line, directly followed by the if body:
  ```nix
  #1b
  if builtins.length matches != 0
    then { inherit path matches; }
  else if path == /.
    then [
      1
      2
    ]
  else go (dirOf path);

  #1c
  if builtins.length matches != 0
  then { inherit path matches; }
  else if path == /.
  then [
    1
    2
  ]
  else go (dirOf path);
  ```

### assert

- `assert <cond>;` mirrors the formatting for [bindings](#bindings).
- `assert`s `<body>` must always start on their own line and the body also starts on its own line without any additional indentation.

```nix
# Good
assert foo;
[
  bar
  baz
]

# Bad
assert foo; [
  bar
  baz
]

# Good
{
  vendor ?
    assert false;
    null,
    
  vendor ? null,
}:
null

let
  # Good
  x =
    assert foo;
    bar;
   
  # Bad
  y = assert foo;
    bar;
in
x

# Multiline condition
assert
  let
    x = true;
  in x;
  true

# Function call condition with absorbed last argument, same formatting as bindings
assert assertMsg (isPath path) ''
  lib.path.append:
    The first argument is of type ${builtins.typeOf path}, but a path was expected
'';
true
```

**Alternatives**
- Treat it the [same as `with`](#with). The reasons not to do that:
  - `assert`'s stand on their own and could be removed without breaking anything. Comparatively, `with`'s can't be removed without breaking the code
  - `assert`'s are a bit like `if-then-else` statements, which are also spread out over multiple lines

### with

- In any situation where a term would get absorbed, the term with a `with` prepended to it may get absorbed as well.
- Otherwise, the body of `with attrs;` must start on a new line without any additional indentation.

**Examples**

```nix
{
  # Good
  foo = with bar; [
    # multiline
    baz
  ];
  
  # Good
  foo =
    with foo;
    with bar;
    [
      # multiline
      baz
    ];
  
  # Good
  foo =
    with bar;
    baz foo {
      # multiline
      qux = 10;
    };

  # Good
  foo =
    with bar;
    if cond then
      foo
    else
      bar;
  
  # Bad
  foo = assert qux; with bar; [
    # multiline
    baz
  ];
  
  # Bad
  foo = with bar;
    [
      # multiline
      baz
    ];

  # Bad
  foo =
    with bar; [
      # multiline
      baz
    ];
    
  # Good
  [
    qux
    quux
  ]
  ++ (with pkgs; [
    baz
    blorp
  ]);
}
```

### let-in

Let bindings must always have this form:
```
let
  <name1> = <value1>;
  <name2> = <value2>;
  ...
in
<body>
```

- Let bindings are *always* multiline.
- Each binding is indented and starts on its own line.
  For more details, see the [bindings section](#bindings).
- The `<body>` always starts on a new line and is not indented.

**Examples**

```nix
let
  foo = "bar";
in
func foo;

let
  foo = "bar";
in
{
  inherit foo;
  baz = "smth";
}

let
  foo = "bar";
in
if foo == "bar" then
  "hello"
else
  "world"
```

**Alternatives**

- To allow having the `<body>` be absorbed after the `in`:
  ```
  let
    <name1> = <value1>;
    <name2> = <value2>;
    ...
  in <body>
  ```

  In particular when `<body>` is an identifier, list, attribute set and/or others.

  Problems with this alternative:
  - It leads to larger diffs when inserting something after the `in`
  - The formatting can change when `<body>` is updated
  - It's less consistent, since the formatting depends on the `<body>`

- The body could be indented by a level
  ```
  let
    <name1> = <value1>;
    <name2> = <value2>;
    ...
  in
    <body>
  ```
  
  Problems with this alternative:
  - Leads to indentation creeps
  - Inconsistent with other expressions that have a `<body>` that is "returned"
  - Favors a style where the body starts on the same line as the in for some values (e.g. attribute sets) to reduce an indentation level, see above.

### Attribute sets and lists

- Brackets and braces must always have a space (or line break) on the inside, like `[ `, ` ]`, `{ ` and ` }`.
  - Empty lists and attribute sets are written as `[ ]` and `{ }`, respectively.
- Lists and attribute sets can only be on a single line if they fit on the line and contain few enough items.
- Lists and attribute sets with more items should be liberally expanded.
- As described under [bindings](#bindings) below, nested attribute sets are always expanded.

**Examples**

```nix
[
  { }
  { foo = "bar"; }
  {
    foo = {
      bar = "baz";
    };
  }
  { foo.bar = "baz"; }
]

[
  [ 1 ]
  [
    2
    3
  ]
]

[
  [
    1
    2
    3
  ]
]

[
  {
    mySingletons = [
      [
        ({
          # stuff in there
        })
      ]
    ];
  
    mySingletons' = [
      [
        (function call)
      ]
    ];
  }
]
```

**Drawbacks**

- Singleton lists may use a lot of indentation

**Alternatives**

- Have a special compact form for singleton lists, to reduce the indentation level and remove two additional lines
  ```nix
  foo = [ {
    # content
  } ];
  ```

### Bindings

Let bindings, attribute sets and default function arguments share the same syntax for their items, which is discussed here together.

For each binding value, if only the first and last line are not indented, the absorbed style is used, otherwise newline and indent.

Bindings have the most special cases to accommodate for many common Nixpkgs idioms.
Generally, the following styles exist, which are used depending on the kind and size of the value:

```nix
#1 The entire binding fits onto a single line
foo = "bar";

#2 The body fits onto a single line, but the binding is too long
length limit
very.long.foo =
  function arg1 arg2 arg3;

#3 Where possible, the body should be absorbed
foo = function {
  # args
};
add = x: y: {
  result = x + y;
};

#4 If neither single-line nor absorbable, start on a new line with indentation
foo =
  function
    arg1
    arg2
    arg3;

# There is non-indented line between the first and last line, so this can't use the absorbed style
bar =
  if baz == null then
    10
  else
    20;
```

Notable special cases are:

- Single line values that would not benefit from style #2 keep using #1, even if this makes it go above the line limit. This mostly applies to simple strings and paths.
- Attribute set values must always be expanded. This has the consequence of always forcing nested attribute sets to be multiline (even if they would be single line otherwise because they only contain a single item), which usually is desired.
  ```nix
  {
    foo.bar.baz = "qux";
    foo' = {
      bar.baz = "qux";
    };
  }
  ```
- As described in the [`with` section](#with), `with` expressions of absorbable terms should be treated the same way as absorbable terms.
  - This means that the attribute set force-expansion also applies to them here.
  - This also means that (multi-line) `with` expressions will use style #3 or #4, depending on their body.
  ```nix
  # Force-expand short attrset
  meta = with lib; {
    maintainers = [];
  };
  # Don't absorb since the body of `with pkgs;` is `with pyPkgs; ...`, which is not absorbable.
  buildInputs =
    with pkgs;
    with pyPkgs;
    [
      some
      dependencies
    ];
  ```

**Alternatives**

Function calls could always be absorbed. This would reduce indentation of their arguments in some cases. However, this may look really weird in other cases, especially when the binding is very long:

```nix
some.very.long.attr = callFunction
  arg1
  arg2
  arg3;
```

Consistent with this would be to also absorb `let` bindings and other expressions, however this might result in double indentation.

```nix
suff = let
   foo = "bar"; # <-- double-indentation
  in
  foo;
```

#### Bindings semicolon placement

The semicolon in bindings must always be placed on the same line as the expression it concludes.

**Examples**

```nix
{
  attr1 = bar;
  attr2 = function call {
    # stuff
  };
  attr3 =
    function call
      many
      arguments;
  attr4 =
    let
      foo = "bar";
    in
    some statement;
  attr5 =
    if foo then
      "bar"
    else
      "baz";
  attr6 =
    let
      foo = false;
    in
    if foo then "bar" else "baz";
  attr7 = function (
    if foo then
      "bar"
    else
      "baz"
  );
  attr8 =
    cond1
    || cond2
    ||
      some function call
      && cond3;
}
```

**Alternatives**


1. On a new line without indentation.
   - This clearly marks a separation between attributes, however it is wasteful of space.
   - The lonely semicolon looks odd, and it's not a commonly used style.
   ```nix
   attr3 =
     function call
       many
       arguments
   ;
   attr3 =
     let
       foo = "bar";
     in
     some statements
   ;
   ```
2. On a new line with one indentation level.
   - Just as wasteful on space as (1), but a bit less clear about signaling the end of the binding.
   ```nix
   attr3 =
     function call
       many
       arguments
     ;
   ```
3. A mix of (1) and (2), where usually the semicolon is placed directly at the end of the binding.
   But with exceptions in which the semicolon is placed onto the following line instead in cases where the value is a multiline `if` expression or nested operator.
   These are the only syntax elements that may result in the semicolon being placed on a line with arbitrarily deep indentation.
   
   ```nix
   attr4 =
     if foo then
       "bar"
     else
       "baz"
   ;

   attr5 =
     let
       foo = false;
     in
     foo || bar;
       
   attr7 =
     cond1
     || cond2
     ||
       some function call
       && cond3
   ;
   ```
4. Always wrapping multi-line expressions with parenthesis.
   - The parenthesis help "ground" the bindings on the top-level and don't look anywhere near as odd as the lonely semicolon.
   - However this is a style not commonly used.

   ```nix
   {
     attr3 = (
       function call
         many
         arguments
     );
     attr3 = (
       let
         foo = "bar";
       in
       some statements
     );
     attr3 = (
       if foo == "bar" then
         function call
       else
         some statements
     );
   }
   ```

### inherit

The items must either be all on the same line, or all on a new line each (with indentation),
in which case the semicolon must be on its own line with indentation.
- The semicolon placement seems inconsistent between bindings and `inherit`s: For bindings it's on the last line of the expression, while for `inherit`s it's on a new line.

  This is because it's much more common to change items in an `inherit` than other expressions. By placing the semicolon on a new line, items can easily be added and removed at the end.

**Examples**

```nix
inherit foo bar baz;
inherit
  foo'
  bar'
  baz'
  ;
```

#### inherit from


For a fragment like this:
```
inherit (<source>) <attr1> ... <attrn>;
```

- If the entire fragment fits in the first line, it must be formatted as such.
- Otherwise if only `inherit (<source>)` fits into the first line, it must be formatted as such,
  with the same style as the normal `inherit` for the attributes.
- Otherwise the `(<source>)` must also be on its own line.

**Examples**

```nix
inherit (pkgs) ap1 ap2 ap3;
inherit (pkgs)
  app1
  app2
  # ...
  app42
  ;
inherit
  (pkgs.callPackage ./foo.nix {
    arg = "val";
  })
  attr1
  attr2
  ;
```
