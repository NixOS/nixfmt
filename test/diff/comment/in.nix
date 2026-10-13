[
/*
*/
  /*
   */

  /*
  */

  /*
    */

    /*
  */

    /*
    */

  /*@*/

  /**
    @
   **/

    /*@
   @
  @*/

  /*@
   @
    @*/

    /*@
@
    @*/

    /*@
     @
    @*/

 /* test
  * test
  */

 /* test
  * test
  *
  */

  /*
   * FOO
   */

  /**
   * FOO
   */

  /*
   * FOO
   * BAR
   */

  /**
    Concatenate a list of strings with a separator between each element

    # Example

    ```nix
    concatStringsSep "/" ["usr" "local" "bin"]
    => "usr/local/bin"
    ```

    # Type

    ```
    concatStringsSep :: string -> [string] -> string
    ```
  */

  /*
    Concatenate a list of strings with a separator between each element

    # Example

    ```nix
    concatStringsSep "/" ["usr" "local" "bin"]
    => "usr/local/bin"
    ```

    # Type

    ```
    concatStringsSep :: string -> [string] -> string
    ```
  */

  /*
   * Concatenate a list of strings with a separator between each element
   *
   * # Example
   *
   * ```nix
   * concatStringsSep "/" ["usr" "local" "bin"]
   * => "usr/local/bin"
   * ```
   *
   * # Type
   *
   * ```
   * concatStringsSep :: string -> [string] -> string
   * ```
   */


  [  # 1
  #2
    a  # 3
    b
    c # 4
    #5

    #6

    d
    #7
  ]

  {
    a = 123;   # comment
  }

  {  # 1
  #2
    a=1;  # 3
    b=1;
    c=1; # 4
    #5

    #6

    d=1;
    #7
  }

  (let  # 1
  #2
    a=1;  # 3
    b=1;
    c=1; # 4
    #5

    #6

    d=1;
    #7
  in
  d)

  ({
    a,    # comment
    b ? 2,# comment
  }: _)

  # Trailing comment after multiline string must not break idempotency
  "
" # c
  t

  # Trailing comment after multiline string, next item indented differently
  "
" # c
      t2

  # Trailing comment after multiline string in binder position
  {
    foo = "
" # c
        ;
  }

  # Trailing comment after multiline string, followed by a comment block
  "
" # c1
    # c2
  t3

  # Trailing block comment after multiline string
  "
" /* c */
      t4

  # The closing bracket is reindented onto the comment's column, so keeping the
  # comment trailing would make it parse as leading the bracket on a reformat.
  # Only the formatted layout reveals the collision: in the input the bracket
  # sits well to the right of it.
  [
    "
" # c
      ]

  # Trailing comment after multiline string as the last list item, one line
  # before the closing bracket: demoted.
  "
" # c
]
