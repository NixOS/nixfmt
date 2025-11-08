{
  # Basic language annotation cases
  luaScript = /* lua */ ''
    print("Hello, world!")
    local x = 42
  '';

  # With extra whitespace
  jsCode = /*   javascript   */ ''
    console.log("Hello from JS");
    const x = 42;
  '';

  # missing whitespace after comment
  noSpace = /*python*/''
    print("No space after comment")
  '';

  # Language annotation with indented multiline string
  indentedCode = {
    script = /* python */ ''
      import os
      def main():
          print("Indented Python")
    '';
  };

  # Language annotation followed by regular string
  regularString = /* json */ "{ \"key\": \"value\" }";
  multilineRegularString = /* js */ "
    console.log('Hello, world!');
  ";

  # Multiple block comments in sequence
  sequentialComments = /* first */ /* second */ ''
    some content
  '';

  # Block comment with line breaks
  multilineBlockComment = /* this is a
                             multiline comment */ ''
    content
  '';

  # Mixed comment styles
  mixedComments = /* inline */ # line comment
    ''
      content
    '';

  # Language annotation in function arguments
  processCode = builtins.readFile (/* lua */ ''
    return "Hello"
  '');
  # without parentheses
  processCode2 = builtins.readFile /* lua */ ''
    return "Hello"
  '';

  # Language annotation in list
  scripts = [
    /* bash */ ''
      echo "Script 1"
    ''
    /* python */ ''
      print("Script 2")
    ''
    /* ruby */ "puts 'Script 3'"
    /* js */
    "console.log('Script 4');"
  ];

  # Language annotation in list on function argument
  runScripts = (lib.mkSomething [
    /* bash */ ''
      echo "Script A"
    ''
  ][
    /* python */ ''
      print("Script B")
    ''
      /* ruby */ "puts 'Script C'"
  ]);

  aboveString = 
    /* bash */
    "echo 'Above string'";

  # Language annotation in attribute set
  languages = {
    lua = /* lua */ ''
      print("Lua")
    '';
    python = /* python */ ''
      print("Python")
    '';
  };

  # Edge case: empty language annotation
  emptyAnnotation = /**/ ''
    content without annotation
  '';

  # Edge case: language annotation with special characters
  specialChars = /* c++ */ ''
    #include <iostream>
    int main() { return 0; }
  '';
  withDot = /* .ts */ ''
    let x: number = 42;
  '';

  # Edge case: very long language annotation
  longAnnotation = /* this-is-a-very-long-language-annotation-that-might-affect-line-length */ ''
    content
  '';

  # Language annotation not followed by string
  object = /* json */ { key = "value"; };
  fn = /* foo */ x: x + 1;
  fnCall =  /* foo */ fnName "bar";

  # Language annotation followed by line break
  lineBreak = /* python */

    ''
      print("Line break after annotation")
    '';


  # Language annotation with interpolated expressions
  interpolatedExpr = /* bash */ ''
    ${/* inline-comment */ "echo hello"}
  '';

  # Language annotation in let expression
  letExpr = let
    code = /* python */ ''
      print("In let")
    '';
  in code;

  # Language annotation in function definition
  mkScript = lang: content: /* lang */ ''
    ${content}
  '';

  # on in block
  expr =
    let
    in
    /* bash */ ''
      echo "Hello"
    '';
}
