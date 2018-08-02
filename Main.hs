import qualified Data.Text.IO
import qualified Nix.Parser
import qualified Nix.Pretty

main = do
  txt <- Data.Text.IO.getContents
  ast <- case Nix.Parser.parseNixText txt of 
    Nix.Parser.Failure err -> error (show err)
    Nix.Parser.Success ast -> return ast

  print $ Nix.Pretty.prettyNix ast
