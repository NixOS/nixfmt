{- © 2019 Serokell <hi@serokell.io>
 -
 - SPDX-License-Identifier: MPL-2.0
 -}

{-# LANGUAGE OverloadedStrings #-}
import GHCJS.Marshal
import GHCJS.Foreign
import GHCJS.Foreign.Callback
import GHCJS.Types
import JavaScript.Object
import qualified JavaScript.Object.Internal as O
import Data.JSString.Text
import qualified Data.JSString as S

import Nixfmt

foreign import javascript unsafe "window.nixfmt_ = $1"
    js_set_logic :: JSVal -> IO ()

main :: IO ()
main = do
    callback <- syncCallback2 ThrowWouldBlock $ \text_ o -> do
        let obj = O.Object o
        Just width <- getProp (textToJSString "width") obj >>= fromJSVal
        Just filename_ <- getProp (textToJSString "filename") obj >>= fromJSVal
        let filename = S.unpack filename_
        let text = textFromJSVal text_
        out <- case format width filename text of
          Left err -> do
            setProp "err" (toJSBool True) obj
            toJSVal $ S.pack err
          Right out_ -> do
            setProp "err" (toJSBool False) obj
            toJSVal out_
        setProp "ret" out obj
    js_set_logic $ jsval callback
