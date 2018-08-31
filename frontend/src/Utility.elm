module Utility exposing (..)

import Debug


log : a -> a
log a =
    Debug.log (toString a) a
