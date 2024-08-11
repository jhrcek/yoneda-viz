module CategoryTest exposing (..)

import Expect
import Test exposing (..)


suite : Test
suite =
    describe "Category"
        [ describe "todo"
            [ test "test1" <|
                \_ -> Expect.equal 1 1
            ]
        ]
