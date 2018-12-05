module Tests exposing (testGetAll)

import EveryDictList
import Expect
import Fuzz exposing (..)
import PaginatedData exposing (..)
import Test exposing (..)


testGetAll : Test
testGetAll =
    describe "getAll"
        [ test "empty data should be empty" <|
            \_ ->
                emptyPaginatedData
                    |> getAll
                    |> EveryDictList.size
                    |> Expect.equal 0
        ]
