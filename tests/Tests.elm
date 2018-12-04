module Tests exposing (testGetAll)

import EveryDictList
import Expect
import Fuzz exposing (..)
import PaginatedData exposing (..)
import Test exposing (..)


testGetAll : Test
testGetAll =
    describe "getAll"
        [ test "empty comtainers should be empty" <|
            \_ ->
                emptyContainer "id"
                    |> getAll "id"
                    |> EveryDictList.size
                    |> Expect.equal 0
        ]
