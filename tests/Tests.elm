module Tests exposing (testFetchAll, testFetchPaginated, testGet, testGetAll, testUpdate)

import EveryDictList
import Expect
import Fuzz exposing (..)
import PaginatedData exposing (..)
import Test exposing (..)


testFetchPaginated : Test
testFetchPaginated =
    describe "fetchPaginated"
        [ fuzz Fuzz.int "If empty, should fetch whatever page we ask for" <|
            \current ->
                fetchPaginated emptyPaginatedData current
                    |> Expect.equal [ current ]
        , test "If we're loading the current page, should do nothing" <|
            \_ ->
                emptyPaginatedData
                    |> setPageAsLoading 1
                    |> flip fetchPaginated 1
                    |> Expect.equal []
        , test "If we have the current page, and we have no entries for other pages, we should not fetch the next page" <|
            \_ ->
                emptyPaginatedData
                    |> insertDirectlyFromClient "key" "value"
                    |> flip fetchPaginated 1
                    |> Expect.equal []
        , test "If we directly request a page we don't expect to exist, we should get it even if we have other pages" <|
            \_ ->
                emptyPaginatedData
                    |> insertDirectlyFromClient "key" "value"
                    |> flip fetchPaginated 2
                    |> Expect.equal [ 2 ]
        ]


testFetchAll : Test
testFetchAll =
    describe "fetchAll"
        [ test "With an empty pager, we should fetch the first page" <|
            \_ ->
                emptyPaginatedData
                    |> fetchAll
                    |> Expect.equal [ 1 ]
        , test "If we have one page, and that's all there is, we should do nothing" <|
            \_ ->
                emptyPaginatedData
                    |> insertDirectlyFromClient "key" "value"
                    |> fetchAll
                    |> Expect.equal []
        ]


testGet : Test
testGet =
    describe "get"
        [ fuzz Fuzz.int "Getting anything from an empty data should return nothing" <|
            \key ->
                emptyPaginatedData
                    |> get key
                    |> Expect.equal Nothing
        , fuzz2 Fuzz.int Fuzz.string "Getting something you just put into a pager should return it" <|
            \key value ->
                emptyPaginatedData
                    |> insertDirectlyFromClient key value
                    |> get key
                    |> Expect.equal (Just value)
        ]


testGetAll : Test
testGetAll =
    describe "getAll"
        [ test "Empty data should be empty" <|
            \_ ->
                emptyPaginatedData
                    |> getAll
                    |> EveryDictList.size
                    |> Expect.equal 0
        , test "If we insert something, we should get it" <|
            \_ ->
                emptyPaginatedData
                    |> insertDirectlyFromClient "key" "value"
                    |> insertDirectlyFromClient "key2" "value2"
                    |> getAll
                    |> EveryDictList.toList
                    |> Expect.equal
                        [ ( "key", "value" )
                        , ( "key2", "value2" )
                        ]
        ]


testUpdate : Test
testUpdate =
    describe "update"
        [ test "Updating an empty pager should do nothing" <|
            \_ ->
                emptyPaginatedData
                    |> update "key" ((+) 1)
                    |> getAll
                    |> EveryDictList.size
                    |> Expect.equal 0
        , test "Updating an existing value should work" <|
            \_ ->
                emptyPaginatedData
                    |> insertDirectlyFromClient "key" 17
                    |> insertDirectlyFromClient "key2" 21
                    |> update "key" ((+) 1)
                    |> getAll
                    |> EveryDictList.toList
                    |> Expect.equal
                        [ ( "key", 18 )
                        , ( "key2", 21 )
                        ]
        ]
