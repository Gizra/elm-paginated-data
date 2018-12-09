module Tests exposing
    ( testFetchAll
    , testFetchPaginated
    , testGet
    , testGetAll
    , testGetItemsByPager
    , testGetPage
    , testGetTotalCount
    , testInsertDirectlyFromClient
    , testInsertMultiple
    , testRemove
    , testSetPageAsLoading
    , testSetTotalCount
    , testUpdate
    )

import EveryDictList
import Expect
import Fuzz exposing (..)
import Http
import PaginatedData exposing (..)
import RemoteData exposing (RemoteData(..))
import Test exposing (..)


{-| Fuzz a realistic page number. Not negative, for instance.
-}
fuzzPage : Fuzzer Int
fuzzPage =
    Fuzz.intRange 1 100


{-| Fuzz a realistic item count. Again, not negative.
-}
fuzzCount : Fuzzer Int
fuzzCount =
    Fuzz.intRange 0 1000


testFetchPaginated : Test
testFetchPaginated =
    describe "fetchPaginated"
        [ fuzz fuzzPage "If empty, should fetch whatever page we ask for" <|
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


testRemove : Test
testRemove =
    describe "remove"
        [ test "Removing something from an empty pager should do nothing" <|
            \_ ->
                emptyPaginatedData
                    |> remove "key"
                    |> getAll
                    |> EveryDictList.size
                    |> Expect.equal 0
        , test "Removing an existing value should work" <|
            \_ ->
                emptyPaginatedData
                    |> insertDirectlyFromClient "key" 17
                    |> insertDirectlyFromClient "key2" 21
                    |> remove "key"
                    |> getAll
                    |> EveryDictList.toList
                    |> Expect.equal
                        [ ( "key2", 21 )
                        ]
        ]


testGetPage : Test
testGetPage =
    describe "getPage"
        [ fuzz fuzzPage "Getting any page from an empty pager should be NotAsked" <|
            \page ->
                getPage page emptyPaginatedData
                    |> Expect.equal NotAsked
        , test "Getting an existing page should work" <|
            \_ ->
                emptyPaginatedData
                    |> insertDirectlyFromClient "key" "value"
                    |> insertDirectlyFromClient "key2" "value2"
                    |> getPage 1
                    |> Expect.equal (Success ( "key", "key2" ))
        ]


testGetTotalCount : Test
testGetTotalCount =
    describe "getTotalCount"
        [ test "Total count of an empty pager should be Nothing" <|
            \_ ->
                getTotalCount emptyPaginatedData
                    |> Expect.equal Nothing
        , test "Total count of a pager with a few values should work" <|
            \_ ->
                emptyPaginatedData
                    |> insertDirectlyFromClient "key" "value"
                    |> insertDirectlyFromClient "key2" "value2"
                    |> getTotalCount
                    |> Expect.equal (Just 2)
        ]


testSetTotalCount : Test
testSetTotalCount =
    fuzz (Fuzz.maybe fuzzCount) "Setting the total count should work" <|
        \count ->
            emptyPaginatedData
                |> setTotalCount count
                |> getTotalCount
                |> Expect.equal count


testSetPageAsLoading : Test
testSetPageAsLoading =
    fuzz fuzzPage "Setting any page as loading should work" <|
        \page ->
            emptyPaginatedData
                |> setPageAsLoading page
                |> getPage page
                |> Expect.equal Loading


testInsertMultiple : Test
testInsertMultiple =
    describe "insertMultiple"
        [ test "Indicating that a page is Loading should leave the pager unchanged" <|
            \_ ->
                emptyPaginatedData
                    |> insertMultiple 1 Loading
                    |> getPage 1
                    |> Expect.equal NotAsked
        , test "Indicating that a page has Failed should work" <|
            \_ ->
                emptyPaginatedData
                    |> insertMultiple 1 (Failure Http.Timeout)
                    |> getPage 1
                    |> Expect.equal (Failure Http.Timeout)
        ]


testInsertDirectlyFromClient : Test
testInsertDirectlyFromClient =
    describe "insertDirectlyFromClient"
        [ test "Inserting into empty pager should work" <|
            \_ ->
                emptyPaginatedData
                    |> insertDirectlyFromClient "key" "value"
                    |> Expect.all
                        [ \pager ->
                            getAll pager
                                |> EveryDictList.toList
                                |> Expect.equal
                                    [ ( "key", "value" ) ]
                        , \pager ->
                            getTotalCount pager
                                |> Expect.equal (Just 1)
                        , \pager ->
                            getPage 1 pager
                                |> Expect.equal (Success ( "key", "key" ))
                        ]
        , test "If we have 1 page, should insert on that page" <|
            \_ ->
                emptyPaginatedData
                    |> insertDirectlyFromClient "key" "value"
                    |> insertDirectlyFromClient "key2" "value2"
                    |> Expect.all
                        [ \pager ->
                            getAll pager
                                |> EveryDictList.toList
                                |> Expect.equal
                                    [ ( "key", "value" )
                                    , ( "key2", "value2" )
                                    ]
                        , \pager ->
                            getTotalCount pager
                                |> Expect.equal (Just 2)
                        , \pager ->
                            getPage 1 pager
                                |> Expect.equal (Success ( "key", "key2" ))
                        ]
        , test "Ignore the new value if we already have the key" <|
            \_ ->
                emptyPaginatedData
                    |> insertDirectlyFromClient "key" "old value"
                    |> insertDirectlyFromClient "key" "new value"
                    |> get "key"
                    |> Expect.equal (Just "old value")
        ]


testGetItemsByPager : Test
testGetItemsByPager =
    describe "getItemsByPager"
        [ test "An empty pager has no items" <|
            \_ ->
                emptyPaginatedData
                    |> flip getItemsByPager 1
                    |> EveryDictList.size
                    |> Expect.equal 0
        , test "Single page" <|
            \_ ->
                emptyPaginatedData
                    |> insertDirectlyFromClient "key" "value"
                    |> insertDirectlyFromClient "key2" "value2"
                    |> flip getItemsByPager 1
                    |> EveryDictList.toList
                    |> Expect.equal
                        [ ( "key", "value" )
                        , ( "key2", "value2" )
                        ]
        , fuzz fuzzPage "If we only have one page, requesting any page should get page 1" <|
            \page ->
                emptyPaginatedData
                    |> insertDirectlyFromClient "key" "value"
                    |> insertDirectlyFromClient "key2" "value2"
                    |> flip getItemsByPager page
                    |> EveryDictList.toList
                    |> Expect.equal
                        [ ( "key", "value" )
                        , ( "key2", "value2" )
                        ]
        ]
