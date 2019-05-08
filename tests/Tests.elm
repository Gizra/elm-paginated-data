module Tests exposing
    ( testFetchAllPages
    , testFetchNextPage
    , testGet
    , testGetAll
    , testGetItemsByPage
    , testGetPage
    , testGetTotalCount
    , testHandleFetchedPage
    , testInsertLocal
    , testRemove
    , testSetPageAsLoading
    , testSetTotalCount
    , testUpdate
    )

import EveryDictList exposing (EveryDictList)
import Expect
import Fuzz exposing (..)
import Http
import PaginatedData exposing (..)
import RemoteData exposing (RemoteData(..))
import Test exposing (..)


{-| Fuzz a realistic page number. Not negative, for instance.
-}
fuzzPageNumber : Fuzzer Int
fuzzPageNumber =
    Fuzz.intRange 1 10


{-| Fuzz a realistic item count. Again, not negative.
-}
fuzzCount : Fuzzer Int
fuzzCount =
    Fuzz.intRange 0 200


fuzzEveryDictList : Fuzzer key -> Fuzzer value -> Fuzzer (EveryDictList key value)
fuzzEveryDictList fuzzKey fuzzValue =
    Fuzz.map2 (,) fuzzKey fuzzValue
        |> Fuzz.list
        |> Fuzz.map EveryDictList.fromList


type alias KeyValue =
    { key : Int
    , value : Int
    }


fuzzItems : Fuzzer (List KeyValue)
fuzzItems =
    Fuzz.map2 KeyValue Fuzz.int Fuzz.int
        |> Fuzz.list


fuzzPage : Fuzzer (RemoteData Int ( EveryDictList Int Int, Int ))
fuzzPage =
    Fuzz.map2 (,) (fuzzEveryDictList Fuzz.int Fuzz.int) fuzzCount
        |> fuzzRemoteData Fuzz.int


fuzzRemoteData : Fuzzer err -> Fuzzer value -> Fuzzer (RemoteData err value)
fuzzRemoteData fuzzErr fuzzValue =
    Fuzz.oneOf
        [ Fuzz.constant Loading
        , Fuzz.constant NotAsked
        , Fuzz.map Failure fuzzErr
        , Fuzz.map Success fuzzValue
        ]


type alias FetchedPage =
    { pageNumber : Int
    , items : RemoteData Int ( EveryDictList Int Int, Int )
    }


fuzzPages : Fuzzer (List FetchedPage)
fuzzPages =
    Fuzz.map2 FetchedPage fuzzPageNumber fuzzPage
        |> Fuzz.list


{-| A basic fuzzer for paginated data.

The pages will vary in page size in a way that is unrealistic, and the
totalCount won't necessarily be sensible in relation to the pages, but it
should do for now. (Especially since we don't prevent clients from doing this
either).

-}
fuzzPaginatedData : Fuzzer (PaginatedData Int Int Int)
fuzzPaginatedData =
    Fuzz.map2
        (\locals pages ->
            let
                withLocals =
                    List.foldl
                        (\{ key, value } data ->
                            insertLocal key value data
                        )
                        emptyPaginatedData
                        locals
            in
            List.foldl
                (\{ pageNumber, items } data ->
                    handleFetchedPage pageNumber items data
                )
                withLocals
                pages
        )
        fuzzItems
        fuzzPages


testFetchNextPage : Test
testFetchNextPage =
    describe "fetchNextPage"
        [ fuzz fuzzPageNumber "If empty, should fetch whatever page we ask for" <|
            \current ->
                fetchNextPage current emptyPaginatedData
                    |> Expect.equal [ current ]
        , test "Should not tell us to fetch negative pages" <|
            \_ ->
                emptyPaginatedData
                    |> fetchNextPage -2
                    |> Expect.equal []
        , test "If we're loading the current page, should do nothing" <|
            \_ ->
                emptyPaginatedData
                    |> handleFetchedPage 1 (Success ( page1, 15 ))
                    |> setPageAsLoading 2
                    |> fetchNextPage 2
                    |> Expect.equal []
        , test "If we have the current page, and we have no entries for other pages, we should not fetch the next page" <|
            \_ ->
                emptyPaginatedData
                    |> handleFetchedPage 1 (Success ( page1, 5 ))
                    |> fetchNextPage 1
                    |> Expect.equal []
        , test "If we have the current page, and we expect the next page, we should fetch it" <|
            \_ ->
                emptyPaginatedData
                    |> handleFetchedPage 1 (Success ( page1, 10 ))
                    |> fetchNextPage 1
                    |> Expect.equal [ 2 ]
        , test "If we have the last page, and we don't have the first page, we should fetch it" <|
            \_ ->
                emptyPaginatedData
                    |> handleFetchedPage 3 (Success ( page1, 15 ))
                    |> fetchNextPage 3
                    |> Expect.equal [ 1 ]
        , test "If we directly request a page we don't expect to exist, we should get it even if we have other pages" <|
            \_ ->
                emptyPaginatedData
                    |> handleFetchedPage 1 (Success ( page1, 5 ))
                    |> fetchNextPage 2
                    |> Expect.equal [ 2 ]
        ]


testFetchAllPages : Test
testFetchAllPages =
    describe "fetchAll"
        [ test "With an empty pager, we should fetch the first page" <|
            \_ ->
                emptyPaginatedData
                    |> fetchAllPages
                    |> Expect.equal [ 1 ]
        , test "If the first page is loading, we should do nothing" <|
            \_ ->
                emptyPaginatedData
                    |> setPageAsLoading 1
                    |> fetchAllPages
                    |> Expect.equal []
        , test "If the first page has errored, we should do nothing" <|
            \_ ->
                emptyPaginatedData
                    |> handleFetchedPage 1 (Failure Http.Timeout)
                    |> fetchAllPages
                    |> Expect.equal []
        , test "If we have one page, and that's all there is, we should do nothing" <|
            \_ ->
                emptyPaginatedData
                    |> handleFetchedPage 1 (Success ( page1, 5 ))
                    |> fetchAllPages
                    |> Expect.equal []
        , test "If there is a second page, we should get it." <|
            \_ ->
                emptyPaginatedData
                    |> handleFetchedPage 1 (Success ( page1, 10 ))
                    |> fetchAllPages
                    |> Expect.equal [ 2 ]
        , test "We should loop around to page 1 if necessary." <|
            \_ ->
                emptyPaginatedData
                    |> handleFetchedPage 2 (Success ( page1, 10 ))
                    |> fetchAllPages
                    |> Expect.equal [ 1 ]
        , test "But not if we already have it." <|
            \_ ->
                emptyPaginatedData
                    |> handleFetchedPage 1 (Success ( page1, 15 ))
                    |> handleFetchedPage 3 (Success ( page2, 15 ))
                    |> fetchAllPages
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
        , fuzz3 Fuzz.int Fuzz.int fuzzPaginatedData "Getting something you just inserted locally should return it" <|
            \key value data ->
                data
                    |> insertLocal key value
                    |> get key
                    |> Expect.equal (Just value)
        , test "And getting something you inserted on a page should work" <|
            \_ ->
                emptyPaginatedData
                    |> handleFetchedPage 1 (Success ( page1, 15 ))
                    |> get "key1"
                    |> Expect.equal (Just "value1")
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
                    |> insertLocal "key" "value"
                    |> insertLocal "key2" "value2"
                    |> getAll
                    |> EveryDictList.toList
                    |> Expect.equal
                        [ ( "key", "value" )
                        , ( "key2", "value2" )
                        ]
        , test "Order should be by page, and then locals" <|
            \_ ->
                emptyPaginatedData
                    |> insertLocal "localKey1" "localValue1"
                    |> insertLocal "localKey2" "localValue2"
                    |> handleFetchedPage 1 (Success ( page1, 10 ))
                    |> handleFetchedPage 2 (Success ( page2, 10 ))
                    |> getAll
                    |> EveryDictList.toList
                    |> Expect.equal
                        [ ( "key1", "value1" )
                        , ( "key2", "value2" )
                        , ( "key3", "value3" )
                        , ( "key4", "value4" )
                        , ( "key5", "value5" )
                        , ( "key6", "value6" )
                        , ( "key7", "value7" )
                        , ( "key8", "value8" )
                        , ( "key9", "value9" )
                        , ( "key10", "value10" )
                        , ( "localKey1", "localValue1" )
                        , ( "localKey2", "localValue2" )
                        ]
        , test "Even if pages arrive in different order" <|
            \_ ->
                emptyPaginatedData
                    |> handleFetchedPage 2 (Success ( page2, 10 ))
                    |> handleFetchedPage 1 (Success ( page1, 10 ))
                    |> insertLocal "localKey1" "localValue1"
                    |> insertLocal "localKey2" "localValue2"
                    |> getAll
                    |> EveryDictList.toList
                    |> Expect.equal
                        [ ( "key1", "value1" )
                        , ( "key2", "value2" )
                        , ( "key3", "value3" )
                        , ( "key4", "value4" )
                        , ( "key5", "value5" )
                        , ( "key6", "value6" )
                        , ( "key7", "value7" )
                        , ( "key8", "value8" )
                        , ( "key9", "value9" )
                        , ( "key10", "value10" )
                        , ( "localKey1", "localValue1" )
                        , ( "localKey2", "localValue2" )
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
                    |> insertLocal "key" 17
                    |> insertLocal "key2" 21
                    |> update "key" ((+) 1)
                    |> getAll
                    |> EveryDictList.toList
                    |> Expect.equal
                        [ ( "key", 18 )
                        , ( "key2", 21 )
                        ]
        , test "In pagers as well" <|
            \_ ->
                emptyPaginatedData
                    |> handleFetchedPage 1 (Success ( page1, 10 ))
                    |> update "key2" (\value -> value ++ "more")
                    |> Expect.all
                        [ \data ->
                            get "key2" data
                                |> Expect.equal (Just "value2more")
                        , \data ->
                            get "key3" data
                                |> Expect.equal (Just "value3")
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
                    |> insertLocal "key" 17
                    |> insertLocal "key2" 21
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
        [ fuzz fuzzPageNumber "Getting any page from an empty pager should be NotAsked" <|
            \page ->
                getPage page emptyPaginatedData
                    |> Expect.equal NotAsked
        , fuzz fuzzPaginatedData "Getting an existing page should work" <|
            \data ->
                data
                    |> handleFetchedPage 1 Loading
                    |> getPage 1
                    |> Expect.equal Loading
        ]


page1 : EveryDictList String String
page1 =
    EveryDictList.fromList
        [ ( "key1", "value1" )
        , ( "key2", "value2" )
        , ( "key3", "value3" )
        , ( "key4", "value4" )
        , ( "key5", "value5" )
        ]


page2 : EveryDictList String String
page2 =
    EveryDictList.fromList
        [ ( "key6", "value6" )
        , ( "key7", "value7" )
        , ( "key8", "value8" )
        , ( "key9", "value9" )
        , ( "key10", "value10" )
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
                    |> handleFetchedPage 1 (Success ( page1, 10 ))
                    |> getTotalCount
                    |> Expect.equal (Just 10)
        ]


testSetTotalCount : Test
testSetTotalCount =
    fuzz2 (Fuzz.maybe fuzzCount) fuzzPaginatedData "Setting the total count should work" <|
        \count data ->
            data
                |> setTotalCount count
                |> getTotalCount
                |> Expect.equal count


testSetPageAsLoading : Test
testSetPageAsLoading =
    fuzz2 fuzzPageNumber fuzzPaginatedData "Setting any page as loading should work" <|
        \page data ->
            data
                |> setPageAsLoading page
                |> getPage page
                |> Expect.equal Loading


testHandleFetchedPage : Test
testHandleFetchedPage =
    describe "handleFetchedPage"
        [ fuzz2 fuzzPageNumber fuzzPaginatedData "Indicating that a page has Failed should work" <|
            \pageNumber data ->
                emptyPaginatedData
                    |> handleFetchedPage pageNumber (Failure Http.Timeout)
                    |> getPage pageNumber
                    |> Expect.equal (Failure Http.Timeout)
        ]


testInsertLocal : Test
testInsertLocal =
    describe "insertLocal"
        [ test "Inserting into empty pager should work" <|
            \_ ->
                emptyPaginatedData
                    |> insertLocal "key" "value"
                    |> Expect.all
                        [ \pager ->
                            getAll pager
                                |> EveryDictList.toList
                                |> Expect.equal
                                    [ ( "key", "value" ) ]
                        , \pager ->
                            getLocal pager
                                |> EveryDictList.toList
                                |> Expect.equal [ ( "key", "value" ) ]
                        ]
        ]


testGetItemsByPage : Test
testGetItemsByPage =
    describe "getItemsByPage"
        [ test "An empty pager has no items" <|
            \_ ->
                emptyPaginatedData
                    |> getItemsByPage 1
                    |> EveryDictList.size
                    |> Expect.equal 0
        , test "Single page" <|
            \_ ->
                emptyPaginatedData
                    |> handleFetchedPage 1 (Success ( page1, 10 ))
                    |> getItemsByPage 1
                    |> EveryDictList.toList
                    |> Expect.equal (EveryDictList.toList page1)
        ]
