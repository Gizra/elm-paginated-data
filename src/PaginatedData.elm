module PaginatedData exposing
    ( PaginatedData, emptyPaginatedData, Pager
    , get, getAll, getItemsByPager, getPager, getTotalCount
    , fetchAll, fetchPaginated
    , insertDirectlyFromClient, insertMultiple, remove, setPageAsLoading, setTotalCount, update
    , viewPager
    )

{-| By using something like
[https://package.elm-lang.org/packages/krisajenkins/remotedata/latest](`RemoteData`),
you can keep track of the status of a request for data: is it in progress?
Completed? Did it fail? But what if the data comes back to you in pages?
Keeping track of those pages is the purpose of `PaginatedData`. Which pages do
you already have? What were their contents? Which pages do you still need? What
requests are in flight?


### Types

@docs PaginatedData, emptyPaginatedData, Pager


### Accessors

@docs get, getAll, getItemsByPager, getPager, getTotalCount


### Fetch helpers

@docs fetchAll, fetchPaginated


### Updaters

@docs insertDirectlyFromClient, insertMultiple, remove, setPageAsLoading, setTotalCount, update


### View

@docs viewPager

-}

import Dict exposing (Dict)
import EveryDictList exposing (EveryDictList)
import Html exposing (Html, a, li, text, ul)
import Html.Attributes exposing (action, class, classList)
import Html.Events exposing (onClick)
import RemoteData exposing (RemoteData(..), WebData)


{-| Represents the status of paged data, where we may have some or all of the
pages, and requests for one or more pages may be in-flight or
have had errors.

The `value` is the type of the value that you are fetching in pages.

The `key` is a type for looking up values. For instance, it might be an ID for
the values. It is used in an `EveryDictList`, so it should be unique, and it
must have work with `toString` in a way that produces unique results.

-}
type PaginatedData key value
    = PaginatedData
        { data : EveryDictList key value
        , pager : Pager key

        -- Represents the number of items which we would have once we receive
        -- all the pages, if we know that. (For instance, the backend might
        -- tell us how many items there in total are while returning just one
        -- page). Note that this is the item count, not the page count.
        , totalCount : Maybe Int
        }


{-| The `Pager` represents the status of each request for a page of data.
The key for the `Dict` is the page number, which is 1-based. The value
represents the status of a request for that page:

  - `NotAsked` means that we haven't requested that page.

  - `Loading` means that a request for that page is in progress.

  - `Failure err` means that a request for that page has failed.

  - `Success (key, key)` means that we received data. The two keys represent
    the keys corresponding to the first and last items on the page.

-}
type alias Pager key =
    Dict Int (WebData ( key, key ))


{-| A starting point for a `PaginatedData`, with no values and no
requests in progress.
-}
emptyPaginatedData : PaginatedData key value
emptyPaginatedData =
    PaginatedData
        { data = EveryDictList.empty
        , pager = Dict.empty
        , totalCount = Nothing
        }


{-| You supply the current page -- for instance, perhaps the page which the
user has selected in the UI. You'll get back a list of pages which you need to
fetch.

Say, for instance, that the current page is page 2.

  - If you haven't asked for page 2 yet, then you'll get `[2]` back.

  - If you successfully received page 2, and there are more items to fetch,
    you'll get `[3]` back. This indicates that it would be a good idea to
    fetch page 3 ... the user may want it soon!

Presumably you are tracking in your model both the `PaginatedData` and some
indicatin of which page the user is interested in right now. So, you can call
this function to help determine what pages you ought to request right now.

When you do issue the requests, it is important to call `setPageLoading` here!
Otherwise, when you call this function again, you'll be told to fetch the page
again. If you mark the page as `Loading`, then this function won't tell you to
load it again.

Note that this function does not tell you to re-fetch pages that have failed.
When to do that is very much dependent on your situation, so you'll need to
check for that yourself.

This function is a nice helper for the
[`andThenFetch`](https://package.elm-lang.org/packages/Gizra/elm-essentials/latest/Gizra-Update#andThenFetch)
function, but is useful in other contexts as well.

-}
fetchPaginated : PaginatedData k v -> Int -> List Int
fetchPaginated (PaginatedData existingDataAndPager) currentPage =
    let
        currentPageData =
            Dict.get currentPage existingDataAndPager.pager
                |> Maybe.withDefault RemoteData.NotAsked

        hasNextPage =
            Dict.member (currentPage + 1) existingDataAndPager.pager

        nextPageData =
            Dict.get (currentPage + 1) existingDataAndPager.pager
                |> Maybe.withDefault RemoteData.NotAsked

        -- Prevent endless fetching in case the previous request has ended with `Failure`.
        isPreviousRequestFailed =
            RemoteData.isFailure currentPageData

        ( totalCount, isFetched ) =
            case existingDataAndPager.totalCount of
                Just val ->
                    ( val, True )

                Nothing ->
                    ( 0, False )
    in
    if not isPreviousRequestFailed then
        if RemoteData.isNotAsked currentPageData then
            [ currentPage ]

        else if
            hasNextPage
                && RemoteData.isNotAsked nextPageData
                -- Check that we haven't already fetched all Items.
                && (EveryDictList.size existingDataAndPager.data < totalCount)
                && isFetched
        then
            [ currentPage + 1 ]

        else
            []

    else
        []


{-| Fetch all existing pages.

Next page is fetched as the previous one arrives successfully.

-}
fetchAll : PaginatedData k v -> List Int
fetchAll (PaginatedData existingDataAndPager) =
    let
        -- Current page is actually the last page that had a successful
        -- response.
        currentPage =
            existingDataAndPager.pager
                |> Dict.toList
                -- Keep only successs values.
                |> List.filter (\( _, webData ) -> RemoteData.isSuccess webData)
                -- Sort the list by page number, and get the highest value.
                |> List.sortBy (\( pageNumber, _ ) -> pageNumber)
                |> List.reverse
                |> List.head
                |> Maybe.andThen (\( pageNumber, _ ) -> Just pageNumber)
                |> Maybe.withDefault 1

        currentPageData =
            Dict.get currentPage existingDataAndPager.pager
                |> Maybe.withDefault RemoteData.NotAsked

        hasNextPage =
            Dict.member (currentPage + 1) existingDataAndPager.pager

        nextPageData =
            Dict.get (currentPage + 1) existingDataAndPager.pager
                |> Maybe.withDefault RemoteData.NotAsked

        -- Prevent endless fetching in case the previous request has ended with `Failure`.
        isPreviousRequestFailed =
            RemoteData.isFailure currentPageData
    in
    if not isPreviousRequestFailed then
        if RemoteData.isNotAsked currentPageData then
            [ currentPage ]

        else if hasNextPage && RemoteData.isNotAsked nextPageData then
            [ currentPage + 1 ]

        else
            []

    else
        []



-- CRUD


{-| Get a single value.
-}
get : key -> PaginatedData key value -> Maybe value
get key (PaginatedData dataAndPager) =
    EveryDictList.get key dataAndPager.data


{-| Get all values.
-}
getAll : PaginatedData key value -> EveryDictList key value
getAll (PaginatedData dataAndPager) =
    dataAndPager.data


{-| Update a single value.
-}
update : key -> (value -> value) -> PaginatedData key value -> PaginatedData key value
update key func ((PaginatedData dataAndPager) as wrapper) =
    case EveryDictList.get key dataAndPager.data of
        Nothing ->
            wrapper

        Just value ->
            PaginatedData
                { dataAndPager | data = EveryDictList.insert key (func value) dataAndPager.data }


{-| Using `remove` is not advised, as it can create a situtation where the item
indicated as first or last in the `pager`, is missing from the `data`.
However, it can be used in situations where all the items are shown, without a
pager, so removing will not have an affect on the pager.
-}
remove : key -> PaginatedData key value -> PaginatedData key value
remove key (PaginatedData dataAndPager) =
    PaginatedData
        { dataAndPager | data = EveryDictList.remove key dataAndPager.data }


{-| Get the pager info.
-}
getPager : PaginatedData key value -> Pager key
getPager (PaginatedData existingDataAndPager) =
    existingDataAndPager.pager


{-| Get the Total count.
-}
getTotalCount : PaginatedData key value -> Maybe Int
getTotalCount (PaginatedData existingDataAndPager) =
    existingDataAndPager.totalCount


{-| Get the Total count, in case you need to set it manually.

Normally, the `totalCount` will be updated via the fetch functions. However you
may have special cases where you would like to change it yourself.

For example, imagine a page with two different Elm widgets. One is resposnbile
of fetching the data, and the other get the data via ports.

In that case, one would have to also pass the `totalCount`, mostly for the case
where the total count is zero. So, if `totalCount` remains a `Nothing`, we would
know that no data was fetched. But if it was `Just 0`, we would know that we have
fetching the data successfully, but it resulted with no items.

-}
setTotalCount : Maybe Int -> PaginatedData key value -> PaginatedData key value
setTotalCount totalCount (PaginatedData existingDataAndPager) =
    PaginatedData
        { existingDataAndPager | totalCount = totalCount }


{-| Used to indicate we're loading a page for the first time.
-}
setPageAsLoading : Int -> PaginatedData key value -> PaginatedData key value
setPageAsLoading pageNumber (PaginatedData existingDataAndPager) =
    let
        pagerUpdated =
            Dict.insert pageNumber RemoteData.Loading existingDataAndPager.pager
    in
    PaginatedData
        { existingDataAndPager | pager = pagerUpdated }


{-| Insert multiple Items into the data and pager dict.
-}
insertMultiple :
    Int
    -> WebData ( EveryDictList k v, Int )
    -> (number -> a1)
    -> (( k, v ) -> Maybe a1)
    -> (k -> v -> EveryDictList a1 value -> EveryDictList a1 value)
    -> (k -> v -> ( a1, EveryDictList a1 value ) -> ( a1, EveryDictList a1 value ))
    -> PaginatedData a1 value
    -> PaginatedData a1 value
insertMultiple pageNumber webdata defaultItemFunc getItemFunc insertFunc insertAfterFunc ((PaginatedData existingDataAndPager) as wrapper) =
    case webdata of
        RemoteData.Success ( items, totalCount ) ->
            let
                maybePreviousItemLastUuid =
                    if pageNumber > 1 then
                        List.foldl
                            (\index accum ->
                                let
                                    pagerInfo =
                                        Dict.get (pageNumber - 1) existingDataAndPager.pager
                                            |> Maybe.andThen RemoteData.toMaybe
                                in
                                case accum of
                                    Just val ->
                                        accum

                                    Nothing ->
                                        case pagerInfo of
                                            Nothing ->
                                                accum

                                            Just pagerInfo ->
                                                Just <| Tuple.second pagerInfo
                            )
                            Nothing
                            (List.reverse <| List.range 1 (pageNumber - 1))

                    else
                        -- This is the first page, so there's nothing before it.
                        Nothing

                itemsUpdated =
                    case maybePreviousItemLastUuid of
                        Nothing ->
                            if totalCount == 0 then
                                -- No items with placed bid.
                                EveryDictList.empty

                            else
                                -- This is the first page, we can enter by order.
                                EveryDictList.foldl
                                    insertFunc
                                    existingDataAndPager.data
                                    items

                        Just previousItemLastUuid ->
                            -- This page is after the previous one. As we know the last
                            -- item from the previous page, we'll have to reverse to new items
                            -- so they will end up correctly.
                            -- That is, if we had these existing items key [1, 2, 3]
                            -- we know that 3 is the last item. So, if we have the new items
                            -- [4, 5, 6] we will reverse them and enter one by one
                            -- [1, 2, 3, 6]. This looks wrong, but since we keep pushing after 3
                            -- the next item will result with [1, 2, 3, 5, 6] and the process
                            -- will end as expected with [1, 2, 3, 4, 5, 6].
                            EveryDictList.foldl
                                insertAfterFunc
                                ( previousItemLastUuid, existingDataAndPager.data )
                                (EveryDictList.reverse items)
                                |> Tuple.second

                totalItems =
                    EveryDictList.size items

                totalPages =
                    -- For example if we have 120 items, and we got 25 items back
                    -- it means there will be 5 pages.
                    (toFloat totalCount / toFloat totalItems)
                        |> ceiling

                -- Get the first and last item, which might be the same one, in case
                -- we have a single item.
                ( firstItem, lastItem ) =
                    ( items
                        |> EveryDictList.getAt 0
                        |> Maybe.andThen getItemFunc
                        |> Maybe.withDefault (defaultItemFunc 0)
                    , items
                        -- If we have 25 items, the last one will be in index 24.
                        |> EveryDictList.getAt (totalItems - 1)
                        |> Maybe.andThen getItemFunc
                        |> Maybe.withDefault (defaultItemFunc 0)
                    )

                pagerUpdated =
                    if totalCount == 0 then
                        -- Update the pager, so we won't continue fetching.
                        Dict.insert pageNumber (RemoteData.Success ( firstItem, lastItem )) existingDataAndPager.pager

                    else if Dict.size existingDataAndPager.pager <= 1 then
                        -- If the pager dict was not built yet, or we just have the
                        -- first page `Loading` - before we knew how many items we'll
                        -- have in total.
                        List.range 1 totalPages
                            |> List.foldl
                                (\index accum ->
                                    let
                                        value =
                                            if index == pageNumber then
                                                RemoteData.Success ( firstItem, lastItem )

                                            else
                                                RemoteData.NotAsked
                                    in
                                    Dict.insert index value accum
                                )
                                Dict.empty

                    else
                        -- Update the existing pager dict.
                        Dict.insert pageNumber (RemoteData.Success ( firstItem, lastItem )) existingDataAndPager.pager
            in
            PaginatedData
                { existingDataAndPager
                    | data = itemsUpdated
                    , pager = pagerUpdated
                    , totalCount = Just totalCount
                }

        RemoteData.Failure error ->
            let
                pager =
                    Dict.insert pageNumber (RemoteData.Failure error) existingDataAndPager.pager
            in
            PaginatedData
                { existingDataAndPager | pager = pager }

        _ ->
            -- Satisfy the compiler.
            wrapper


{-| @todo: Add docs, and improve
-}
insertDirectlyFromClient : key -> value -> PaginatedData key value -> PaginatedData key value
insertDirectlyFromClient key value ((PaginatedData existingDataAndPager) as wrapper) =
    case get key wrapper of
        Just _ ->
            -- Value is already in dict.
            wrapper

        Nothing ->
            -- Very naively just add it to the end of the last page
            let
                ( page, pager ) =
                    existingDataAndPager.pager
                        |> Dict.toList
                        |> List.sortBy (\( key, _ ) -> key)
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault ( 1, RemoteData.NotAsked )

                pagerUpdated =
                    case pager of
                        RemoteData.NotAsked ->
                            -- First and last key are now the only page.
                            RemoteData.Success ( key, key )

                        RemoteData.Success ( start, _ ) ->
                            -- Last key is now the new key.
                            RemoteData.Success ( start, key )

                        _ ->
                            -- Satisfy the compiler.
                            pager

                totalCount =
                    existingDataAndPager.totalCount
                        |> Maybe.withDefault 0
            in
            PaginatedData
                { existingDataAndPager
                    | data = EveryDictList.insert key value existingDataAndPager.data
                    , pager = Dict.insert page pagerUpdated existingDataAndPager.pager
                    , totalCount = Just <| totalCount + 1
                }


{-| View helper.
-}
viewPager : PaginatedData k v -> Int -> (Int -> msg) -> Html msg
viewPager (PaginatedData { pager }) currentPage func =
    if Dict.size pager <= 1 then
        text ""

    else
        -- @todo :Allow adding own attributes to ul/ li
        ul [ class "pagination" ]
            (pager
                |> Dict.keys
                |> List.sort
                |> List.map
                    (\pageNumber ->
                        let
                            aAttr =
                                if pageNumber == currentPage then
                                    [ action "javascript:void(0);" ]

                                else
                                    [ onClick <| func pageNumber ]
                        in
                        li [ classList [ ( "active", pageNumber == currentPage ) ] ]
                            [ a aAttr [ text <| toString pageNumber ]
                            ]
                    )
            )


{-| Get localy Items from the dict, by their page number.
-}
getItemsByPager : PaginatedData k v -> Int -> EveryDictList k v
getItemsByPager (PaginatedData { data, pager }) currentPage =
    if
        Dict.size pager <= 1
        -- We have only a single page.
    then
        data

    else
        let
            pagerInfo =
                Dict.get currentPage pager
                    |> Maybe.withDefault RemoteData.NotAsked
        in
        case pagerInfo of
            RemoteData.Success ( firstItem, lastItem ) ->
                let
                    firstIndex =
                        EveryDictList.indexOfKey firstItem data
                            |> Maybe.withDefault 0

                    lastIndex =
                        EveryDictList.indexOfKey lastItem data
                            |> Maybe.withDefault 0
                in
                -- Rebuild the subset of items.
                List.foldl
                    (\index accum ->
                        case EveryDictList.getAt index data of
                            Just ( k, v ) ->
                                EveryDictList.insert k v accum

                            Nothing ->
                                -- Satisfy the compiler.
                                accum
                    )
                    EveryDictList.empty
                    (List.range firstIndex lastIndex)

            _ ->
                -- We have no pager info yet, so we don't know which items to return.
                EveryDictList.empty
