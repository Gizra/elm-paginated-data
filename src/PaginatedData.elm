module PaginatedData exposing
    ( PaginatedData, emptyPaginatedData
    , get, getAll, getItemsByPager, getPager, getTotalCount
    , fetchAll, fetchPaginated
    , insertDirectlyFromClient, insertMultiple, remove, setPageAsLoading, setTotalCount, update
    , viewPager
    )

{-| A `PaginatedData` represents a dict of values, that are paginated on the
server.


### Types

@docs PaginatedData, emptyPaginatedData


### Accessors

@docs get, getAll, getItemsByPager, getPager, getTotalCount


### Fetch helpers

@docs fetchAll, fetchPaginated


### Updaters

@docs insertDirectlyFromClient, insertMultiple, remove, setPageAsLoading, setTotalCount, update


### View

@docs viewPager

-}

import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
import Html exposing (Html, a, li, text, ul)
import Html.Attributes exposing (action, class, classList)
import Html.Events exposing (onClick)
import RemoteData exposing (WebData)


{-| We need to know how much pages we have so we could lazy load.
The pager holds the a tuple with the first and last Item key for that page, so
it's easier to insert new items in the correct place.
-}
type alias PaginatedData key value =
    { data : EveryDictList key value
    , pager : EveryDict Int (WebData ( key, key ))

    -- We keep the total count, so if we are asked to `fetchAll`, we can
    -- calcualte how many pages we'll have based on the first page's result count.
    -- We have this as a Maybe value, so it's easier to know if we already fetched
    -- values (and there might be zero), or not.
    , totalCount : Maybe Int
    }


{-| Empty data, that has not been fetched yet.
-}
emptyPaginatedData : PaginatedData key value
emptyPaginatedData =
    { data = EveryDictList.empty
    , pager = EveryDict.empty
    , totalCount = Nothing
    }


{-| Fetch helper.

@todo: Move <https://github.com/Gizra/elm-essentials/blob/4df1aba4ca15f52552e0ceca34495661826a9a4c/src/Gizra/Update.elm#L1> to own
module.

-}
fetchPaginated : PaginatedData k v -> Int -> (Int -> c) -> List (Maybe c)
fetchPaginated existingDataAndPager currentPage func =
    let
        currentPageData =
            EveryDict.get currentPage existingDataAndPager.pager
                |> Maybe.withDefault RemoteData.NotAsked

        hasNextPage =
            EveryDict.member (currentPage + 1) existingDataAndPager.pager

        nextPageData =
            EveryDict.get (currentPage + 1) existingDataAndPager.pager
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
            [ Just <| func currentPage ]

        else if
            hasNextPage
                && RemoteData.isNotAsked nextPageData
                -- Check that we haven't already fetched all Items.
                && (EveryDictList.size existingDataAndPager.data < totalCount)
                && isFetched
        then
            [ Just <| func (currentPage + 1) ]

        else
            []

    else
        []


{-| Fetch all existing pages.

Next page is fetched as the previous one arrives successfully.

-}
fetchAll : PaginatedData k v -> (Int -> c) -> List (Maybe c)
fetchAll existingDataAndPager func =
    let
        -- Current page is actually the last page that had a successful
        -- response.
        currentPage =
            existingDataAndPager.pager
                |> EveryDict.toList
                -- Keep only successs values.
                |> List.filter (\( _, webData ) -> RemoteData.isSuccess webData)
                -- Sort the list by page number, and get the highest value.
                |> List.sortBy (\( pageNumber, _ ) -> pageNumber)
                |> List.reverse
                |> List.head
                |> Maybe.andThen (\( pageNumber, _ ) -> Just pageNumber)
                |> Maybe.withDefault 1

        currentPageData =
            EveryDict.get currentPage existingDataAndPager.pager
                |> Maybe.withDefault RemoteData.NotAsked

        hasNextPage =
            EveryDict.member (currentPage + 1) existingDataAndPager.pager

        nextPageData =
            EveryDict.get (currentPage + 1) existingDataAndPager.pager
                |> Maybe.withDefault RemoteData.NotAsked

        -- Prevent endless fetching in case the previous request has ended with `Failure`.
        isPreviousRequestFailed =
            RemoteData.isFailure currentPageData
    in
    if not isPreviousRequestFailed then
        if RemoteData.isNotAsked currentPageData then
            [ Just <| func currentPage ]

        else if hasNextPage && RemoteData.isNotAsked nextPageData then
            [ Just <| func (currentPage + 1) ]

        else
            []

    else
        []



-- CRUD


{-| Get a single value.
-}
get : key -> PaginatedData key value -> Maybe value
get key dataAndPager =
    EveryDictList.get key dataAndPager.data


{-| Get all values.
-}
getAll : PaginatedData key value -> EveryDictList key value
getAll dataAndPager =
    dataAndPager.data


{-| Update a single value.
-}
update : key -> (value -> value) -> PaginatedData key value -> PaginatedData key value
update key func dataAndPager =
    case EveryDictList.get key dataAndPager.data of
        Nothing ->
            dataAndPager

        Just value ->
            { dataAndPager | data = EveryDictList.insert key (func value) dataAndPager.data }


{-| Using `remove` is not advised, as it can create a situtation where the item
indicated as first or last in the `pager`, is missing from the `data`.
However, it can be used in situations where all the items are shown, without a
pager, so removing will not have an affect on the pager.
-}
remove : key -> PaginatedData key value -> PaginatedData key value
remove key dataAndPager =
    { dataAndPager | data = EveryDictList.remove key dataAndPager.data }


{-| Get the pager info.
-}
getPager : PaginatedData key value -> EveryDict Int (WebData ( key, key ))
getPager existingDataAndPager =
    existingDataAndPager.pager


{-| Get the Total count.
-}
getTotalCount : PaginatedData key value -> Maybe Int
getTotalCount existingDataAndPager =
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
setTotalCount totalCount existingDataAndPager =
    { existingDataAndPager | totalCount = totalCount }


{-| Used to indicate we're loading a page for the first time.
-}
setPageAsLoading : Int -> PaginatedData key value -> PaginatedData key value
setPageAsLoading pageNumber existingDataAndPager =
    let
        pagerUpdated =
            EveryDict.insert pageNumber RemoteData.Loading existingDataAndPager.pager
    in
    { existingDataAndPager | pager = pagerUpdated }


{-| Insert multiple Items into the data and pager dict.
-}
insertMultiple :
    identifier
    -> Int
    -> RemoteData.RemoteData e ( EveryDictList k v, Int )
    -> (number -> a1)
    -> (( k, v ) -> Maybe a1)
    -> (k -> v -> EveryDictList a1 value -> EveryDictList a1 value)
    -> (k -> v -> ( a1, EveryDictList a1 value ) -> ( a1, EveryDictList a1 value ))
    -> EveryDict identifier (RemoteData.RemoteData e (PaginatedData a1 value))
    -> EveryDict identifier (RemoteData.RemoteData e (PaginatedData a1 value))
insertMultiple identifier pageNumber webdata defaultItemFunc getItemFunc insertFunc insertAfterFunc dict =
    let
        existing =
            EveryDict.get identifier dict
                |> Maybe.withDefault (RemoteData.Success emptyPaginatedData)

        existingDataAndPager =
            existing
                |> RemoteData.toMaybe
                |> Maybe.withDefault emptyPaginatedData
    in
    case webdata of
        RemoteData.Success ( items, totalCount ) ->
            let
                maybePreviousItemLastUuid =
                    if pageNumber > 1 then
                        List.foldl
                            (\index accum ->
                                let
                                    pagerInfo =
                                        EveryDict.get (pageNumber - 1) existingDataAndPager.pager
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
                        EveryDict.insert pageNumber (RemoteData.Success ( firstItem, lastItem )) existingDataAndPager.pager

                    else if EveryDict.size existingDataAndPager.pager <= 1 then
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
                                    EveryDict.insert index value accum
                                )
                                EveryDict.empty

                    else
                        -- Update the existing pager dict.
                        EveryDict.insert pageNumber (RemoteData.Success ( firstItem, lastItem )) existingDataAndPager.pager

                existingDataAndPagerUpdated =
                    { existingDataAndPager
                        | data = itemsUpdated
                        , pager = pagerUpdated
                        , totalCount = Just totalCount
                    }
            in
            EveryDict.insert identifier (RemoteData.Success existingDataAndPagerUpdated) dict

        RemoteData.Failure error ->
            EveryDict.insert identifier (RemoteData.Failure error) dict

        _ ->
            -- Satisfy the compiler.
            dict


{-| @todo: Add docs, and improve
-}
insertDirectlyFromClient : key -> value -> PaginatedData key value -> PaginatedData key value
insertDirectlyFromClient key value existingDataAndPager =
    case get key existingDataAndPager of
        Just _ ->
            -- Value is already in dict.
            existingDataAndPager

        Nothing ->
            -- Very naively just add it to the end of the last page
            let
                ( page, pager ) =
                    existingDataAndPager.pager
                        |> EveryDict.toList
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
            { existingDataAndPager
                | data = EveryDictList.insert key value existingDataAndPager.data
                , pager = EveryDict.insert page pagerUpdated existingDataAndPager.pager
                , totalCount = Just <| totalCount + 1
            }


{-| View helper.
-}
viewPager :
    identifier
    -> { dataAndPager | pager : EveryDict Int v }
    -> Int
    -> (Int -> msg)
    -> Html msg
viewPager identifier { pager } currentPage func =
    if EveryDict.size pager <= 1 then
        text ""

    else
        -- @todo :Allow adding own attributes to ul/ li
        ul [ class "pagination" ]
            (pager
                |> EveryDict.keys
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
getItemsByPager :
    PaginatedData k v
    -> Int
    -> EveryDictList k v
getItemsByPager { data, pager } currentPage =
    if
        EveryDict.size pager <= 1
        -- We have only a single page.
    then
        data

    else
        let
            pagerInfo =
                EveryDict.get currentPage pager
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
