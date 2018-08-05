module PaginatedData
    exposing
        ( ContainerDict
        , PaginatedData
        , emptyContainer
        , emptyPaginatedData
        , fetchAll
        , fetchPaginated
        , get
        , getAll
        , getItemsByPager
        , getPagerState
        , insertDirectlyFromClient
        , insertMultiple
        , remove
        , setPageAsLoading
        , update
        , viewPager
        )

{-| A `PaginatedData` represents a dict of values, that are paginated on the
server.

@docs ContainerDict, PaginatedData, emptyContainer, emptyPaginatedData, fetchAll, fetchPaginated, get, getAll, getItemsByPager, getPagerState, insertDirectlyFromClient, insertMultiple, remove, setPageAsLoading, update, viewPager

-}

import EveryDict exposing (EveryDict)
import EveryDictList exposing (EveryDictList)
import Html exposing (Html, a, li, text, ul)
import Html.Attributes exposing (action, class, classList)
import Html.Events exposing (onClick)
import RemoteData exposing (WebData)


{-| A container Dict can act as a local cache.

@todo: Add docs.

-}
type alias ContainerDict identifier key value =
    EveryDict identifier (WebData (PaginatedData key value))


{-| We need to know how much pages we have so we could lazy load.
The pager holds the a tuple with the first and last Item key for that page, so
it's easier to insert new items in the correct place.
-}
type alias PaginatedData key value =
    { data : EveryDictList key value
    , pager : EveryDict Int (WebData ( key, key ))

    -- We keep the total count, so if we are asked to `fetchAll`, we can
    -- calcualte how many pages we'll have based on the first page's result count.
    , totalCount : Int
    }


{-| Return an empty container.
-}
emptyContainer : identifier -> ContainerDict identifier key value
emptyContainer identifier =
    EveryDict.singleton identifier RemoteData.NotAsked


{-| Empty data, that has not been fetched yet.
-}
emptyPaginatedData : PaginatedData key value
emptyPaginatedData =
    { data = EveryDictList.empty
    , pager = EveryDict.empty
    , totalCount = 0
    }


{-| Fetch helper.

@todo: Move <https://github.com/Gizra/elm-essentials/blob/4df1aba4ca15f52552e0ceca34495661826a9a4c/src/Gizra/Update.elm#L1> to own
module.

-}
fetchPaginated :
    ( a, EveryDict a (WebData (PaginatedData key value)) )
    -> ( b, EveryDict b number )
    -> (Int -> c)
    -> List (Maybe c)
fetchPaginated ( backendIndentifier, backendDict ) ( pageIdentifier, pageDict ) func =
    let
        existingData =
            EveryDict.get backendIndentifier backendDict
                |> Maybe.withDefault RemoteData.NotAsked

        existingDataAndPager =
            existingData
                |> RemoteData.toMaybe
                |> Maybe.withDefault emptyPaginatedData

        currentPage =
            EveryDict.get pageIdentifier pageDict
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
            EveryDict.get backendIndentifier backendDict
                |> Maybe.withDefault RemoteData.NotAsked
                |> RemoteData.isFailure
    in
    if not isPreviousRequestFailed then
        if RemoteData.isNotAsked currentPageData then
            [ Just <| func currentPage ]
        else if
            hasNextPage
                && RemoteData.isNotAsked nextPageData
                -- Check that we haven't already fetched all Items.
                && (EveryDictList.size existingDataAndPager.data < existingDataAndPager.totalCount)
        then
            [ Just <| func (currentPage + 1) ]
        else
            []
    else
        []


{-| Fetch all existing pages.

Next page is fetched as the previous one arrives successfully.

-}
fetchAll :
    ( a, EveryDict a (WebData (PaginatedData key value)) )
    -> (Int -> c)
    -> List (Maybe c)
fetchAll ( backendIndentifier, backendDict ) func =
    let
        existingData =
            EveryDict.get backendIndentifier backendDict
                |> Maybe.withDefault RemoteData.NotAsked

        existingDataAndPager =
            existingData
                |> RemoteData.toMaybe
                |> Maybe.withDefault emptyPaginatedData

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
            EveryDict.get backendIndentifier backendDict
                |> Maybe.withDefault RemoteData.NotAsked
                |> RemoteData.isFailure
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
get :
    identifier
    -> key
    -> EveryDict identifier (WebData (PaginatedData key value))
    -> Maybe value
get identifier key dict =
    let
        existing =
            EveryDict.get identifier dict
                |> Maybe.withDefault (RemoteData.Success emptyPaginatedData)

        dataAndPager =
            existing
                |> RemoteData.toMaybe
                |> Maybe.withDefault emptyPaginatedData
    in
    EveryDictList.get key dataAndPager.data


{-| Get all values.
-}
getAll :
    identifier
    -> EveryDict identifier (WebData (PaginatedData key value))
    -> EveryDictList key value
getAll identifier dict =
    let
        existing =
            EveryDict.get identifier dict
                |> Maybe.withDefault (RemoteData.Success emptyPaginatedData)

        dataAndPager =
            existing
                |> RemoteData.toMaybe
                |> Maybe.withDefault emptyPaginatedData
    in
    dataAndPager.data


{-| Update a single value.
-}
update :
    identifier
    -> key
    -> (value -> value)
    -> EveryDict identifier (WebData (PaginatedData key value))
    -> EveryDict identifier (WebData (PaginatedData key value))
update identifier key func dict =
    let
        existing =
            EveryDict.get identifier dict
                |> Maybe.withDefault (RemoteData.Success emptyPaginatedData)

        dataAndPager =
            existing
                |> RemoteData.toMaybe
                |> Maybe.withDefault emptyPaginatedData
    in
    case EveryDictList.get key dataAndPager.data of
        Nothing ->
            dict

        Just value ->
            let
                valueUpdated =
                    func value

                dataAndPagerUpdated =
                    { dataAndPager | data = EveryDictList.insert key valueUpdated dataAndPager.data }
            in
            EveryDict.insert identifier (RemoteData.Success dataAndPagerUpdated) dict


{-| Using `remove` is not advised, as it can create a situtation where the item
indicated as first or last in the `pager`, is missing from the `data`.
However, it can be used in situations where all the items are shown, without a
pager, so removing will not have an affect on the pager.
-}
remove :
    identifier
    -> key
    -> EveryDict identifier (WebData (PaginatedData key value))
    -> EveryDict identifier (WebData (PaginatedData key value))
remove identifier key dict =
    let
        existing =
            EveryDict.get identifier dict
                |> Maybe.withDefault (RemoteData.Success emptyPaginatedData)

        dataAndPager =
            existing
                |> RemoteData.toMaybe
                |> Maybe.withDefault emptyPaginatedData

        dataAndPagerUpdated =
            { dataAndPager | data = EveryDictList.remove key dataAndPager.data }
    in
    EveryDict.insert identifier (RemoteData.Success dataAndPagerUpdated) dict


{-| Get the pager state of a specific page number.
-}
getPagerState : identifier -> Int -> EveryDict identifier (WebData (PaginatedData key value)) -> WebData ( key, key )
getPagerState identifier pageNumber dict =
    let
        existing =
            EveryDict.get identifier dict
                |> Maybe.withDefault (RemoteData.Success emptyPaginatedData)

        existingDataAndPager =
            existing
                |> RemoteData.toMaybe
                |> Maybe.withDefault emptyPaginatedData
    in
    EveryDict.get pageNumber existingDataAndPager.pager
        |> Maybe.withDefault RemoteData.NotAsked


{-| Used to indicate we're loading a page for the first time.
-}
setPageAsLoading :
    identifier
    -> Int
    -> EveryDict identifier (WebData (PaginatedData key value))
    -> EveryDict identifier (WebData (PaginatedData key value))
setPageAsLoading identifier pageNumber dict =
    let
        existing =
            EveryDict.get identifier dict
                |> Maybe.withDefault (RemoteData.Success emptyPaginatedData)

        existingDataAndPager =
            existing
                |> RemoteData.toMaybe
                |> Maybe.withDefault emptyPaginatedData

        pagerUpdated =
            EveryDict.insert pageNumber RemoteData.Loading existingDataAndPager.pager

        existingDataAndPagerUpdated =
            { existingDataAndPager | pager = pagerUpdated }
    in
    EveryDict.insert identifier (RemoteData.Success existingDataAndPagerUpdated) dict


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
                        , totalCount = totalCount
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
insertDirectlyFromClient :
    identifier
    -> ( key, value )
    -> EveryDict identifier (WebData (PaginatedData key value))
    -> EveryDict identifier (WebData (PaginatedData key value))
insertDirectlyFromClient identifier ( key, value ) dict =
    case get identifier key dict of
        Just _ ->
            -- Value is already in dict.
            dict

        Nothing ->
            -- Very naively just add it to the end of the last page
            let
                existing =
                    EveryDict.get identifier dict
                        |> Maybe.withDefault (RemoteData.Success emptyPaginatedData)

                existingDataAndPager =
                    existing
                        |> RemoteData.toMaybe
                        |> Maybe.withDefault emptyPaginatedData

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

                existingDataAndPagerUpdated =
                    { existingDataAndPager
                        | data = EveryDictList.insert key value existingDataAndPager.data
                        , pager = EveryDict.insert page pagerUpdated existingDataAndPager.pager
                        , totalCount = existingDataAndPager.totalCount + 1
                    }
            in
            EveryDict.insert identifier (RemoteData.Success existingDataAndPagerUpdated) dict


{-| View helper.
-}
viewPager :
    identifier
    -> { dataAndPager | pager : EveryDict Int v }
    -> EveryDict identifier Int
    -> (Int -> msg)
    -> Html msg
viewPager identifier { pager } pageProperty func =
    if EveryDict.size pager <= 1 then
        text ""
    else
        let
            currentPage =
                EveryDict.get identifier pageProperty
                    |> Maybe.withDefault 1
        in
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
    identifier
    ->
        { dataAndPager
            | data : EveryDictList k v
            , pager : EveryDict Int (WebData ( k, k ))
        }
    -> EveryDict identifier Int
    -> EveryDictList k v
getItemsByPager identifier { data, pager } pageProperty =
    if
        EveryDict.size pager <= 1
        -- We have only a single page.
    then
        data
    else
        let
            currentPage =
                EveryDict.get identifier pageProperty
                    |> Maybe.withDefault 1

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
