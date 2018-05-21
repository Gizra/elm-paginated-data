module PaginatedData
    exposing
        ( ContainerDict
        , PaginatedData
        , emptyPaginatedData
        , fetchPaginated
        , getItemsByPager
        , viewPager
        )

{-| A `PaginatedData` represents a dict of values, that are paginated on the
server.

@docs ContainerDict, PaginatedData, emptyPaginatedData, fetchPaginated, getItemsByPager, viewPager

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
    }


{-| Empty data, that has not been fetched yet.
-}
emptyPaginatedData : PaginatedData key value
emptyPaginatedData =
    { data = EveryDictList.empty
    , pager = EveryDict.empty
    }


{-| Fetch helper.

@todo: Move <https://github.com/Gizra/elm-essentials/blob/4df1aba4ca15f52552e0ceca34495661826a9a4c/src/Gizra/Update.elm#L1> to own
module.

-}
fetchPaginated :
    ( b, EveryDict b (RemoteData.RemoteData e { c | pager : EveryDict number (RemoteData.RemoteData e1 a) }) )
    -> ( d, EveryDict d number1 )
    -> { c | pager : EveryDict number (RemoteData.RemoteData e1 a) }
    -> (number2 -> f)
    -> List (Maybe f)
fetchPaginated ( backendIndentifier, backendDict ) ( pageIdentifier, pageDict ) emptyDataAndPager func =
    let
        existingData =
            EveryDict.get backendIndentifier backendDict
                |> Maybe.withDefault RemoteData.NotAsked

        existingDataAndPager =
            existingData
                |> RemoteData.toMaybe
                |> Maybe.withDefault emptyDataAndPager

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
        else if hasNextPage && RemoteData.isNotAsked nextPageData then
            [ Just <| func (currentPage + 1) ]
        else
            []
    else
        []



-- CRUD


{-| Insert multiple Items into the data and pager dict.
-}
insertMultipletoDataAndPager identifier pageNumber webdata emptyDataAndPager defaultItemFunc getItemFunc insertFunc insertAfterFunc dict =
    let
        existing =
            EveryDict.get identifier dict
                |> Maybe.withDefault (RemoteData.Success emptyDataAndPager)

        existingDataAndPager =
            existing
                |> RemoteData.toMaybe
                |> Maybe.withDefault emptyDataAndPager
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
                    { existingDataAndPager | data = itemsUpdated, pager = pagerUpdated }
            in
            EveryDict.insert identifier (RemoteData.Success existingDataAndPagerUpdated) dict

        RemoteData.Failure error ->
            EveryDict.insert identifier (RemoteData.Failure error) dict

        _ ->
            -- Satisfy the compiler.
            dict


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
