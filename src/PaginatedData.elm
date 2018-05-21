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
