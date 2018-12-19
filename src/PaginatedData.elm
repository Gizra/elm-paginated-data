module PaginatedData exposing
    ( PaginatedData, PaginatedWebData, emptyPaginatedData
    , get, getAll, getItemsByPage, getTotalCount, getPage, getLocal
    , fetchAllPages, fetchNextPage
    , insertLocal, handleFetchedPage, remove, setPageAsLoading, setTotalCount, update
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

@docs PaginatedData, PaginatedWebData, emptyPaginatedData


### Accessors

@docs get, getAll, getItemsByPage, getTotalCount, getPage, getLocal


### Fetch helpers

@docs fetchAllPages, fetchNextPage


### Mutators

@docs insertLocal, handleFetchedPage, remove, setPageAsLoading, setTotalCount, update


### View

@docs viewPager

-}

import Dict exposing (Dict)
import EveryDictList exposing (EveryDictList)
import Html exposing (Html, a, li, text, ul)
import Html.Attributes exposing (action, class, classList)
import Html.Events exposing (onClick)
import Http
import RemoteData exposing (RemoteData(..))


{-| Represents the status of paged data, where we may have some or all of the
pages, and requests for one or more pages may be in-flight or have had errors.
You may also have some "local" data which is not on any page. For instance,
values that may already be saved in the backend, but you didn't re-fetch a list
of items, thus do not yet know the page those items should belong to.

The `value` is the type of the value that you are fetching in pages.

The `key` is a type for looking up values. For instance, it might be an ID for
the values. It is used as the key for `EveryDictList`, so it should be unique,
and it must work with `toString` in a way that produces unique results.

The `err` is a type for tracking errors fetching pages. For the usual case
of `Http` requests, this will be `Http.Error`. The `PaginatedWebData` type
is a convenient alias for that common case.

-}
type PaginatedData err key value
    = PaginatedData
        -- The first part is a record representing an attempt to fetch each
        -- page. In `handleFetchedPage`, we look at the `totalCount` and try to
        -- infer the page size, so we will actually have a `NotAsked` entry
        -- here for every page we expect to exist.
        --
        -- The second part represents the number of items which we would have
        -- once we receive all the pages, if we know that. (For instance, the
        -- backend might tell us how many items there in total are while
        -- returning just one page). Note that this is the item count, not the
        -- page count.
        { backend : ( Pager err key value, Maybe Int )

        -- Local values which were **not** obtained from the backend, and thus
        -- are not on any page. These are not included in the totalCount, since
        -- that is what the backend reports.
        , local : EveryDictList key value
        }


type alias Pager err key value =
    Dict Int (RemoteData err (EveryDictList key value))


{-| A convenient alias for the common case where your `PaginatedData`
is fetched via `Http` requests, so your errors are `Http.Error`.
-}
type alias PaginatedWebData key value =
    PaginatedData Http.Error key value


{-| A starting point for a `PaginatedData`, with no values and no
requests in progress.
-}
emptyPaginatedData : PaginatedData err key value
emptyPaginatedData =
    PaginatedData
        { backend = ( Dict.empty, Nothing )
        , local = EveryDictList.empty
        }


{-| An internal function to give us the first page which was successfully
fetched. We use this, for instance, to infer the page size. We can't infer it
from the last page, since that might be a partial page.
-}
firstPageWithItems : Pager err key value -> Maybe ( Int, EveryDictList key value )
firstPageWithItems =
    Dict.foldl accumulatorOrItems Nothing


{-| Like firstPageWithItems, but the opposite.
-}
lastPageWithItems : Pager err key value -> Maybe ( Int, EveryDictList key value )
lastPageWithItems =
    Dict.foldr accumulatorOrItems Nothing


{-| Keep the accumulator if it is a `Just`. Otherwise, use the provided `RemoteData`
if it is a `Success`.
-}
accumulatorOrItems : Int -> RemoteData err (EveryDictList key value) -> Maybe ( Int, EveryDictList key value ) -> Maybe ( Int, EveryDictList key value )
accumulatorOrItems page remoteData accum =
    case accum of
        Just _ ->
            accum

        Nothing ->
            remoteData
                |> RemoteData.map (\data -> ( page, data ))
                |> RemoteData.toMaybe


{-| You supply the current page -- for instance, perhaps the page which the
user has selected in the UI. You'll get back a list of pages which you need to
fetch.

Say, for instance, that the current page is page 2.

  - If you haven't asked for page 2 yet, then you'll get `[2]` back.

  - If you successfully received page 2, and there are more items to fetch,
    you'll get `[3]` back. This indicates that it would be a good idea to
    fetch page 3 ... the user may want it soon! (If there is no page 3, we'll
    check whether to loop around and fetch page 1).

  - If we have both page 2 and page 3, you'll get `[]` back ... there's
    nothing to fetch right now.

Presumably you are tracking in your model both the `PaginatedData` and some
indication of which page the user is interested in right now. So, you can call
this function to help determine what pages you ought to request right now.

When you do issue the requests, it is important to call `setPageLoading` to indicate
that the page is now loading! Otherwise, when you call this function again,
you'll be told to fetch the page again. If you mark the page as `Loading`, then
this function won't tell you to load it again.

Note that this function does not tell you to re-fetch pages that have failed.
When to do that is very much dependent on your situation, so you'll need to
check for that yourself.

This function is a nice helper for the
[`andThenFetch`](https://package.elm-lang.org/packages/Gizra/elm-essentials/latest/Gizra-Update#andThenFetch)
function, but is useful in other contexts as well.

If you'd like to fetch all the pages (one at a time), rather than just the
current page and possibly the next page, take a look at `fetchAllPages` instead.

-}
fetchNextPage : Int -> PaginatedData e k v -> List Int
fetchNextPage currentPage (PaginatedData { backend }) =
    -- Page number should be valid
    if currentPage < 1 then
        []

    else
        let
            pager =
                Tuple.first backend

            currentPageData =
                Dict.get currentPage pager
                    |> Maybe.withDefault NotAsked
        in
        case currentPageData of
            NotAsked ->
                -- If we haven't asked for the current page yet, we should do so.
                [ currentPage ]

            Loading ->
                -- If the current page is loading, we wait for an answer.
                []

            Failure _ ->
                -- We don't automatically re-try failures ... and, if we got a
                -- failure, we don't try to pre-fetch the next page.
                []

            Success _ ->
                -- If we have data for the current page, we check whether to fetch
                -- the next page.
                let
                    nextPage =
                        currentPage + 1
                in
                case Dict.get nextPage pager of
                    Just NotAsked ->
                        -- If we expect a page to exist here, and it is a `NotAsked`,
                        -- then we ask for it.
                        [ nextPage ]

                    Just _ ->
                        -- If we expect a page to exist here, and it's something other
                        -- than `NotAsked`, we don't do anything.
                        []

                    Nothing ->
                        -- If we get a Nothing here, it means that we don't
                        -- expect the next page to exist.  Or, to put it
                        -- another way, we have the current page, and we think
                        -- it is the last page. So, we think about asking for
                        -- page 1 instead.  That way, we'll eventually loop
                        -- around, even if we didn't start at page 1.
                        case Dict.get 1 pager of
                            Just NotAsked ->
                                [ 1 ]

                            _ ->
                                []


{-| Suppose you'd like to fetch all the pages, but just one at a time. You'll
start with the first page -- when it successfully arrives, you'll request the
next page, and so on. That's what this function helps with.

  - The first time you call it (with an `emptyPaginatedData`), it will return
    `[1]` ... suggesting that you get the first page.

  - Once you call `setPageLoading` to indicate that a request for page 1 is in
    progress, calling this again will return `[]` -- we wait for a result.

  - If that request ends up being an error, we'll return `[]` when you call this
    again. We don't help with error recovery here, because that will differ too
    much dependning on your context.

  - If that request succeeds, and you insert the results here, we'll return `[2]`
    the next time you call this. It will be time to fetch the next page!

If you don't want to fetch all the pages at once, take a look at
`fetchNextPage` instead.

-}
fetchAllPages : PaginatedData e k v -> List Int
fetchAllPages ((PaginatedData { backend }) as data) =
    let
        -- Current page is actually the last page that had a successful
        -- response.
        currentPage =
            backend
                |> Tuple.first
                |> lastPageWithItems
                |> Maybe.map Tuple.first
                |> Maybe.withDefault 1
    in
    fetchNextPage currentPage data



-- CRUD


{-| Get a single value, whether it is local or on a page.

We assume that keys are unique. If not, we will return a value with that key,
but which one is undefined.

-}
get : key -> PaginatedData err key value -> Maybe value
get key (PaginatedData data) =
    -- We iterate through the pages looking for the key. If
    -- necessary, we could optimize this by keeping an index
    -- of which page each key is on.
    Dict.foldl
        (\page data accum ->
            case accum of
                Just found ->
                    -- We don't keep testing once we find one.
                    accum

                Nothing ->
                    case data of
                        Success items ->
                            EveryDictList.get key items

                        _ ->
                            Nothing
        )
        -- We start by looking in local, and then iterate through
        -- the pages.
        (EveryDictList.get key data.local)
        (Tuple.first data.backend)


{-| Get all values, whether from a page or local.

We assume that keys are unique. If not, we will include one value for each key,
but it is undefined which value will be used.

The values will be returned in order by page, with local values
at the end.

-}
getAll : PaginatedData err key value -> EveryDictList key value
getAll (PaginatedData data) =
    -- We do a `foldr` so that we can append to the beginning of
    -- the DictList, which is faster than appending at the end.
    Dict.foldr
        (\page data accum ->
            case data of
                Success items ->
                    EveryDictList.append items accum

                _ ->
                    accum
        )
        data.local
        (Tuple.first data.backend)


{-| A convenience to map over the pages (i.e. not local values).
-}
mapPages : (Int -> RemoteData err (EveryDictList key value) -> RemoteData err (EveryDictList key value)) -> PaginatedData err key value -> PaginatedData err key value
mapPages func (PaginatedData data) =
    PaginatedData
        { data | backend = Tuple.mapFirst (Dict.map func) data.backend }


{-| A convenience to map over our local values.
-}
mapLocal : (EveryDictList key value -> EveryDictList key value) -> PaginatedData err key value -> PaginatedData err key value
mapLocal func (PaginatedData data) =
    PaginatedData
        { data | local = func data.local }


{-| Update a single value.

If they key is not found, the `PaginatedData` is returned unchanged.

We assume that keys are unique. If there is more than one item with the same
key, we will update each of them.

-}
update : key -> (value -> value) -> PaginatedData err key value -> PaginatedData err key value
update key func data =
    data
        |> mapPages (always (RemoteData.map (EveryDictList.update key (Maybe.map func))))
        |> mapLocal (EveryDictList.update key (Maybe.map func))


{-| Remove a value from the data.

We assume that keys are unique. If there is more than one item wiht the same
key, we will remove them all.

-}
remove : key -> PaginatedData err key value -> PaginatedData err key value
remove key data =
    data
        |> mapPages (always (RemoteData.map (EveryDictList.remove key)))
        |> mapLocal (EveryDictList.remove key)


{-| Get the pager info for the specified page (which is 1-based ... that is,
the first page is page 1).

  - `NotAsked` means that we haven't requested that page.

  - `Loading` means that a request for that page is in progress.

  - `Failure err` means that a request for that page has failed.

  - `Success (key, key)` means that we received data. The two keys represent
    the keys corresponding to the first and last items on the page.

-}
getPage : Int -> PaginatedData err key value -> RemoteData err (EveryDictList key value)
getPage page (PaginatedData { backend }) =
    backend
        |> Tuple.first
        |> Dict.get page
        |> Maybe.withDefault NotAsked


{-| Get values which are not on any page.
-}
getLocal : PaginatedData err key value -> EveryDictList key value
getLocal (PaginatedData { local }) =
    local


{-| Get the total count of all the values on all the pages. This includes
values on pages which we have not fetched yet. However, it does not include
any "local" items (items not on any page).

If we don't know what the total count is yet, this will be `Nothing`.

-}
getTotalCount : PaginatedData err key value -> Maybe Int
getTotalCount (PaginatedData { backend }) =
    Tuple.second backend


{-| Set the total count, in case you need to set it manually.

Normally, the `totalCount` will be updated via the fetch functions. However you
may have special cases where you would like to change it yourself.

For example, imagine a page with two different Elm widgets. One is resposnbile
of fetching the data, and the other get the data via ports.

In that case, one would have to also pass the `totalCount`, mostly for the case
where the total count is zero. So, if `totalCount` remains a `Nothing`, we would
know that no data was fetched. But if it was `Just 0`, we would know that we have
fetching the data successfully, but it resulted with no items.

Note that this should include only those items that are on pages, not
"local" items which are not on any page.

-}
setTotalCount : Maybe Int -> PaginatedData err key value -> PaginatedData err key value
setTotalCount totalCount (PaginatedData data) =
    PaginatedData
        { data | backend = Tuple.mapSecond (always totalCount) data.backend }


{-| Mark that a request for the specified page (1-based) is currently in progress.
-}
setPageAsLoading : Int -> PaginatedData err key value -> PaginatedData err key value
setPageAsLoading pageNumber =
    handleFetchedPage pageNumber Loading


{-| When you receive a response from the backend to your request for a page of
data, use this function to update the `PaginatedData` with the response.

  - The first parameter is the page number which you requested (1-based).

  - The second parameter is the response received from the backend.

    In the `Success` case, the `EveryDictList` is the data you received, and
    the `Int` is the total count of all items available on the backend (not
    just the items on this page). We will update the `PaginatedData` to reflect
    the successful request.

    If you provide a `Failure` response, we will update the page to record the
    failed request.

    If you provide `Loading` or `NotAsked`, we will return the `PaginatedData`
    unchanged. (You can use `setPageAsLoading` to return a page to `Loading`
    status.)

-}
handleFetchedPage :
    Int
    -> RemoteData err ( EveryDictList key value, Int )
    -> PaginatedData err key value
    -> PaginatedData err key value
handleFetchedPage pageNumber webdata (PaginatedData existing) =
    let
        pagerWithResponse =
            existing.backend
                |> Tuple.first
                |> Dict.insert pageNumber (RemoteData.map Tuple.first webdata)
    in
    if pageNumber < 1 then
        -- Page number must be valid
        PaginatedData existing

    else
        case webdata of
            Success ( items, totalCount ) ->
                let
                    -- This should be accurate unless we only have the last page.
                    -- If we have only one page, it's not in general possible to
                    -- figure out whether it's the last page, unless we already
                    -- know the page size (which is what we're trying to figure out
                    -- here).
                    pageSize =
                        firstPageWithItems pagerWithResponse
                            |> Maybe.map (Tuple.second >> EveryDictList.size)

                    lastPageNumber =
                        -- For example if we have 120 items, and we got 25 items
                        -- back it means there will be 5 pages.
                        Maybe.map
                            (\size ->
                                -- If we're asking for the first page and it has no
                                -- items, then we do want to keep a record of
                                -- having requested the one page. And, avoid
                                -- dividing by zero!
                                if size == 0 then
                                    1

                                else
                                    ceiling (toFloat totalCount / toFloat size)
                            )
                            pageSize

                    pagerWithAdjustedPageCount =
                        case lastPageNumber of
                            Nothing ->
                                pagerWithResponse

                            Just lastPage ->
                                let
                                    pagerWithoutNonexistentPages =
                                        Dict.filter
                                            (\page data ->
                                                -- We'll keep pages if they are
                                                -- within our total pages, or if
                                                -- they have data.
                                                page <= lastPage || RemoteData.isSuccess data
                                            )
                                            pagerWithResponse

                                    allNotAsked =
                                        List.range 1 lastPage
                                            |> List.map (\page -> ( page, NotAsked ))
                                            |> Dict.fromList
                                in
                                -- `union` prefers the first argument. So,
                                -- basically, we're providing `NotAsked` as the
                                -- defaults where we don't have an entry for a
                                -- page yet.
                                Dict.union pagerWithoutNonexistentPages allNotAsked
                in
                PaginatedData
                    { existing
                        | backend = ( pagerWithAdjustedPageCount, Just totalCount )
                    }

            _ ->
                -- In the other cases, we just remember the result of the request.
                PaginatedData
                    { existing | backend = Tuple.mapFirst (always pagerWithResponse) existing.backend }


{-| Insert a value which is not on any page.
-}
insertLocal : key -> value -> PaginatedData err key value -> PaginatedData err key value
insertLocal key value =
    mapLocal (EveryDictList.insert key value)


{-| Generate some HTML with links to each page.

  - The first parameter is a function that takes a page the user clicks
    on, and returns a `msg` that will navigate to that page.

  - The second parameter is the current page number (1-based). That page will
    be given an "active" class, and will not be clickable.

You'll get back some Html that looks something like this (assuming
there are three pages and the current page is page 2).

    ul
        [ class "pagination" ]
        [ li [] [ onClick ... ] [ text "1" ]
        , li [] [ class "active" ] [ text "2" ]
        , li [] [ onClick ... ] [ text "3" ]
        ]

If we haven't attempted to fetch any pages yet, you'll get back an empty text
node.

Note that this does not take into account any "local" values.

-}
viewPager : (Int -> msg) -> Int -> PaginatedData e k v -> Html msg
viewPager func currentPage (PaginatedData { backend }) =
    backend
        |> Tuple.first
        |> Dict.keys
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
        |> ul [ class "pagination" ]


{-| Get all the items which are on the specified page (1-based).

This will return an empty `EveryDictList` for pages which don't exist, are
loading, or had errors. For more specific information about those cases, use
`getPage` instead.

-}
getItemsByPage : Int -> PaginatedData e k v -> EveryDictList k v
getItemsByPage currentPage =
    getPage currentPage >> RemoteData.withDefault EveryDictList.empty
