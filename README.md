[![Build Status](https://travis-ci.org/Gizra/elm-paginated-data.svg?branch=master)](https://travis-ci.org/Gizra/elm-paginated-data)

# elm-paginated-data

{-| By using something like
[https://package.elm-lang.org/packages/krisajenkins/remotedata/latest](`RemoteData`),
you can keep track of the status of a request for data: is it in progress?
Completed? Did it fail? But what if the data comes back to you in pages?
Keeping track of those pages is the purpose of `PaginatedData`. Which pages do
you already have? What were their contents? Which pages do you still need? What
requests are in flight?

## API

For the detailed API, see the
[Elm package site](http://package.elm-lang.org/packages/Gizra/elm-compat-017/latest),
or the links to the right, if you're already there.

## Installation

Try `elm-package install Gizra/elm-paginated-data`

## Development

Try something like:

    git clone https://github.com/Gizra/elm-paginated-data
    cd elm-paginated-data
    npm install
    npm test
