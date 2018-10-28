# Movies Haskell

This is the Haskell implementation of the [movies exercise](https://github.com/julianespinel/movies).

`movies-haskell` was created with educational purposes, to be used as a resource for this meetup: [Cálculo Lambda con Javascript & Haskell Fuera de la Academia](https://www.meetup.com/meetup-group-fMACkbLu/events/255235149/)

## Installation

1. Install Haskell and Stack: https://haskell-lang.org/get-started
1. Install Yesod: `stack install yesod-bin --install-ghc`
1. Clone the repository: `git clone git@github.com:julianespinel/movies-haskell.git`

## Development

Start a development server with:

```
stack exec -- yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

## Testing

```
stack test --flag movies:library-only --flag movies:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

## Usage

With the movies microservice you are able to execute the following actions:

1. Check if microservice is alive
1. Create a movie
1. Find a specific movie
1. Find a set of movies
1. Update a movie
1. Delete a movie

Each API call is specified below:

### 1. Check if microservice is alive
**Request**
```
curl http://localhost:3000/admin/ping
```

**Response**
```json
{
    "message": "pong"
}
```

### 2. Create a movie
**Request**
```
curl -H "Content-Type: application/json" \
       -X POST http://localhost:3000/movies \
       -d @- << EOF
{
  "imdbId": "tt0133093",
  "title": "The Matrix",
  "runtimeInMinutes": 136,
  "releaseDate": "1999-03-31 00:00:00.000Z",
  "filmRating": "R",
  "genre": "Action, Sci-Fi",
  "director": "The Wachowski brothers",
  "plot": "A computer hacker learns from mysterious rebels about the true nature of his reality and his role in the war against its controllers.",
  "metascore": 73,
  "imdbRating": 8.7,
  "imdbVotes": 1023621
}
EOF
```

**Response**
```json
{
    "movie": {
        "imdbRating": 8.7,
        "metascore": 73,
        "director": "The Wachowski brothers",
        "runtimeInMinutes": 136,
        "imdbVotes": 1023621,
        "imdbId": "tt0133093",
        "plot": "A computer hacker learns from mysterious rebels about the true nature of his reality and his role in the war against its controllers.",
        "id": "tt0133093",
        "releaseDate": "1999-03-31T00:00:00Z",
        "title": "The Matrix",
        "genre": "Action, Sci-Fi",
        "filmRating": "R"
    }
}
```

### 3. Find a specific movie
**Request**
```
curl http://localhost:3000/movies/tt0133093
```

**Response**
```json
{
    "movie": {
        "imdbRating": 8.7,
        "metascore": 73,
        "director": "The Wachowski brothers",
        "runtimeInMinutes": 136,
        "imdbVotes": 1023621,
        "imdbId": "tt0133093",
        "plot": "A computer hacker learns from mysterious rebels about the true nature of his reality and his role in the war against its controllers.",
        "id": "tt0133093",
        "releaseDate": "1999-03-31T00:00:00Z",
        "title": "The Matrix",
        "genre": "Action, Sci-Fi",
        "filmRating": "R"
    }
}
```

### 4. Find a set of movies
**Request**
```
curl -sb -H "Accept: application/json" "http://localhost:3000/movies?title=Matrix&runtimeInMinutes=130&metascore=6&imdbRating=7.2&imdbVotes=1000"
```

**Response**
```json
{
    "movies": [
        {
            "imdbRating": 8.7,
            "metascore": 73,
            "director": "The Wachowski brothers",
            "runtimeInMinutes": 136,
            "imdbVotes": 1023621,
            "imdbId": "tt0133093",
            "plot": "A computer hacker learns from mysterious rebels about the true nature of his reality and his role in the war against its controllers.",
            "id": "tt0133093",
            "releaseDate": "1999-03-31T00:00:00Z",
            "title": "The Matrix Reloaded",
            "genre": "Action, Sci-Fi",
            "filmRating": "R"
        }
    ]
}
```

### 5. Update a movie
**Request**
```
curl -H "Content-Type: application/json" \
-X PUT http://localhost:3000/movies/tt0133093 \
-d @- << EOF
{
    "imdbId": "tt0133093",
    "title": "The Matrix Reloaded",
    "runtimeInMinutes": 136,
    "releaseDate": "1999-03-31 00:00:00.000Z",
    "filmRating": "R",
    "genre": "Action, Sci-Fi",
    "director": "The Wachowski brothers",
    "plot": "A computer hacker learns from mysterious rebels about the true nature of his reality and his role in the war against its controllers.",
    "metascore": 73,
    "imdbRating": 8.7,
    "imdbVotes": 1023621
}
EOF
```

**Response**
```json
{
    "movie": {
        "imdbRating": 8.7,
        "metascore": 73,
        "director": "The Wachowski brothers",
        "runtimeInMinutes": 136,
        "imdbVotes": 1023621,
        "imdbId": "tt0133093",
        "plot": "A computer hacker learns from mysterious rebels about the true nature of his reality and his role in the war against its controllers.",
        "id": "tt0133093",
        "releaseDate": "1999-03-31T00:00:00Z",
        "title": "The Matrix Reloaded",
        "genre": "Action, Sci-Fi",
        "filmRating": "R"
    }
}
```

### 6. Delete a movie
**Request**
```
curl -X DELETE http://localhost:3000/movies/tt0133093
```

**Response**
```json
{
  "deleted": true
}
```
