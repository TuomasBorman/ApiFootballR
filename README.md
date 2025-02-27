# ApiFootballR

This package faciliates retrieal of data from [API-Football](https://www.api-football.com/sports#) directly from R interface.
The basic usage goes something like this:

```
addApiFootball(sport = "football", datatype = "leagues", api.key = "your_api_key")
```

To install, do this:

```
remotes::install_github("https://github.com/TuomasBorman/ApiFootballR")
```

The data can stored conveniently to SQLite database.
See further information and examples from vignettes and function documentation.

This package is not anymore actively developed.
