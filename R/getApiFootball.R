#' Get data from ApiFootball.
#'
#' @details
#' With this function, you can fetch data from
#' [API-Football](https://www.api-football.com/sports#).
#'
#' \code{getApiFootball} returns the results while \code{addApiFootball} adds
#' them to SQL database. \code{updateApiFootball} can bet utilized to
#' to fetch all available data. It is especially useful if you want to update
#' your local database daily; just run \code{updateFootballApi} and it
#' updates the database for you.
#'
#' By default, the database is stored to directory
#' \code{"data/apifootball")}. You can cpntrpl this with
#' \code{dir.name}.
#'
#' @param sport \code{Character(1L)}. Sport to be fetched.
#'
#' @param datatype \code{Character(1L)}. Datatype to be fetched.
#'
#' @param api.key \code{Character(1L)}. Api key to Api-Football.
#'
#' @param all \code{Logical(1L)}. Whether to get leagues and seasons from
#' database and fetch results on all of them. If \code{FALSE}, you have to
#' specify league and season, for instance, when fetching games.
#'
#' @return Fetched data or path to database file.
#'
#' @examples
#'
#' # Get games
#' res <- getApiFootball("basketball", "games", league = 1, season = 2023, api.key = "your_api_key")
#'
#' # Fetch leagues and add them to database
#' addApiFootball("basketball", "leagues", api.key = "your_api_key")
#'
#' # Update the database
#' updateApiFootball(api.key = "your_api_key")
#'
#' @name API-Football
NULL

#' @rdname API-Football
#' @export
getApiFootball <- function(
        sport,
        datatype,
        api.key = NULL,
        ...){
    # Input check
    available_sports <- c(
        "afl", "american-football", "baseball", "basketball", "football",
        "formula-1", "handball", "hockey", "nba", "rugby", "volleyball")
    if( !(length(sport) == 1 && sport %in% available_sports)  ){
        stop(
            "'sport' must be one of the following options: '",
            paste(available_sports, collapse = "', '"), "'", call. = FALSE)
    }
    #
    FUN <- switch(
        sport,
        "afl" = .check_datatype_v1,
        "american-football" = .check_datatype_american_football,
        "baseball" = .check_datatype_v1,
        "basketball" = .check_datatype_v1,
        "football" = .check_datatype_v3,
        "formula-1" = .check_datatype_formula,
        "handball" = .check_datatype_v1,
        "hockey" = .check_datatype_v1,
        "nba" = .check_datatype_v2,
        "rugby" = .check_datatype_v1,
        "volleyball" = .check_datatype_v1
    )
    FUN(datatype)
    #
    if( !(
          is.null(api.key) ||
          (length(api.key) == 1 && is.character(api.key)) ||
          (length(api.key) == 1 && api.key %% 1 == 0 && api.key > 0) ) ){
      stop(
          "'api.key' must be NULL, a positive integer or a string.",
          call. = FALSE)
    }
    # Get api key from file.
    if( is.null(api.key) || (is.numeric(api.key) && api.key %% 1 == 0) ){
        api.key <- .get_api_key(api.key, service = "API-Football", ...)
    }
    #
    .check_params(sport, datatype, ...)
    #
    # Get host
    host <- .get_host(sport, ...)
    # Get url
    url <- .get_url(sport, datatype, host, ...)
    # Create a request
    req <- request(url)
    # Add headers
    req <- req_headers(
        req, `x-rapidapi-host` = host, `x-rapidapi-key` = api.key)
    # Perform request
    res <- .perform_request(req)
    # If error, wait and try again
    if( length(res[["errors"]]) != 0 ){
        res <- .wait_and_try_again(res, req, ...)
    }
    # If there is an error from api; e.g. request limit exceeded
    dat <- list(NULL)
    # If no results, give warning message
    if( res[["results"]] == 0 ){
        msg <- paste0("sport='", sport, "', datatype='", datatype, "'")
        # Add parameter info
        if( length(res[["parameters"]]) > 0 ){
            msg_params <- paste0(
                names(res[["parameters"]]),
                paste0("='", res[["parameters"]], "'"), collapse = ", ")
            msg <- paste0(msg, ", ", msg_params)
        }
        warning(
            "No results found for the following query:\n", msg, call. = FALSE)
    } else{
        # Get only the data
        dat <- res[["response"]]

        # Polish the data, replace NULLs with NA. NULLs lead to problems,
        # because they are not evaluated as values but just empty lists.
        dat <- .simple_rapply(dat, function(x) if(is.null(x)) NA else x)

        # Get function for parsing
        FUN <- .get_parser(sport, datatype)
        # Parse
        dat <- FUN(res = dat, datatype = datatype)
        # Ensure that there is no nested lists anymore
        if( !is.data.frame(dat) ){
            dat <- .unnest_list_of_lists(dat)
            dat <- lapply(
                dat, function(tab) as.data.frame(lapply(tab, unlist)) )
        } else{
            # Ensure that the result is always list.
            dat <- list(table = dat)
        }
    }

    # Add query info if specified
    dat <- .add_query_info(dat, res, ...)

    return(dat)
}

#' @rdname API-Football
#' @export
addApiFootball <- function(
        sport, datatype, api.key, all = FALSE, ...){
    # Input check
    available_sports <- c(
        "afl", "american-football", "baseball", "basketball", "football",
        "handball", "hockey", "nba", "rugby", "volleyball")
    if( !(length(sport) == 1 && sport %in% available_sports)  ){
        stop(
            "'sport' must be one of the following options: '",
            paste(available_sports, collapse = "', '"), "'", call. = FALSE)
    }
    #
    FUN <- switch(
        sport,
        "afl" = .check_datatype_afl,
        "american-football" = .check_datatype_american_football,
        "baseball" = .check_datatype_v1,
        "basketball" = .check_datatype_v1,
        "football" = .check_datatype_v3,
        "formula-1" = .check_datatype_formula,
        "handball" = .check_datatype_v1,
        "hockey" = .check_datatype_v1,
        "nba" = .check_datatype_v2,
        "rugby" = .check_datatype_v1,
        "volleyball" = .check_datatype_v1
    )
    FUN(datatype)
    # Only certain datatypes are supported
    supported_datatypes <- c(
        "fixtures", "games", "leagues",  "odds", "standings", "teams")
    if( !datatype %in% supported_datatypes ){
        stop(
            "'datatype' must be one of the followings: '",
            paste(supported_datatypes, collapse = "', '"), "'", call. = FALSE)
    }
    #
    if( !(
        is.null(api.key) ||
        (length(api.key) == 1 && is.character(api.key)) ||
        (length(api.key) == 1 && api.key %% 1 == 0 && api.key > 0) ) ){
        stop(
            "'api.key' must be NULL, a positive integer or a string.",
            call. = FALSE)
    }
    # Get api key from file.
    if( is.null(api.key) || is.numeric(api.key) && api.key %% 1 == 0 ){
        api.key <- .get_api_key(api.key, service = "API-Football", ...)
    }
    #
    if( !(length(all) == 1 && is.logical(all)) ){
        stop("'all' must be TRUE or FALSE.")
    }
    supported_datatypes <- c("fixtures", "games", "standings", "teams")
    if( all && !datatype %in% supported_datatypes ){
        stop(
            "'all' can be TRUE only when the datatype is one the followings: '",
            paste(supported_datatypes, collapse = "', '"), "'", call. = FALSE)
    }
    #
    # Get parameters to fetch with
    if( all ){
        params <- .get_all_params(sport, datatype, ...)
    } else{
        params <- .get_param_df(sport, datatype, ...)
    }
    # Get the location of db to return
    return_msg <- .get_storing_file(sport, ...)
    # If there is no params to fetch
    if( nrow(params) == 0 ){
        message(
            "Database is already updated. No queries to be made.")
        return(return_msg)
    }
    params <- t(params)
    params <- as.data.frame(params)
    param_names <- rownames(params)

    # Loop through parameters
    temp <- pblapply(params, function(args){
        # Create an argument list
        args <- as.list(args)
        names(args) <- param_names
        # Add query info so that it can be stored to db
        args[["add.query.info"]] <- TRUE
        # Add api key
        args[["api.key"]] <- api.key
        # Add hidden parameters
        args <- c(args, list(...))
        args <- args[ !duplicated(names(args)) ]
        # Search results
        res <- do.call(getApiFootball, args)
        # Get query info
        query_info <- res[["query_information"]]
        res <- res[ !names(res) %in% c("query_information") ]
        # If result was found, store it to database
        bool <- FALSE
        if( query_info[["n_results"]] > 0  ){
            # If datatype is teams, add season_id and league_id
            if( datatype %in% c("teams") ){
                res <- .add_seasonsTeams(res, args)
            }
            # If sport is afl and there is "games" table, the id column is named
            # as "game". Convert to id.
            if( sport == "afl" && "games" %in% names(res) ){
                  if( !"id" %in% colnames(res[["games"]]) && "game" %in% colnames(res[["games"]]) ){
                      colnames(res[["games"]])[ colnames(res[["games"]]) == "game" ] <- "id"
                }
            }

            # Get correct function for converting the data
            FUN <- .get_converter(datatype)
            # Convert for storing
            if( sport == "football" && datatype == "fixtures"){
                res <- FUN(res, main_tab = "fixtures", ...)
            } else{
                res <- FUN(res, ...)
            }

            # Get correct function for storing the data
            FUN <- .get_storing_method(datatype)
            # Store
            res <- FUN(res, sport, ...)
            bool <- TRUE
        }

        # Store query at the end to ensure the data was really fetched and
        # stored to database. Store the query only when there was no errors.
        if( length(query_info[["errors"]]) == 0 ){
            do.call(.store_query, c(args, query_info))
        }

        # Wait some time, because of the rate limit. If multiple queries is
        # done.
        if( ncol(params) > 5 ){
            .add_wait_time(...)
        }
        return(bool)
    })
    return(return_msg)
}

#' @rdname API-Football
#' @export
updateApiFootball <- function(
        update.games = TRUE, update.teams = FALSE, ...){
    #
    if( !(length(update.games) == 1 && is.logical(update.games)) ){
        stop("'update.games' must be TRUE or FALSE.")
    }
    #
    if( !(length(update.teams) == 1 && is.logical(update.teams)) ){
        stop("'update.teams' must be TRUE or FALSE.")
    }
    #
    # Loop over available sports
    available_sports <- c(
        "afl", "american-football", "baseball", "basketball", "football",
        "handball", "hockey", "rugby", "volleyball")
    for( sport in available_sports ){
        # If the day limit for queries is exceeded, error occurs. It is specific
        # for certain sport so we can just skip loading the current sport and
        # continue with others.
        temp <- tryCatch({
            message("\nFetching ", sport, " data...")
            # Get leagues once a week
            datatype <- "leagues"
            if( .update_leagues(sport, datatype, ...) ){
                message(datatype, ":")
                temp <- addApiFootball(
                    sport = sport, datatype = datatype,  ...)

            }

            # Search games
            datatype <- ifelse(sport == "football", "fixtures", "games")
            message(datatype, ":")
            temp <- addApiFootball(
                sport = sport, datatype = datatype,
                all = TRUE, update.current = update.games, ...)

            # Do not update teams information for certain seasons since it does
            # not change.
            datatype <- "teams"
            message(datatype, ":")
            temp <- addApiFootball(
                sport = sport, datatype = datatype,
                all = TRUE, update.current = update.teams, ...)

        }, error = function(e) {
            message(e)
            message(
                "\nSkipping loading rest of the ", sport, " data.")
        }, finally = {
            next
        })
    }
    return(TRUE)
}

################################ HELP FUNCTIONS ################################

.update_leagues <- function(
        sport, datatype, dir.name = file.path("data", "apifootball"),
        league.update.freq = 7, ...){
    #
    if( !(length(dir.name) == 1 && is.character(dir.name)) ){
        stop("'dir.name' must be string.")
    }
    #
    if( !(
        length(league.update.freq) == 1 && is.numeric(league.update.freq)) ){
        stop("'league.update.freq' must be single numeric value.")
    }
    #
    update_leagues <- TRUE
    # If there is db for queries
    file_name <- file.path(dir.name, paste0("query", ".db"))
    if( file.exists(file_name) ){
        # Connect
        db <- dbConnect(SQLite(), file_name)
        # If there is correct table
        query_tab <- sport
        query_tab <- gsub("-", "_", query_tab)
        if( query_tab %in% dbListTables(db) ){
            query_tab <- dbReadTable(db, query_tab)
            # Subset by taking only datatype values
            query_tab <- query_tab[ query_tab[["datatype"]] == datatype, ]
            # Get latest value
            latest_update <- max(query_tab[["datetime"]])
            # Check if the difference is under x days
            if( Sys.Date() - as.Date(latest_update)  <= league.update.freq ){
                update_leagues <- FALSE
            }
        }
        # Disconnect
        dbDisconnect(db)
    }
    return(update_leagues)
}

.get_api_key <- function(api.key, service, key.file = "api_keys.csv", ...){
    # Get api keys
    if( !file.exists(key.file) ){
        stop("If 'api.key' was not specified, you must provide 'key.file'. It ",
            "must be csv file that has 2 columns: 'service' and 'key'. ",
            "The first one specifies the API, in this case 'API-Football'",
            "The latter one specifies the API key. The current path from ",
            "where the file is searched is: ", key.file, call. = FALSE)
    }
    api_keys <- read.csv(key.file)
    if( all(c("service", "key") %in% colnames(api_keys)) ){
        stop("If 'api.key' was not specified, you must provide 'key.file'. It ",
            "must be csv file that has 2 columns: 'service' and 'key'. ",
            "The first one specifies the API, in this case 'API-Football'",
            "The latter one specifies the API key.", call. = FALSE)
    }
    # Get only for specific service
    api_keys <- api_keys[ api_keys[["service"]] == service, ]
    # If user wants not specific key
    if( is.null(api.key) ){
        api.key <- api_keys[1, "key"]
    } else{
        # If user specified the key number
        if( api.key > nrow(api_keys) ){
            stop(
            "'api.key' must specify the integer of api key from the file'",
            path, "'. The file has total of ", nrow(api_keys), " keys for ",
            service, ".",
            call. = FALSE)
        }
        api.key <- api_keys[api.key, "key"]
    }
    return(api.key)
}

.perform_request <- function(req){
    # Perform request
    res <- req_perform(req)
    # Get the json data
    res <- resp_body_json(res)
    return(res)
}

.add_query_info <- function(dat, res, add.query.info = FALSE, ...){
    #
    if( !(length(add.query.info) == 1 && (
        is.logical(add.query.info)) || add.query.info == "TRUE") ){
        stop("'add.query.info' must be TRUE or FALSE.")
    }
    #
    if( add.query.info ){
        query_info <- res[ c("results", "errors" )]
        names(query_info) <- c("n_results", "errors")
        dat[["query_information"]] <- query_info
    }
    return(dat)
}

.add_wait_time <- function(wait.time = TRUE, ...){
    #
    if( !(length(wait.time) == 1 && (
        is.logical(wait.time) || is.numeric(wait.time))) ){
        stop("'wait.time' must be TRUE or FALSE or numeric value.")
    }
    #
    # If wait.time is specified
    if( is.numeric(wait.time) || wait.time ){
        # If it is TRUE, it is random
        if( is.logical(wait.time) ){
            # wait.time <- runif(1, min = 2, max = 20)
            wait.time <- 6 # 10 queries in minute.
        }
        Sys.sleep(wait.time)
    }
}

###

.wait_and_try_again <- function(res, req, rate.wait = TRUE, ...){
    #
    if( !(length(rate.wait) == 1 && is.logical(rate.wait)) ){
        stop("'rate.wait' must be TRUE or FALSE.")
    }
    #
    times_tried <- 0
    total_tries <- 3
    wait_time <- 30
    # Try again
    while( TRUE ){
        times_tried <- times_tried + 1
        # Get error
        error <- res[["errors"]]
        # If there is no errors, break
        if( length(error) == 0 ){
            break
        }

        # Wait if specified and the error is rate limit error.
        if( rate.wait && "rateLimit" %in% names(error) &&
            times_tried <= total_tries ){
            message("Rate limit exceeded. Waiting ", wait_time, " seconds.")
            Sys.sleep(wait_time)
            # Try again fetching the data
            res <- .perform_request(req)
        } else{
            # If user do not want to wait and the error is not because of
            # rate limit, stop the run.
            stop(error[[1]], call. = FALSE)
        }
        # Add more wait time
        wait_time <- wait_time * 2
    }
    return(res)
}

###

.get_all_params <- function(
        sport, datatype, dir.name = file.path("data", "apifootball"),
        requery = FALSE, update.current = TRUE, update.freq = 0.9,
        current.days = 7, ...){
    #
    if( !(length(dir.name) == 1 && is.character(dir.name)) ){
        stop("'dir.name' must be string.")
    }
    #
    if( !(length(requery) == 1 && is.logical(requery)) ){
        stop("'requery' must be TRUE or FALSE.")
    }
    #
    if( !(length(update.current) == 1 && is.logical(update.current)) ){
        stop("'update.current' must be TRUE or FALSE.")
    }
    #
    if( !(length(update.freq) == 1 && is.numeric(update.freq) &&
          update.freq >= 0) ){
        stop("'update.freq' must be single positive numeric value.")
    }
    #
    if( !(length(current.days) == 1 && is.numeric(current.days) &&
          current.days >= 0) ){
        stop("'current.days' must be single positive numeric value.")
    }
    #
    # Get file path to database
    file_name <- paste0(sport, ".db")
    file_name <- file.path(dir.name, file_name)
    # Give error if file does not exist
    if( !file.exists(file_name) ){
        stop(
            "When 'all=TRUE', database must include information on available ",
            "seasons. \nThe following file was not ",
            "found. Consider specifying directory with 'dir.name'.\n",
            "The file: '", file_name, "'", call. = FALSE)
    }
    # Create a connection
    db <- dbConnect(SQLite(), file_name)

    # Check if there is seasons tables
    season_tab <- "seasons"
    if( !season_tab %in% dbListTables(db) ){
        # Disconnect
        dbDisconnect(db)
        stop(
            "The data base must include 'seasons' table if ",
            "'all=TRUE'.", call. = FALSE)
    }
    # Get the table
    tab <- params <- dbReadTable(db, season_tab)
    # If user do not want to requery, remove those paramaters if query db
    # is found.
    if( !requery ){
        query_name <- "query.db"
        query_name <- file.path(dir.name, query_name)
        if( file.exists(query_name) ){
            # Connect query
            db_query <- dbConnect(SQLite(), query_name)
            # If query table is found
            query_tab <- sport
            # Sql table name must not have "-"
            query_tab <- gsub("-", "_", query_tab)
            if( query_tab %in% dbListTables(db_query) ){
                # Get query table
                query_tab <- dbReadTable(db_query, query_tab)
                # Subset by datatype
                query_tab <- query_tab[ query_tab[["datatype"]] == datatype, ]
                # If the sport is nba and datatype is teams, only league must be
                # specified
                if( sport == "nba" && datatype == "teams" ){
                    query_id <- query_tab[["league"]]
                    query_id <- !params[["league_id"]] %in%  query_id
                } else{
                    query_id <- paste0(
                        query_tab[["league"]], "_", query_tab[["season"]])
                    query_id <- !params[["id"]] %in%  query_id
                }
                params <- params[ query_id, ]

            }
            # Disconnect query
            dbDisconnect(db_query)
        }
    }

    # If user wants to update current seasons
    if( update.current ){
        # Current and those that have ended x days ago, and will start in 5 days
        not_old <- as.Date(tab[["end"]]) - Sys.Date() >= -current.days
        is_starting <-  Sys.Date() - as.Date(tab[["start"]]) >= -5
        current <- tab[ not_old & is_starting, ]
        # Add to params
        params <- rbind(params, current)
        # There might be duplicated rows. Remove them.
        params <- params[ !duplicated(params), ]
    }

    # Remove queries that was made previously (they are already new info).
    # I user wants to requery, there are all parameters including previously
    # searched.
    query_name <- "query.db"
    query_name <- file.path(dir.name, query_name)
    if( file.exists(query_name) ){
        # Connect query
        db_query <- dbConnect(SQLite(), query_name)
        # If query table is found
        query_tab <- sport
        # Sql table name must not have "-"
        query_tab <- gsub("-", "_", query_tab)
        if( query_tab %in% dbListTables(db_query) ){
            # Get query table
            query_tab <- dbReadTable(db_query, query_tab)
            # Subset by datatype
            query_tab <- query_tab[ query_tab[["datatype"]] == datatype, ]
            # Get those queries that was done just 'update.freq' days ago
            ind <- Sys.time() - as.POSIXct(
                query_tab[["datetime"]]) <= update.freq
            query_tab <- query_tab[ind, ]
            query_tab <- query_tab[ , c("league", "season")]
            # Match between season ids. Remove matched from parameters.
            # If the sport is nba and datatype is teams, only league must be
            # specified
            if( sport == "nba" && datatype == "teams" ){
                query_id <- query_tab[["league"]]
                query_id <- !params[["league_id"]] %in%  query_id
            } else{
                query_id <- paste0(
                    query_tab[["league"]], "_", query_tab[["season"]])
                query_id <- !params[["id"]] %in%  query_id
            }
            params <- params[ query_id, ]

        }
        # Disconnect query
        dbDisconnect(db_query)
    }

    # If there is no queries to be made.get_all_params
    if( nrow(params) > 0 ){
        # Create parameter set
        params <- params[, c("league_id", "season")]
        colnames(params) <- c("league", "season")
        params[["sport"]] <- sport
        params[["datatype"]] <- datatype
    }

    # If the sport is nba and datatype is teams, only league must be specified
    if( sport == "nba" && datatype == "teams" ){
        params[["season"]] <- NULL
        params <- params[ !duplicated(params), ]
    }

    # Disconnect
    dbDisconnect(db)

    return(params)
}

###

.store_query <- function(
        sport, datatype, store.query = TRUE, n_results = NULL,
        coach = NULL, fixture = NULL, game = NULL, id = NULL, league = NULL,
        player = NULL, season = NULL, team = NULL,
        dir.name = file.path("data", "apifootball"), ...){
    #
    if( !(length(store.query) == 1 && (
        is.logical(store.query) || store.query == "TRUE")) ){
        stop("'store.query' must be TRUE or FALSE.", call. = FALSE)
    }
    #
    # If query is stored
    if( store.query ){
        # Get params
        params <- list(
            datatype = datatype, coach = coach, fixture = fixture,
            game = game, id = id, league = league, player = player,
            season = season, team = team)
        # Get id
        query_id <- paste(unlist(params), collapse = "_")
        # Crete one row of df
        params <- .simple_rapply(params, function(x) if(is.null(x)) NA else x)
        params <- as.data.frame(params)
        params[["n_results"]] <- n_results
        params[["id"]] <- query_id
        params[["datetime"]] <- Sys.time()

        # Open connection to database
        if( !dir.exists(dir.name) ){
            dir.create(dir.name, recursive = TRUE)
        }
        file_name <- paste0("query", ".db")
        file_name <- file.path(dir.name, file_name)
        db <- dbConnect(SQLite(), file_name)

        # Info for sql table
        sql_table <- sport
        # Info for initializing table
        col_types <- data.frame(
            orig_cols = c(
                "id", "datatype", "coach", "fixture", "game", "league",
                "player", "season", "team", "n_results", "datetime"),
            name = c(
                "id", "datatype", "coach", "fixture", "game", "league",
                "player", "season", "team", "n_results", "datetime"),
            type = c(
                "TEXT PRIMARY KEY", "TEXT", "TEXT", "TEXT", "TEXT", "TEXT",
                "TEXT", "TEXT", "TEXT", "INTEGER", "DATETIME"))
        # Update
        temp <- .update_table(
            db, params, sql_table, col_types)

        # Disconnect
        dbDisconnect(db)
    }
    return(NULL)
}

###

.get_storing_file <- function(
        sport, dir.name = file.path("data", "apifootball"), ...){
    #
    if( !(length(dir.name) == 1 && is.character(dir.name)) ){
        stop("'dir.name' must be string.")
    }
    #
    file_name <- paste0(sport, ".db")
    res <- file.path(dir.name, file_name)
    return(res)
}

###

.add_seasonsTeams <- function(res, args){
    # Get team_id
    tab_name <- c("teams", "team", "table")
    tab_name <- tab_name[ tab_name %in% names(res) ]
    tab <- res[[tab_name]][, "id", drop = FALSE]
    colnames(tab) <- "team_id"
    # Add season_id
    tab[["season_id"]] <- paste0(args[["league"]], "_", args[["season"]])
    # Add it to results
    res[["seasonsTeams"]] <- tab
    return(res)
}

###

.get_storing_method <- function(datatype){
    FUN <- switch(
        datatype,
        fixtures = .update_games_database,
        games = .update_games_database,
        leagues = .update_leagues_database,
        odds = .update_odds_database,
        standings = .update_standings_database,
        teams = .update_teams_database,
    )
    return(FUN)
}

###

.get_converter <- function(datatype){
    FUN <- switch(
        datatype,
        fixtures = .convert_games,
        games = .convert_games,
        leagues = .convert_leagues,
        odds = .convert_odds,
        standings = .convert_standings,
        teams = .convert_teams
    )
    return(FUN)
}

###

.get_param_df <- function(
        sport, datatype,
        coach = NULL, fixture = NULL, game = NULL, id = NULL, league = NULL,
        player = NULL, race = NULL, season = NULL, team = NULL,
        ...){
    params <- list(
        coach = coach, fixture = fixture, game = game, id = id, league = league,
        player = player, race = race, season = season, team = team)
    params <- params[ lengths(params) > 0 ]
    # If there were no paramaters
    if( length(params) == 0 ){
        params <- data.frame(sport, datatype)
    } else{
        # Check if number of parameters do not match
        if( !all(lengths(params) == max(lengths(params))) ){
            stop(
                "The number of values must match between paramaters.",
                call. = FALSE)
        }
        # Combine parameters to single df
        params <- do.call(cbind, params)
        params <- as.data.frame(params)

        # Add sport and datatype
        params[["sport"]] <- sport
        params[["datatype"]] <- datatype
    }
    return(params)
}

###

.get_parser <- function(sport, datatype){
    if( sport %in% c(
        "baseball", "basketball", "handball", "rugby", "volleyball") ){
        FUN <- switch(
            datatype,
            "games" = .parse_games_v1,
            "leagues" = .parse_leagues_v1,
            "odds" = .parse_odds_v1,
            "standings" = .parse_standings_v1,
            "teams" = .parse_teams_v1
        )
    } else if( sport %in% c("american-football") ){
        FUN <- switch(
            datatype,
            "games" = .parse_games_v1,
            "injuries" = .parse_injuries_v1,
            "leagues" = .parse_leagues_american_football,
            "odds" = .parse_odds_v1,
            "players" = .parse_players_v1,
            "standings" = .parse_standings_american_football,
            "teams" = .parse_teams_v1
        )
    } else if( sport == "football" ){
        # Parse results
        FUN <- switch(
            datatype,
            "coachs" = .parse_coachs_v3,
            "fixtures" = .parse_fixtures_v3,
            "injuries" = .parse_injuries_v3,
            "leagues" = .parse_leagues_v3,
            "odds" = .parse_odds_v3,
            "players" = .parse_players_v3,
            "players/squads" = .parse_squads_v3,
            "predictions" = .parse_predictions_v3,
            "sidelined" = .parse_trophies_or_sidelined_v3,
            "standings" = .parse_standings_v3,
            "teams" = .parse_teams_v3,
            "transfers" = .parse_transfers_v3,
            "trophies" = .parse_trophies_or_sidelined_v3
        )
    } else if( sport == "formula-1" ){
        FUN <- switch(
            datatype,
            "circuits" = .parse_games_v1,
            "competitions" = .parse_competitions,
            "drivers" = .parse_drivers,
            "pitstops" = .parse_games_v1,
            "races" = .parse_games_v1,
            "rankings/drivers" = .parse_games_v1,
            "rankings/fastestlaps" = .parse_games_v1,
            "rankings/races" = .parse_games_v1,
            "rankings/startinggrid" = .parse_games_v1,
            "rankings/teams" = .parse_games_v1,
            "seasons" = .parse_seasons_formula,
            "teams" = .parse_competitions
        )
    } else if( sport == "hockey" ){
        FUN <- switch(
            datatype,
            "games" = .parse_games_hockey,
            "leagues" = .parse_leagues_v1,
            "odds" = .parse_odds_v1,
            "standings" = .parse_standings_v1,
            "teams" = .parse_teams_v1
        )
    } else if( sport == "nba" ){
        FUN <- switch(
            datatype,
            "games" = .parse_games_v2,
            "leagues" = .parse_leagues_v2,
            "players" = .parse_players_v2,
            "seasons" = .parse_leagues_v2,
            "standings" = .parse_standings_v2,
            "teams" = .parse_teams_v2
        )
    } else if( sport == "afl" ){
      FUN <- switch(
          datatype,
          "games" = .parse_games_v1,
          "leagues" = .parse_leagues_afl,
          "standings" = .parse_standings_afl,
          "teams" = .parse_teams_afl
      )
    }

    return(FUN)
}

.get_host <- function(sport, host = "api-sports.io", ...){
    if( sport == "football"){
        host <- paste0("v3.", sport, ".", host)
    } else if( sport == "nba") {
        host <- paste0("v2.", sport, ".", host)
    } else {
        host <- paste0("v1.", sport, ".", host)
    }
    return(host)
}

.get_url <- function(
        sport, datatype, host,
        coach = NULL, fixture = NULL, game = NULL, id = NULL, league = NULL,
        player = NULL, race = NULL, season = NULL, team = NULL,
        ...){
    # Construct url address
    url <- paste0("https://", host, "/", datatype)
    # Construct query options
    query_opt <- c()
    if( !is.null(coach) ){
        opt <- paste0("coach=", coach)
        query_opt <- c(query_opt, opt)
    }
    if( !is.null(fixture) ){
        opt <- paste0("fixture=", fixture)
        query_opt <- c(query_opt, opt)
    }
    if( !is.null(game) ){
        opt <- paste0("game=", game)
        query_opt <- c(query_opt, opt)
    }
    if( !is.null(id) ){
        opt <- paste0("id=", id)
        query_opt <- c(query_opt, opt)
    }
    if( !is.null(league) ){
        opt <- paste0("league=", league)
        query_opt <- c(query_opt, opt)
    }
    if( !is.null(player) ){
        opt <- paste0("player=", player)
        query_opt <- c(query_opt, opt)
    }
    if( !is.null(race) ){
        opt <- paste0("race=", race)
        query_opt <- c(query_opt, opt)
    }
    if( !is.null(season) ){
        opt <- paste0("season=", season)
        query_opt <- c(query_opt, opt)
    }
    if( !is.null(team) ){
        opt <- paste0("team=", team)
        query_opt <- c(query_opt, opt)
    }
    # Add query options
    url <- paste0(url, "?", paste(query_opt, collapse = "&"))
    return(url)
}

.check_datatype_v1 <- function(datatype){
    available_datatypes <- c(
        "games", "leagues", "odds", "standings", "teams"
    )
    if( !( length(datatype) == 1 && datatype %in% available_datatypes) ){
        stop(
            "'datatype' must be one of the following options: '",
            paste(available_datatypes, collapse = "', '"), "'", call. = FALSE)
    }
}

.check_datatype_afl <- function(datatype){
  available_datatypes <- c(
    "games", "leagues", "teams"
  )
  if( !( length(datatype) == 1 && datatype %in% available_datatypes) ){
    stop(
      "'datatype' must be one of the following options: '",
      paste(available_datatypes, collapse = "', '"), "'", call. = FALSE)
  }
}

.check_datatype_american_football <- function(datatype){
    available_datatypes <- c(
        "games", "injuries", "leagues", "odds", "players", "standings", "teams"
    )
    if( !( length(datatype) == 1 && datatype %in% available_datatypes) ){
        stop(
            "'datatype' must be one of the following options: '",
            paste(available_datatypes, collapse = "', '"), "'", call. = FALSE)
    }
}

.check_datatype_formula <- function(datatype){
    available_datatypes <- c(
        "circuits", "competitions", "drivers", "pitstops", "races",
        "rankings/drivers", "rankings/fastestlaps", "rankings/races",
        "rankings/startinggrid", "rankings/teams", "seasons", "teams"
    )
    if( !( length(datatype) == 1 && datatype %in% available_datatypes) ){
        stop(
            "'datatype' must be one of the following options: '",
            paste(available_datatypes, collapse = "', '"), "'", call. = FALSE)
    }
}

.check_datatype_v2 <- function(datatype){
    available_datatypes <- c(
        "games", "leagues", "players", "seasons", "standings", "teams"
    )
    if( !( length(datatype) == 1 && datatype %in% available_datatypes) ){
        stop(
            "'datatype' must be one of the following options: '",
            paste(available_datatypes, collapse = "', '"), "'", call. = FALSE)
    }
}

.check_datatype_v3 <- function(datatype){
    available_datatypes <- c(
        "coachs", "fixtures", "injuries", "leagues", "odds", "players",
        "players/squads", "predictions", "sidelined", "standings", "teams",
        "transfers", "trophies"
    )
    if( !( length(datatype) == 1 && datatype %in% available_datatypes) ){
        stop(
            "'datatype' must be one of the following options: '",
            paste(available_datatypes, collapse = "', '"), "'", call. = FALSE)
    }
}

.check_params <- function(sport, datatype, ...){
    if( sport %in% c(
        "afl", "baseball", "basketball", "handball", "rugby", "volleyball") ){
        FUN <- switch(
            datatype,
            "games" = .require_league_and_season,
            "leagues" = .require_nothing,
            "odds" = .require_league_and_season,
            "standings" = .require_league_and_season,
            "teams" = .require_league_and_season
        )
    } else if( sport %in% c("american-football") ){
        FUN <- switch(
            datatype,
            "games" = .require_league_and_season,
            "injuries" = .require_team,
            "leagues" = .require_nothing,
            "odds" = .require_game,
            "players" = .require_team_and_season,
            "standings" = .require_league_and_season,
            "teams" = .require_league_and_season
        )
    } else if( sport == "football" ){
        # Parse results
        FUN <- switch(
            datatype,
            "coachs" = .require_team,
            "fixtures" = .require_league_and_season,
            "injuries" = .require_league_and_season,
            "leagues" = .require_nothing,
            "odds" = .require_league_and_season,
            "players" = .require_league_and_season,
            "players/squads" = .require_team,
            "predictions" = .require_fixture,
            "sidelined" = .require_player_or_coach,
            "standings" = .require_league_and_season,
            "teams" = .require_league_and_season,
            "transfers" = .require_team,
            "trophies" = .require_player_or_coach
        )
    } else if( sport == "formula-1" ){
        FUN <- switch(
            datatype,
            "circuits" = .require_nothing,
            "competitions" = .require_nothing,
            "drivers" = .require_id,
            "pitstops" = .require_race,
            "races" = .require_season,
            "rankings/drivers" = .require_season,
            "rankings/fastestlaps" = .require_race,
            "rankings/races" = .require_race,
            "rankings/startinggrid" = .require_race,
            "rankings/teams" = .require_season,
            "seasons" = .require_nothing,
            "teams" = .require_nothing
        )
    } else if( sport == "hockey" ){
        FUN <- switch(
            datatype,
            "games" = .require_league_and_season,
            "leagues" = .require_nothing,
            "odds" = .require_league_and_season,
            "standings" = .require_league_and_season,
            "teams" = .require_league_and_season
        )
    } else if( sport == "nba" ){
        FUN <- switch(
            datatype,
            "games" = .require_league_and_season,
            "leagues" = .require_nothing,
            "players" = .require_team_and_season,
            "seasons" = .require_nothing,
            "standings" = .require_league_and_season,
            "teams" = .require_league
        )
    }

    FUN(sport, datatype, ...)
}

.require_league_and_season <- function(
        sport, datatype,
        coach = NULL, fixture = NULL, game = NULL, id = NULL, league = NULL,
        player = NULL, race = NULL, season = NULL, team = NULL,
        ...){
    params <- c(coach, fixture, game, id, league, player, race, season, team)
    bool1 <- length(params) == 2
    bool2 <- length(c(season, league)) == 2
    if( !(bool1 && bool2)){
        stop("Specify 'league' and 'season'.", call. = FALSE)
    }
}

.require_team <- function(
        sport, datatype,
        coach = NULL, fixture = NULL, game = NULL, id = NULL, league = NULL,
        player = NULL, race = NULL, season = NULL, team = NULL,
        ...){
    params <- c(coach, fixture, game, id, league, player, race, season, team)
    bool1 <- length(params) == 1
    bool2 <- length(c(team)) == 1
    if( !(bool1 && bool2)){
        stop("Specify 'team'.", call. = FALSE)
    }
}

.require_id <- function(
        sport, datatype,
        coach = NULL, fixture = NULL, game = NULL, id = NULL, league = NULL,
        player = NULL, race = NULL, season = NULL, team = NULL,
        ...){
    params <- c(coach, fixture, game, id, league, player, race, season, team)
    bool1 <- length(params) == 1
    bool2 <- length(c(id)) == 1
    if( !(bool1 && bool2)){
        stop("Specify 'id'.", call. = FALSE)
    }
}

.require_race <- function(
        sport, datatype,
        coach = NULL, fixture = NULL, game = NULL, id = NULL, league = NULL,
        player = NULL, race = NULL, season = NULL, team = NULL,
        ...){
    params <- c(coach, fixture, game, id, league, player, race, season, team)
    bool1 <- length(params) == 1
    bool2 <- length(c(race)) == 1
    if( !(bool1 && bool2)){
        stop("Specify 'race'.", call. = FALSE)
    }
}

.require_game <- function(
        sport, datatype,
        coach = NULL, fixture = NULL, game = NULL, id = NULL, league = NULL,
        player = NULL, race = NULL, season = NULL, team = NULL,
        ...){
    params <- c(coach, fixture, game, id, league, player, race, season, team)
    bool1 <- length(params) == 1
    bool2 <- length(c(game)) == 1
    if( !(bool1 && bool2)){
        stop("Specify 'game'.", call. = FALSE)
    }
}

.require_season <- function(
        sport, datatype,
        coach = NULL, fixture = NULL, game = NULL, id = NULL, league = NULL,
        player = NULL, race = NULL, season = NULL, team = NULL,
        ...){
    params <- c(coach, fixture, game, id, league, player, race, season, team)
    bool1 <- length(params) == 1
    bool2 <- length(c(season)) == 1
    if( !(bool1 && bool2)){
        stop("Specify 'season'.", call. = FALSE)
    }
}

.require_fixture <- function(
        sport, datatype,
        coach = NULL, fixture = NULL, game = NULL, id = NULL, league = NULL,
        player = NULL, race = NULL, season = NULL, team = NULL,
        ...){
    params <- c(coach, fixture, game, id, league, player, race, season, team)
    bool1 <- length(params) == 1
    bool2 <- length(c(fixture)) == 1
    if( !(bool1 && bool2)){
        stop("Specify 'fixture'.", call. = FALSE)
    }
}

.require_league <- function(
        sport, datatype,
        coach = NULL, fixture = NULL, game = NULL, id = NULL, league = NULL,
        player = NULL, race = NULL, season = NULL, team = NULL,
        ...){
    params <- c(coach, fixture, game, id, league, player, race, season, team)
    bool1 <- length(params) == 1
    bool2 <- length(c(league)) == 1
    if( !(bool1 && bool2)){
        stop("Specify 'league'.", call. = FALSE)
    }
}

.require_player_or_coach <- function(
        sport, datatype,
        coach = NULL, fixture = NULL, game = NULL, id = NULL, league = NULL,
        player = NULL, race = NULL, season = NULL, team = NULL,
        ...){
    params <- c(coach, fixture, game, id, league, player, race, season, team)
    bool1 <- length(params) == 1
    bool2 <- length(c(player, coach)) == 1
    if( !(bool1 && bool2)){
        stop("Specify 'player' or 'coach'.", call. = FALSE)
    }
}

.require_nothing <- function(
        sport, datatype,
        coach = NULL, fixture = NULL, game = NULL, id = NULL, league = NULL,
        player = NULL, race = NULL, season = NULL, team = NULL,
        ...){
    params <- c(coach, fixture, game, id, league, player, race, season, team)
    bool1 <- length(params) == 0
    if( !bool1 ){
        stop("Specify no parameters.", call. = FALSE)
    }
}

.require_team_and_season <- function(
        sport, datatype,
        coach = NULL, fixture = NULL, game = NULL, id = NULL, league = NULL,
        player = NULL, race = NULL, season = NULL, team = NULL,
        ...){
    params <- c(coach, fixture, game, id, league, player, race, season, team)
    bool1 <- length(params) == 2
    bool2 <- length(c(team, season)) == 2
    if( !(bool1 && bool2)){
        stop("Specify 'team' and 'season'.", call. = FALSE)
    }
}
