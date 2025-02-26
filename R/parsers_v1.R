
.parse_standings_v1 <- function(res, datatype = "standings", ...){
    # Create a matrix
    # There might be multiple standing tables, e.g., regular league and
    # playoffs. Combine those tables into one. Without looping, there would
    # be a warning In (function (..., deparse.level = 1)  :
    # number of columns of result is not a multiple of vector length (arg 2)
    if( length(res) > 1 ){
        # Combine data so that each column is one position in standings table
        res <- lapply(res, function(x) do.call(rbind, list(x)))
        res <- do.call(cbind, res)
    } else{
        res <- do.call(rbind, res)
    }

    # Initialize a result list
    tables <- list()

    # Parse so that each element of list is a team and element includes list
    # of information about certain team/position.
    res <- apply(res, 2, function(x){
        x <- do.call(rbind, x)
        x <- .parse_games_v1(x, datatype)
        return(x)
    })
    # Get the datatypes
    datatypes <- names(res[[1]])

    # Loop over datatypes and combine the data
    for( d in res ){
        # Lop over data types
        for( dtype in datatypes ){
            # Combine data
            tables[[dtype]] <- rbind(tables[[dtype]], d[[dtype]])
        }
    }

    # Standings table does not contain IDs --> add them
    tables[[datatype]][["id"]] <- seq_len( nrow(tables[[datatype]]) )

    return(tables)
}

###

.parse_standings_afl <- function(res, datatype = "standings", ...){
  # Create a matrix
  if( length(res) > 1 ){
    # Combine data so that each column is one position in standings table
    res <- lapply(res, function(x) do.call(rbind, list(x)))
    res <- do.call(cbind, res)
  } else{
    res <- do.call(rbind, res)
  }
  res <- do.call(rbind, res)

  # Initialize a result list
  tables <- list()

  # Parse so that each element of list is a team and element includes list
  # of information about certain team/position.
  res <- apply(res, 2, function(x){
    x <- do.call(rbind, x)
    return(x)
  })
  # Add colnames if missing
  res <- lapply(names(res), function(x){
      tab <- res[[x]]
      if( is.null(colnames(tab)) && ncol(tab) == 1 ){
          colnames(tab) <- x
      }
      return(tab)
  })
  # Combine tables
  res <- do.call(cbind, res)
  # Add to list
  tables[[datatype]] <- res

  return(tables)
}

###

.parse_games_v1 <- function(res, datatype = "games", ...){
    # Create a matrix if it is not
    if( !is(res, "matrix") ){
        res <- do.call(rbind, res)
    }

    # Initialize a result list
    tables <- list()

    # First get table that do not have nested information
    ind <- apply(res, 2, function(x) all(lengths(x) <= 1))

    # If there are flattened values, i.e., no list with multiple values
    if( any(ind) ){
        # Get flat values from the data
        temp  <- res[ , ind, drop = FALSE]
        # Columns include lists. Convert them to vectors.
        temp <- .bind_columns_to_dataframe(temp)
        # Add to list
        tables[[datatype]] <- temp
    }

    # Now work with nested information.
    ind <- !ind
    ind <- names(ind)[ind]

    # ind <- ind[ !ind %in% c("country", "league")]

    # Loop over nested information. Create data.frames from them.
    temp <- lapply(ind, function(x){
        result <- list()
        # Get one column as matrix
        col <- do.call(rbind, res[ , x, drop = FALSE])

        # If there was non-nested information, convert it to data.frame
        jind <- colSums( lengths(col) <= 1 ) == nrow(col)
        if( any(jind) ){
            # Get those columnt that are already flat
            flat <- col[ , jind, drop = FALSE]
            flat <- .bind_columns_to_dataframe(flat)
            # Add to list
            result[[x]] <- flat
        }

        # If there is still nested information, unnest it one more time.
        if( any(!jind) ){
            # Get columns of nested information
            nams <- colnames(col[ , !jind, drop = FALSE])
            indices <- which( !jind )
            # Loop over those columns
            nested <- lapply(indices, function(i){
                # Get column
                col_sub <- col[, i, drop = FALSE]
                # Convert column to matrix
                col_sub <- do.call(rbind, col_sub)
                # Flatten the data. There might still be nested information.
                col_sub <- .bind_columns_to_dataframe(col_sub)
                # Polish those column names that have nested info. Like:
                # x.innings.innings.x --> x.innings.x
                nams <- strsplit(colnames(col_sub), "\\.")
                for( nam in nams ){
                    ind <- nam[1:(length(nam)-1)] == nam[2:length(nam)]
                    if( any(ind) && length(nam) > 1 ){
                        nam <- nam[1:(length(nam)-1)][ ind ]
                        colnames(col_sub) <- gsub(
                            paste0(nam, "\\.", nam), nam, colnames(col_sub))
                    }
                }
                return(col_sub)
            })
            names(nested) <- nams
            # Add to list
            result <- c(result, nested)
        }
        # If there is only one element, return the element
        if( length(result) == 1){
            result <- result[[1]]
        }
        return(result)
    })
    names(temp) <- ind
    # Unnest the list so that there is only one level
    temp <- .unnest_list_of_lists(temp)
    # Add to tables
    tables <- c(tables, temp)
    return(tables)
}

###

.parse_teams_v1 <- function(res, datatype = "teams", ...){
    # Create a matrix
    res <- do.call(rbind, res)

    # Initialize a result list
    tables <- list()

    # First get table that do not have nested information
    ind <- apply(res, 2, function(x) all(lengths(x) <= 1))

    # If there are flattened values, i.e., no list with multiple values
    if( any(ind) ){
        # Get flat values from the data
        temp  <- res[ , ind]
        # Columns include lists. Convert them to vectors.
        temp <- .bind_columns_to_dataframe(temp)
        # Add to list
        tables[[datatype]] <- temp
    }

    # Countries
    temp <- res[ , c("country")]

    temp <- rbindlist(temp, fill=TRUE)
    temp <- as.data.frame(temp)
    tables[[datatype]][["country_id"]] <- temp[["id"]]
    tables[["country"]] <- temp

    # If there is colors table
    tab_name <- "colors"
    if( any(colnames(res) %in% tab_name) ){
        # Get the data and add names
        temp <- res[ , tab_name]
        names(temp) <- tables[[datatype]][["name"]]
        # Collapse data into one vector. ( if team has multiple colors, they
        # are collapsed into one value)
        temp <- lapply(temp, function(x){
            if( length(x) == 0){
                x <- NA
            } else{
                x <- paste(unlist(x), collapse = ", ")
            }
            return(x)
        })
        # Add the data to original table
        tables[[datatype]][[tab_name]] <- unlist(temp)
    }

    return(tables)
}

###

.parse_teams_afl <- function(res, datatype = "teams", ...){
  # Create a matrix
  res <- do.call(rbind, res)

  # Initialize a result list
  tables <- list()

  # First get table that do not have nested information
  ind <- apply(res, 2, function(x) all(lengths(x) <= 1))

  # If there are flattened values, i.e., no list with multiple values
  if( any(ind) ){
      # Get flat values from the data
      temp  <- res[ , ind]
      # Columns include lists. Convert them to vectors.
      temp <- .bind_columns_to_dataframe(temp)
      # Add to list
      tables[[datatype]] <- temp
  }

  # If there is colors table
  tab_name <- "colors"
  if( any(colnames(res) %in% tab_name) ){
    # Get the data and add names
    temp <- res[ , tab_name]
    names(temp) <- tables[[datatype]][["name"]]
    # Collapse data into one vector. ( if team has multiple colors, they
    # are collapsed into one value)
    temp <- lapply(temp, function(x){
      if( length(x) == 0){
        x <- NA
      } else{
        x <- paste(unlist(x), collapse = ", ")
      }
      return(x)
    })
    # Add the data to original table
    tables[[datatype]][[tab_name]] <- unlist(temp)
  }

  return(tables)
}

###

.parse_leagues_v1 <- function(res, datatype = "leagues", ...){
    # Create a matrix
    res <- do.call(rbind, res)

    # Initialize a result list
    tables <- list()

    # First get table that do not have nested information
    ind <- apply(res, 2, function(x) all(lengths(x) <= 1))

    # If there are flattened values, i.e., no list with multiple values
    if( any(ind) ){
        # Get flat values from the data
        temp  <- res[ , ind]
        # Columns include lists. Convert them to vectors.
        temp <- .bind_columns_to_dataframe(temp)
        # Add to list
        tables[[datatype]] <- temp
    }

    # Countries
    temp <- res[ , c("country")]
    # Parse to data.frame
    temp <- rbindlist(temp, fill = TRUE)
    temp <- as.data.frame(temp)
    # Add country id to league table
    tables[[datatype]][["country_id"]] <- temp[["id"]]
    # # Drop duplicated countries
    # temp <- temp[ !duplicated(temp), ]
    # Add to list
    tables[["country"]] <- temp

    # Seasons
    temp <- res[ , c("seasons")]
    # Parse to data.frame
    temp <- lapply(temp, function(x) rbindlist(x, fill = TRUE))
    rep_times <- lapply(temp, function(x) nrow(x))
    temp <- do.call(rbind, temp)
    temp <- as.data.frame(temp)

    # Coverage might have nested list. We do not support them currently. -->
    # Remove those columns
    if( "coverage" %in% colnames(temp) && length(unlist(temp[["coverage"]])) != nrow(temp) ){
        temp[["coverage"]] <- NULL
        warning("'coverage' includes a nested list that is not currently supported. The column is removed.", call. = FALSE)
    }

    # Create a season id; league + season
    temp[["league_id"]] <- rep(tables[[datatype]][["id"]], rep_times)
    temp[["id"]] <- paste0(temp[["league_id"]], temp[["season"]])

    # Add to list
    tables[["seasons"]] <- temp

    return(tables)
}

###

.parse_leagues_afl <- function(res, datatype = "leagues", ...){
  # Create a matrix
  res <- do.call(rbind, res)

  # Initialize a result list
  tables <- list()

  # First get table that do not have nested information
  ind <- apply(res, 2, function(x) all(lengths(x) <= 1))

  # If there are flattened values, i.e., no list with multiple values
  if( any(ind) ){
    # Get flat values from the data
    temp  <- res[ , ind]
    # Columns include lists. Convert them to vectors.
    temp <- .bind_columns_to_dataframe(temp)
    # Add to list
    tables[[datatype]] <- temp
  }

  # League id column is the column named id
  tables[[datatype]][["league_id"]] <- tables[[datatype]][["id"]]

  # Add season id
  tables[[datatype]][["id"]] <- paste0(
      tables[[datatype]][["league_id"]], tables[[datatype]][["season"]])

  # The table is called seasons
  names(tables) <- "seasons"

  return(tables)
}

###

.add_season_id <- function(res, datatype, league, season){
    # Choose correct column that contains ids for the datatype
    id_col <- "id"
    # Get name of column that includes season
    season_id_col <- "season_id"
    # Create new id_column
    new_id_col <- paste0( substr(datatype, 1, nchar(datatype)-1), "_id")

    # Add data-specific id to new column
    res[[datatype]][[new_id_col]] <- res[[datatype]][[id_col]]
    # Add season info to id
    res[[datatype]][["id"]] <- paste0(
        res[[datatype]][[season_id_col]], "_", res[[datatype]][[new_id_col]])

    return(res)
}

###

.parse_odds_v1 <- function(res, ...){
    tables <- list()
    # Convert to df
    res <- do.call(rbind, res)
    res <- as.data.frame(res)

    # Get flat info
    ind <- lapply(res, function(x) all(lengths(x) <= 1))
    ind <- unlist(ind)
    if( any(ind) ){
        temp <- res[ , ind, drop = FALSE]
        # Convert flat columns to df
        temp <- lapply(temp, function(x) as.data.frame(do.call(rbind, x)) )
        tables <- c(tables, temp)
        # Get rest of the data
        res <- res[, !ind, drop = FALSE]
    }

    # If there is update table, adjust it's colnames
    if( any( names(tables) %in% "update" ) ){
        colnames(tables[["update"]]) <- "datetime"
    }

    # If there is still games table, it includes nested info on games. For
    # example, baseball data includes nested info on games.
    if( any(colnames(res) %in% "game") ){
        temp <- res[["game"]]
        temp <- .parse_games_v1(temp)
        # Add to list
        tables <- c(tables, temp)
        # Remove from results
        res <- res[, !colnames(res) %in% "game", drop = FALSE]
    }

    # Get all the other data than odds data
    nams <- !colnames(res) %in% "bookmakers"
    temp <- res[, nams, drop = FALSE]

    # Create data.frames from the data
    temp <- lapply(temp, function(x){
        x <- do.call(rbind, x)
        x <- .bind_columns_to_dataframe(x)
        return(x)
    })
    # Add to data
    tables <- c(tables, temp)

    # Get the bookmakers data and parse it
    res <- res[["bookmakers"]]
    # Loop over games
    temp <- .parse_bets(res)
    # The value is now list of data.frames. Each element is a single game.
    # Add game ids
    games_tab <- ifelse("game" %in% names(tables), "game", "games")
    for( i in seq_len(length(temp)) ){
        temp[[i]][ , "game_id" ] <- tables[[games_tab]][i, "id"]
    }
    # Combine odds data into single data.frame
    temp <- do.call(rbind, temp)
    temp <- as.data.frame(temp)
    rownames(temp) <- NULL
    # Add it
    tables[["odds"]] <- temp

    # At this point, we can have league and country tables duplicated. One
    # comes directly and other comes from games data. Remove duplication.
    tables <- tables[ !duplicated(names(tables)) ]

    return(tables)
}

###

.parse_players_v1 <- function(res, ...){
    # To df
    res <- do.call(rbind, res)
    res <- .bind_columns_to_dataframe(res)
    return(res)
}

###

.parse_injuries_v1 <- function(res, datatype = "injuries", ...){
    tables <- list()
    # To df
    res <- do.call(rbind, res)
    res <- as.data.frame(res)
    # Get flat columns
    ind <- lapply(res, function(x) all(lengths(x) <= 1) )
    ind <- unlist(ind)

    # Create df from flat cols
    if( any(ind) ){
        temp <- res[ , ind, drop = FALSE]
        temp <- do.call(rbind, temp)
        temp <- t(temp)
        temp <- as.data.frame(temp)
        # Add to list
        tables[[datatype]] <- temp
        # Remove flats
        res <- res[ , !ind, drop = FALSE]
    }

    # For nested data columns, create dfs
    temp <- lapply(res, function(x){
        x <- do.call(rbind, x)
        x <- .bind_columns_to_dataframe(x)
        return(x)
    })
    # Add to lists
    tables <- c(tables, temp)
    return(tables)
}
