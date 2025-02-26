
.parse_leagues_v3 <- function(res, ...){
    # Create a data.frame
    res <- do.call(rbind, res)
    res <- as.data.frame(res)
    
    # Get league and country tables as they are now 1-level nested table.
    names <- c("league", "country")
    # Create data.frames from them
    tables <- lapply(
        res[, names], function(x) as.data.frame(do.call(rbind, x)) )
    # Make sure that they are unenested
    tables <- lapply(tables, function(tab) as.data.frame(lapply(tab, unlist) ))
    # Taken coverage table which is highly nested
    names <- colnames(res)[ !colnames(res) %in% names ]
    res <- res[[names]]
    
    # Loop over columns of coverage. These columns represent individual league.
    temp <- .parse_coverage(res, tables)
    
    # Add to list
    tables <- c(tables, temp)
    
    return(tables)
}

###

.parse_standings_v3 <- function(res, ...){
    # Initialize result list
    tables <- list()
    # Create a matrix
    res <- do.call(rbind, res)
    # The result is a matrix with one column, take the column and unnest it.
    names <- colnames(res)
    res <- rbindlist(res[, names], fill = TRUE)
    res <- as.data.frame(res)
    # Get those column that are flat.
    ind <- lapply(res, function(x) all(lengths(x) <= 1))
    ind <- unlist(ind)
    # The data includes leagues information, add it to result list.
    tab <- res[, ind, drop = FALSE]
    tables[[names]] <- tab
    # Take all the other columns including standings info. Create df from them.
    res <- res[ , !ind, drop = FALSE]
    res <- do.call(rbind, res)
    res <- as.data.frame(res)
    
    # Loop over each column. Each column includes single position in league.
    temp <- lapply(res, function(single_position){
        # The single positon is nested including only one element. Take the
        # element.
        col_name <- names(single_position)
        single_position <- single_position[[col_name]]
        # Loop through standing information for single position.
        temp <- lapply(single_position, function(pos){
            # Unlist it to vector
            pos <- unlist(pos)
            # Different nested information/levels are separated with ".". Get
            # levels of nested info.
            current_levels <- strsplit(names(pos), "\\.")
            # For each information, each level is separated.
            # List standing.level2.column1 is now
            # c("standing", "level2", "column1"). Get the number of levels for
            # each information.
            lengths <- lengths(current_levels)
            # Get the lowest level name. These are the column names for
            # dfs that will be constructed.
            column_names <- lapply(current_levels, function(x) x[[length(x)]])
            column_names <- unlist(column_names)
            # Get the table names. Different levels are separated to unique
            # table.
            table_names <- lapply(current_levels, function(x) paste(x[ seq_len(length(x)-1) ], collapse = ".") )
            table_names <- unlist(table_names)
            # Add final column names.
            names(pos) <- column_names
            # Separate data based on final tables.
            tables <- split(pos, table_names)
            # Create dfs from separated datasets.
            temp_tab <- lapply(tables, function(x) as.data.frame(t(data.frame(x))))
            
            return(temp_tab)
        })
        # Thre result is nested including one additional level with one element.
        # Take the element.
        temp <- temp[[1]]
        # The first element is without the name. It is the highest level, i.e.,
        # standings.
        names(temp)[1] <- col_name
        return(temp)
    })
    
    # Initialize table list
    temp_tab <- list()
    # Get all the datatypes
    datatypes <- names(temp[[1]])
    # Loop over the data and combine tables for single position to single
    # tables. 
    for( tab in temp ){
        for( dtype in datatypes ){
            temp_tab[[ dtype ]] <- rbind(temp_tab[[ dtype ]], tab[[ dtype ]])
            rownames(temp_tab[[ dtype ]]) <- NULL
        }
    }
    
    # Add the data to result list
    tables <- c(tables, temp_tab)
    return(tables)
}

###

.parse_fixtures_v3 <- function(res, ...){
    # Convert to df
    res <- do.call(rbind, res)
    res <- as.data.frame(res)
    
    # All columns are lists. Loop through columns / data types.
    tables <- lapply(colnames(res), function(nam){
        temp_list <- list()
        # Get the single column.
        temp <- res[[nam]]
        # Convert to data.frame
        temp <- do.call(rbind, temp)
        temp <- as.data.frame(temp)
        
        # Check which column are flat.
        ind <- lapply(temp, function(x) all(lengths(x) <= 1))
        ind <- unlist(ind)
        
        # If there is flat columns
        if( any(ind) ){
            # Add those column to list as table
            temp_tab <- temp[, ind, drop = FALSE]
            temp_list[[ nam ]] <- temp_tab
        }
        
        
        # If there were nested columns
        if( any(!ind) ){
            # Subset
            temp <- temp[ , !ind, drop = FALSE]
            # Create tables from the rest of the results.
            temp_tab <- lapply(temp, function(x){
                # To matrix
                x <- do.call(rbind, x)
                # Use this function to ensure to get all the data unnested
                x <- .bind_columns_to_dataframe(x)
                return(x)
            })
            # Add to list
            temp_list <- c(temp_list, temp_tab)
        }
        
        return(temp_list)
    })
    names(tables) <- colnames(res)
    # Unnest nested list to list that includes only data.frames
    tables <- .unnest_list_of_lists(tables)
    
    return(tables)
}

###

.parse_teams_v3 <- function(res, ...){
    # Convert to data.frame
    res <- do.call(rbind, res)
    res <- as.data.frame(res)
    
    # Loop over data types
    tables <- lapply(res, function(x){
        # Convert to matrix
        x <- do.call(rbind, x)
        # Convert to data.frames. Ensure that there is no data that has left behind.
        x <- .bind_columns_to_dataframe(x)
        return(x)
    })
    
    return(tables)
}

###

.parse_players_v3 <- function(
        res, datatype = "players", ...){
    tables <- list()
    # Convert to df
    res <- do.call(rbind, res)
    res <- as.data.frame(res)
    
    # Get player info and add it to list
    name <- "player"
    players <- do.call(rbind, res[[name]])
    # Get flat information
    ind <- apply(players, 2, function(x) all(lengths(x) <= 1))
    ind <- unlist(ind)
    # Add flat info
    players_flat <- as.data.frame(players[, ind])
    tables[[datatype]] <- players_flat
    # Add nested players info
    players <- players[, !ind, drop = FALSE]
    players <- do.call(rbind, players)
    players <- .bind_columns_to_dataframe(players)
    tables[[ names(ind)[ !ind ] ]] <- players
    
    # Get statistics
    name <- "statistics"
    res <- res[[name]]
    res <- do.call(rbind, res)
    res <- as.data.frame(res)
    # There is only one element, take it
    res <- res[[1]]
    res <- do.call(rbind, res)
    res <- as.data.frame(res)
    # Loop over data types
    temp <- lapply(res, function(x){
        # Convert to matrix
        x <- do.call(rbind, x)
        # Convert to data.frames. Ensure that there is no data that has left behind.
        x <- .bind_columns_to_dataframe(x)
        return(x)
    })
    # Add to list
    tables <- c(tables, temp)
    
    return(tables)
}

###

.parse_coachs_v3 <- function(res, datatype = "coachs", ...){
    tables <- list()
    
    # Convert to data.frame
    res <- do.call(rbind, res)
    res <- as.data.frame(res)
    
    # Get flat columns and add them to list
    ind <- lapply(res, function(x) all(lengths(x) == 1))
    ind <- unlist(ind)
    tab <- res[, ind, drop = FALSE]
    tables[[datatype]] <- tab
    
    # Get nested column
    res <- res[ , !ind, drop = FALSE]
    # Loop through them and convert to dfs
    temp <- lapply(res, function(x){
        # Convert to matrix
        x <- do.call(rbind, x)
        # Convert to data.frames. Ensure that there is no data that has left
        # behind.
        x <- .bind_columns_to_dataframe(x)
        return(x)
    })
    # Career table is not parsed correctly. For each career, there is a prefix.
    # Remove them
    career <- "career"
    tab <- temp[[career]]
    # Add coach id. Add prefix, because this function removes prefixes and makes
    # tables tidy.
    tab[["prefix.coach_id"]] <- tables[[datatype]][["id"]]
    temp[[career]] <- .tidy_coach_career(tab)
    
    # Add tables to final result
    tables <- c(tables, temp)
    # Unnest to 1 level list
    tables <- .unnest_list_of_lists(tables)
    
    return(tables)
}

###

.tidy_coach_career <- function(tab, ...){
    # The table contains columns that specifies the same data but for different
    # coach. For example: 1.team.id and 2.team.id. Make these columns similarly
    # named.
    col_names <- colnames(tab)
    col_names <- lapply(
        strsplit(col_names, "\\."), function(x) paste(
            x[2:length(x)], collapse = "."))
    col_names <- unlist(col_names)
    colnames(tab) <- col_names
    
    # Loop through these columns and combine the info from different coaches
    temp_tab <- list()
    for( col in col_names ){
        temp <- tab[, colnames(tab) == col, drop = FALSE]
        temp_tab[[col]] <- unlist(temp)
    }
    temp_tab <- as.data.frame(temp_tab)
    rownames(temp_tab) <- NULL
    
    tables <- list()
    # Get team and date tables separately
    name <- "team."
    team <- grepl(name, colnames(temp_tab))
    date <- temp_tab[, !team, drop = FALSE]
    team <- temp_tab[, team, drop = FALSE]
    # Remove prefix from column names of team table and add it to list
    colnames(team) <- gsub(name, "", colnames(team))
    name <- gsub("\\.", "", name)
    tables[[name]] <- team
    # Add date table
    name <- "date"
    tables[[name]] <- date
    
    return(tables)
}

###

.parse_transfers_v3 <- function(res, ...){
    tables <- list()
    # Convert to df
    res <- do.call(rbind, res)
    res <- as.data.frame(res)
    
    # Get table that tells when the database was updated, and add it to list
    ind <- lapply(res, function(x) all(lengths(x) <= 1))
    ind <- unlist(ind)
    tab <- res[, ind, drop = FALSE]
    name <- "update"
    tables[[name]] <- tab
    
    # Get other columns
    res <- res[, !ind, drop = FALSE]
    
    # Get column that tells the player info and add it to lists
    name <- "player"
    tab <- res[[name]]
    tab <- do.call(rbind, tab)
    tab <- .bind_columns_to_dataframe(tab)
    tables[[name]] <- tab
    
    # Get other columns, i.e., the transfer info column
    name <- colnames(res)[ !colnames(res) %in% name ]
    res <- res[ , name]
    
    # Loop through columns
    temp <- lapply(res, function(x){
        temp <- list()
        # Convert column to df
        x <- do.call(rbind, x)
        x <- as.data.frame(x)
        # Get those column that are flat
        ind <- lapply(x, function(y) all(lengths(y) <= 1))
        ind <- unlist(ind)
        # Add flat column to list
        if( any(ind) ){
            tab <- x[, ind, drop = FALSE]
            temp[[name]] <- tab
            # Remove flat columns
            x <- x[, !ind, drop = FALSE]
        }
        # Get nested columns and create df from them. Ensure that all the data is
        # returned.
        if( any(!ind) ){
            temp_tabs <- lapply(x, function(tab){
                tab <- do.call(rbind, tab)
                tab <- .bind_columns_to_dataframe(tab)
            })
            # Add to list
            temp <- c(temp, temp_tabs)
        }
        return(temp)
    })
    # The returned value is a list of dfs. Each element is one player / transfer
    # Bind transfers and create df.
    temp <- do.call(rbind, temp)
    temp <- as.data.frame(temp)
    
    # Now the value is a df that is nested. Take first column that includes
    # transfer info and add it to list.
    name <- colnames(temp)[[1]]
    tab <- do.call(rbind, temp[[name]])
    tab <- as.data.frame(tab)
    tables[[name]] <- tab
    
    # The other column includes from where to which team the transfer
    # was made.
    tab <- do.call(rbind, temp[[ colnames(temp)[ !colnames(temp) %in% name ] ]])
    # Take indices of teams from where the palyer was left.
    col_names <- colnames(tab)
    name <- "in."
    ind <- grepl(name, col_names)
    
    # Remove prefix (in or out) from column names
    col_names <- lapply(strsplit(col_names, "\\."), function(x) x[[length(x)]])
    col_names <- unlist(col_names)
    colnames(tab) <- col_names
    
    # Divide in and out info to separate tables
    tables[["team.in"]] <- tab[, ind, drop = FALSE]
    tables[["team.out"]] <- tab[, !ind, drop = FALSE]
    
    return(tables)
}

###

.parse_trophies_or_sidelined_v3 <- function(res, ...){
    # Convert to df
    res <- do.call(rbind, res)
    res <- .bind_columns_to_dataframe(res)
    return(res)
}

#######################################

.parse_injuries_v3 <- function(res, ...){
    res <- do.call(rbind, res)
    res <- as.data.frame(res)
    # For each datatype, create a df
    res <- lapply(res, function(x){
        x <- do.call(rbind, x)
        x <- .bind_columns_to_dataframe(x)
        return(x)
    })
    
    # If there is a players table, it includes the injuries info. Get injuries
    # info to different table
    tab_name <- "player"
    if( tab_name %in% names(res) ){
        tab <- res[[tab_name]]
        # Get injuries info and player separately
        cols <- c("type", "reason")
        injuries <- tab[, cols, drop = FALSE]
        players <- tab[, !colnames(tab) %in% cols, drop = FALSE]
        # Add to results
        res[["injury"]] <- injuries
        res[[tab_name]] <- players
    }
    
    return(res)
}

###################################

.parse_predictions_v3 <- function(res, datatype = "predictions", ...){
    # Convert to df
    res <- do.call(rbind, res)
    res <- as.data.frame(res)
    
    # For each datatype, create a df
    tables <- lapply(res, function(tab){
        temp <- list()
        # Convert column to df
        tab <- do.call(rbind, tab)
        tab <- as.data.frame(tab)
        # Get flat data
        ind <- lapply(tab, function(x) all(lengths(x) <= 1) )
        ind <- unlist(ind)
        
        # Add flat data to list
        if( any(ind) ){
            temp_tab <- tab[, ind, drop = FALSE]
            # Add to list
            temp[[datatype]] <- temp_tab
            # Remove flat info from resultss
            tab <- tab[, !ind, drop = FALSE]
        }
        # For nested data, loop through column and create data.frame per column.
        if( any(!ind) ){
            tab <- lapply(tab, function(x){
                x <- do.call(rbind, x)
                x <- .bind_columns_to_dataframe(x)
                return(x)
            })
            # Add to list
            temp <- c(temp, tab)
        }
        # If only one element, return only it
        if( length(temp) == 1 ){
            temp <- temp[[1]]
        }
        return(temp)
    })
    # Tidy teams and h2h tables
    tables[["teams"]] <- .tidy_teams_predictions(tables[["teams"]])
    tables[["h2h"]] <- .tidy_teams_predictions(tables[["h2h"]], first_tab = "fixture")
    
    # Unnest list so that there is only one level
    tables <- .unnest_list_of_lists(tables)
    
    return(tables)
}

###

.tidy_teams_predictions <- function(tab, datatype = "teams", ...){
    tables <- list()
    # Loop over home and away tables
    temp <- lapply(tab, function(temp_tab){
        # Unlist to vector
        temp_tab <- unlist(temp_tab)
        # Get table levels
        col_names <- strsplit( names(temp_tab), "\\.")
        # Get name of tables
        table_names <- lapply(col_names, function(x) paste(x[ seq_len(length(x)-1) ], collapse = ".") )
        table_names <- unlist(table_names)
        # Tidy up names
        table_names <- gsub("last_5.last_5", "last_5", table_names)
        table_names <- gsub("league.league.", "", table_names)
        table_names[ table_names == "league.league" ] <- "form"
        # Get name of columns in tables
        col_names <- lapply(col_names, function(x) x[[length(x)]] )
        col_names <- unlist(col_names)
        # Add new column names
        names(temp_tab) <- col_names
        
        # Divide into separate data.frames
        temp_tab <- split(temp_tab, table_names)
        temp_tab <- lapply(temp_tab, function(x) as.data.frame(t(x)))
        
        return(temp_tab)
    })
    
    # Combine data for home and away
    for( name in seq_len(length(temp[[1]])) ){
        tables[[ name ]] <- rbind(temp[[1]][[name]], temp[[2]][[name]])
    }
    # Add names
    names <- names(temp[[1]])
    names[[1]] <- datatype
    names(tables) <- names
    
    return(tables)
}

###

.parse_odds_v3 <- function(res, ...){
    tables <- list()
    # Convert to df
    res <- do.call(rbind, res)
    res <- as.data.frame(res)
    
    # Get table that tells when the database was updated, and add it to list
    ind <- lapply(res, function(x) all(lengths(x) <= 1))
    ind <- unlist(ind)
    tab <- res[, ind, drop = FALSE]
    name <- "update"
    tables[[name]] <- tab
    
    # Get other columns
    res <- res[, !ind, drop = FALSE]
    
    # Loop through league and fixtures data sets
    datatypes <- c("league", "fixture")
    # For each datatype, create a df
    temp <- lapply(res[, datatypes, drop = FALSE], function(x){
        x <- do.call(rbind, x)
        x <- .bind_columns_to_dataframe(x)
        return(x)
    })
    # Add to list
    tables <- c(tables, temp)
    
    # Get bookmakers data
    data_name <- colnames(res)[ !colnames(res) %in% datatypes ]
    res <- res[[ data_name ]]
    
    # Parse bets info
    res <- .parse_bets(res)
    # The value is now list of data.frames. Each element is a single game.
    # Add game ids
    for( i in seq_len(length(res)) ){
        res[[i]][ , "fixture_id" ] <- tables[["fixture"]][i, "id"]
        # And if there is update table, add the info there
        if( "update" %in% names(tables) ){
            res[[i]][ , "last_update" ] <- tables[["update"]][i, "update"]
        }
    }
    # Remove update table
    if( "update" %in% names(tables) ){
        tables[["update"]] <- NULL
    }
    # Combine odds daata into single data.frame
    res <- do.call(rbind, res)
    res <- as.data.frame(res)
    
    # Add it to list
    tables[["odds"]] <- res
    
    return(tables)
}

###

.parse_squads_v3 <- function(res, ...){
    tables <- list()
    # To df
    res <- do.call(rbind, res)
    res <- as.data.frame(res)
    
    # If there is players table, parse it
    col_name <- "players"
    if( any( colnames(res) %in% col_name ) ){
        # Get the table
        temp <- res[[col_name]]
        # Loop through datatypes
        temp <- lapply(temp, function(x){
            x <- do.call(rbind, x)
            x <- .bind_columns_to_dataframe(x)
            return(x)
        })
        # Add to list and remove from results
        tables <- c(tables, temp)
        res <- res[ , !colnames(res) %in% col_name, drop = FALSE]
    }
    
    # For other nested data columns, create dfs
    temp <- lapply(res, function(x){
        x <- do.call(rbind, x)
        x <- .bind_columns_to_dataframe(x)
        return(x)
    })
    tables <- c(tables, temp)
    
    return(tables)
}
