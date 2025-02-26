
.parse_leagues_v2 <- function(res, sport = "nba", datatype = "leagues", ...){
    res <- as.matrix(res)
    res <- .bind_columns_to_dataframe(res)
    colnames(res) <- datatype
    
    # Get season data and add it
    if( datatype == "leagues" ){
        res2 <- getApiFootball(sport, "seasons")
        res2 <- res2[[1]]
        # Create combination of season and leagues. The result is factors
        # which are converted into characters.
        res <- expand.grid(res2[, 1], res[, 1])
        res <- as.data.frame(lapply(res, as.character))
        colnames(res) <- c("season", "league_id")
        res[["id"]] <- paste0(res[["league_id"]], "_", res[["season"]])
        
        # Create a list so that the format is the same as in other datatypes.
        res2 <- data.frame(
            id = res[["league_id"]], name = res[["league_id"]])
        res <- list(res, res2)
        names(res) <- c("seasons", "leagues")
    }
    
    return(res)
}

###

.parse_teams_v2 <- function(res, ...){
    tables <- list()
    # To df
    res <- do.call(rbind, res)
    res <- as.data.frame(res)
    
    # If there is leagues table
    nam <- "leagues"
    if( any(colnames(res) %in% nam) ){
        # Take leagues and loop throuh them and create dfs
        league <- res[[ nam ]]
        league <- lapply(league, function(x){
            x <- do.call(rbind, x)
            x <- .bind_columns_to_dataframe(x)
            return(x)
        })
        # Combine data
        league <- do.call(rbind, league)
        league <- as.data.frame(league)
        # Add to tables list and remove from results
        tables[[nam]] <- league
        res <- res[ !colnames(res) %in% nam ]
    }
    
    # For all the other columns, create one df
    res <- lapply(res, unlist)
    res <- as.data.frame(res)
    
    tables[["table"]] <- res
    # Combine data
    tables <- do.call(cbind, tables)
    tables <- as.data.frame(tables)
    colnames(tables) <- gsub("leagues\\.", "", colnames(tables))
    colnames(tables) <- gsub("table\\.", "", colnames(tables))
    tables <- list(table = tables)
    
    return(tables)
}

###

.parse_players_v2 <- function(res, ...){
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
        temp <- lapply(temp, function(x) do.call(rbind, x))
        temp <- as.data.frame(temp)
        # Add to list
        tables[["table"]] <- temp
        # Remove flats
        res <- res[ , !ind, drop = FALSE]
    }
    
    # If there is a leagues data, parse it
    col_name <- "leagues"
    if( any(colnames(res) %in% col_name) ){
        # Get the table
        tab <- res[[col_name]]
        # Create a list of list of data.frames. Loop through players
        tab <- lapply(seq_len(length(tab)), function(i){
            # Loop through where they have played
            temp <- lapply(names(tab[[i]]), function(nam){
                temp_tab <- tab[[i]][[nam]]
                temp_tab <- as.data.frame(t(do.call(rbind, temp_tab)))
                # Add index of player
                temp_tab[["id"]] <- i
                # Add league
                temp_tab[["league"]] <- nam
                return(temp_tab)
            })
            temp <- do.call(rbind, temp)
            temp <- as.data.frame(temp)
            return(temp)
        })
        # Combine into one df
        tab <- do.call(rbind, tab)
        tab <- as.data.frame(tab)
        
        # Add player id and add the table to list
        tab[["player_id"]] <- tables[["table"]][tab[["id"]], "id"]
        tables[[col_name]] <- tab
        # Remove from the results
        res <- res[ , !names(res) %in% col_name, drop = FALSE ]
    }
    
    # For nested data columns, create dfs
    temp <- lapply(res, function(x){
        x <- do.call(rbind, x)
        x <- .bind_columns_to_dataframe(x)
        return(x)
    })
    # Add to list
    tables <- c(tables, temp)
    
    return(tables)
}

###

.parse_games_v2 <- function(res, ...){
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
        temp <- lapply(temp, function(x) do.call(rbind, x))
        temp <- as.data.frame(temp)
        # Add to list
        tables[["table"]] <- temp
        # Remove flats
        res <- res[ , !ind, drop = FALSE]
    }
    
    # If there is a scores data, parse it
    col_name <- "scores"
    if( any(colnames(res) %in% col_name) ){
        # Get the table
        tab <- res[[col_name]]
        # Create df from it
        tab <- do.call(rbind, tab)
        tab <- as.data.frame(tab)
        # Loop over home and away
        tab <- lapply(tab, function(x){
            # Bind columns
            x <- do.call(rbind, x)
            x <- apply(x, 1, function(x) as.data.frame(t(as.data.frame(unlist((x))))) )
            # Bind rows
            x <- rbindlist(x, fill = TRUE)
            x <- lapply(x, as.numeric)
            x <- as.data.frame(x)
            return(x)
        })
        # Combine home and away
        tab <- do.call(cbind, tab)
        # All the levels are forced into same df. Take levels / final columns and
        # table names.
        table_names <- strsplit( colnames(tab), "\\." )
        column_names <- lapply(table_names, function(x) x[length(x)] )
        column_names <- unlist(column_names)
        table_names <- lapply(table_names, function(x) paste(x[seq_len(length(x)-1)], collapse = ".") )
        table_names <- unlist(table_names)
        table_names <- paste0(col_name, ".", table_names)
        # Add new column names
        colnames(tab) <- column_names
        # Based on table names, divide data to separate tables
        temp <- list()
        for( tab_nam in table_names ){
            temp[[tab_nam]] <- tab[ , which(table_names %in% tab_nam), drop = FALSE]
        }
        tables <- c(tables, temp)
        # Remove from the results
        res <- res[ , !names(res) %in% col_name, drop = FALSE ]
    }
    
    # If there is a teams data, parse it
    col_name <- "teams"
    if( any(colnames(res) %in% col_name) ){
        # Get the table
        tab <- res[[col_name]]
        # Create df from it
        tab <- do.call(rbind, tab)
        tab <- as.data.frame(tab)
        # Create dfs separately for home and away
        temp <- lapply(tab, function(x){
            x <- do.call(rbind, x)
            x <- .bind_columns_to_dataframe(x)
            return(x)
        })
        # Add to data
        tables <- c(tables, temp)
        # Remove from the results
        res <- res[ , !names(res) %in% col_name, drop = FALSE ]
    }
    
    # If there is a officials data, parse it
    col_name <- "officials"
    if( any(colnames(res) %in% col_name) ){
        # Get the table
        tab <- res[[ col_name ]]
        # Get officials to separate columns
        tab <- lapply(tab, function(x){
            x <- unlist(x)
            if( is.null(x) ){
                x <- NA
            }
            x <- as.data.frame(t(as.data.frame(x)))
            return(x)
        }  )
        tab <- rbindlist(tab, fill = TRUE)
        tab <- as.data.frame(tab)
        colnames(tab) <- paste0(
            substr(col_name, 1, nchar(col_name)-1), seq_len(ncol(tab)))
        # Combine to one column
        tab <- apply(tab, 1, function(x) paste(x[ !is.na(x) ], collapse = ", "))
        tab <- as.data.frame(tab)
        colnames(tab) <- substr(col_name, 1, nchar(col_name)-1)
        # Add to tables
        tables[[ col_name ]] <- tab
        # Remove from the results
        res <- res[ , !names(res) %in% col_name, drop = FALSE ]
    }
    
    
    # For nested data columns, create dfs
    temp <- lapply(res, function(x){
        x <- do.call(rbind, x)
        x <- .bind_columns_to_dataframe(x)
        return(x)
    })
    # Add to list
    tables <- c(tables, temp)
    
    return(tables)
}

###

.parse_standings_v2 <- function(res, datatype = "standings", ...){
    res <- .parse_games_v1(res, datatype, ...)
    # Modify colnames and add season id
    colnames(res[[datatype]])[ colnames(res[[datatype]]) == "league"] <-
        "league_id"
    res[[datatype]][["season_id"]] <- paste0(
        res[[datatype]][["league_id"]], "_", res[[datatype]][["season"]])
    return(res)
}

