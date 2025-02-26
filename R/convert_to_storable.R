
.convert_to_storable <- function(res, main_tab = "games", ...){
    tables <- list()

    names(res) <- gsub("goals\\.", "", names(res))
    names(res) <- gsub("goal\\.", "", names(res))
    # names(res)[ names(res) == "periods" ] <- "goals.periods"
    names(res) <- gsub("league\\.", "", names(res))
    names(res) <- gsub("leagues\\.", "", names(res))
    names(res) <- gsub("game\\.", "", names(res))
    names(res) <- gsub("games\\.", "", names(res))
    names(res) <- gsub(paste0(main_tab, "."), "", names(res))

    # Modify table names
    names(res)[ names(res) == "home" ] <- "teams.home"
    names(res)[ names(res) %in% c("away", "visitor", "visitors") ] <- "teams.away"
    names(res)[ names(res) == "table" ] <- main_tab
    names(res)[ names(res) %in% c("game.game", "games.games") ] <- "games"
    names(res)[ names(res) == "game" ] <- "games"
    names(res)[ names(res) == "team" ] <- "teams"
    names(res)[ names(res) == "country" ] <- "countries"
    names(res)[ names(res) %in% c("fixture", "fixtures") ] <- "games"
    names(res)[ names(res) %in% c("arena", "arenas", "venue") ] <- "venues"
    names(res)[ names(res) == "league" ] <- "leagues"
    names(res)[ names(res) == "injury" ] <- "injuries"
    names(res)[ names(res) == "player" ] <- "players"
    names(res)[ names(res) == "standing" ] <- "standings"

    # names(res) <- gsub("goals\\.", "", names(res))
    # names(res) <- gsub("goal\\.", "", names(res))
    # # names(res)[ names(res) == "periods" ] <- "goals.periods"
    # names(res) <- gsub("league\\.", "", names(res))
    # names(res) <- gsub("leagues\\.", "", names(res))
    # names(res) <- gsub("game\\.", "", names(res))
    # names(res) <- gsub("games\\.", "", names(res))
    # names(res) <- gsub(paste0(main_tab, "."), "", names(res))
    # names(res) <- gsub("visitors", "away", names(res))

    # Get venue table
    table_name <- "venues"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        # All venues do not have unique ID even though they have name.
        # But let's do it like this.
        res[[main_tab]][["venue_id"]] <- tab[["id"]]
        tab <- tab[ !duplicated(tab), ]
        tables[["venues"]] <- tab
        res[[table_name]] <- NULL
    }

    # Get player table
    table_name <- "players"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        res[[main_tab]][["player_id"]] <- tab[["id"]]
        tab <- tab[ !duplicated(tab), ]
        tables[["players"]] <- tab
        res[[table_name]] <- NULL
    }

    # Get game table
    table_name <- "games"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        res[[main_tab]][["game_id"]] <- tab[["id"]]
        tab <- tab[ !duplicated(tab), ]
        # Add to tables if not odds data
        if( main_tab != "odds" ){
            tables[["games"]] <- tab
        }
        res[[table_name]] <- NULL
    }

    # If update data
    table_name <- "update"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        colnames(tab) <- paste0( "update_", colnames(tab))
        # Add it back to result, not to tables. It will be added to main table.
        res[[table_name]] <- tab
    }

    # Get country table
    table_name <- "countries"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        # All venues do not have unique ID even though they have name.
        # But let's do it like this.
        res[[main_tab]][["country_id"]] <- tab[["id"]]
        tab <- tab[ !duplicated(tab), ]
        tables[["countries"]] <- tab
        res[[table_name]] <- NULL
    }
    # Get teams table
    table_name <- "teams"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        res[[main_tab]][["team_id"]] <- tab[["id"]]
        tab <- tab[ !duplicated(tab), ]
        tables[["teams"]] <- tab
        res[[table_name]] <- NULL
    }
    # Get league table
    table_name <- "leagues"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        # Add IDs
        res[[main_tab]][["league_id"]] <- tab[["id"]]
        res[[main_tab]][["season"]] <- tab[["season"]]
        res[[main_tab]][["season_id"]] <- paste0(
            res[[main_tab]][["league_id"]], "_", res[[main_tab]][["season"]])
        res[[main_tab]][["round"]] <- tab[["round"]]
        tab[["season"]] <- tab[["round"]] <- NULL
        # Remove duplicates
        tab <- tab[ !duplicated(tab), ]
        tables[["leagues"]] <- tab
        res[[table_name]] <- NULL
    }
    # Combine home and away tables
    table_name <- "teams.home"
    table_name2 <- "teams.away"
    if( all( c(table_name, table_name2) %in% names(res) ) ){
        teams_home <- res[[table_name]]
        teams_away <- res[[table_name2]]
        # Add IDs
        res[[main_tab]][["home_id"]] <- teams_home[["id"]]
        res[[main_tab]][["away_id"]] <- teams_away[["id"]]
        tab <- rbind(teams_home, teams_away)
        tab[["winner"]] <- NULL
        # Remove duplicates
        tab <- tab[ !duplicated(tab), ]
        tables[["teams"]] <- tab
        res[[table_name]] <- res[[table_name2]] <- NULL
    }

    # Combine career info
    table_name <- "career.team"
    table_name2 <- "career.date"
    if( all( c(table_name, table_name2) %in% names(res) ) ){
        tab1 <- res[[table_name]]
        tab2 <- res[[table_name2]]
        # Add IDs
        tab2[["team_id"]] <- tab1[["id"]]

        # Remove duplicates
        tab1 <- tab1[ !duplicated(tab1), ]
        tab2 <- tab2[ !duplicated(tab2), ]
        tables[["teams"]] <- tab1
        tables[["careers"]] <- tab2
        res[[table_name]] <- res[[table_name2]] <- NULL
    }

    # Combine rest of the tables
    res <- do.call(cbind, res)
    # Modify names
    colnames(res) <- gsub("\\.$", "", colnames(res))
    colnames(res) <- gsub("date\\.", "", colnames(res))
    colnames(res) <- gsub(paste0(main_tab, "."), "", colnames(res))
    colnames(res) <- gsub("\\.", "_", colnames(res))

    # If there is streak column in standings data, convert it to numeric
    col_name <- "streak"
    if( main_tab == "standings" && col_name %in% colnames(res) ){
        col <- res[[col_name]]
        num <- as.numeric(substr(col, 2, nchar(col)))
        win <- substr(col, 1, 1) == "W"
        win_streak <- ifelse(win, num, 0)
        lose_streak <- ifelse(!win, num, 0)
        res[["current_win_streak"]] <- win_streak
        res[["current_lose_streak"]] <- lose_streak
    }

    # Add the main table
    tables[[main_tab]] <- res

    return(tables)
}

###

.convert_leagues <- function(res, ...){
    tables <- list()
    main_tab <- "seasons"

    # Harmonize naming between sports
    names(res)[ names(res) == "league" ] <- "leagues"
    names(res)[ names(res) == "country" ] <- "countries"
    names(res)[ names(res) == "coverage" ] <- "coverages"
    names(res)[ names(res) == "season" ] <- "seasons"
    names(res)[ names(res) == "game" ] <- "games"
    names(res)[ names(res) == "fixture" ] <- "fixtures"
    if( "year" %in% colnames(res[[main_tab]]) ){
        colnames(res[[main_tab]])[ colnames(res[[main_tab]]) == "year" ] <- "season"
    }

    # Add season ID
    res[[main_tab]][["id"]] <- paste0(
        res[[main_tab]][["league_id"]], "_", res[[main_tab]][["season"]])

    # Get country table
    table_name <- "countries"
    if( table_name %in% names(res) && main_tab != table_name ){
        # Get table
        tab <- res[[table_name]]
        # Add IDs to leagues table. Get the id column
        id_col <- ifelse( "id" %in% colnames(tab), "id", "name")
        table_name2 <- "leagues"
        if( table_name2 %in% names(res) ){
            # Get ids
            ids <- tab[[id_col]]
            # In some datasets, ID is empty even though the name is same as
            # one country that has ID. Convert these so that replace NA with
            # known country ID.
            if( any(is.na(ids)) && "name" %in% colnames(tab) ){
                # Get IDs and order the table so that rows missing info are
                # at the end.
                id_empty <- tab[ is.na(ids), ]
                temp <- tab[ order(rowSums( is.na(tab) )), ]
                # Loop over missing info and replace
                for( empty_name in id_empty[["name"]] ){
                    empty_id <- temp[ temp[["name"]] == empty_name, id_col][[1]]
                    tab[ tab[["name"]] == empty_name, id_col ] <- empty_id
                }
            }
            # Add IDs to leagues table
            res[[table_name2]][["country_id"]] <- tab[["id"]] <- tab[[id_col]]
        }
        # There might be duplicated countries so that other duplicate has more
        # info. Order to get most of the info.
        tab <- tab[ order(rowSums( is.na(tab) )), ]
        tab <- tab[ !duplicated(tab[[id_col]]), ]
        # Remove from the result and add to tables
        res[[table_name]] <- NULL
        tables[[table_name]] <- tab
    }

    # Get league table
    table_name <- "leagues"
    if( table_name %in% names(res) && main_tab != table_name ){
        # Get the table and remove duplicates
        tab <- res[[table_name]]
        tab <- tab[ !duplicated(tab), ]
        # Remove from the result and add to tables
        res[[table_name]] <- NULL
        tables[[table_name]] <- tab
    }

    # Get covergae table
    table_name <- "coverages"
    if( table_name %in% names(res) && main_tab != table_name ){
        # Get the table
        tab <- res[[table_name]]
        # Games table contains more coverage info --> combine.
        table_name2 <- c("games", "fixtures")
        if( any(table_name2 %in% names(res)) && !main_tab %in% table_name2 ){
            table_name2 <- ifelse(
                table_name2[[1]] %in% names(res),
                table_name2[[1]], table_name2[[2]])
            tab2 <- res[[table_name2]]
            tab <- cbind(tab, tab2)
            colnames(tab) <- gsub("\\.", "_", colnames(tab))
            # There is typo at least in american football data
            colnames(tab) <- gsub("statisitcs", "statistics", colnames(tab))
            res[[table_name2]] <- NULL

        }
        # Add IDs
        tab[["id"]] <- res[[main_tab]][["id"]]
        # Remove duplicates after sorting. There can be some coverage info that
        # has same season_id but is different data.
        tab <- tab[ order(rowSums(tab == "TRUE")), ]
        tab <- tab[ !duplicated(tab[["id"]]), ]
        # Remove from the result and add to tables
        res[[table_name]] <- NULL
        tables[[table_name]] <- tab
    }

    # Remove current column since it is dependent on query date
    res[[main_tab]][["current"]] <- NULL
    # Cbind to ensure all the data is preserved
    tab <- do.call(cbind, res)
    colnames(tab) <- gsub(paste0(main_tab, "\\."), "", colnames(tab))
    tables[[main_tab]] <- tab

    # There might be duplicated season info, combine the info
    tables[["seasons"]] <- tab[!duplicated(tab), , drop = FALSE]
    tab <- tables[["seasons"]]
    ids <- tab[["id"]]
    if( any(duplicated(ids)) ){
        # Get duplicated IDs
        dupl <- ids[ duplicated(ids) ]
        # Get duplicated values and non-duplicated
        orig_tab <- tab[ !tab[["id"]] %in% dupl, ]
        dupl <- tab[ tab[["id"]] %in% dupl, ]

        # Loop over duplicated seasons
        dupl <- lapply(unique(dupl[["id"]]), function(season){
            # Get certain season
            temp <- dupl[ dupl[["id"]] == season, ]

            # Get season start and end days
            start_days <-  temp[["start"]]
            start <- min(start_days)
            end_days <-  temp[["end"]]
            end <- max(end_days)
            names(start) <- "start"
            names(end) <- "end"
            # Get season break days. Take into account if there are multiple
            # breaks.
            break_start <- start_days[ !start_days %in% start ]
            break_start <- sort(break_start)
            nams <- "break_start"
            if( length(break_start) > 1 ){
                nams <- c(
                    nams, paste0("break", 2:(length(break_start)), "_start"))
            }
            names(break_start) <- nams
            break_end <- end_days[ !end_days %in% end ]
            break_end <- sort(break_end)
            nams <- "break_end"
            if( length(break_end) > 1 ){
                nams <- c(nams, paste0("break", 2:(length(break_end)), "_end"))
            }
            names(break_end) <- nams

            # Combine values and add league etc info from original data
            col <- c(start, end, break_start, break_end)
            temp <- unlist( temp[1, !colnames(temp) %in% c("start", "end")] )
            temp <- c(temp, col)
            # CReate df
            temp <- as.data.frame(temp)
            temp <- t(temp)
            temp <- as.data.frame(temp)

            return(temp)
        })
        # Combine duplicated seasons together
        dupl <- rbindlist(dupl, fill = TRUE)
        # Add to original data
        tab <- rbindlist(list(orig_tab, dupl), fill = TRUE)
        tab <- as.data.frame(tab)
        # Ad back to tables
        tables[["seasons"]] <- tab
    }

    return(tables)
}

###

.convert_games <- function(
        res, main_tab = "games", remove.officials = FALSE, ...){
    #
    if( !(length(remove.officials) == 1 && is.logical(remove.officials)) ){
        stop("'remove.officials' must be TRUE or FALSE.")
    }
    #
    tables <- list()

    names(res) <- gsub("goals\\.", "", names(res))
    names(res) <- gsub("goal\\.", "", names(res))
    names(res) <- gsub("league\\.", "", names(res))
    names(res) <- gsub("leagues\\.", "", names(res))
    names(res) <- gsub("game\\.", "", names(res))
    names(res) <- gsub("games\\.", "", names(res))
    names(res) <- gsub("fixture\\.", "", names(res))
    names(res) <- gsub("fixtures\\.", "", names(res))
    names(res) <- gsub(paste0(main_tab, "."), "", names(res))
    # names(res) <- gsub("^periods\\.", "scores_periods_", names(res))

    # Modify table names
    names(res)[ names(res) == "home" ] <- "teams.home"
    names(res)[ names(res) %in% c(
        "away", "visitor", "visitors") ] <- "teams.away"
    names(res)[ names(res) == "table" ] <- main_tab
    names(res)[ names(res) %in% c("game.game", "games.games") ] <- "games"
    names(res)[ names(res) == "game" ] <- "games"
    names(res)[ names(res) == "fixture" ] <- "fixtures"
    names(res)[ names(res) == "team" ] <- "teams"
    names(res)[ names(res) == "country" ] <- "countries"
    names(res)[ names(res) %in% c(
        "arena", "arenas", "venue", "stadium", "stadiums") ] <- "venues"
    names(res)[ names(res) == "league" ] <- "leagues"
    # names(res)[ names(res) == "periods" ] <- "score.periods"


    # Get venue table
    table_name <- "venues"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        # All venues do not have unique ID even though they have name.
        # But let's do it like this. Venus without ID are dropped off.
        id_col <- ifelse("id" %in% colnames(tab), "id", "name")
        # Some IDs are NA. Give name as ID
        ids <- tab[[id_col]]
        ids[ is.na(ids) ] <- tab[ is.na(ids), "name"]
        res[[main_tab]][["venue_id"]] <- tab[["id"]] <-  ids
        tab <- tab[ !duplicated(tab), ]
        colnames(tab)[ colnames(tab) == "country" ] <- "country_id"
        if( nrow(tab) > 0 && !all(rowSums(is.na(tab)) == ncol(tab)) ){
            tables[["venues"]] <- tab
        }
        res[[table_name]] <- NULL
    }

    # Get game table
    table_name <- c("games", "fixtures")
    if( any(table_name %in% names(res)) && !main_tab %in% table_name ){
        table_name <- ifelse(
            table_name[[1]] %in% names(res),
            table_name[[1]], table_name[[2]])
        tab <- res[[table_name]]

        id_col <- paste0(substr(table_name, 1, nchar(table_name)-1), "_id")
        res[[main_tab]][[id_col]] <- tab[["id"]]
        tab <- tab[ !duplicated(tab), ]
        # Add to tables if not odds data
        if( main_tab != "odds" ){
            tables[["games"]] <- tab
        }
        res[[table_name]] <- NULL
    }

    # Remove officials table if specified
    table_name <- "officials"
    if( table_name %in% names(
        res) && main_tab != table_name && remove.officials ){
        tab <- res[[table_name]]
        res[[table_name]] <- NULL
    }

    # If update data
    table_name <- "update"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        colnames(tab) <- paste0( "update_", colnames(tab))
        # Add it back to result, not to tables. It will be added to main table.
        res[[table_name]] <- tab
    }

    # Get country table
    table_name <- "countries"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        # For those rows that do not have ID, add name as ID
        id_col <- ifelse( "id" %in% colnames(tab), "id", "name")
        ids <- tab[[id_col]]
        ids[ is.na(ids) ] <- tab[ is.na(ids), "name"]
        res[[main_tab]][["country_id"]] <- tab[["id"]] <-  ids
        tab <- tab[ !duplicated(tab), ]
        # tables[["countries"]] <- tab
        res[[table_name]] <- NULL
    }
    # Get teams table
    table_name <- "teams"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        res[[main_tab]][["team_id"]] <- tab[["id"]]
        tab <- tab[ !duplicated(tab), ]
        # tables[["teams"]] <- tab
        res[[table_name]] <- NULL
    }
    # Get league table
    table_name <- "leagues"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        # Add IDs
        res[[main_tab]][["league_id"]] <- tab[["id"]]
        res[[main_tab]][["season"]] <- tab[["season"]]
        res[[main_tab]][["season_id"]] <- paste0(
            res[[main_tab]][["league_id"]], "_", res[[main_tab]][["season"]])
        res[[main_tab]][["round"]] <- tab[["round"]]
        tab[["season"]] <- tab[["round"]] <- NULL
        # Add country ID
        table_name2 <- "countries"
        if( table_name2 %in% names(tables) ){
            tab[["country_id"]] <- tables[[table_name2]][["id"]]
        }
        # Convert "country" column to "country_id". In football, there is
        # "country" column.
        colnames(tab)[ colnames(tab) %in% "country" ] <- "country_id"

        # Remove duplicates
        tab <- tab[ !duplicated(tab), ]
        # tables[["leagues"]] <- tab
        res[[table_name]] <- NULL
    }
    # Combine home and away tables
    table_name <- "teams.home"
    table_name2 <- "teams.away"
    if( all( c(table_name, table_name2) %in% names(res) ) ){
        teams_home <- res[[table_name]]
        teams_away <- res[[table_name2]]
        # Add IDs
        res[[main_tab]][["home_id"]] <- teams_home[["id"]]
        res[[main_tab]][["away_id"]] <- teams_away[["id"]]
        tab <- rbind(teams_home, teams_away)
        tab[["winner"]] <- NULL
        # Remove duplicates
        tab <- tab[ !duplicated(tab), ]
        # tables[["teams"]] <- tab
        res[[table_name]] <- res[[table_name2]] <- NULL
    }

    # Cbind to ensure all the data is preserved
    tab <- do.call(cbind, res)
    # Polish names
    colnames(tab) <- gsub(paste0(main_tab, "\\."), "", colnames(tab))
    colnames(tab) <- gsub("date\\.", "", colnames(tab))
    colnames(tab) <- gsub("\\.", "_", colnames(tab))
    tables[[main_tab]] <- tab

    return(tables)
}

###

.convert_standings <- function(res, ...){
    tables <- list()
    main_tab <- "standings"

    names(res) <- gsub("goals\\.", "", names(res))
    names(res) <- gsub("goal\\.", "", names(res))
    names(res) <- gsub("league\\.", "", names(res))
    names(res) <- gsub("leagues\\.", "", names(res))
    names(res) <- gsub("game\\.", "", names(res))
    names(res) <- gsub("games\\.", "", names(res))
    names(res) <- gsub(paste0(main_tab, "."), "", names(res))

    # Modify table names
    names(res)[ names(res) == "home" ] <- "teams.home"
    names(res)[ names(res) %in% c("away", "visitor", "visitors") ] <- "teams.away"
    names(res)[ names(res) == "table" ] <- main_tab
    names(res)[ names(res) %in% c("game.game", "games.games") ] <- "games"
    names(res)[ names(res) == "game" ] <- "games"
    names(res)[ names(res) == "fixture" ] <- "fixtures"
    names(res)[ names(res) == "team" ] <- "teams"
    names(res)[ names(res) == "country" ] <- "countries"
    names(res)[ names(res) %in% c("arena", "arenas", "venue") ] <- "venues"
    names(res)[ names(res) == "league" ] <- "leagues"
    names(res)[ names(res) == "injury" ] <- "injuries"
    names(res)[ names(res) == "player" ] <- "players"
    names(res)[ names(res) == "standing" ] <- "standings"

    # Get country table
    table_name <- "countries"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        # For those rows that do not have ID, add name as ID
        id_col <- ifelse( "id" %in% colnames(tab), "id", "name")
        ids <- tab[[id_col]]
        ids[ is.na(ids) ] <- tab[ is.na(ids), "name"]
        res[[main_tab]][["country_id"]] <- tab[["id"]] <-  ids
        tab <- tab[ !duplicated(tab), ]
        # tables[["countries"]] <- tab
        res[[table_name]] <- NULL
    }
    # Get teams table
    table_name <- "teams"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        res[[main_tab]][["team_id"]] <- tab[["id"]]
        tab <- tab[ !duplicated(tab), ]
        # tables[["teams"]] <- tab
        res[[table_name]] <- NULL
    }
    # Get league table
    table_name <- "leagues"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        # Add IDs
        res[[main_tab]][["league_id"]] <- tab[["id"]]
        res[[main_tab]][["season"]] <- tab[["season"]]
        res[[main_tab]][["season_id"]] <- paste0(
            res[[main_tab]][["league_id"]], "_", res[[main_tab]][["season"]])
        res[[main_tab]][["round"]] <- tab[["round"]]
        tab[["season"]] <- tab[["round"]] <- NULL
        # Remove duplicates
        tab <- tab[ !duplicated(tab), ]
        # tables[["leagues"]] <- tab
        res[[table_name]] <- NULL
    }

    # Combine rest of the tables
    res <- do.call(cbind, res)
    # Modify names
    colnames(res) <- gsub("\\.$", "", colnames(res))
    colnames(res) <- gsub("date\\.", "", colnames(res))
    colnames(res) <- gsub("officials\\.", "", colnames(res))
    names(res) <- gsub("road\\.", "away\\.", names(res))
    colnames(res) <- gsub(paste0(main_tab, "."), "", colnames(res))
    colnames(res) <- gsub("\\.", "_", colnames(res))

    # # If there is streak column in standings data, convert it to numeric
    # col_name <- "streak"
    # if( main_tab == "standings" && col_name %in% colnames(res) ){
    #     col <- res[[col_name]]
    #     # If there is winStreak column like in nba data
    #     if( "winStreak" %in% colnames(res) ){
    #         win <- as.logical(res[["winStreak"]])
    #         num <- as.numeric(col)
    #     } else{
    #         # Else win info is in the prefix
    #         win <- substr(col, 1, 1) == "W"
    #         num <- as.numeric(substr(col, 2, nchar(col)))
    #     }
    #     win_streak <- ifelse(win, num, 0)
    #     lose_streak <- ifelse(!win, num, 0)
    #     res[["current_win_streak"]] <- win_streak
    #     res[["current_lose_streak"]] <- lose_streak
    # }

    # Add id
    res[["id"]] <- paste0( res[["season_id"]], "_", seq_len(nrow(res)) )
    # Replace group and update column name since hey lead to an error in sql
    colnames(res)[ colnames(res) == "group" ] <- "league_group"
    colnames(res)[ colnames(res) == "update" ] <- "last_update"
    # Add the main table
    tables[[main_tab]] <- res

    # # Create a data.frame that matches between leagues and teams. Team might
    # # be in multiple leagues.
    # tab <- data.frame(
    #     league_id = tables[[main_tab]][["league_id"]],
    #     team_id = tables[["teams"]][["id"]])
    # tab[["id"]] <- paste0(tab[["league_id"]], "_", tab[["team_id"]])
    #
    # tables[["leagueTeams"]] <- tab

    return(tables)
}

###

.convert_odds <- function(res, ...){
    tables <- list()
    main_tab <- "odds"

    names(res) <- gsub("league\\.", "", names(res))
    names(res) <- gsub("leagues\\.", "", names(res))
    names(res) <- gsub("game\\.", "", names(res))
    names(res) <- gsub("games\\.", "", names(res))
    names(res) <- gsub(paste0(main_tab, "."), "", names(res))

    # Modify table names
    names(res)[ names(res) == "home" ] <- "teams.home"
    names(res)[ names(res) %in% c("away", "visitor", "visitors") ] <- "teams.away"
    names(res)[ names(res) == "table" ] <- main_tab
    names(res)[ names(res) %in% c("game.game", "games.games") ] <- "games"
    names(res)[ names(res) == "game" ] <- "games"
    names(res)[ names(res) == "fixture" ] <- "fixtures"
    names(res)[ names(res) == "team" ] <- "teams"
    names(res)[ names(res) == "country" ] <- "countries"
    names(res)[ names(res) %in% c("arena", "arenas", "venue") ] <- "venues"
    names(res)[ names(res) == "league" ] <- "leagues"

    # Remove certain tables
    table_name <- c(
        "games", "fixtures", "status", "leagues", "countries", "teams.home",
        "teams.away", "scores.home", "scores.away", "scores", "periods")
    res <- res[ !names(res) %in% table_name ]

    # Combine rest of the tables
    res <- do.call(cbind, res)
    # Modify names
    colnames(res) <- gsub("\\.$", "", colnames(res))
    colnames(res) <- gsub(paste0(main_tab, "."), "", colnames(res))
    colnames(res) <- gsub("\\.", "_", colnames(res))
    colnames(res)[ colnames(res) %in% c("datetime", "update") ] <- "last_update"

    # Divide bookmaker and odds info to separate tables. Remove names.
    cols <- c("bookmaker_id", "bookmaker_name")
    df <- res[, cols, drop = FALSE]
    colnames(df) <- c("id", "name")
    df <- df[ !duplicated(df), ]
    res[[cols[[2]]]] <- NULL
    tables[["bookmakers"]] <- df
    cols <- c("id", "name")
    df <- res[, cols, drop = FALSE]
    df <- df[ !duplicated(df), ]
    res[[cols[[2]]]] <- NULL
    tables[["oddsInfo"]] <- df

    # Modify IDs
    res[["odd_id"]] <- res[["id"]]
    id_col <- ifelse("game_id" %in% colnames(res), "game_id", "fixture_id")
    res[["id"]] <- paste0(
        res[[id_col]], "_", res[["odd_id"]], "_", res[["value"]])

    # Add the main table
    tables[[main_tab]] <- res

    # # Give only the element if there is only one table
    # if( length(tables) == 1 ){
    #     tables <- tables[[1]]
    # }

    return(tables)
}

###

.convert_predictions <- function(res, ...){
    tables <- list()
    main_tab <- "predictions"

    names(res) <- gsub("goals\\.", "", names(res))
    names(res) <- gsub("goal\\.", "", names(res))
    names(res) <- gsub("league\\.", "", names(res))
    names(res) <- gsub("leagues\\.", "", names(res))
    names(res) <- gsub("game\\.", "", names(res))
    names(res) <- gsub("games\\.", "", names(res))
    names(res) <- gsub("team\\.", "", names(res))
    names(res) <- gsub("teams\\.", "", names(res))
    names(res) <- gsub(paste0(main_tab, "."), "", names(res))

    # Modify table names
    names(res)[ names(res) == "home" ] <- "teams.home"
    names(res)[ names(res) %in% c("away", "visitor", "visitors") ] <- "teams.away"
    names(res)[ names(res) == "table" ] <- main_tab
    names(res)[ names(res) %in% c("game.game", "games.games") ] <- "games"
    names(res)[ names(res) == "game" ] <- "games"
    names(res)[ names(res) == "fixture" ] <- "fixtures"
    names(res)[ names(res) == "team" ] <- "teams"
    names(res)[ names(res) == "country" ] <- "countries"
    names(res)[ names(res) %in% c("arena", "arenas", "venue") ] <- "venues"
    names(res)[ names(res) == "league" ] <- "leagues"
    names(res)[ names(res) == "injury" ] <- "injuries"
    names(res)[ names(res) == "player" ] <- "players"
    names(res)[ names(res) == "standing" ] <- "standings"

    # Get teams table
    table_name <- "teams"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        res[[main_tab]][["team_id"]] <- tab[["id"]]
        tab <- tab[ !duplicated(tab), ]
        tables[["teams"]] <- tab
        res[[table_name]] <- NULL
    }
    # Get league table
    table_name <- "leagues"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        # Add IDs
        res[[main_tab]][["league_id"]] <- tab[["id"]]
        res[[main_tab]][["season"]] <- tab[["season"]]
        res[[main_tab]][["season_id"]] <- paste0(
            res[[main_tab]][["league_id"]], "_", res[[main_tab]][["season"]])
        res[[main_tab]][["round"]] <- tab[["round"]]
        tab[["season"]] <- tab[["round"]] <- NULL
        # Remove duplicates
        tab <- tab[ !duplicated(tab), ]
        # tables[["leagues"]] <- tab
        res[[table_name]] <- NULL
    }

    # Combine rest of the tables
    res <- do.call(cbind, res)
    # Modify names
    colnames(res) <- gsub("\\.$", "", colnames(res))
    colnames(res) <- gsub("date\\.", "", colnames(res))
    colnames(res) <- gsub(paste0(main_tab, "."), "", colnames(res))
    colnames(res) <- gsub("\\.", "_", colnames(res))


    # Add the main table
    tables[[main_tab]] <- res

    # # Give only the element if there is only one table
    # if( length(tables) == 1 ){
    #     tables <- tables[[1]]
    # }

    return(tables)
}

###

.convert_teams <- function(res, ...){
    tables <- list()
    main_tab = "teams"

    names(res) <- gsub("goals\\.", "", names(res))
    names(res) <- gsub("goal\\.", "", names(res))
    # names(res)[ names(res) == "periods" ] <- "goals.periods"
    names(res) <- gsub("league\\.", "", names(res))
    names(res) <- gsub("leagues\\.", "", names(res))
    names(res) <- gsub("game\\.", "", names(res))
    names(res) <- gsub("games\\.", "", names(res))
    names(res) <- gsub(paste0(main_tab, "."), "", names(res))

    # Modify table names
    names(res)[ names(res) == "home" ] <- "teams.home"
    names(res)[ names(res) %in% c("away", "visitor", "visitors") ] <- "teams.away"
    names(res)[ names(res) == "table" ] <- main_tab
    names(res)[ names(res) %in% c("game.game", "games.games") ] <- "games"
    names(res)[ names(res) == "game" ] <- "games"
    names(res)[ names(res) == "team" ] <- "teams"
    names(res)[ names(res) == "country" ] <- "countries"
    names(res)[ names(res) %in% c("fixture", "fixtures") ] <- "games"
    names(res)[ names(res) %in% c("arena", "arenas", "venue") ] <- "venues"
    names(res)[ names(res) == "league" ] <- "leagues"

    # Get venue table
    table_name <- "venues"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        # Some IDs are NA. Give name as ID
        id_col <- ifelse( "id" %in% colnames(tab), "id", "name")
        ids <- tab[[id_col]]
        ids[ is.na(ids) ] <- tab[ is.na(ids), "name"]
        res[[main_tab]][["venue_id"]] <- tab[["id"]] <-  ids
        tab <- tab[ !duplicated(tab), ]
        tables[["venues"]] <- tab
        res[[table_name]] <- NULL
    }

    # Get venue table
    table_name <- "seasonsTeams"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        # Add unique id
        tab[["id"]] <- paste0(tab[["season_id"]], "_", tab[["team_id"]])
        tables[[table_name]] <- tab
        res[[table_name]] <- NULL
    }

    # Get country table
    table_name <- "countries"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        # Some IDs are NA. Give name as ID
        id_col <- ifelse( "id" %in% colnames(tab), "id", "name")
        ids <- tab[[id_col]]
        ids[ is.na(ids) ] <- tab[ is.na(ids), "name"]
        res[[main_tab]][["country_id"]] <- tab[["id"]] <-  ids
        tab <- tab[ !duplicated(tab), ]
        tables[["countries"]] <- tab
        res[[table_name]] <- NULL
    }

    # Get league table
    table_name <- "leagues"
    if( table_name %in% names(res) && main_tab != table_name ){
        tab <- res[[table_name]]
        # Add IDs
        res[[main_tab]][["league_id"]] <- tab[["id"]]
        res[[main_tab]][["season"]] <- tab[["season"]]
        res[[main_tab]][["season_id"]] <- paste0(
            res[[main_tab]][["league_id"]], "_", res[[main_tab]][["season"]])
        res[[main_tab]][["round"]] <- tab[["round"]]
        tab[["season"]] <- tab[["round"]] <- NULL
        # Remove duplicates
        tab <- tab[ !duplicated(tab), ]
        tables[["leagues"]] <- tab
        res[[table_name]] <- NULL
    }

    # Combine rest of the tables
    res <- do.call(cbind, res)
    # Modify names
    colnames(res) <- gsub("\\.$", "", colnames(res))
    colnames(res) <- gsub("date\\.", "", colnames(res))
    colnames(res) <- gsub(paste0(main_tab, "."), "", colnames(res))
    colnames(res) <- gsub("\\.", "_", colnames(res))

    colnames(res)[ colnames(res) == "country" ] <- "country_id"
    colnames(res)[ colnames(res) == "stadium" ] <- "venue_id"
    colnames(res)[ colnames(res) == "established" ] <- "founded"
    colnames(res)[ colnames(res) == "nationnal" ] <- "national"

    # Convert national column to logical
    col <- "national"
    if( col %in% colnames(res) ){
        res[[col]] <- as.logical(res[[col]])
    }

    # Add the main table
    tables[[main_tab]] <- res

    return(tables)
}
