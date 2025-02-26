

.update_leagues_database <- function(
        res, sport, dir.name = file.path("data", "apifootball"),
        add.venues = FALSE, ...){
    #
    if( !(length(dir.name) == 1 && is.character(dir.name)) ){
        stop("'dir.name' must be string.")
    }
    #
    if( !(is.logical(add.venues) && length(add.venues) == 1) ){
        stop("'add.venues' must be TRUE or FALSE.", call. = FALSE)
    }
    #
    # Connect to an SQLite database (create a new one if not exists)
    file_name <- paste0(sport, ".db")
    if( !dir.exists(dir.name) ){
        dir.create(dir.name, recursive = TRUE)
    }
    file_name <- file.path(dir.name, file_name)
    db <- dbConnect(SQLite(), file_name)

    # Update seasons table
    table <- "seasons"
    sql_table <- table
    # Info for initializing table
    # Get column names and types
    tab <- res[[table]]
    # Remove empty columns from the data
    empty_col <- colSums(is.na(tab)) == nrow(tab)
    if( any(empty_col) ){
        tab <- tab[ , !empty_col, drop = FALSE]
    }
    col_order <- c(
        "id", "league_id", "season", "start", "end", "break_start",
        "break_end", "break2_start", "break2_end", "break3_start",
        "break3_end", "break4_start", "break4_end")
    col_types <- .create_col_types(tab, col_order)
    foreign_key <- data.frame(
        foreign_key = c("league_id"),
        ref_table = c("leagues"),
        ref_col = c("id"))
    foreign_key <- .subset_foreign_key(foreign_key, tab)
    # Update
    temp <- .update_table(
        db, tab, sql_table, col_types, foreign_key)

    # Info for country table
    table <- "countries"
    if( table %in% names(res) ){
        sql_table <- table
        # Info for initializing table
        # Get column names and types
        tab <- res[[table]]
        # Remove empty columns from the data
        empty_col <- colSums(is.na(tab)) == nrow(tab)
        if( any(empty_col) ){
            tab <- tab[ , !empty_col, drop = FALSE]
        }
        col_order <- c("id", "name", "code", "flag")
        col_types <- .create_col_types(tab, col_order)
        # Update
        temp <- .update_table(
            db, tab, sql_table, col_types)
    }

    # Update leagues table
    table <- "leagues"
    if( table %in% names(res) ){
        sql_table <- table
        # Info for initializing table
        # Get column names and types
        tab <- res[[table]]
        # Remove empty columns from the data
        empty_col <- colSums(is.na(tab)) == nrow(tab)
        if( any(empty_col) ){
            tab <- tab[ , !empty_col, drop = FALSE]
        }
        col_order <- c("id", "country_id", "name", "type", "logo")
        col_types <- .create_col_types(tab, col_order)
        foreign_key <- data.frame(
            foreign_key = c("country_id"),
            ref_table = c("country"),
            ref_col = c("id"))
        foreign_key <- .subset_foreign_key(foreign_key, tab)
        # Update
        temp <- .update_table(
            db, tab, sql_table, col_types, foreign_key)
    }

    # If there is coverages table
    table <- "coverages"
    if( table %in% names(res) ){
        sql_table <- table
        # Info for initializing table
        # Get column names and types
        tab <- res[[table]]
        # Remove empty columns from the data
        empty_col <- colSums(is.na(tab)) == nrow(tab)
        if( any(empty_col) ){
            tab <- tab[ , !empty_col, drop = FALSE]
        }
        col_order <- c(
            "id", "events", "injuries", "lineups", "odds", "players",
            "predictions", "standings", "statistics", "statistics_fixtures",
            "statistics_players", "statistics_teams", "top_assists",
            "top_cards", "top_scorers")
        col_types <- .create_col_types(tab, col_order)
        foreign_key <- data.frame(
            foreign_key = c("id"),
            ref_table = c("seasons"),
            ref_col = c("id"))
        foreign_key <- .subset_foreign_key(foreign_key, tab)
        # Update
        temp <- .update_table(
            db, tab, sql_table, col_types, foreign_key)
    }

    # Arena info
    table <- "venues"
    if( table %in% names(res) ){
        sql_table <- table
        # Info for initializing table
        # Get column names and types
        tab <- res[[table]]
        # Remove empty columns from the data
        empty_col <- colSums(is.na(tab)) == nrow(tab)
        if( any(empty_col) ){
            tab <- tab[ , !empty_col, drop = FALSE]
        }
        col_order <- c(
            "id", "name", "address", "city", "capacity", "surface",
            "image")
        col_types <- .create_col_types(tab, col_order)
        # Update
        temp <- .update_table(
            db, tab, sql_table, col_types)
    }

    # Disconnect
    dbDisconnect(db)
}

.update_odds_database <- function(
        res, sport, dir.name = file.path("data", "apifootball"), ...){
    #
    if( !(length(dir.name) == 1 && is.character(dir.name)) ){
        stop("'dir.name' must be string.")
    }
    #
    # Connect to an SQLite database (create a new one if not exists)
    file_name <- paste0(sport, ".db")
    if( !dir.exists(dir.name) ){
        dir.create(dir.name, recursive = TRUE)
    }
    file_name <- file.path(dir.name, file_name)
    db <- dbConnect(SQLite(), file_name)

    # Update odds table
    table <- "odds"
    sql_table <- table
    # Info for initializing table
    # Get column names and types
    tab <- res[[table]]
    # Remove empty columns from the data
    empty_col <- colSums(is.na(tab)) == nrow(tab)
    if( any(empty_col) ){
        tab <- tab[ , !empty_col, drop = FALSE]
    }
    col_order <- c(
        "id", "league_id", "season_id", "season", "game_id", "fixture_id",
        "bookmaker_id", "odd_id", "last_update", "value", "odd")
    col_types <- .create_col_types(tab, col_order)
    foreign_key <- data.frame(
        foreign_key = c(
            "league_id", "season_id", "game_id", "fixture_id",
            "bookmaker_id", "odd_id"),
        ref_table = c(
            "leagues", "seasons", "games", "fixtures", "bookmakers",
            "oddsInfo"),
        ref_col = c("id", "id", "id", "id", "id", "id"))
    foreign_key <- .subset_foreign_key(foreign_key, tab)
    # Update
    temp <- .update_table(
        db, tab, sql_table, col_types, foreign_key)

    # Update oddsInfo table
    table <- "oddsInfo"
    sql_table <- table
    # Info for initializing table
    # Get column names and types
    tab <- res[[table]]
    # Remove empty columns from the data
    empty_col <- colSums(is.na(tab)) == nrow(tab)
    if( any(empty_col) ){
        tab <- tab[ , !empty_col, drop = FALSE]
    }
    col_order <- c("id", "name")
    col_types <- .create_col_types(tab, col_order)
    # Update
    temp <- .update_table(
        db, tab, sql_table, col_types)

    # Update bookmakers table
    table <- "bookmakers"
    sql_table <- table
    # Info for initializing table
    # Get column names and types
    tab <- res[[table]]
    # Remove empty columns from the data
    empty_col <- colSums(is.na(tab)) == nrow(tab)
    if( any(empty_col) ){
        tab <- tab[ , !empty_col, drop = FALSE]
    }
    col_order <- c("id", "name")
    col_types <- .create_col_types(tab, col_order)
    # Update
    temp <- .update_table(
        db, tab, sql_table, col_types)

    # Disconnect
    dbDisconnect(db)
}

###

.update_standings_database <- function(
        res, sport, dir.name = file.path("data", "apifootball"),
        update_teams = FALSE, ...){
    #
    if( !(length(dir.name) == 1 && is.character(dir.name)) ){
        stop("'dir.name' must be string.")
    }
    #
    if( !(is.logical(update_teams) && length(update_teams) == 1) ){
        stop("'update_teams' must be TRUE or FALSE.", call. = FALSE)
    }
    #
    # Connect to an SQLite database (create a new one if not exists)
    file_name <- paste0(sport, ".db")
    if( !dir.exists(dir.name) ){
        dir.create(dir.name, recursive = TRUE)
    }
    file_name <- file.path(dir.name, file_name)
    db <- dbConnect(SQLite(), file_name)

    # Update teams table
    table <- "teams"
    if( update_teams && table %in% names(res) ){
        sql_table <- table
        # Info for initializing table
        # Get column names and types
        tab <- res[[table]]
        # Remove empty columns from the data
        empty_col <- colSums(is.na(tab)) == nrow(tab)
        if( any(empty_col) ){
            tab <- tab[ , !empty_col, drop = FALSE]
        }
        col_order <- c(
            "id", "country_id", "venue_id", "name", "code", "founded",
            "national", "logo")
        col_types <- .create_col_types(tab, col_order)
        foreign_key <- data.frame(
            foreign_key = c("country_id", "venue_id"),
            ref_table = c("countries", "venues"),
            ref_col = c("id", "id"))
        foreign_key <- .subset_foreign_key(foreign_key, tab)
        # Update
        temp <- .update_table(
            db, tab, sql_table, col_types, foreign_key)
    }

    # Update standings table
    table <- "standings"
    sql_table <- table
    # Info for initializing table
    # Get column names and types
    tab <- res[[table]]
    # Remove empty columns from the data
    empty_col <- colSums(is.na(tab)) == nrow(tab)
    if( any(empty_col) ){
        tab <- tab[ , !empty_col, drop = FALSE]
    }
    col_order <- c(
        "id", "country_id", "league_id", "season_id", "team_id", "season")
    col_types <- .create_col_types(tab, col_order)
    foreign_key <- data.frame(
        foreign_key = c("country_id", "league_id", "season_id", "team_id"),
        ref_table = c("countries", "leagues", "seasons", "teams"),
        ref_col = c("id", "id", "id", "id"))
    foreign_key <- .subset_foreign_key(foreign_key, tab)
    # Update
    temp <- .update_table(
        db, tab, sql_table, col_types, foreign_key)

    # Disconnect
    dbDisconnect(db)
}

###

.update_teams_database <- function(
        res, sport, dir.name = file.path("data", "apifootball"), ...){
    #
    if( !(length(dir.name) == 1 && is.character(dir.name)) ){
        stop("'dir.name' must be string.")
    }
    #
    if( !(length(dir.name) == 1 && is.character(dir.name)) ){
        stop("'dir.name' must be string.")
    }
    #
    # Connect to an SQLite database (create a new one if not exists)
    file_name <- paste0(sport, ".db")
    if( !dir.exists(dir.name) ){
        dir.create(dir.name, recursive = TRUE)
    }
    file_name <- file.path(dir.name, file_name)
    db <- dbConnect(SQLite(), file_name)

    # Update teams table
    table <- "teams"
    sql_table <- table
    # Info for initializing table
    # Get column names and types
    tab <- res[[table]]
    # Remove empty columns from the data
    empty_col <- colSums(is.na(tab)) == nrow(tab)
    if( any(empty_col) ){
        tab <- tab[ , !empty_col, drop = FALSE]
    }
    col_order <- c(
        "id", "country_id", "venue_id", "name", "nickname", "code", "city",
        "national", "founded", "logo", "allStar", "nbaFranchise",
        "conference", "division", "coach", "owner", "colors")
    col_types <- .create_col_types(tab, col_order)
    foreign_key <- data.frame(
        foreign_key = c("country_id", "venue_id"),
        ref_table = c("countries", "venues"),
        ref_col = c("id", "id"))
    foreign_key <- .subset_foreign_key(foreign_key, tab)
    # Update
    temp <- .update_table(
        db, tab, sql_table, col_types, foreign_key)

    # Update seasonsTeams table
    table <- "seasonsTeams"
    if( table %in% names(res) ){
        sql_table <- table
        # Info for initializing table
        # Get column names and types
        tab <- res[[table]]
        # Remove empty columns from the data
        empty_col <- colSums(is.na(tab)) == nrow(tab)
        if( any(empty_col) ){
            tab <- tab[ , !empty_col, drop = FALSE]
        }
        col_order <- c("id", "season_id", "team_id")
        col_types <- .create_col_types(tab, col_order)
        foreign_key <- data.frame(
            foreign_key = c("season_id", "team_id"),
            ref_table = c("seasons", "teams"),
            ref_col = c("id", "id"))
        foreign_key <- .subset_foreign_key(foreign_key, tab)
        # Update
        temp <- .update_table(
            db, tab, sql_table, col_types, foreign_key)
    }

    # Arena info
    table <- "venues"
    if( table %in% names(res) ){
        sql_table <- table
        ## Info for initializing table
        # Get column names and types
        tab <- res[[table]]
        # Remove empty columns from the data
        empty_col <- colSums(is.na(tab)) == nrow(tab)
        if( any(empty_col) ){
            tab <- tab[ , !empty_col, drop = FALSE]
        }
        col_order <- c(
            "id", "name", "address", "city", "capacity", "surface",
            "image")
        col_types <- .create_col_types(tab, col_order)
        # Update
        temp <- .update_table(
            db, tab, sql_table, col_types)
    }

    # Info for country table
    table <- "countries"
    if( table %in% names(res) ){
        sql_table <- table
        # Info for initializing table
        # Get column names and types
        tab <- res[[table]]
        # Remove empty columns from the data
        empty_col <- colSums(is.na(tab)) == nrow(tab)
        if( any(empty_col) ){
            tab <- tab[ , !empty_col, drop = FALSE]
        }
        col_order <- c("id", "name", "code", "flag")
        col_types <- .create_col_types(tab, col_order)
        # Update
        temp <- .update_table(
            db, tab, sql_table, col_types)
    }

    # Disconnect
    dbDisconnect(db)
}

###

.update_games_database <- function(
        res, sport, dir.name = file.path("data", "apifootball"),
        add.venues = TRUE, ...){
    #
    if( !(length(dir.name) == 1 && is.character(dir.name)) ){
        stop("'dir.name' must be string.")
    }
    #
    if( !(is.logical(add.venues) && length(add.venues) == 1) ){
        stop("'add.venues' must be TRUE or FALSE.", call. = FALSE)
    }
    #
    # Connect to an SQLite database (create a new one if not exists)
    file_name <- paste0(sport, ".db")
    if( !dir.exists(dir.name) ){
        dir.create(dir.name, recursive = TRUE)
    }
    file_name <- file.path(dir.name, file_name)
    db <- dbConnect(SQLite(), file_name)

    # Update teams table
    table <- "games"
    table <- ifelse(table %in% names(res), table, "fixtures")
    sql_table <- table
    # Info for initializing table
    # Get column names and types
    tab <- res[[table]]
    # Remove empty columns from the data
    empty_col <- colSums(is.na(tab)) == nrow(tab)
    if( any(empty_col) ){
        tab <- tab[ , !empty_col, drop = FALSE]
    }
    col_order <- c(
        "id", "country_id", "league_id", "season_id", "home_id", "away_id",
        "season", "timezone", "date", "time", "timestamp")
    col_types <- .create_col_types(tab, col_order)
    foreign_key <- data.frame(
        foreign_key = c("country_id", "venue_id"),
        ref_table = c("countries", "venues"),
        ref_col = c("id", "id"))
    foreign_key <- .subset_foreign_key(foreign_key, tab)
    # Update
    temp <- .update_table(
        db, tab, sql_table, col_types, foreign_key)

    # Info for country table
    table <- "venues"
    if( add.venues && table %in% names(res) ){
        sql_table <- table
        # Info for initializing table
        # Get column names and types
        tab <- res[[table]]
        # Remove empty columns from the data
        empty_col <- colSums(is.na(tab)) == nrow(tab)
        if( any(empty_col) ){
            tab <- tab[ , !empty_col, drop = FALSE]
        }
        col_order <- c(
            "id", "name", "address", "city", "state", "country",
            "country_id","capacity", "surface", "image")
        col_types <- .create_col_types(tab, col_order)
        # Update
        temp <- .update_table(
            db, tab, sql_table, col_types)
    }

    # Disconnect
    dbDisconnect(db)
}

###

.subset_foreign_key <- function(foreign_key, tab){
    foreign_key <- foreign_key[
        foreign_key[["foreign_key"]] %in% colnames(tab), ]
    if( nrow(foreign_key) == 0 ){
        foreign_key <- NULL
    }
    return(foreign_key)
}

###

.create_col_types <- function(tab, col_order = NULL){
    # Get column names
    column_names <- colnames(tab)
    # Order the names if order was specified
    if( !is.null(col_order) ){
        column_names <- c(
            col_order, column_names[ !column_names %in% col_order])
        column_names <- column_names[ column_names %in% colnames(tab) ]
    }
    # List known cols
    id_cols <- c("id")
    text_cols <- c(
        "season", "week", "stage", "name", "address", "city", "state",
        "country", "country_id", "surface", "image", "season_id", "team_id",
        "league_id", "bookmaker_id", "odd_id", "country_id", "venue_id",
        "home_id", "away_id", "game_id", "fixture_id", "name", "nickname",
        "code", "city", "logo", "conference", "division", "coach", "owner",
        "colors")
    date_cols <- c(
        "date", "start", "end", "break_start", "break_end", "break2_start",
        "break2_end", "break3_start", "break3_end", "break4_start",
        "break4_end")
    datetime_cols <- c("timestamp", "updated", "last_update", "odd_updated")
    time_cols <- c("time")
    int_cols <- c(
        "founded", "national", "capacity", "allStar", "nbaFranchise", "events",
        "injuries", "lineups", "odds", "players", "predictions", "standings",
        "statistics", "statistics_fixtures", "statistics_players",
        "statistics_teams", "top_assists", "top_cards", "top_scorers",
        "num_of_bookmakers")
    num_cols <- c("odd", "odd_1", "odd_x", "odd_2", "payout")

    # Loop over column and get their type
    types <- lapply(column_names, function(nam){
        col <- tab[[nam]]
        # If the column is ID column
        if( nam %in% id_cols ){
            type <- "TEXT PRIMARY KEY"
        } else if( nam %in% date_cols ){
            type <- "DATE"
        } else if( nam %in% datetime_cols ){
            type <- "DATETIME"
        } else if ( nam %in% time_cols ){
            type <- "TIME"
        } else if( nam %in% text_cols ){
            type <- "TEXT"
        # If col is unknown, but numeric (there is no _id suffix or it is not
        # NA --> then we cannot be sure). INTEGER and DECIMAL are problematic
        # compared to textr. That is why int and num columns are double checked.
        # There might be some columns listed here that are really not numeric.
        # Still if no data is available for int and num cols, they get the
        # default value as numeric. "_id" suffix denotes id column. Do not
        # convert it to numeric.
        } else if( (
            !(all(is.na(col)) && grepl("*_id$", col)) ||
            nam %in% c(int_cols, num_cols) ) && all(
                is.na(col) == is.na(
                    suppressWarnings(as.numeric(col)) ) ) ){
            # If col is an integer
            if( all( as.numeric( col ) %% 1 == 0, na.rm = TRUE ) || (
                all(is.na(col)) && nam %in% int_col) ) {
                type <- "INTEGER"
            } else{
                type <- "DECIMAL(10, 5)"
            }
        } else{
            # Type defaults to "TEXT"
            type <- "TEXT"
        }
        return(type)
    })
    types <- unlist(types)

    # Info for initializing table
    col_types <- data.frame(
        orig_cols = column_names,
        name = column_names,
        type = types)

    return(col_types)
}

###
.update_table <- function(
        db, df, sql_table, col_types, ...){
    # If the table is empty, do not add it. Or if there is no ID column
    if( nrow(df) == 0 || ncol(df) == 0 || !"id" %in% colnames(df) ){
        return(NULL)
    }
    # Sql table name must not have "-", "/", or " "
    sql_table <- gsub("-|/| ", "_", sql_table)
    # Sql table cannot start with number
    if( !is.na(suppressWarnings(as.numeric(substr(sql_table, 1, 1)))) ){
        sql_table <- paste0("_", sql_table)
    }
    # Order the df columns
    df <- df[ , col_types[["orig_cols"]], drop = FALSE]
    # Remove rows that do not have IDs
    df <- df[ !is.na(df[["id"]]), ]
    # Convert datatypes to correct class
    df <- .convert_class(df, col_types)

    # If the table cannot be found, initialize it.
    if( !sql_table %in% dbListTables(db) ){
        temp <- .initialize_table(
            db, df, col_types, sql_table, ...)
    } else{
        # If it can be found, get the fields of the table.
        temp <- dbListFields(db, sql_table)
        # Add columns to database if they are not found
        cols_dont_exist <- colnames(df)[ !colnames(df) %in% temp ]
        if( length(cols_dont_exist) > 0 ){
            temp <- .add_new_fields(db, sql_table, col_types, cols_dont_exist)
        }
    }
    # Update table. If there are IDs that are already included, update row
    # to ensure that our database includes the most updated information.
    # Otherwise add new row.
    query <- paste0(
        "INSERT OR REPLACE INTO ", sql_table, " (",
        paste(col_types[["name"]], collapse = ", "), ") VALUES (",
        paste0(":", col_types[["orig_cols"]], collapse = ", "),
        ");")
    dbExecute(db, query, df)
}

###

.add_new_fields <- function(db, sql_table, col_types, cols_dont_exist){
    # Take those columns that do not exist
    col_types <- col_types[ col_types[["orig_cols"]] %in% cols_dont_exist, ]
    # Create query
    statements <- paste(
        paste("ALTER TABLE", sql_table, "ADD COLUMN"),
        col_types[["name"]], col_types[["type"]])
    # Add fields
    for( statement in statements ){
        dbExecute(db, statement)
    }
    return(NULL)
}

###

.initialize_table <- function(
        db, df, col_types, sql_table, foreign_key = NULL, ...){
    # If foreign_key is not NULL, parse it to correct form.
    if( !is.null(foreign_key) ){
        # Construct foreign keys
        foreign_key <- paste0(
            "FOREIGN KEY (", foreign_key[["foreign_key"]], ") REFERENCES ",
            foreign_key[["ref_table"]], "(", foreign_key[["ref_col"]], ")")
        # Add comma before the foreign keys
        foreign_key <- paste0(", ", paste(foreign_key, collapse = ", "))
    }
    # Construct query
    query <- paste0(
        "CREATE TABLE IF NOT EXISTS ", sql_table, "(",
        paste(
            paste(col_types[["name"]], col_types[["type"]]),
            collapse = ", "),
        ifelse(is.null(foreign_key), "", foreign_key),
        ");"
    )
    # Create a table if it does not exist
    dbExecute(db, query)

    return(TRUE)
}

###

.convert_class <- function(df, col_types){
    # Loop over columns of df
    for( i in seq_len(nrow(col_types)) ){
        dtype <- col_types[i, "type"]
        # Get correct function to modify the column type.
        # Date must be in character because sql cannot read date format.
        col <- df[[ col_types[i, "orig_cols"] ]]
        if( dtype == "TEXT PRIMARY KEY" ){
            col <- as.character(col)
        } else if( dtype == "TEXT" ){
            col <- as.character(col)
        } else if( dtype == "INTEGER PRIMARY KEY" ){
            col <- as.integer(col)
        } else if( dtype == "INTEGER" ){
            col <- as.integer(col)
        } else if( dtype == "DATE" ){
            col <- as.character(col)
        } else if( dtype == "DATETIME" ){
            col <- as.character(col)
        } else if( dtype == "TIME" ){
            col <- as.character(col)
        } else if( grepl("DECIMAL", dtype) ){
            col <- as.numeric(col)
        }
        # Add column back
        df[[ col_types[i, "orig_cols"] ]] <- col
    }
    return(df)
}
