
.parse_games_hockey <- function(res, datatype = "games", ...){
    # Parse games with general parser
    tables <- .parse_games_v1(res, datatype)
    # Divide scores to home and away tables. Now they are in format
    # home-away
    col_name <- "periods"
    table <- tables[[col_name]]
    table <- .divide_periods(table)
    # Replace old values with new
    names(table) <- paste0(col_name, ".", names(table))
    tables[[col_name]] <- NULL
    tables <- c(tables, table)
    return(tables)
}

###

.divide_periods <- function(table){
    # Get only home values
    home <- lapply(table, function(x){
        if( !all(is.na(x)) ){
            x <- strsplit(x, "-")
            x <- lapply(x, function(val) val[[1]])
            x <- unlist(x)
        }
        
        return(x)
    })
    home <- as.data.frame(home)
    
    # Get only away values
    away <- lapply(table, function(x){
        if( !all(is.na(x)) ){
            x <- strsplit(x, "-")
            x <- lapply(x, function(val) val[[length(val)]])
            x <- unlist(x)
        }
        return(x)
    })
    away <- as.data.frame(away)
    table <- list(home = home, away = away)
    
    return(table)
}
