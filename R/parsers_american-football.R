.parse_standings_american_football <- function(
        res, datatype = "standings", ...){
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
        # Combine into one df
        col_names <- names(temp)
        temp <- do.call(cbind, temp)
        temp <- as.data.frame(temp)
        colnames(temp) <- col_names
        # Add to lsit
        tables[[datatype]] <- temp
        # Get rest of the data
        res <- res[, !ind, drop = FALSE]
    }
    
    # Loop over leagues
    temp <- lapply(colnames(res), function(nam){
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
    names(temp) <- colnames(res)
    
    # Add to result list
    tables <- c(tables, temp)
    
    # Unnest list of list to list
    tables <- .unnest_list_of_lists(tables)
    
    # Ensure that all values are not nested in list
    tables <- lapply(tables, function(tab) as.data.frame( lapply(tab, unlist) ))
    
    # Tidy up names
    names <- strsplit(names(tables), "\\.")
    names <- lapply(names, function(x){
        if( length(x) > 1 && all(x[[length(x)]] == x) ){
            x <- x[[length(x)]]
        }
        x <- paste(x, collapse = ".")
        return(x)
    })
    names <- unlist(names)
    names(tables) <- names
    
    # Some records do not have ties. wins-losses-ties. If there is no ties,
    # there is only wins and ties...
    col_name <- "records"
    table <- tables[[col_name]]
    table <- lapply(table, function(x){
        if( !all(is.na(x)) ){
            # Add ties if there is not that information.
            x <- strsplit(x, "-")
            x <- lapply(x, function(val){
                if( length(val) != 3 ){
                    val <- c(val, rep("0", 3-length(val)))
                }
                val <- paste(val, collapse = "-")
                return(val)
            })
            x <- unlist(x)
            
            # Divide into wins-losses-ties
            x <- strsplit(x, "-")
            x <- lapply(x, function(val){
                t(as.data.frame(val))
            })
            x <- do.call(rbind, x)
            x <- as.data.frame(x)
        } else{
            # If there is no info, create empty df with 3 cols
            x <- data.frame(x, x, x)
        }
        colnames(x) <- c("wins", "losses", "ties")
        rownames(x) <- NULL
        return(x)
    })
    
    # Replace old values with new
    names(table) <- paste0(col_name, ".", names(table))
    tables[[col_name]] <- NULL
    tables <- c(tables, table)
    
    return(tables)
}

###

.parse_leagues_american_football <- function(res, ...){
    # To df
    res <- do.call(rbind, res)
    res <- as.data.frame(res)
    
    # Create dfs from league and country data
    names <- c("league", "country")
    temp <- res[ , names, drop = FALSE]
    tables <- lapply(temp, function(x){
        x <- do.call(rbind, x)
        x <- .bind_columns_to_dataframe(x)
        return(x)
    })
    # Get coverage data and parse it
    names <- colnames(res)[ !colnames(res) %in% names ]
    res <- res[ , names]
    temp <- .parse_coverage(res, tables)
    
    # Combine tables
    tables <- c(tables, temp)
    
    return(tables)
}
