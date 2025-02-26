
.parse_seasons_formula <- function(res, datatype, ...){
    res <- as.matrix(res)
    res <- .bind_columns_to_dataframe(res)
    colnames(res) <- datatype
    
    return(res)
}

###

.parse_competitions <- function(res, ...){
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
        tables[["table"]] <- temp
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


###

.parse_drivers <- function(res, datatype = "drivers", ...){
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
    # Combine teams info
    ind <- grepl("teams", names(temp))
    teams <- temp[ ind ]
    teams <- do.call(rbind, teams)
    teams <- as.data.frame(teams)
    rownames(teams) <- NULL
    colnames(teams) <- gsub("team.team.", "", colnames(teams))
    # Replace teams info from
    temp <- temp[ !ind ]
    temp[["teams"]] <- teams
    
    # Add to tables
    tables <- c(tables, temp)
    return(tables)
}
