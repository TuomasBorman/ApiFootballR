
.simple_rapply <- function(x, fn){
    if( is.list(x) ){
        lapply(x, .simple_rapply, fn)
    } else{
        fn(x)
    }
}

###

.parse_bets <- function(res){
    # Loop through games
    res <- lapply(res, function(x){
        # Convert column to df
        x <- do.call(rbind, x)
        x <- as.data.frame(x)
        # Get those column that are flat
        ind <- lapply(x, function(y) all(lengths(y) <= 1))
        ind <- unlist(ind)
        # Get bookmaker info
        temp <- x[, ind, drop = FALSE]
        # Get odds info
        nam <- names(ind)[ !ind ]
        x <- x[, nam]

        # Loop through bookmakers
        temp2 <- lapply(x, function(tab){
            # Convert to df
            tab <- do.call(rbind, tab)
            tab <- as.data.frame(tab)

            # Get those column that are flat. Info about type of odds.
            ind <- lapply(tab, function(y) all(lengths(y) <= 1))
            ind <- unlist(ind)
            # Store flat info
            odds_info <- tab[ , ind, drop = FALSE]

            # Get nested info
            tab <- tab[ , !ind]
            # Unnest the data. Loop through bookmakers and get their odds
            tab <- lapply(tab, function(y){
                y <- do.call(rbind, y)
                y <- .bind_columns_to_dataframe(y)
                return(y)
            })
            # Add odds info to values
            for( i in seq_len(length(tab)) ){
                tab[[i]][ , colnames(odds_info) ] <- odds_info[i, ]
            }
            # Combine all the odds data to single df
            tab <- do.call(rbind, tab)
            tab <- as.data.frame(tab)
            return(tab)
        })
        # Add bookmaker info to odds info
        colnames(temp) <- paste0("bookmaker_", colnames(temp))
        for( i in seq_len(length(temp2)) ){
            temp2[[i]][ , colnames(temp) ] <- temp[i, ]
        }
        # Combine all odds information into one data.frame
        temp2 <- do.call(rbind, temp2)
        temp2 <- as.data.frame(temp2)
        return(temp2)
    })
    return(res)
}

###

.unnest_list_of_lists <- function(x){
    result <- list()
    # Loop through elements of list
    for( i in seq_len(length(x)) ){
        dtype <- ifelse( is.null(names(x)[[i]]), i, names(x)[[i]] )
        # If the data is already data.frame, add it as it is.
        if( is( x[[i]], "data.frame") ){
            result[[dtype]] <- x[[dtype]]
        } else{
            # Otherwise unnest it
            for( j in seq_len(length(x[[dtype]])) ){
                dtype_sub <- ifelse(
                    is.null( names(x[[dtype]])[[j]] ),
                    j, names(x[[dtype]])[[j]])
                result[[ paste0(dtype, ".", dtype_sub) ]] <-
                    x[[dtype]][[dtype_sub]]
            }
        }
    }
    return(result)
}

###

.bind_columns_to_dataframe <- function(x){
    # Get how many observations there are
    num_rows <- nrow(x)
    # Loop through columns
    x <- lapply(seq_len(ncol(x)), function(i){
        # Get name of column
        nam <- ifelse( is.null(colnames(x)), i, colnames(x)[ i ])
        # Get column
        col <- x[, nam]
        # Replace empty values with NA
        col[ lengths(col) == 0 ] <- NA
        # Create a vector
        col <- unlist(col)
        # Create a matrix from it. Create this way (final columns at rows) so
        # that the matrix is built correctly.
        col <- matrix(
            col, ncol = num_rows, dimnames = list(unique(names(col)), NULL))
        # Put in correct orientation
        col <- t(col)
        # If there are multiple columns i.e. the data was nested,
        # add the column name to names. Otherwise the column name is the
        # original column name.
        if( ncol(col) > 1 ){
            colnames(col) <- paste(nam, colnames(col), sep = ".")
        } else{
            colnames(col) <- nam
        }
        return(col)
    })
    # Bind columns to matrix
    x <- do.call(cbind, x)
    # Convert to data.frame
    x <- as.data.frame(x)
    return(x)
}

###

.parse_coverage <- function(res, tables, datatype = "seasons", ...){
    # Loop over leagues
    temp <- lapply(res, function(temp){
        temp_res <- list()
        # Convert to df
        temp <- do.call(rbind, temp)
        temp <- as.data.frame(temp)
        # Get those values that are not nested anymore.
        ind <- lapply(temp, function(x) all(lengths(x) <= 1))
        ind <- unlist(ind)
        # Save those columns as coverage df
        temp_tab <- temp[, ind, drop = FALSE]
        temp_tab <- as.data.frame(temp_tab)
        # Add league and season id to table
        temp_res[[ datatype ]] <- temp_tab

        # Get those values that are nested and convert to df
        temp <- temp[, !ind]
        temp <- do.call(rbind, temp)
        temp <- as.data.frame(temp)
        # Get those values that are not nested anymore
        ind2 <- lapply(temp, function(x) all(lengths(x) <= 1))
        ind2 <- unlist(ind2)
        # Add them to result list
        temp_tab <- temp[, ind2, drop = FALSE]
        temp_tab <- as.data.frame(temp_tab)
        temp_res[[ names(ind)[ !ind ] ]] <- temp_tab

        # Get rest of the values
        temp <- temp[, !ind2]
        temp <- do.call(rbind, temp)
        # Convert to df. This function converts also nested information to flat.
        # This is to ensure if there was nested data at this point --> get all
        # the data.
        temp_tab <- .bind_columns_to_dataframe(temp)
        temp_res[[ names(ind2)[ !ind2 ] ]] <- temp_tab

        return(temp_res)
    })
    # Add league ids for season table
    rep_times <- lapply(temp, function(x) nrow(x[[datatype]]))
    league_id <- rep(tables[["league"]][["id"]], rep_times)

    # Get the datatypes
    datatypes <- names(temp[[1]])

    temp_full <- list()
    # Loop over leagues and combine the data to one table
    for( d in temp ){
        # Loop over data types
        for( dtype in datatypes ){
            # Combine data
            temp_full[[dtype]] <- rbind(temp_full[[dtype]], d[[dtype]])
        }
    }

    # Add league and season id to season table
    temp_full[["seasons"]][["league_id"]] <- league_id
    temp_full[["seasons"]][["id"]] <- paste0(
        temp_full[["seasons"]][["league_id"]], temp_full[["seasons"]][["year"]])

    return(temp_full)
}

###

.require_package <- function(pkg){
    if(!requireNamespace(pkg, quietly = TRUE)){
        stop(
            "'",pkg,"' package not found. Please install the '",
            pkg, "' package ", "to use this function.", call. = FALSE)
    }
    library(pkg, character.only = TRUE)
}

###

# Compress database into zip file
#' @importFrom utils zip
compressDatabase <- function(
        dir = "data", exdir = "archive", zipfile = dir, chunk.size = 49L, ...){
    #
    if( !(length(dir) == 1 && is.character(dir) && dir.exists(dir)) ){
        stop("Directory specified by 'dir' do not exist.", call. = FALSE)
    }
    #
    if( !(length(exdir) == 1 && is.character(exdir)) ){
        stop("'exdir' must be a single character value.", call. = FALSE)
    }
    #
    if( !(length(zipfile) == 1 && is.character(zipfile)) ){
        stop("'zipfile' must be a single character value.", call. = FALSE)
    }
    #
    if( !(length(chunk.size) == 1 && is.integer(chunk.size)) ){
        stop("'chunk.size' must be a single integer value.", call. = FALSE)
    }
    # Create dir if it not existing
    if( !dir.exists(exdir) ){
        dir.create(exdir, recursive = TRUE)
    }
    # Remove files from zip directory so that new ones can be stored
    rm_files <- list.files(exdir, full.names = TRUE)
    ind <- grepl(paste0( strsplit(zipfile, "\\.")[[1]][[1]], "."), rm_files)
    ind2 <- grepl(".zip$|\\.z(\\d{1,2}|100)$", rm_files)
    rm_files <- rm_files[ ind & ind2 ]
    if( length(rm_files) > 0 ){
        temp <- file.remove(rm_files)
    }
    # Get files that are stored to zip file
    files <- list.files(dir, full.names = TRUE, recursive = TRUE)
    zipfile <- file.path(exdir, zipfile)
    # Create zip in chunks
    flags <- paste0("-q -9 -s ", chunk.size, "m")
    zip(zipfile = zipfile, files = files, flags = flags)
    return(TRUE)
}

###

# Uncompress compressed database file
#' @importFrom utils unzip
uncompressDatabase <- function(
        zipfile = "archive/data.zip", exdir = "archive", overwrite = FALSE) {
    #
    if( !(length(zipfile) == 1 && is.character(zipfile) && file.exists(
        zipfile)) ){
        stop("Zip file specified by 'zipfile' do not exist.", call. = FALSE)
    }
    #
    if( !(length(exdir) == 1 && is.character(exdir)) ){
        stop("'exdir' must be a single character value.", call. = FALSE)
    }
    #
    if( !(is.logical(overwrite) && length(overwrite) == 1) ){
        stop("'overwrite' must be TRUE or FALSE.", call. = FALSE)
    }
    # Create dir if it not existing
    if( !dir.exists(exdir) ){
        dir.create(exdir, recursive = TRUE)
    }
    # Unzip with 7zip
    unzip_command <- paste0("7z ", "x ", zipfile, " -o", exdir)
    # Overwrite or not
    if( overwrite ){
        unzip_command <- paste0(unzip_command, " -y")
    } else{
        unzip_command <- paste0(unzip_command, " -aos")
    }
    system(unzip_command)

    return(TRUE)
}

###

# Get counties list and it to package data
.get_country_table <- function(){
    .require_package("rvest")
    .require_package("countrycode")

    # Get continents from wikipedia
    url <- "https://en.wikipedia.org/wiki/Continent"
    page <- read_html(url)
    tab = html_node(page, ".wikitable")
    tab = html_table(tab, fill = TRUE)
    # Get continents
    tab <- tab[ tab[["Number"]] == "Seven", colnames(tab) %in% c(
        "Continents"), drop = FALSE]
    tab <- t(tab)
    tab <- as.data.frame(tab)
    colnames(tab) <- "continents"
    rownames(tab) <- NULL

    # Add certain countries manually
    add_countries <- c(
        "World", "USA", "Czech Republic", "North & Central America")
    # Get countries from countrycode package
    countries <- c(
        codelist[["country.name.en"]], tab[["continents"]], add_countries)
    countries <- countries[ !countries %in% c("Unknown") ]

    countries <- countries[ !duplicated(countries) ]
    df <- data.frame(country = countries)

    # Save the file
    path <- system.file("extdata", "countries.csv", package = "esas")
    write.csv(df, path)
    return(df)
}

###

# Read the country table from package data
.read_country_table <- function(){
    path <- system.file("extdata", "countries.csv", package = "esas")
    if( !file.exists(path) ){
        stop(
            "There is no country table. Run esas:::.get_country_table().",
            call. = FALSE)
    }
    df <- read.csv(path)
    return(df)
}

###

.capitalize <- function(word){
    if( grepl("esport", word, ignore.case = TRUE) ){
        word <- "eSport"
    } else{
        word <- paste0(
            toupper(substr(word, 1, 1)), substr(word, 2, nchar(word)))
    }
    return(word)
}

###

.rename_cols <- function(df, cols, new_cols){
    names(cols) <- new_cols
    cols <- cols[ match(colnames(df), cols) ]
    cols[ is.na(cols) ] <- names(cols)[ is.na(cols) ] <-
        colnames(df)[ is.na(cols) ]
    colnames(df) <- names(cols)
    return(df)
}

###

.can_be_numeric <- function(x){
    !is.na(suppressWarnings(as.numeric(x)))
}
