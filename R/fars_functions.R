#' Reading a FARS csv data set
#'
#' This function reads a csv file (provided as the argument "filename") and
#' returns the data as a \code{\link[dplyr]{tbl_df}} object. The intention
#' of the function is to be used to read in a FARS data set.
#'
#' @param filename name of the csv file from which FARS data should be read
#'
#' @examples
#' data1 <- fars_read("accident_2015.csv.bz2")
#'
#' @return return a tbl_df with the FARS data of the input file
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}



#' Creating FARS data file name from year
#'
#' This function takes a year as input and returns the file name
#' of the file containing FARS data for that particular year.
#'
#' @param year the year for which to create the file name. Can be
#' either integer or character
#'
#' @return a character string with the file name of the FARS data
#' for the given year.
#'
#' @examples
#' filename1 <- make_filename(2015)
#' filename2 <- make_filename("2014")
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}



#' Read in FARS data for multiple years
#'
#' This function takes a vector of years and return a list of
#' data frames with month and years for the FARS data for the
#' input vector of years
#'
#' @param years a vector of years (as integers or characters)
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' datalist <- fars_read_years(list(2013, 2014))
#'
#' @return a list of tibble data frames, one for each of
#' the specified years. Each of the data frames contains
#' only the columns MONTH and year.
#'
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                          dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year, e)
                        return(NULL)
                })
        })
}



#' Summarizing FARS data
#'
#' This function take a vector of years and returns a summary
#' of how many data point that are for each year and month
#' in the given range of years.
#'
#' @inheritParams fars_read_years
#'
#' @importFrom dplyr %>%
#'
#' @examples
#' summary <- fars_summarize_years(list(2013, 2014))
#'
#' @return a tibble data frame with a column for each
#' input year and an additional column "MONTH".
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}



#' Plot state accidents for a given year
#'
#' This function take a state (as a numeric value) and a year
#' and plots the accident that happens that year in that state.
#'
#' @param year the year for which to plot accidents
#' @param state.num number indicating for which state to plot accidents
#'
#' @importFrom maps stateMapEnv
#'
#' @examples
#' fars_map_state(6, 2013)
#'
#' @return Plots the different accidents on a state map
#' for the given state and year
#'
#' @export
fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter(data, STATE == state.num)
        if(nrow(data.sub) == 0L) {
                message("no accidents to plot")
                return(invisible(NULL))
        }
        is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
        is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
        with(data.sub, {
                maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                          xlim = range(LONGITUD, na.rm = TRUE))
                graphics::points(LONGITUD, LATITUDE, pch = 46)
        })
}
