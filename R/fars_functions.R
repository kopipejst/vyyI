#' Reading and parsing csv
#'
#' It will check if file exist, try to parse as csv, and it will return
#' data as data.frame
#' In case that file doesn't exist it will return error. If file can't be
#' parsed as csv error will be thrown.
#'
#' @param filename path to a data file
#' @return data.frame
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}

#' Generates file name based on year parameter
#'
#' If year parameter can't be converted to integer error will be thrown.
#'
#' @param year in four digits
#' @return string
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' Add year variable to data, and return month, year list
#'
#' It will add year variable for all requested years data, and return a list.
#' In case that there's no data file for a year, warrning will be returned.
#' Data files has to be in the same directory as this script.
#'
#' @param years a vector of years
#' @return list
#' @import dplyr
#' @export
fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate(dat, year = year) %>%
                                dplyr::select(MONTH, year)
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' Get count of records for each year by month
#'
#' For each requested year it will calculate count of records. Results for all
#' years and months will be returned as data.frame
#' In case that data from one of years doesn't exist warning will be returned
#' for that year only.
#'
#' @param years a vector of years
#' @return data.frame
#' @import dplyr
#' @import tidyr
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}

#' Make chart with plots of accidents locations in some state for a year
#'
#' If state number doesn't exist error will be thrown.
#' If there's no accidents in state for year message will be returned
#' Otherwise map of state will accident locations will be created.
#' All locations has to be within boundaries.
#'
#' @param state.num integer
#' @param year in four digits
#' @return plot chart
#' @import dplyr
#' @import maps
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
