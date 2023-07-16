#' Identify Outbreaks
#' 
#' @description Returns a new version of df with two new columns: "outbreak" and "outbreak_id". The
#' the first is a bool indicating if an outbreak is present at that time point and the second is
#' an alphanumeric id with the format "{health-zone-name}_{number}" where the number indicates if it
#' is the first, second, etc outbreak identified (within the overall data) for that health zone. 
#' Arguments are available to play with the identification criteria being used. Please refer to 
#' the methods section of the final report for more information on how the identification algorithm
#' works.
#' 
#' @param df `dataframe` Historical data to be analyzed. note, this function has **strong** 
#'   expectations of column names.
#' @param min_weekly `int` Number of minimum cases expected per week during the 'main phase' of the
#'   outbreak. Note that gaps of up to `max_gap` in length are tolerated. 
#' @param min_size `int` Minimum number of cases required during the 'main phase' of the epidemic.
#' @param min_duration `int` Minimum duration (in weeks) required during the 'main phase' of the 
#'   epidemic.
#' @param max_gap `int` Maximum lull (in weeks) during which an epidemic can dip below the expected
#'   20 case per week threshold. Gaps longer than this period would indicate that there were two 
#'   seperate epidemics.
#' @param tail_threshold `int` Threshold of weekly cases needed for inclusion within the 'tails' of
#'   of the epidemic. 
#' @param tail_tolerance `int` Minimum number of weeks with an incidence below `tail_threshold` 
#'   required for the tails to end.
#'
#' @return df
#'
#' @importFrom dplyr filter mutate pull last
#' @importFrom zoo na.locf
#' @importFrom R.utils seqToIntervals
#' @importFrom rlang .data
#' 
#' @export
identify_outbreaks <- function(df, min_weekly = 20, min_size = 100, min_duration = 6, 
                               max_gap = 16, tail_threshold = 5, tail_tolerance = 4) {

  df <- df %>% 
          mutate(outbreak = NA,
                 outbreak_id = '',
                 finished = NA) 
  
  for (z in unique(df$zone)) {
    tmp <- df %>%
      filter(.data$zone == z) %>%
      mutate(outbreak = ifelse(.data$cases >= min_weekly, TRUE, NA),
             outbreak = zoo::na.locf(.data$outbreak,
                                     maxgap = max_gap - 1,
                                     na.rm = FALSE))

    # fix na.locf behavior on tails -- it will fill TRUE to the end if there is a TRUE <= max_gap
    # places before the end of the dataset
    if (!is.na(last(tmp$outbreak)) & last(tmp$cases) < min_weekly) {
      #epi_curve <- tidyr::replace_na(tmp$cases, 0)
      end_of_main <- purrr::detect_index(tmp$cases,
                                         function(value) value >= min_weekly,
                                         .dir = 'backward')

      tmp[(end_of_main + 1):nrow(tmp), 'outbreak'] <- NA
    }


    # build data frame with the start/end idexes for each observed outbreak
    outbreak_indices <- which(tmp$outbreak == 1) %>%
                        R.utils::seqToIntervals() %>%
                        as.data.frame()

    id <- 1

    if (nrow(outbreak_indices) > 0) {                      # if there are outbreaks, validate them

      for (i in 1:nrow(outbreak_indices)) {

        # determine start and end
        start <- outbreak_indices$from[[i]]
        end <- outbreak_indices$to[[i]]

        # check if i is part of an already identified multipeak outbreak, if so skip
        if (tmp$outbreak_id[start] != '') {                
          next                                             
        }                                                  

        # determine start and end of tails
        tail_start <- run_index(tmp[1:start, 'cases'],     
                                FUN = function(x) x < tail_threshold,
                                n = tail_tolerance,
                                direction = 'backward')
        tail_start <- tail_start + 1                       # tail starts first obs without < 5/w for a 
                                                           # full month

        tail_end <- run_index(tmp[end:nrow(tmp), 'cases'],
                              FUN = function(x) x < tail_threshold,
                              n = tail_tolerance,
                              direction = 'forward')
        tail_end <- ifelse(tail_end >= 0, end + tail_end - 1, end)  # tail ends last obs before a month of < 5/w

        if (end - start < (min_duration - 1) |                  # require outbreak length 6+ weeks
            sum(tmp$cases[start:end]) < min_size) {             # require 100+ cases total
          tmp$outbreak[start:end] <- NA
          real <- FALSE

        } else {                                          # if outbreak is valid, id it [zone name]_# 
          tmp$outbreak_id[tail_start:tail_end] <- paste0(tmp$zone[[1]], '_', id)
          tmp$outbreak[tail_start:tail_end] <- TRUE
          tmp$finished[tail_start:tail_end] <- TRUE

          real <- TRUE
          id <- id + 1
        }

        if (real & (tail_end == nrow(tmp) |                 # remove incomplete outbreaks (no end)
            tail_start == 1)) {                             # remove incomplete outbreaks (no start)
          tmp$finished[tail_start:tail_end] <- FALSE

        }
      }
    }

    df[df$zone == z, ] <- tmp
  }

  return(df)
}

