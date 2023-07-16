#' Rename columns of a dataframe using a dictionary
#'
#' @description Uses a dictionary either from a file or named list to rename the relevant columns of
#' a dataframe.
#'
#' @param df `dataframe` Dataframe to be renamed
#' @param dictionary `char` Named character vector where the names are the new column names and the 
#'   values are the old ones. 
#' @param dictionary_fn `char` String indicating the location of a file containing the renaming
#'   dictionary as a dataframe (for exmaple in an csv or xlsx). This dataframe is expected to have
#'   columns called "original_name" and "new_name", if it does not then the first two columns will
#'   be used as "original" and "new" respectively. If `dictionary` is provided, this argument will
#'   be ignored.
#' @param ... Additional arguments to be passed to [rio::import()] when loading `dictionary_fn`. 
#'   This could be useful, for example  if you have stored the renaming dictionaries for multiple
#'   datasets in a single xlsx where each sheet coresponds to a separate dictionary.
#'
#' @examples
#' df <- data.frame(fizz = c(1, 2, 3),
#'                  buzz = c('one', 'two', 'three'))
#' 
#' dictionary <- c(foo = 'fizz',
#'                 bar = 'buzz')
#' 
#' rename_from_dictionary(df, dictionary)
#' 
#' @importFrom rio import
#' @importFrom dplyr rename
#' @export
rename_from_dictionary <- function(df, dictionary = NULL, dictionary_fn = NULL, ...) {
  if (is.null(dictionary) & is.null(dictionary_fn)) {
    stop('you must provide either a list of filename with the renaming dictionary to be used...')
  }

  if (!is.null(dictionary) & !is.null(dictionary_fn)) {
    warning('both `dictionary` and `dictionary_fn` provided...using `dictionary`.')
    dictionary_fn <- NULL
  }

  if (!is.null(dictionary_fn)) {
    dictionary_df <- rio::import(dictionary_fn,
                                 ...)

    if (!is.data.frame(dictionary_df)) {
      stop(paste0('`names_fn` must contain a dataframe but i found a ', class(dictionary_df),
                  'instead...'))
    }

    cols <- names(dictionary_df)
    if (!('original_name' %in% cols & 'new_name' %in% cols)) {
      cols[1] <- 'original_name'
      cols[2] <- 'new_name'
    }

    dictionary_df <- dictionary_df[ , c('new_name', 'original_name')]

    dictionary <- dictionary_df$original_name
    names(dictionary) <- dictionary_df$new_name
  }

  out <- dplyr::rename(df, !!dictionary)

  return(out)
}
    

