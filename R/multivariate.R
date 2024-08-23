#' Multiple Distributions 
#'
#' @description validates and returns an object of class \sQuote{tsmodel.distribution_list} 
#' which holds a validated list of tsmodel.distribution objects for use in 
#' multivariate models.
#' @param distributions a list with \sQuote{tsmodel.distribution} objects.
#' @param names an optional vector of names for each slot in the list.
#' @returns an object of class \sQuote{tsmodel.distribution_list}
#' @details
#' The function will validate the distributions passed as belonging to class
#' \sQuote{tsmodel.distribution}, check whether they are similar in terms
#' of number of draws (rows), horizon (columns) and dates. If the list is not
#' named, then unless the \dQuote{names} argument is passed, then will name
#' the slots as series1, series2, etc.
#' @rdname distribution_list
#' @aliases distribution_list
#' @examples
#' x1 <- matrix(rnorm(100), 10, 10)
#' colnames(x1) <- as.character(as.Date(1:10, origin = "1970-01-01"))
#' x2 <- matrix(rnorm(100), 10, 10)
#' colnames(x2) <- as.character(as.Date(1:10, origin = "1970-01-01"))
#' class(x1) <- class(x2) <- "tsmodel.distribution"
#' distributions <- list(s1 = x1, s2 = x2)
#' L <- distribution_list(distributions)
#' str(L)
#' @export
#'
#
distribution_list <- function(distributions = NULL, names = NULL)
{
    # check class
    m <- length(distributions)
    check_class <- sapply(1:m, function(i) {
        is(distributions[[i]], 'tsmodel.distribution')
    })
    if (any(!check_class)) {
        flag <- which(!check_class)
        stop(paste0("\nsome of the objects in the list not of class tsmodel.distribution : ", flag))
    }
    # check rows (draws)
    check_rows <- sapply(1:m, function(i) {
        NROW(distributions[[i]])
    })
    sim_rows_check <- .check_equal(check_rows)
    if (sim_rows_check) equal_draws <- TRUE else equal_draws <- FALSE
    
    # check columns
    check_cols <- sapply(1:m, function(i) {
        NCOL(distributions[[i]])
    })
    sim_cols_check <- .check_equal(check_cols)
    if (sim_cols_check) equal_cols <- TRUE else equal_cols <- FALSE
    
    if (sim_cols_check) {
        # check dates
        date_ref <- colnames(distributions[[1]])
        check_dates <- sapply(2:m, function(i) {
            all.equal(date_ref, colnames(distributions[[i]]))
        })
        if (all(check_dates)) equal_horizon <- TRUE else equal_horizon <- FALSE
    } else {
        equal_horizon <- FALSE
    }
    if (is.null(names(distributions))) {
        if (is.null(names)) {
            names(distributions) <- paste0("series",1:m)
        } else {
            if (length(names) != m) stop("\nlength of names not equal to length of distributions")
            names(distributions) <- names
        }
    }
    
    attr(distributions,'equal_cols') <- equal_cols
    attr(distributions,'equal_horizon') <- equal_horizon
    attr(distributions,'equal_draws') <- equal_draws
    class(distributions) <- "tsmodel.distribution_list"
    return(distributions)
}

.check_equal <- function(x)
{
    length(unique(x)) == 1
}