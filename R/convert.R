#' Convert a distribution object to a long form data.table
#' @description
#' Converts an object of class \dQuote{tsmodel.distribution} or \dQuote{tsmodel.distribution_list}
#' to a long form data.table object.
#' @param object a \dQuote{tsmodel.distribution} or \dQuote{tsmodel.distribution_list} object.
#' @param to output format. Currently only \dQuote{data.table} supported.
#' @param name an optional string for the name of the series which will be
#' added to the table (only for the \dQuote{tsmodel.distribution}, as the list object
#' is already validated with names).
#' @param ... not currently used.
#' @returns a data.table object
#' @examples
#' x1 <- matrix(rnorm(100), 10, 10)
#' colnames(x1) <- as.character(as.Date(1:10, origin = "1970-01-01"))
#' class(x1) <- "tsmodel.distribution"
#' head(tsconvert(x1, name = "SeriesA"))
#' @method tsconvert tsmodel.distribution
#' @aliases tsconvert.tsmodel.distribution
#' @rdname tsconvert.tsmodel.distribution
#' @export
#' 
tsconvert.tsmodel.distribution <- function(object, to = "data.table", name = NULL, ...)
{
    variable <- draw <- NULL
    tmp <- as.data.table(unclass(object))
    d <- NCOL(tmp)
    tmp[,draw := 1:nrow(tmp)]
    out <- melt(tmp, id.vars = 'draw', measure.vars = 1:d, variable.name = "index",
                variable.factor = FALSE)
    if (!is.null(name)) {
        out[,variable := name]
    }
    return(out)
}

#' @method tsconvert tsmodel.distribution_list
#' @aliases tsconvert.tsmodel.distribution_list
#' @rdname tsconvert.tsmodel.distribution
#' @export
tsconvert.tsmodel.distribution_list <- function(object, to = "data.table", ...)
{
    names <- names(object)
    out <- lapply(1:length(object), function(i){
        return(tsconvert(object[[i]], name = names[i]))
    })
    out <- rbindlist(out)
   return(out)
}