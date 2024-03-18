#' Ensemble specification
#'
#' @description Creates and validates an ensemble specification.
#' @param ... objects of either all of class \dQuote{tsmodel.predict} or 
#' \dQuote{tsmodel.distribution} representing the probabilistic forecasts for 
#' the same horizon of optionally different models on the same series and with 
#' same number of draws. It is expected that the predictive distributions are 
#' based on joint simulated draws passed to the \code{innov} argument in 
#' the \code{predict} function of the supporting models. Instead of \ldots 
#' it is also possible to pass a list of the objects.
#' @returns An object of class \dQuote{ensemble.spec}.
#' @aliases ensemble_modelspec
#' @rdname ensemble_modelspec
#' @export
#'
#'
ensemble_modelspec <- function(...)
{
    m <- list(...)
    # if its already a list
    if (length(m) == 1) {
        if (is.list(m[[1]])) {
            m <- m[[1]]
        }
    }
    object_class <- sapply(1:length(m), function(i) tail(class(m[[i]]),1))
    if (length(unique(object_class)) != 1) stop("\nThe object must have the same class (tsmodel.predict or tsmodel.distribution)")
    if (!object_class[1] %in% c("tsmodel.predict","tsmodel.distribution")) stop("\nThe object must be of class tsmodel.predict or tsmodel.distribution")
    object_class <- object_class[1]
    if (object_class == "tsmodel.predict") {
        original_series <- do.call(cbind, lapply(1:length(m), function(i) m[[i]]$original_series))
    } else {
        original_series <- NULL
    }
    n <- length(m)
    if (object_class == "tsmodel.predict") {
        horizon <- NCOL(m[[1]]$distribution)
        forc_dates <- colnames(m[[1]]$distribution)
        sims <- sapply(1:length(m), function(i) NROW(m[[i]]$distribution))
        if (length(unique(sims)) != 1) stop("\nThe simulated distributions must have the same number of draws (rows)")
        # check horizon match
        check <- sapply(2:length(m), function(i){
            all.equal(forc_dates, colnames(m[[i]]$distribution))
        })
        if (!all(check)) stop("\nhorizon and forecast dates of each distribution must match exactly.")
        M <- array(0, dim = c(dim(m[[1]]$distribution), length(m)))
        for (i in 1:length(m)) M[,,i] <- as.matrix(m[[i]]$distribution)
    } else {
        horizon <- NCOL(m[[1]])
        forc_dates <- colnames(m[[1]])
        sims <- sapply(1:length(m), function(i) NROW(m[[i]]))
        if (length(unique(sims)) != 1) stop("\nThe simulated distributions must have the same number of draws (rows)")
        # check horizon match
        check <- sapply(2:length(m), function(i){
            all.equal(forc_dates, colnames(m[[i]]))
        })
        if (!all(check)) stop("\nhorizon and forecast dates of each distribution must match exactly.")
        M <- array(0, dim = c(dim(m[[1]]), length(m)))
        for (i in 1:length(m)) M[,,i] <- as.matrix(m[[i]])
    }
    out <- list(distribution = M, original_series = original_series, object_class = object_class, forc_dates = forc_dates)
    class(out) <- "ensemble.spec"
    return(out)
}

#' @method tsensemble ensemble.spec
#' @details Returns the weighted distribution, under the assumption that the 
#' predictions were generated using a joint error distribution whose values were 
#' passed to the \code{innov} argument of the predict function used for each model.
#' @returns An object of class \dQuote{tsmodel.predict} or \dQuote{tsmodel.distribution} 
#' depending on the input class in \code{\link{ensemble_modelspec}}.
#' @rdname tsensemble
#' @export
#'
#
tsensemble.ensemble.spec <- function(object, weights = NULL, ...)
{
    n <- dim(object$distribution)[3]
    if (is.null(weights)) {
        weights <- matrix(rep(1/n, n), ncol = 1)
        warning("\nweights are NULL. Setting to equal weight ensemble")
    } else {
        if (length(as.numeric(weights)) != n) stop("\nlength of weights must be equal to number of distributions passed.")
        weights <- matrix(weights, ncol = 1)
    }
    d <- t(apply(object$distribution, 1, function(x) x %*% weights))
    colnames(d) <- object$forc_dates
    class(d) <- "tsmodel.distribution"
    # ensemble the original series if present
    if (object$object_class == "tsmodel.predict") {
        ind1 <- index(object$original_series)
        original_series <- xts(rowSums(coredata(object$original_series) * matrix(weights, ncol = n, nrow = nrow(object$original_series), byrow = TRUE), na.rm = TRUE), ind1)
        L <- list(distribution = d, original_series = original_series, mean = zoo(colMeans(d), as.Date(object$forc_dates)))
        class(L) <- "tsmodel.predict"
    } else {
        L <- d
        class(L) <- "tsmodel.distribution"
    }
    return(L)
}


#' @param object an object.
#' @param d the period back to look at for growth calculations.
#' @param type the type of growth calculation. \dQuote{diff} is simply the 
#' difference in values over n periods, \dQuote{simple} if the 
#' rate of change and \dQuote{log} the difference in logs.
#' @param ... additional parameters passed to the method.
#' @returns an object of class \dQuote{tsmodel.predict} transformed to a growth distribution.
#' @method tsgrowth tsmodel.predict
#' @rdname tsgrowth
#' @export
#'
#
tsgrowth.tsmodel.predict <- function(object, d = 1, type = c("diff","simple","log"), ...)
{
    Z <- matrix(coredata(object$original_series), ncol = length(object$original_series), nrow = nrow(object$distribution), byrow = TRUE)
    colnames(Z) <- as.character(index(object$original_series))
    G <- cbind(Z, as.matrix(object$distribution))
    if (type == "diff") {
        P <- G[,(d + 1):ncol(G)] - G[,1:(ncol(G) - d)]
    } else if (type == "simple") {
        P <- G[,(d + 1):ncol(G)]/G[,1:(ncol(G) - d)] - 1
    } else if (type == "log") {
        P <- log(G)
        P <- P[,(d + 1):ncol(P)] - P[,1:(ncol(P) - d)]
    }
    N <- NCOL(object$distribution)
    PN <- NCOL(P)
    D <- P[,tail(1:PN,N)]
    class(D) <- "tsmodel.distribution"
    original_series <- zoo(P[1,1:(PN - N)], index(object$original_series)[-(1:d)])
    L <- list(original_series = original_series, distribution = D)
    class(L) <- "tsmodel.predict"
    return(L)
}