plot.tsmodel.distribution <- function(x, y = NULL, median_color = "black", median_type = 1, median_width = 3,
                                   interval_quantiles = c(0.025, 0.975), gradient_color = "steelblue",
                                   interval_color = "cyan", interval_type = 2, interval_width = 2,
                                   ylim = NULL, ylab="", n_x = NCOL(x), x_axes=TRUE, add = FALSE,
                                   zero_out = FALSE, date_class = "Date", ...)
{
  N = NCOL(x)
  prediction <- list()
  prediction$distribution = x[,pmax(1,N - n_x):N]
  prediction$interval <- apply(prediction$distribution, 2, quantile, interval_quantiles)
  prediction$median <- apply(prediction$distribution, 2, median)
  qtl <- seq(interval_quantiles[1], interval_quantiles[2], by = 0.01)
  quantile_matrix <- t(apply(prediction$distribution, 2, quantile, probs = qtl, na.rm = TRUE))
  if (!is.null(attr(x, "date_class"))) {
    date_class <- attr(x, "date_class")
    date_form <- match.fun(paste0("as.", date_class))
  } else {
    date_form <- match.fun(paste0("as.", date_class))
  }
  if (is.null(ylim)) {
    ylim <- range(quantile_matrix)
  }
  if (is.null(colnames(prediction$distribution))) {
    dt <- 1:ncol(prediction$distribution)
  } else {
    dt <- date_form(colnames(prediction$distribution))
  }
  if (!add) {
    .plot.dynamic.distribution(curves = prediction$distribution, timestamps = dt, add = add, ylim = ylim, 
                               gradient_color = gradient_color, ylab = ylab, interval_quantiles = interval_quantiles, 
                               x_axes = x_axes, zero_out = zero_out, ...)
  }
  lines(dt, prediction$median, col = median_color, lty = median_type, lwd = median_width, ...)
  if (zero_out) {
    for (j in 1:nrow(prediction$interval)) {
      if (any(prediction$interval[j,] <= 0, na.rm = TRUE)) {
        ix <- which(prediction$interval[j,] <= 0)
        prediction$interval[j,ix] <- 0
      }
    }
  }
  for (i in 1:nrow(prediction$interval)) {
    lines(dt, prediction$interval[i, ], col = interval_color, lty = interval_type, lwd = interval_width, ...)
  }
  grid()
  return(invisible(NULL))
}


plot.tsmodel.predict <- function(x, y = NULL, plot_original = TRUE, median_color = "black", median_type = 1,
                              median_width = 3, interval_quantiles = c(0.025, 0.975),
                              gradient_color = "steelblue", interval_color = "cyan",
                              interval_type = 2, interval_width = 2, ylim = NULL, ylab = "",
                              n_original = NULL, x_axes = TRUE, zero_out = FALSE, ...)
{
  if (is.null(x$original_series)) {
    # dispatch to tsmodel.distribution method instead
      plot(x$distribution)
      return(invisible(NULL))
  }
  prediction <- x
  prediction$interval <- apply(prediction$distribution, 2, quantile, interval_quantiles)
  prediction$median   <- apply(prediction$distribution, 2, median)

  original_series <- prediction$original_series
  if (!is.null(n_original)) {
    original_series = tail(original_series, n_original)
  }
  if (is.numeric(plot_original)) {
    original_series <- tail(original_series, plot_original)
    plot_original <- TRUE
  }
  if (!is.null(attr(x$distribution, "date_class"))) {
    date_class <- attr(x$distribution, "date_class")
    date_form <- match.fun(paste0("as.", date_class))
  } else {
    if (class(index(original_series))[1] == "Date") {
      date_form <- as.Date
    } else if (class(index(original_series))[1] == "POSIXct") {
      date_form <- as.POSIXct
    } else{
      stop("\nunregognized date format")
    }
  }
  qtl <- seq(interval_quantiles[1], interval_quantiles[2], by = 0.01)
  quantile_matrix <- t(apply(prediction$distribution, 2, quantile, probs = qtl, na.rm = TRUE))

  time <- date_form(index(original_series))
  lasttimebeforeprediction = max(which(time < date_form(colnames(prediction$distribution))[1]))

  if (is.null(ylim)) {
    ylim <- range(quantile_matrix, original_series, na.rm = TRUE)
  }
  if (plot_original) {
    # Pre-append original series date
    pred_time <- c(time[lasttimebeforeprediction], date_form(colnames(prediction$distribution))) 
    if (!x_axes) xx <- "n" else xx <- "s"
    plot(time, as.numeric(original_series), type = "l", xlim = range(time, pred_time), ylim = ylim, ylab = ylab, xaxt = xx, ...)
  }
  else {
    pred_time <- date_form(colnames(prediction$distribution))
  }
  curve_dat <- prediction$distribution
  if (plot_original) {
    curve_dat <- cbind(as.numeric(original_series[lasttimebeforeprediction]), curve_dat)
    colnames(curve_dat)[1] <- as.character(time[lasttimebeforeprediction])
    prediction$median <- c(as.numeric(original_series[lasttimebeforeprediction]), prediction$median)
    names(prediction$median)[1] <- as.character(time[lasttimebeforeprediction])
    if (!is.null(prediction$mean)) {
      prediction$mean <- c(as.numeric(original_series[lasttimebeforeprediction]), prediction$mean)
      names(prediction$mean)[1] <- as.character(time[lasttimebeforeprediction])
    }
    prediction$interval <- cbind(as.numeric(original_series[lasttimebeforeprediction]), prediction$interval)
    colnames(prediction$interval)[1] <- as.character(time[lasttimebeforeprediction])
  }
  if (zero_out) {
    for (j in 1:nrow(prediction$interval)) {
      if (any(prediction$interval[j,] <= 0, na.rm = TRUE)) {
        ix = which(prediction$interval[j,] <= 0)
        prediction$interval[j,ix] <- 0
      }
    }
  }
  .plot.dynamic.distribution(curves = curve_dat, timestamps = pred_time, add = plot_original, 
                             ylim = ylim, gradient_color = gradient_color, ylab = ylab, 
                             interval_quantiles = interval_quantiles, x_axes = x_axes, 
                             zero_out = zero_out, ...)
  lines(pred_time, prediction$median, col = median_color, lty = median_type, lwd = median_width, ...)

  for (i in 1:nrow(prediction$interval)) {
    lines(pred_time, prediction$interval[i, ], col = interval_color, lty = interval_type, lwd = interval_width, ...)
  }

  if (!is.null(prediction$mean)) lines(pred_time, as.numeric(prediction$mean), col = transparent_color("violet"))
  if (plot_original) {
    lines(time, as.numeric(original_series), col = "red")
  }
  grid()
  return(invisible(NULL))
}

.plot.dynamic.distribution = function(curves, timestamps = NULL, quantile_step = 0.01, xlim = NULL, xlab = "Time", 
                                      ylim = range(curves, na.rm = TRUE), ylab = "distribution", 
                                      add = FALSE, x_axes = TRUE, gradient_color = "gray", 
                                      interval_quantiles = c(0.025, 0.975), zero_out = FALSE, ...)
{
  stopifnot(ncol(curves) >= 1)
  qtl <- seq(interval_quantiles[1], interval_quantiles[2], by = quantile_step)
  quantile_matrix <- t(apply(curves, 2, quantile, probs = qtl, na.rm = TRUE))
  nc <- ncol(quantile_matrix)
  number_of_quantile_steps <- (nc + 1)/2
  if (number_of_quantile_steps < 3) { stop("'quantile.step' is too large in PlotDynamicDistribution") }
  lower_quantile <- quantile_matrix[, 1]
  upper_quantile <- quantile_matrix[, nc]
  if (is.null(timestamps)) {
    timestamps <- tryCatch(as.POSIXct(colnames(curves)),error = function(e) {
      return(NULL)
    })
    if (is.null(timestamps)) {
      timestamps <- 1:ncol(curves)
    }
  }
  if (is.null(xlim)) {
    xlim <- range(timestamps)
  }
  if (gradient_color == "gray") {
    gcolor = gray(1 - 1/number_of_quantile_steps)
  } else {
    colfunc <- colorRampPalette(c(gradient_color,"white"))
    gcolor <- colfunc(200)[175:200]
  }
  if (zero_out) {
    if (any(lower_quantile <= 0, na.rm = TRUE)) {
      ix = which(lower_quantile <= 0)
      lower_quantile[ix] <- 0
    }
  }
  .filled.plot(timestamps, cbind(lower_quantile, upper_quantile), poly_color = gcolor, axes = FALSE, 
               add = add, xlim = xlim, xlab = xlab, ylim = ylim, ylab = ylab, ...)
  box()
  if (x_axes) {
    if (inherits(timestamps, "Date")) {
      axis.Date(1, timestamps, xpd = NA)
    } else if (inherits(timestamps, "POSIXt")) {
      axis.POSIXct(1, as.POSIXct(timestamps), xpd = NA)
    } else {
      axis(1, xpd = NA)
    }
  }
  axis(2, xpd = NA)
  for (i in 2:(number_of_quantile_steps - 1)) {
    if (gradient_color == "gray") {
      gcolor = gray(1 - i/number_of_quantile_steps)
    } else {
      colfunc <- colorRampPalette(c(gradient_color, "white"))
      gcolor <- colfunc(200)
      gcolor <- gcolor[125 + i]
    }
    lower_quantile <- quantile_matrix[, i]
    upper_quantile <- quantile_matrix[, nc + 1 - i]
    .filled.plot(timestamps, cbind(lower_quantile, upper_quantile), add = TRUE, poly_color = gcolor)
  }
  return(NULL)
}

.filled.plot <- function(timestamps, quantile_matrix, poly.color, add = FALSE, xlab, ylab, ylim, xlim, poly_color = NULL, ...) {
  ylo <- quantile_matrix[, 1]
  yhi <- quantile_matrix[, 2]
  stopifnot(length(timestamps) == nrow(quantile_matrix))
  if (any(yhi < ylo, na.rm = TRUE)) { warning("second column of quantile_matrix must be >= the first") }
  if (!add) {
    plot(timestamps, ylo, xlim = xlim, ylim = ylim, xlab = xlab, ylab = ylab, type = "n", ...)
  }
  observed_values <- !(is.na(ylo) | is.na(yhi))
  observed_rle <- rle(observed_values)
  number_of_contiguous_regions <- length(observed_rle$lengths)
  for (i in 1:number_of_contiguous_regions) {
    if (observed_rle$values[i]) {
      if (i == 1) {
        start <- 1
      } else {
        start <- sum(observed_rle$lengths[1:(i - 1)])
      }
      finish <- start + observed_rle$lengths[i] - 1
      index <- seq(start, finish)
      X <- c(timestamps[index], rev(timestamps[index]), timestamps[index][1])
      Y <- c(ylo[index], rev(yhi[index]), ylo[index][1])
      polygon(X, Y, border = NA, col = poly_color)
    }
  }
}

transparent_color <- function(col, alpha = 100)
{
  tcolor <- col2rgb(col)
  apply(tcolor, 2, function(x){
    rgb(red = x[1], green = x[2], blue = x[3], alpha = alpha, 
        maxColorValue = 255)
    })
}
