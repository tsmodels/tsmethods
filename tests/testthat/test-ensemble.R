test_that("ensemble_test", {
    set.seed(200)
    d1 <- matrix(rnorm(2000 * 10, mean = 1, sd = 0.5), ncol = 10, nrow = 2000)
    d2 <- matrix(rnorm(2000 * 10, mean = 4, sd = 2), ncol = 10, nrow = 2000)
    dates <- as.Date(1:10, origin = "1970-01-01")
    colnames(d1) <- colnames(d2) <- as.character(dates)
    class(d1) <- class(d2) <- "tsmodel.distribution"
    spec <- ensemble_modelspec(d1, d2)
    ens <- tsensemble(spec, weights = c(0.5, 0.5))
    expected_means <- 0.5 * 1 + 0.5 * 4
    expect_equal(mean(colMeans(ens)), mean(expected_means), tolerance = 0.1)
})

test_that("ensemble_class", {
    set.seed(200)
    d1 <- matrix(rnorm(200 * 10, mean = 1, sd = 0.5), ncol = 10, nrow = 200)
    d2 <- matrix(rnorm(200 * 10, mean = 4, sd = 2), ncol = 10, nrow = 200)
    dates <- as.Date(1:10, origin = "1970-01-01")
    colnames(d1) <- colnames(d2) <- as.character(dates)
    class(d1) <- class(d2) <- "tsmodel.distribution"
    spec <- ensemble_modelspec(d1, d2)
    ens <- tsensemble(spec, weights = c(0.5, 0.5))
    expect_s3_class(ens, 'tsmodel.distribution')
})


test_that("growth_test", {
    d1 <- matrix(1:10, ncol = 10, nrow = 20, byrow = TRUE)
    dates <- as.Date(1:20, origin = "1970-01-01")
    colnames(d1) <- as.character(dates[11:20])
    class(d1) <- "tsmodel.distribution"
    L <- list()
    L$original_series <- xts(rep(0,10), dates[1:10])
    L$distribution <- d1
    class(L) <- "tsmodel.predict"
    out <- tsgrowth(L,  d = 1, type = "diff")
    expect_true(all(abs(out$distribution == 1)))
})


