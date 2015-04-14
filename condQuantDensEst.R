densEst <- function(x, beta0, beta1, std, x1, kernel = "gaussian") {
    N <- length(x)
    errorTerm <- rnorm(N, 0, std)
    y <- beta0 + beta1 * x + errorTerm
    library(quantreg)
    tau <- 2:98 * .01
    modQR <- rq(y ~ 1 + x, tau = tau)
    tempData <- cbind(seq(1, 1, length = N), x)
    predCondQuant <- tempData %*% modQR$coefficients
    ind <- which(x == x1)
    newData <- predCondQuant[ind, ]
    quantDens <- density(newData, kernel = kernel)
    linDens <- density(rnorm(10000, beta0 + beta1 * x[ind], std), kernel = kernel)
    return(list(quantDens = quantDens, linDens = linDens))
}
