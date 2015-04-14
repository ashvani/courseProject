linRegSim <- function(x, beta0, beta1, std) {
    N <- length(x)
    set.seed(100)
    errorTerm <- rnorm(N, mean = 0, sd = std)
    linComp <- beta0 + beta1*x
    y <- linComp + errorTerm
    res <- data.frame(x, linComp, errorTerm, y)
    return(res)
}
