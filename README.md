# epmfd: package-easy person misfit detection

`epmfd` (Easy person misfit detection) wraps a practical workflow to:

1.  Load polytomous item responses.\
2.  Scale items via MIRT (graded response) or non-parametric Mokken, dropping weak items automatically.\
3.  Compute three robust person-fit statistics (lpz, Gp, U3p).\
4.  Flag and remove mis-fitting persons in one line.\
5.  Visualise each step and export clean data for further modelling.

## How to install

``` r
remotes::install_github("hsnbulut/epmfd")
```

# Quick Start

```r
library(epmfd)
library(mirt)
set.seed(42)

# simulate data
m <- 12; K <- 5
a <- c(runif(m-1, .8, 1.2), .2)
d <- matrix(sort(runif(m*(K-1), -1.5, 1.5)), m, K-1, byrow=TRUE)
normal <- simdata(a, d, "graded", Theta = matrix(rnorm(195)))
bad    <- matrix(sample(1:K, 5*m, TRUE), 5, m)
toy    <- as.data.frame(rbind(normal, bad))
names(toy) <- paste0("Item", 1:m)

# pipeline
raw    <- load_epmfd(toy)
scaled <- scale_epmfd(raw, method="auto")
misfit <- misfit_epmfd(scaled, alpha=.05)
clean  <- clean_epmfd(misfit)
plot(clean)
```

# License

GPL-3 © 2025 Hasan Bulut & Asiye Şengül Avşar
