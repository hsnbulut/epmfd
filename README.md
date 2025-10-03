# epmfd: package-easy person misfit detection

`epmfd` (Easy person misfit detection) wraps a practical workflow to:

1.  Load polytomous item responses.
2.  Scale items via MIRT (graded response) or non-parametric Mokken, dropping weak items automatically.
3.  Compute three robust person-fit statistics (lpz, Gp, U3p).
4.  Flag and remove mis-fitting persons in one line.
5.  Visualise each step and export clean data for further modelling.

## How to install

``` r
remotes::install_github("hsnbulut/epmfd")

# or

devtools::install_github("hsnbulut/epmfd")
```

# Quick Start

``` r
library(epmfd)
library(mirt)

# simulate data
m <- 12; K <- 5
set.seed(123)
a <- c(runif(m - 1, .8, 1.2), 0.2)
d <- matrix(
  sort(runif(m * (K - 1), -2, 2)),
  nrow = m, ncol = K - 1, byrow = TRUE)
n_norm <- 190
theta  <- rnorm(n_norm)

# normal observations 
normal <- mirt::simdata(
  a        = a,
  d        = d,
  itemtype = "graded",
  Theta    = matrix(theta))
# bad observations
n_bad <- 10
bad   <- matrix(
  sample(1:K, n_bad * m, replace = TRUE),
  nrow = n_bad)

# all data in toy
toy    <- as.data.frame(rbind(normal, bad))
names(toy) <- paste0("Item", 1:m)

#### Analysis with the package epmfd

# load data
raw    <- load_epmfd(toy)
# scaling data
scaled <- scale_epmfd(raw, method="auto")
print(scaled)
plot(scaled)

# mis fit 
misfit <- misfit_epmfd(scaled, alpha=.05)
print(misfit)
plot(misfit)

# scatter plot for 2 or bubble plot for 3 mis-fit statistics
plot_misfit_scatter(misfit,x_stat = "lpz",
                    y_stat = "U3p",
                    z_stat="Gp")

# cleaning data
clean  <- clean_epmfd(misfit)
print(clean)
plot(clean)

# export results 
export_epmfd(clean,format = "xlsx",save_rds = FALSE)
export_epmfd(misfit,format = "csv",save_rds = FALSE)
export_epmfd(scaled,format = "xlsx")
```

# Item Selection: Customising the Thresholds

By default, scale_epmfd() automatically removes weak items based on the selected method:

-   If method = "mirt" (graded response model), items with discrimination parameter a \< 0.5 are removed.

-   If method = "mokken" (non-parametric scaling), items with scalability H_i \< 0.3 are removed.

You can customise these thresholds using the arguments a_thr and H_thr:

```{r}
library(epmfd)
library(mirt)
library(mokken)
scale_epmfd(raw, method = "mirt", a_thr = 0.6)         # for MIRT
scale_epmfd(raw, method = "mokken", H_thr = 0.35)      # for Mokken
```

To keep all items, simply set the threshold to zero:

```{r}
library(epmfd)
library(mirt)
library(mokken)
scale_epmfd(raw, method = "mirt", a_thr = 0)
scale_epmfd(raw, method = "mokken", H_thr = 0)
```

When method = "auto" (the default), the function uses:

-   "mokken" if the sample size is under 100,

-   "mirt" otherwise.

Even in auto mode, the thresholds can be overridden:

```{r}
library(epmfd)
library(mirt)
library(mokken)
scale_epmfd(raw, method = "auto", a_thr = 0.6, H_thr = 0.35)
```

**Note:** An item may be removed in `"mirt"` mode but retained in `"mokken"` mode, or vice versa, because the selection criteria differ between methods.

# License

GPL-3 © 2025 Hasan Bulut & Asiye Şengül Avşar
