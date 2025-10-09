# epmfd 1.0.6

-   Add references.
-   Fixed problems in the clean_epmfd() function.
-   Fixed problems in the misfit_epmfd() function.

# epmfd 1.0.5

-   Add `Depends: R (>= 4.1.0)` to satisfy usage of base pipe and lambda syntax.
-   Move example `.sav` file to `inst/extdata/` and ignore build artefacts.
-   Add `inst/WORDLIST` for technical terms (GRM, Mokken, mirt, misfit).
-   Minor DESCRIPTION cleanup.

# epmfd 1.0.4

-   plot_misfit function has been generated. plot_misfit_scatter function has been removed.

# epmfd 1.0.3

# epmfd 1.0.2

# epmfd 1.0.1

# epmfd 0.4.1

-   **Fixed `plot(…)` for Mokken mode:** the histogram now correctly uses `aes(x = x$Hi)` so that `plot(scaled)` no longer throws a “PANEL size” error when `method = "mokken"`.

-   **Robustness against failed GRM fits**: If the MIRT model did not converge, `lpz` is skipped automatically and the user is warned. This prevents invalid inferences from unstable models.

-   **New `drop_constant_persons` argument in `scale_epmfd()`**: If `drop_constant_persons = TRUE`, persons who gave the same score to all items (e.g., all 1s or all 5s) are automatically removed **before scaling**. This avoids model estimation failures in `mirt()` or `mokken::coefH()` due to non-informative response patterns. Default is `TRUE`, but users can set it to `FALSE` to keep such respondents (not recommended).

# epmfd 0.4.0

# epmfd 0.2.8

# epmfd 0.2.7

# epmfd 0.2.6

# epmfd 0.2.5

# epmfd 0.2.4

# epmfd 0.2.3

# epmfd 0.2.2

# epmfd 0.2.1

# epmfd 0.2.0

-   Created the package "epmfd" version 0.2.0
