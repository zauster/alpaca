# alpaca

## Info
An R-package for fitting glm's with high-dimensional k-way fixed effects.

This is a preliminary R-package based on the working paper "Fast and Feasible Estimation of Generalized Linear Models with High-Dimensional k-way Fixed Effects" (Stammann, 2018), https://arxiv.org/abs/1707.01815. A special iteratively re-weighted least squares pseudo-demeaning algorithm is implemented, such that the estimation of glm's with high-dimensional fixed effects becomes feasible. 

This version of the package is able to estimate poisson, probit, and logit models with high-dimensional k-way fixed effects.

This package is well suited to estimate so called "pseudo poisson maximum likelihood" (PPML) models with high-dimensional fixed effects that are commonly used in the international trade literature (structural gravity models). See the empirical example in Stammann 2018.

If you have any suggestions for improvements or questions, feel free to contact me (Amrei.Stammann@hhu.de).

The package can be installed by `devtools::install_github("amrei-stammann/alpaca", build_vignettes = TRUE)`.


## News

### alpaca v0.1.2 (Release Date: 2018-03-04)

Changes:

* `factor()` should now work as intended.

### alpaca v0.1.1 (Release Date: 2018-01-21)

Changes:

* added option `"probit"` to argument `family`.
* some performance tweaks.
* extract names of the fixed effects and `getFEs()` returns a named vector.
* adjusted computation of starting values.
* computation of the update step (irls) made numerically more stable.


Bugfix:

* construction of the regressor matrix such that factor variables are correctly dummy encoded.
* dropping perfectly classified observations for binomial models should now work as intended (thanks to jmboehm@github).
