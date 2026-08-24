set.seed(17)
nq <- 6000
dq <- data.frame(x = runif(nq), Exposure = round(runif(nq, .3, 1), 3))
dq$AantalClaims <- rpois(nq, dq$Exposure * exp(-2.0 + 1.1 * dq$x))
mq_pois <- glm(AantalClaims ~ x + offset(log(Exposure)), poisson(), dq)

dqs <- data.frame(x = runif(4000), w = 1 + rpois(4000, 1.5))
mu_q <- exp(7 + .5 * dqs$x)
dqs$y <- vapply(seq_len(4000), function(i)
  mean(rgamma(dqs$w[i], 2.5, scale = mu_q[i] / 2.5)), numeric(1))
mq_gam <- glm(y ~ x, Gamma("log"), dqs, weights = w)

test_that("quantile residuals are standard normal on a correct model", {
  # the whole point: for counts, deviance residuals are not normal even
  # when the model is right, so the classic Q-Q condemns a correct fit
  set.seed(2)
  r <- .quantile_residuals(mq_pois, "t")
  expect_equal(mean(r), 0, tolerance = 0.05)
  expect_equal(sd(r), 1, tolerance = 0.05)
  expect_gt(stats::ks.test(r, "pnorm")$p.value, 0.01)
  expect_lt(abs(mean(abs(r) > 1.96) - 0.05), 0.015)

  # deviance residuals on the same data do not manage that
  dv <- as.numeric(scale(residuals(mq_pois, type = "deviance")))
  expect_lt(stats::ks.test(dv, "pnorm")$p.value, 1e-6)

  # Gamma too, and there the response is continuous so no randomisation
  rg <- .quantile_residuals(mq_gam, "t")
  expect_equal(sd(rg), 1, tolerance = 0.05)
  expect_gt(stats::ks.test(rg, "pnorm")$p.value, 0.01)
  expect_identical(.quantile_residuals(mq_gam, "t"), rg)   # deterministic
})

test_that("quantile residuals match statmod, the reference implementation", {
  skip_if_not_installed("statmod")
  set.seed(11); mine <- .quantile_residuals(mq_pois, "t")
  set.seed(11); ref  <- as.numeric(statmod::qresid(mq_pois))
  expect_equal(mine, ref, tolerance = 1e-10)
  expect_equal(.quantile_residuals(mq_gam, "t"),
               as.numeric(statmod::qresid(mq_gam)), tolerance = 1e-10)
})

test_that("the Q-Q plot flags a distribution that is genuinely wrong", {
  # a Poisson fitted to overdispersed counts must show fatter tails
  set.seed(4)
  dq$Over <- rnbinom(nq, mu = dq$Exposure * exp(-1.2 + 1.1 * dq$x), size = .4)
  m_over <- glm(Over ~ x + offset(log(Exposure)), poisson(), dq)
  r_ok   <- .quantile_residuals(mq_pois, "t")
  r_bad  <- .quantile_residuals(m_over, "t")
  expect_gt(sd(r_bad), sd(r_ok))
  expect_gt(mean(abs(r_bad) > 1.96), mean(abs(r_ok) > 1.96))
})

test_that("plot_glm_qq returns a plotly object and its residuals", {
  p <- plot_glm_qq(mq_pois, seed = 1)
  expect_s3_class(p, "plotly")
  expect_identical(attr(p, "type"), "quantile")
  expect_length(attr(p, "residuals"), nq)
  expect_s3_class(plot_glm_qq(mq_gam), "plotly")

  # seed makes the randomisation reproducible
  expect_equal(attr(plot_glm_qq(mq_pois, seed = 7), "residuals"),
               attr(plot_glm_qq(mq_pois, seed = 7), "residuals"))

  # thinning draws fewer points but leaves the residuals untouched
  b <- plotly::plotly_build(plot_glm_qq(mq_pois, n_max = 200, band = FALSE,
                                        seed = 1))
  pts <- b$x$data[[length(b$x$data)]]
  expect_lte(length(pts$y), 210)
  expect_length(attr(plot_glm_qq(mq_pois, n_max = 200, seed = 1),
                     "residuals"), nq)

  expect_error(plot_glm_qq(mq_pois, n_max = 3), "at least 10")
  expect_error(plot_glm_qq("nope"), "glm object")
  expect_error(plot_glm_qq(mq_pois, y_range = c(3, 1)), "lo < hi")
})

test_that("a family without a closed-form CDF falls back and says so", {
  m_ig <- suppressWarnings(
    glm(y ~ x, inverse.gaussian(link = "log"), dqs))
  expect_warning(p <- plot_glm_qq(m_ig), "deviance residuals are shown")
  expect_identical(attr(p, "type"), "deviance")
  expect_s3_class(p, "plotly")
})

test_that("a quasipoisson is drawn but the caveat is raised", {
  m_qp <- glm(AantalClaims ~ x + offset(log(Exposure)), quasipoisson(), dq)
  expect_warning(plot_glm_qq(m_qp, seed = 1), "no likelihood")
})

test_that("plot_glm_influence ranks rows and returns them", {
  p <- plot_glm_influence(mq_pois, n_label = 5)
  expect_s3_class(p, "plotly")
  inf <- attr(p, "influential")
  expect_s3_class(inf, "data.frame")
  expect_equal(nrow(inf), 5)
  expect_true(all(c("Row", "CooksD", "Leverage", "StdResid",
                    "Actual", "Fitted") %in% names(inf)))
  # sorted, strongest first, and they really are the strongest
  expect_false(is.unsorted(rev(inf$CooksD)))
  expect_equal(inf$CooksD,
               as.vector(sort(cooks.distance(mq_pois),
                              decreasing = TRUE)[1:5]),
               tolerance = 1e-10)
  # the model's own variables are carried along for context
  expect_true("x" %in% names(inf))
})

test_that("an extreme predictor value tops the ranking, and masks itself", {
  d2 <- dq
  d2$g <- round(rnorm(nq, 1350, 200), -1)
  d2$g[1] <- 20000                       # a keying error in a predictor
  d2$AantalClaims[1] <- 3
  m2 <- glm(AantalClaims ~ x + g + offset(log(Exposure)), poisson(), d2)
  inf <- attr(plot_glm_influence(m2, n_label = 3), "influential")

  expect_identical(inf$Row[1], rownames(model.frame(m2))[1])
  expect_gt(inf$CooksD[1], inf$CooksD[2])
  expect_gt(inf$Leverage[1], 0.5)

  # and it hides from a residual check, because at that leverage the fit
  # bends to meet it - which is why the plot needs both axes
  rp <- abs(residuals(m2, type = "pearson"))
  expect_gt(rank(-rp)[1], 100)
})

test_that("a low-exposure keying error is invisible to Cook's distance", {
  # The IRLS weight of a Poisson row is its fitted mean, so a sliver of
  # exposure carries no leverage whatever its response. This is a
  # characterisation test: it pins down a real blind spot rather than a
  # behaviour worth relying on.
  d2 <- dq
  d2$Exposure[1] <- 0.001
  d2$AantalClaims[1] <- 2
  m2 <- glm(AantalClaims ~ x + offset(log(Exposure)), poisson(), d2)

  rp <- abs(residuals(m2, type = "pearson"))
  expect_equal(unname(rank(-rp)[1]), 1)          # worst-fitted row there is
  expect_lt(hatvalues(m2)[1], 1e-4)              # yet no leverage at all

  # so a residual-based check is the complement that catches it
  r <- .quantile_residuals(m2, "t")
  expect_gt(abs(r[1]), 3)
})

test_that("plot_glm_influence caps the row count with a message", {
  expect_message(p <- plot_glm_influence(mq_pois, max_rows = 1000),
                 "max_rows")
  expect_s3_class(p, "plotly")
  expect_error(plot_glm_influence(mq_pois, n_label = 0), "at least 1")
  expect_error(plot_glm_influence("nope"), "glm object")
})
