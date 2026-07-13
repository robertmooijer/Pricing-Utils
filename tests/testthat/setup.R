# Shared simulated portfolio and fitted models for all tests.
# Note: no library(splines) here on purpose - ns() must be available
# through the package re-export.

set.seed(42)
n <- 20000
dat <- data.frame(
  LEEFTIJD  = round(runif(n, 18, 80)),
  REGIO     = factor(sample(c("Noord", "Zuid", "West"), n, TRUE, c(.5, .3, .2))),
  BOEKJAAR  = sample(2019:2023, n, TRUE),
  Exposure  = runif(n, 0.1, 1)
)
lin <- -2.3 + 0.015 * (dat$LEEFTIJD - 40) +
  ifelse(dat$REGIO == "Zuid", .25, ifelse(dat$REGIO == "West", .4, 0))
dat$AantalClaims <- rpois(n, dat$Exposure * exp(lin))
dat$SCHADELAST   <- ifelse(
  dat$AantalClaims > 0,
  rgamma(n, shape = 2, rate = 2 / (1500 * exp(0.01 * (dat$LEEFTIJD - 40)))) *
    dat$AantalClaims,
  0)

m_freq <- glm(AantalClaims ~ ns(LEEFTIJD, 3) + REGIO + factor(BOEKJAAR) +
                offset(log(Exposure)),
              family = poisson(), data = dat)

d_sev <- dat[dat$AantalClaims > 0, ]
d_sev$AvgLoss <- d_sev$SCHADELAST / d_sev$AantalClaims
m_sev <- glm(AvgLoss ~ LEEFTIJD + REGIO, family = Gamma(link = "log"),
             data = d_sev, weights = AantalClaims)

# Rating tables used by several test files
tbl <- make_rating_table(m_freq, m_sev, data = dat)

m_freq2 <- glm(AantalClaims ~ LEEFTIJD * REGIO + offset(log(Exposure)),
               family = poisson(), data = dat)
tbl2 <- make_rating_table(m_freq2, NULL, data = dat)

# Extract the y values of a named trace from a plotly object
trace_y <- function(p, nm) {
  tr <- Filter(function(t) identical(t$name, nm),
               plotly::plotly_build(p)$x$data)[[1]]
  as.numeric(unlist(tr$y))
}
