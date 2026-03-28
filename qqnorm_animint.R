# QQ Plot Simulation - animint2 port
# animation package original: https://yihui.org/animation/example/sim-qqnorm/
# GSoC 2026 - animint2 medium task

library(animint2)

set.seed(42)

nmax <- 50
obs  <- 100

# --- data frames ---

qq_df <- do.call(rbind, lapply(seq_len(nmax), function(n) {
  x <- sort(rnorm(obs))
  theoretical_q <- qnorm(ppoints(obs))
  data.frame(
    n          = n,
    theoretical = theoretical_q,
    sample      = x,
    point_id    = seq_along(x)
  )
}))

ref_df <- data.frame(x = c(-3, 3), y = c(-3, 3))

sw_df <- do.call(rbind, lapply(seq_len(nmax), function(n) {
  set.seed(n)
  x  <- rnorm(obs)
  sw <- shapiro.test(x)
  data.frame(
    n      = n,
    w_stat = as.numeric(sw$statistic),
    pvalue = sw$p.value
  )
}))

resid_df <- do.call(rbind, lapply(seq_len(nmax), function(n) {
  x    <- qq_df$sample[qq_df$n == n]
  theo <- qq_df$theoretical[qq_df$n == n]
  data.frame(
    n        = n,
    theo     = theo,
    residual = x - theo,
    point_id = seq_along(x)
  )
}))

# --- plots ---

pqq <- ggplot() +
  geom_tallrect(
    data         = sw_df,
    aes(xmin = n - 0.5, xmax = n + 0.5),
    clickSelects = "n",
    alpha        = 0.2,
    fill         = "gold"
  ) +
  geom_line(
    data  = ref_df,
    aes(x = x, y = y),
    color = "red",
    linetype = "dashed",
    size  = 1
  ) +
  geom_point(
    data         = qq_df,
    aes(x = theoretical, y = sample, key = point_id),
    showSelected = "n",
    color        = "steelblue",
    size         = 2,
    alpha        = 0.7
  ) +
  labs(
    title = "QQ Plot: Sample vs Theoretical Normal",
    x     = "Theoretical quantiles",
    y     = "Sample quantiles"
  ) +
  theme_bw()

presid <- ggplot() +
  geom_tallrect(
    data         = sw_df,
    aes(xmin = n - 0.5, xmax = n + 0.5),
    clickSelects = "n",
    alpha        = 0.2,
    fill         = "gold"
  ) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
  geom_point(
    data         = resid_df,
    aes(x = theo, y = residual, key = point_id),
    showSelected = "n",
    color        = "steelblue",
    size         = 2,
    alpha        = 0.7
  ) +
  labs(
    title = "Residuals from Normal Line",
    x     = "Theoretical quantiles",
    y     = "Sample - Theoretical"
  ) +
  theme_bw()

psw <- ggplot() +
  geom_tallrect(
    data         = sw_df,
    aes(xmin = n - 0.5, xmax = n + 0.5),
    clickSelects = "n",
    alpha        = 0.2,
    fill         = "gold"
  ) +
  geom_hline(yintercept = 0.05, color = "red", linetype = "dashed") +
  geom_line(
    data  = sw_df,
    aes(x = n, y = pvalue),
    color = "darkgreen",
    size  = 0.9
  ) +
  geom_point(
    data  = sw_df,
    aes(x = n, y = pvalue),
    color = "darkgreen",
    size  = 1.5
  ) +
  geom_point(
    data         = sw_df,
    aes(x = n, y = pvalue, key = n),
    showSelected = "n",
    color        = "orange",
    size         = 5
  ) +
  labs(
    title = "Shapiro-Wilk p-value across simulations",
    x     = "Simulation index",
    y     = "p-value"
  ) +
  theme_bw()

pwstat <- ggplot() +
  geom_tallrect(
    data         = sw_df,
    aes(xmin = n - 0.5, xmax = n + 0.5),
    clickSelects = "n",
    alpha        = 0.2,
    fill         = "gold"
  ) +
  geom_hline(yintercept = 1, color = "red", linetype = "dashed") +
  geom_line(
    data  = sw_df,
    aes(x = n, y = w_stat),
    color = "steelblue",
    size  = 0.9
  ) +
  geom_point(
    data         = sw_df,
    aes(x = n, y = w_stat, key = n),
    showSelected = "n",
    color        = "orange",
    size         = 5
  ) +
  labs(
    title = "Shapiro-Wilk W statistic ",
    x     = "Simulation index",
    y     = "W statistic"
  ) +
  theme_bw()

viz <- animint(
  qqplot  = pqq,
  resid   = presid,
  swpval  = psw,
  wstat   = pwstat,
  first    = list(n = 1),
  duration = list(n = 500),
  time     = list(variable = "n", ms = 800),
  title    = "QQ Plot Simulation",
  source   = "https://github.com/Nishita-shah1/qqnorm-animint"
)

animint2pages(viz, "qqnorm-animint")
print(viz)
