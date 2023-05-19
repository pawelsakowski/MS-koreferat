kpssResults4grid <- rep(NA, nrow(grid))
# KPSS
for (k in 1:nrow(grid)) {
  nObs  <- grid[k, "nObs"]
  alpha <- grid[k, "alpha"]
  x  <- rep(0, nObs)
  for (j in 1:nReps) {
    for (i in 2:nObs) {
      x[i] <- alpha * x[i - 1] + rnorm(1, 0, 0.1)
    }
    kpss.test <- ur.kpss(x, type = c("mu"), use.lag = 0)
    # 1 for correct decision, 0 for Type I Error, 
    results[j] <- as.numeric(kpss.test@teststat[1] < kpss.test@cval[2])
  }
  # hence: mean() is the fraction of correcto decisions
  kpssResults4grid[k] <- mean(results)
}
# hence: 1-mean() is the fraction of correct decisions
KPSS.results <- cbind(grid, `1-TypeIError` = kpssResults4grid)

KPSS.results %>%
  mutate(nObs  = factor(nObs, ordered = T),
         alpha = factor(alpha, ordered = T)) %>%
  ggplot(aes(nObs, alpha, z = `1-TypeIError`)) +
  geom_tile(aes(fill = `1-TypeIError`)) +
  theme_bw() +
  labs(title = "(1 - Type I Error) for KPSS test, ie. fraction of correct decisions") +
  scale_fill_distiller(palette = "Spectral")


left_join(
  DF.results %>% rename(DF.correct = power),
  KPSS.results %>% rename(KPSS.correct = `1-TypeIError`),
  by = c("nObs", "alpha")
) %>%
  mutate(nObs  = factor(nObs, ordered = T),
         alpha = factor(alpha, ordered = T)) %>%
  mutate(diff = DF.correct - KPSS.correct) %>%
  ggplot(aes(nObs, alpha, z = diff)) +
  geom_tile(aes(fill = diff)) +
  theme_bw() +
  labs(title = "Differences between DF and KPSS fractions of correct decisions") +
  scale_fill_distiller(palette = "Spectral")
