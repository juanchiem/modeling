library(dyn)

set.seed(123)
y <- numeric(100)
y[1] <- 2
for (i in 2:100) {
  u <- rnorm(1, mean = 0, sd = 1)
  y[i] <- 1 + 0.5 * y[i-1] + u
}
y

z <- zoo(y)
model <- dyn$lm(z ~ lag(z, -1) + lag(z, -2) + lag(z, -3))
anova(model)
model

model2 <- dyn$lm(z ~ lag(z, -1:-2))
anova(model2)
model2