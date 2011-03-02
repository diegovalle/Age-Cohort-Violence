#Regressions
mod <- glm(total ~ log(age + 10) + age,
           family=poisson(log), data = rate.g)
summary(mod)
yhat <- predict (mod, type="response")
z <- (rate.g$total-yhat)/sqrt(yhat)
cat ("overdispersion ratio is ", sum(z^2)/(781), "\n")
cat ("p-value of overdispersion test is ", pchisq (sum(z^2), 780), "\n")


mod.disp <- glm.poisson.disp(mod, maxit = 200)
summary(mod.disp)
plot(mod)
mod.disp$dispersion

# compute predictions on a grid of x-values...
x0 <- seq(min(rate.g$age), max(rate.g$age))
new <- data.frame(log.age...10 = log(x0),
                  age = x0,
                  log.pop. = log(1000000))
new <- cbind(new, "log(pop)" = log(1000000))
eta0 <- predict.glm(mod, new = new, se = TRUE)
eta0.disp <- predict(mod.disp, new = new, se = TRUE)
# ... and plot the mean functions with variab1ility bands
plot(rate.g$age, rate.g$total)
lines(x0, exp(eta0$fit))
lines(x0, exp(eta0$fit+2*eta0$se), lty=2)
lines(x0, exp(eta0$fit-2*eta0$se), lty=2)
lines(x0, exp(eta0.disp$fit), col=2)
lines(x0, exp(eta0.disp$fit+2*eta0.disp$se), lty=2, col=2)
lines(x0, exp(eta0.disp$fit-2*eta0.disp$se), lty=2, col=2)


#One regression per birth cohort decade
llist <- dlply(rate.g, .(group),
      function(df) glm(total ~ age + log(age+10) + offset(log(pop)),
                       family = quasipoisson, data = df))
lapply(llist, summary)

#multi-level model
lme.fit <- lmer(total ~ age + log(age+10) + (1 | group) + offset(log(pop)),
                family = "poisson",
                data = rate.g)
coef(lme.fit)
coefplot(llist[[4]])
