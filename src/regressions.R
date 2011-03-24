#Regressions
rates.reg <- subset(rates, age>= 12 & age <= 50 & year <= 2007)
rates.reg <- subset(rates.reg, group != "none" &
                    group != "none2", drop = TRUE)
rates.reg$group <- drop.levels(rates.reg$group)
#First visualize an overdisperced poisson regression and a normal one
#with all the data

mod <- glm(total ~ log(age + 10) + age,
           family=poisson(log), data = rates.reg)
summary(mod)
dispersiontest(mod)
yhat <- predict (mod, type="response")
z <- (rates.reg$total-yhat)/sqrt(yhat)
cat("finding out if the data is overdispersed:\n\n")
cat ("overdispersion ratio is ", sum(z^2)/(896), "\n")
cat ("p-value of overdispersion test is ", pchisq (sum(z^2), 896), "\n")


mod.disp <- glm.poisson.disp(mod, maxit = 200)
summary(mod.disp)
#plot(mod)
mod.disp$dispersion

# compute predictions on a grid of x-values...
x0 <- seq(min(rates.reg$age), max(rates.reg$age))
new <- data.frame(log.age...10 = log(x0),
                  age = x0,
                  log.pop. = log(1000000))
new <- cbind(new, "log(pop)" = log(1000000))
eta0 <- predict.glm(mod, new = new, se = TRUE)
eta0.disp <- predict(mod.disp, new = new, se = TRUE)
# ... and plot the mean functions with variab1ility bands
plot(rates.reg$age, rates.reg$total)
lines(x0, exp(eta0$fit))
lines(x0, exp(eta0$fit+2*eta0$se), lty=2)
lines(x0, exp(eta0$fit-2*eta0$se), lty=2)
lines(x0, exp(eta0.disp$fit), col=2)
lines(x0, exp(eta0.disp$fit+2*eta0.disp$se), lty=2, col=2)
lines(x0, exp(eta0.disp$fit-2*eta0.disp$se), lty=2, col=2)


#Second - one regression per birth cohort decade
cat("results of a quasipoisson regression for each decade cohort:\n\n")
llist <- dlply(rates.reg, .(group),
      function(df) glm(total ~ age + log(age + 10) + offset(log(pop)),
                       family = quasipoisson, data = df))
lapply(llist, function(x) print(summary(x)))
coefplot(llist[[4]])

#Third - multi-level model

#from nlme
cat("multi-level model: \n\n\n")
lme.fit <- lme(total ~ age + log(age + 10) + offset(log(pop)),
               random = (~ age | group),    
               data = rates.reg)
print(coef(lme.fit))

#from lme with poisson
#lmer.fit <- lmer(total ~  (1 | group) + offset(log(pop)),
#                family = "poisson",
#                data = rates.reg)
#coef(lmer.fit)
