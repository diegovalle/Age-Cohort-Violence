library(ggplot2)
library(stringr)
library(gdata)
library(tseries)
library(pspline)
library(Hmisc)
library(arm)
library(dispmod)
library(Cairo)
options(stringsAsFactors = FALSE)
theme_set(theme_bw())

source("clean-census.R")

SavePlot <- function(filename,
                     plot = last_plot(),
                     width = 640, height = 480) {
  filename <- file.path("graphs", str_c(filename, ".png"))
  Cairo(filename, width = width, height = height)
  print(plot)
  dev.off()
}

CleanHomicides <- function(filename) {
  age <- read.csv(file.path("data", filename), skip = 4)
  names(age) <- c("year", "age", "males", "females", "na", "total")
  age <- subset(age, year != "Grand Total" &
                     age != "No especificado" &
                     age != "No Especificados")
  age$year <- rep(1979:2009, each = 150)
  age$age <- gsub("Menores de 1 año", "0", age$age)
  age$age <- gsub(" año.*", "", age$age)
  age$age <- as.numeric(age$age)
  age <- subset(age, age <= 90)

  age$total[age$total == ""] <- 0
  age$males[age$males == ""] <- 0
  age$females[age$females == ""] <- 0
  age[, 3:6] <- sapply(age[, 3:6], as.numeric)
  tail(age)
  age$yobirth <- age$year - age$age
  age <- subset(age, age <= 80 & year >= 1985)
  return(age)
}

#Homicides for all of Mexico by age
age <- CleanHomicides("age.csv")

#Homicides for all of Michoacán, State of México, Guerrero, Oaxaca, and
#Morelos
age.south <- CleanHomicides("age-southwest.csv")


#Join the homicide data with the population projections
rates <- merge(age, popm, by = c("year", "age"))
rates$rate <- rates$total / rates$pop * 100000
#Crappy smoothing that doesn't work
rates <- ddply(rates, .(year), transform,
               smooth = smooth.spline(rate)$y)
#The smoothing looks wrong for the homicides
ggplot(subset(rates,
              yobirth %in% seq(1959, 1975, by = 5) &
              age >= 12 & age <= 50 & year < 2006),
       aes(age, rate, group = yobirth, color = yobirth)) +
  geom_line() +
  geom_line(aes(y = smooth), linetype = 2) +
  scale_colour_gradient("cohort", low = "gray90",
                        high = "black")

#What would homicide rates have been if the Mexico had the same
#population structure as in 2000?
imgnry <- subset(rates, age >=12 & age <= 60) 
pops <- ddply(rates, .(year),
      function(df) sum(subset(df, age >=12 & age <= 60)$pop)) 
per <- subset(rates, year == 2000 & age >=12 & age <= 60)$per
const.per <- c()
for(i in 1:25) {
  const.per <- c(const.per, pops$V1[i] * per)
}
imgnry$pop <- const.per
ggplot(ddply(imgnry, .(year),
             function(df) sum(df$total) / sum(df$pop) * 10^5),
       aes(year, V1)) +
  geom_line(size = 1.2) +
  ylab("homicide rate") +
  opts(title = "Homicide rate in Mexico for ages 12-60\n(assuming a population pyramid equal to that in 2000)") +
  ylim(0, 50)
SavePlot("imaginary-homicides")



#Without controling for population
ggplot(subset(age, year <= 2004 & yobirth <= 1990 & yobirth >= 1960),
       aes(age, males, group = yobirth, color = yobirth)) +
  geom_line() +
  scale_colour_gradient("cohort", low = "gray80",
                        high = "black")


rates$agegroup <- cut(rates$age, c(-Inf, 4, 12, 17, 29, 39, 49, 59, Inf),
                      labels = c("0-4", "5-12", "13-17", "18-30", "30-39", "40-49", "50-59", ">60"))
rate.g <- ddply(rates,
                .(year, agegroup), function(df) mean(df$rate))
rate.g$agegroup <- reorder(rate.g$agegroup, -rate.g$V1)
ggplot(rate.g, aes(year, V1, group = agegroup, color = agegroup)) +
  geom_line(size = 1.2) +
  opts(title = "Homicide rates by age group (1985-2007)") +
  ylab("homicide rate") +
  scale_color_brewer("Age Group", palette = "Dark2")
SavePlot("ages-homicide")

ggplot(subset(rates, year %in% c(1985, 1995, 2005, 2009)),
              aes(age, rate, group = factor(year), color = factor(year))) +
  geom_line(size = 1.2) +
  opts(title = "") +
  scale_color_hue("year") +
  opts(title = "Homicide rates by age and year")
SavePlot("age-rate-period")


rates$group <- cut(rates$yobirth, c(-Inf, 1949, 1959, 1969, 1979,
                                    1989, 1999, 2010),
                   labels = c("none", "50-59", "60-69", "70-79", "80-89",
                     "90-99", "none2"))
rate.g <- subset(rates, year <= 2007 &
                       group != "none" & group != "none2", drop = TRUE)
rate.g$group <- drop.levels(rate.g$group)
rate.g <- ddply(rate.g,
                .(age, group), function(df) mean(df$rate))
ggplot(rate.g, aes(age, V1, group = group, color = group)) +
  geom_line(size = 1.2) +
  scale_colour_grey("birth\ncohort", start = .8,
                        end = 0) +
  opts(title = "Age specific mean homicide rates from 1985 to 2007, by birth cohort") +
  xlab("age of homicide victim") +
  ylab("homicide rate")
SavePlot("cohort-mean-homicide")



ggplot(subset(rates,
              yobirth %in% seq(1960, 1975, by = 2) &
              age >= 12 & age <= 50 & year < 2006),
       aes(age, rate, group = yobirth, color = yobirth)) +
  geom_line() +
  scale_colour_gradient("cohort", low = "gray90",
                        high = "black") +
  theme_bw()

ggplot(subset(rates,
              yobirth %in% seq(1975, 1990, by = 3) &
              age >= 12 & age <= 50 & year < 2007),
       aes(age, rate, group = yobirth, color = yobirth)) +
  geom_line() +
  scale_colour_gradient("cohort", low = "gray90",
                        high = "black")

ggplot(subset(rates,
              yobirth %in% seq(1960, 1990, by = 5) &
              age >= 12 & age <= 50 & year < 2010),
       aes(age, rate, group = yobirth, color = yobirth)) +
  geom_line(size = 1.2) +
  scale_colour_gradient("cohort", low = "gray80",
                        high = "black") 
SavePlot("cohort-homicide-drug-war")

ggplot(subset(rates,
              age <= 50 & year < 2007 &
              yobirth %in% seq(1930, 1990, by = 3)),
       aes(age, rate, group = yobirth, color = yobirth)) +
  geom_line() +
  scale_colour_gradient("cohort", low = "gray90",
                        high = "black") 

ggplot(subset(rates,
              yobirth %in% seq(1930, 1990, by = 3) &
              age >= 12 & age <= 50 & year < 2007),
       aes(age, rate, group = yobirth, color = yobirth)) +
  geom_line() +
  scale_colour_gradient("cohort", low = "gray90",
                        high = "black")




rates$group <- cut(rates$yobirth, c(-Inf,
                                    1954, 1959,
                                    1964, 1969,
                                    1974, 1979,
                                    1984, 1989,
                                    2010),
                   labels = c("none", "55-59",
                                      "60-64", "65-69",
                                      "70-74", "75-79",
                     "80-84", "85-89", "none2"))
rates$group <- cut(rates$yobirth, c(-Inf, 1949,
                                    1959,
                                    1969,
                                    1979,
                                    1989,
                                    2010),
                   labels = c("none", "50-59",
                                      "60-69",
                                      "70-79",
                                      "80-89", "none2"))
rate.g <- subset(rates, year <= 2007 &
                       group != "none" & group != "none2", drop = TRUE)
rate.g$group <- drop.levels(rate.g$group)



ggplot(subset(rate.g,
              age >= 12 & age <= 50),
       aes(age, rate, group = group, color = group)) +
  geom_smooth(size = 1.1) +
  geom_point() +
  scale_colour_brewer("birth\ncohort", palette="Dark2") +
  xlab("age of homicide victim") +
  ylab("homicide rate") +
  opts(title = "Age specific homicide rates from 1985 to 2007, by birth cohort")
SavePlot("age-cohorts-regression")

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
