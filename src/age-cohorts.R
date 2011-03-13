
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

CohortRatePlot <- function(df, lastyear = 2007) {
  df$group <- cut(df$yobirth, c(-Inf,
                                      1949,
                                      1954, 1959,
                                      1964, 1969,
                                      1974, 1979,
                                      1984, 1989,
                                      2010),
                     labels = c("none", "50-54", "55-59",
                       "60-64", "65-69",
                       "70-74", "75-79",
                       "80-84", "85-89", "none2"))
  df <- subset(df, year <= lastyear & age >= 12 & age <= 60 &
                   group != "none" & group != "none2", drop = TRUE)
  df$group <- drop.levels(df$group)
  ggplot(df, aes(age, rate, group = group, color = group)) +
    geom_smooth(size = 1.1, method = loess) +
    geom_point() +
    scale_colour_brewer("birth\ncohort", palette="Dark2") +
    xlab("age of homicide victim") +
    ylab("homicide rate") +
    opts(title = str_c("Age specific homicide rates from 1985 to ",
           lastyear, ", by birth cohort"))
}

AgeCohort <- function(df,
                      sequence = seq(1960, 1975, by = 2),
                      agemin = 12,
                      agemax = 60,
                      lastyear = 2007) {
  ggplot(subset(df,
                yobirth %in% sequence &
                age >= agemin & age <= agemax & year <= lastyear),
         aes(age, rate, group = yobirth, color = yobirth)) +
    geom_line() +
    scale_colour_gradient("birth\ncohort", low = "gray90",
                        high = "black") +
    opts(title = str_c("Age specific homicide rates (1985",
           "-", lastyear,
           "), by cohort"))
}

PoissonPlot <- function(df,
                        variable = "total",
                        title = "Age specific homicide rates from 1985 to 2007, by birth cohort\n (Male victims living in Mexico only)"){
#For the males living in the formerly very violent poor southern states
  df$decade <- cut(df$yobirth, c(-Inf, 1949,
                                    1959,
                                    1969,
                                    1979,
                                    1989,
                                    2010),
                   labels = c("none", "50-59",
                                      "60-69",
                                      "70-79",
                                      "80-89", "none2"))
  df <- subset(df, year <= 2007 & age >= 12 & age <= 60 &
                       decade != "none" & decade != "none2", drop = TRUE)
  df$decade <- drop.levels(df$decade)
  ggplot(df,
         aes_string(x = "age", y = variable,
                    group = "decade", color = "decade")) +
    geom_smooth(size = 1.1, method = glm,
                     family = "quasipoisson",
                     formula = "y ~ x + log(x + 10)") +
    geom_point() +
    scale_colour_brewer("birth\ncohort", palette="Dark2") +
    xlab("age of homicide victim") +
    ylab("number of homicides") +
    opts(title = title) 
    #geom_text(data = data.frame(x = 50, y = 500, decade = ""), aes(x,y),
     #         label = "hat(y[i]) *\176* Poisson(x + log(x))",
      #            parse = TRUE, legend = FALSE,
       #       color = "black")
}


#Homicides for all of Mexico by age and sex
age <- CleanHomicides("age.csv")

#Homicides for all of Michoacán, State of México, Guerrero, Oaxaca, and
#Morelos by age and sex
age.south <- CleanHomicides("age-southwest.csv")

#Homicides for Baja California, Sonora, Sinaloa, Chihuahua, Durango,
#Nuevo León and Tamaulipas
age.drug <- CleanHomicides("age-drug.csv")


#Join the homicide data with the population projections
rates <- merge(age, popm, by = c("year", "age"))
rates$rate <- rates$total / rates$pop * 100000
#Crappy smoothing that doesn't work
rates <- ddply(rates, .(year), transform,
               smooth = smooth.spline(rate)$y)
#The smoothing looks wrong for the homicides, use some regressions
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
real <- subset(rates, age >= 5 & age <= 60)
real <- ddply(real, "year", transform, per = pop / sum(pop))
imgnry <- real 
pops <- ddply(real, .(year),
              function(df) sum(df$pop)) 
per <- subset(real, year == 2009)$per
const.per <- c()
for(i in 1:25) {
  const.per <- c(const.per, pops$V1[i] * per)
}

real$imaginary.pop <- const.per
real$rate <- real$total / real$pop * 10^5
real$imaginary.rate <- real$total / real$imaginary.pop * 10^5
real.pop <- ddply(real, .(year),
             function(df) c(sum(df$total) / sum(df$pop) * 10^5,
                            wtd.mean(df$rate, df$imaginary.pop) )
                  )

ggplot(melt(real.pop, id= "year"),
       aes(year, value, group = variable, linetype = variable)) +
  geom_line(size = 1.2, alpha = .8) +
  ylab("homicide rate") +
  opts(title = "Homicide rate in Mexico (only counting ages 5-60)") +
  ylim(0, max(real.pop$V2)) +
  opts(legend.position = "top") +
  scale_linetype("",
                 breaks = c("V1", "V2"),
                 labels = c("real homicide rate",
                   "simulated homicide rate with a population structure equal to 2009"))
SavePlot("imaginary-homicides")


#All years Without controling for population
ggplot(subset(age, year <= 2007 & yobirth <= 1990 & yobirth >= 1950),
       aes(age, total, group = yobirth, color = yobirth)) +
  geom_line() +
  scale_colour_gradient("cohort", low = "gray80",
                        high = "black")


#Poisson regression by 10 year cohort
PoissonPlot(age, "total", "Age specific homicide rates from 1985 to 2007, by birth cohort\n(Total Murders in all of Mexico)")
SavePlot("age-cohorts-regression-total")

PoissonPlot(age, "males")
SavePlot("age-cohorts-regression-males")

PoissonPlot(age, "females", "Age specific homicide rates from 1985 to 2007, by birth cohort\n(Female Murders in all of Mexico)")
SavePlot("age-cohorts-regression-females")

PoissonPlot(age.south, "total", "Age specific homicide rates from 1985 to 2007, by birth cohort\n(Michoacán, Morelos, Guerrero and Oaxaca)")

PoissonPlot(age.south, "males", "Age specific homicide rates from 1985 to 2007, by birth cohort\n(Male victims living in Michoacán, Morelos, Guerrero, Oaxaca and the State of México)")
SavePlot("age-cohorts-regression-south-males")

PoissonPlot(age.drug, "males","Age specific homicide rates from 1985 to 2007, by birth cohort\n(Male victims living in Baja California, Sinaloa, Sonora, Durango, Chihuahua,\nNuevo Leon and Tamaulipas)")
SavePlot("age-cohorts-regression-drug-males")
                                    
#Homicide rates by age group
rates$agegroup <- cut(rates$age, c(-Inf, 4, 12, 17, 29, 39, 49, 59, Inf),
                      labels = c("0-4", "5-12", "13-17", "18-30", "30-39", "40-49", "50-59", ">60"))
rate.g <- ddply(rates,
                .(year, agegroup), function(df) mean(df$rate))
rate.g$agegroup <- reorder(rate.g$agegroup, -rate.g$V1)
ggplot(rate.g, aes(year, V1, group = agegroup, color = agegroup)) +
  geom_line(size = 1.2) +
  opts(title = "Homicide rates by age group (1985-2007)") +
  ylab("homicide rate") +
  xlab("year homicide occurred")
  scale_color_brewer("Age Group", palette = "Dark2")
SavePlot("ages-homicide")

#Age vs Homicide rate
ggplot(subset(rates, age <= 60 & year %in% c(1985, 1995, 2005, 2009)),
              aes(age, rate, group = factor(year), color = factor(year))) +
  geom_line(size = 1.2) +
  opts(title = "") +
  scale_color_hue("year\nhomicide\noccurred") +
  opts(title = "Homicide rates by age and year")
SavePlot("age-rate-period")


#Age specific mean homicide rates
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


#Age specific homicide rates by birth cohort (no smoothing)
AgeCohort(rates, seq(1960, 1975, by = 2), 12, 60, 2007)
AgeCohort(rates, seq(1975, 1990, by = 2), 12, 60, 2007)
AgeCohort(rates, seq(1960, 1990, by = 5), 12, 60, 2010)
SavePlot("cohort-homicide-drug-war")

AgeCohort(rates, seq(1930, 1990, by = 3), 0, 60, 2007)
AgeCohort(rates, seq(1930, 1990, by = 5), 12, 60, 2007)



#Cohort year smoothed
CohortRatePlot(rates, 2007)
SavePlot("age-cohorts-regression", w = 800, h = 600)
#Shows the effect of the drug war on all cohorts
CohortRatePlot(rates, 2009)
SavePlot("age-cohorts-regression-2009", w = 800, h = 600)

