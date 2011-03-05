mf <- read.csv("data/male-female-rates.csv", skip = 6)
mf <- na.omit(mf)

ggplot(mf, aes(1985:2009, Female.Per, color = "Females")) +
  geom_line(size = 1.2) +
  geom_line(aes(y = Male.Per, color = "Males"),
            size = 1.2) +
  scale_color_manual("Sex", values = c("pink", "#ADD8E6")) +
  xlab("") + ylab("homicide rate as a percentage of that in 1985") +
  scale_y_continuous(format = "percent", limits = c(0, 1.2)) +
  opts(title = "Homicide rates for males and females in Mexico (1985 = 100%)")
SavePlot("male-female")


#Extrapolate the population from the 1980 Census
na.spline(c(33039307, rep(NA, 9), mf$Male.Population))
na.spline(c(33807526, rep(NA, 9), mf$Female.Population))
