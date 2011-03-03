mf <- read.csv("data/male-female-rates.csv", skip = 6)
mf <- na.omit(mf)

ggplot(mf, aes(1990:2009, Female.Per, color = "Females")) +
  geom_line(size = 1.2) +
  geom_line(aes(y = Male.Per, color = "Males"),
            size = 1.2) +
  scale_color_manual("Sex", values = c("pink", "#ADD8E6")) +
  xlab("") + ylab("homicide rate as a percentage of that in 1990") +
  scale_y_continuous(format = "percent", limits = c(0, 1.2)) +
  opts(title = "Homicide rates for males and females in Mexico (1990 = 100%)")
SavePlot("male-female")


