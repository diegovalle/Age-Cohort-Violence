mf <- read.csv("data/male-female-rates.csv", skip = 6)
mf <- na.omit(mf)

ggplot(mf, aes(1990:2009, Female.Per, linetype = "Females")) +
  geom_line(size = 1.2) +
  geom_line(aes(y = Male.Per, linetype = "Males")) +
  scale_linetype("Sex") +
  xlab("") + ylab("homicide rate as a percentage of that in 1990") +
  scale_y_continuous(formatter = "percent") +
  ylim(0, 1.2) +
  opts(title = "Homicide rates for males and females in Mexico (1990 = 100%)")
SavePlot("male-female")


