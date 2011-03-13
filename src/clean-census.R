CleanCensus <- function(filename, sex = "pop") {
#Clean the Census and Population Survey data
  df <- read.csv(filename, skip = 4,
                fileEncoding = "windows-1252")
  names(df) <-  c("age", "pop", "popm", "popf")
  df <- subset(df, age != "Total" &
                age != "No especificado")
  df <- df[-grep("De.*", df$age),]
  df$age <- as.numeric(gsub(" año.*", "", df$age))
  df[2:4] <- apply(df[2:4], 2, function(x) as.numeric(gsub(",", "", x)))
  df <- na.omit(df)
  fit <- smooth.spline(1:100, df[[sex]])
  #print(plot(fit$y, type = "l"))
  #print(points(df$pop))
#  df$pop <- c(df$pop[1:2] ,
  #            filter(df$pop, rep(1/5, 5), sides = 2)[3:98],
   #           df$pop[99:100])
  #df$pop
  fit$y
}

CleanCensus80 <- function(sex = "total") {
#The Census data for 1980 comes in a different format 
  c80 <- read.csv("census/Pob_1980_02.csv", skip = 7)
  names(c80) <- c("X", "age", "total", "males", "females")
  c80$X <- NULL
  c80 <- c80[1:139,]
  c80 <- c80[-grep("-", c80$age),]
  c80 <- na.omit(c80)
  c80$age <- gsub(" *AÑO.*", "", c80$age)
                                        #plot(c80$total, type = "p")
  fit <- smooth.spline(1:100, c80[[sex]])
  fit$y
#c80$total <- c(c80$total[1:2] ,
 #             filter(c80$total, rep(1/5, 5), sides = 2)[3:98],
  #            c80$total[99:100])
#lines(1:100, c80$total)
}

pop <- data.frame(rbind(t(CleanCensus("census/census1990sex.csv")),
                        t(CleanCensus("census/census1995sex.csv")),
                        t(CleanCensus("census/census2000sex.csv")),
                        t(CleanCensus("census/census2005sex.csv"))
                        ))



#Population by age is available from 2005 onwards by the CONAPO
conapo <- read.csv("census/conapo05-07.csv")


names(pop) <- str_c("X", 0:99)
pop <- subset(pop, X1 != "age")
pop <- pop[c(rep(6,9),
             1,rep(6,4),
             2,rep(6,4),
             3,rep(6,4)),]
pop <- rbind(CleanCensus80(), pop,
             conapo$X2005, conapo$X2006, conapo$X2007,
             conapo$X2008, conapo$X2009)




pop <- apply(pop, 2, na.spline)

nrow(pop)

popm <- melt(pop)
head(popm)
names(popm) <-  c("year", "age", "pop")
popm$year <- 1980:2009
popm$age <- rep(c(0:99), each = nrow(pop))


popm <- ddply(popm, "year", transform, per = pop / sum(pop))
ggplot(subset(popm, year %in% seq(1985, 2009, by = 3) &
              age >= 12 & age <= 60),
       aes(per, age, group = year, color = year)) +
  geom_line(alpha = .9) +
  scale_x_continuous(trans = 'reverse', formatter = "percent",
                     xlim = c(0, max(popm$age)) ) +
  opts(title = "Population structure in Mexico (ages 12-60)") +
  xlab("percentage of the population")+
  scale_colour_gradient("year", low = "gray70",
                        high = "black")
SavePlot("pyramid")

#compare it to the colmex population estimate
popm$agegroup <- cut(popm$age, c(-Inf,0, 4, 9, 14, 19, 24, 29, 34, 39, Inf))
write.csv(cast(ddply(popm, .(year, agegroup), function(df) sum(df$pop)),
     year ~ agegroup, value = "V1"), "census-estimates.csv")

