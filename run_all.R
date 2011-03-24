library(ggplot2)
library(stringr)
library(gdata)
library(tseries)
library(Hmisc)
library(dispmod)
library(arm)
library(Cairo)
library(nlme)
library(AER)
options(stringsAsFactors = FALSE)
theme_set(theme_bw())

CairoFonts(
	regular="Aller:style=Regular",
	bold="Aller:style=Bold",
	italic="Aller:style=Italic",
	bolditalic="Aller:style=Bold Italic,BoldItalic",
	symbol="Symbol"
)


source("src/utils.R")
source("src/clean-census.R")
source("src/male-female.R")
source("src/age-cohorts.R")
source("src/regressions.R")

