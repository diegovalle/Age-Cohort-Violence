library(ggplot2)
library(stringr)
library(gdata)
library(tseries)
library(Hmisc)
library(dispmod)
library(arm)
library(Cairo)
library(nlme)
options(stringsAsFactors = FALSE)
theme_set(theme_bw())

CairoFonts(
	regular="Aller:style=Regular",
	bold="Helvetica:style=Bold",
	italic="Helvetica:style=Italic",
	bolditalic="Helvetica:style=Bold Italic,BoldItalic",
	symbol="Symbol"
)


source("src/utils.R")
source("src/clean-census.R")
source("src/male-female.R")
source("src/age-cohorts.R")
source("src/regressions.R")

