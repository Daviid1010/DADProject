library(tidyverse)
library(readxl)


docStats = read.csv("doctorStats.csv", header = T, sep = ",")
nrow(docStats)
nrow(docStats)
summary(docStats)

nurseStats = read.csv("nursesStats.csv")
nrow(nurseStats)
ncol(nurseStats)
summary(nurseStats)

PharmaStats = read.csv("PharmaStats.csv")
nrow(PharmaStats)
ncol(PharmaStats)
summary(PharmaStats)

NutritionStats = read.csv("healthNutritionPopulationStats.csv")
nrow(NutritionStats)
ncol(NutritionStats)
summary(NutritionStats)

SuicideRates = read.csv("SuicideRates.csv")
nrow(SuicideRates)
ncol(SuicideRates)
summary(SuicideRates)

HealthExpendature = read.csv("HealthExpStats.csv")
nrow(HealthExpendature)
ncol(HealthExpendature)
summary(HealthExpendature)
