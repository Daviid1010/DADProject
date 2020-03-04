library(tidyverse)
library(readxl)


docStats = read.csv("doctorStats.csv", header = T, sep = ",")

nurseStats = read.csv("nursesStats.csv")

PharmaStats = read.csv("PharmaStats.csv")

NutritionStats = read.csv("healthNutritionPopulationStats.csv")

SuicideRates = read.csv("SuicideRates.csv")

HealthExpendature = read.csv("HealthExpStats.csv")
