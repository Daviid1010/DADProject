library(tidyverse)
library(readxl)

OECDCountries = c("Australia","Austria","Belgium","Canada","Chile","Czech Republic","Denmark","Estonia","Finland","France",
                  "Germany","Greece","Hungary","Iceland","Ireland","Israel","Italy","Japan","Korea","Latvia","Lithuania",
                  "Luxembourg","Mexico","Netherlands","New Zealand","Norway","Poland","Portugal","Slovakia","Slovenia",
                  "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States")


ColsRm = c("INDICATOR","SUBJECT","FREQUENCY","Flag.Codes","MEASURE")
###### Doc Stats
docStats = read.csv("doctorStats.csv", header = T, sep = ",")
nrow(docStats)
nrow(docStats)
summary(docStats)
#####Filter Doc Stats from 2000 to 2010
docStats = filter(docStats, between(TIME, 2000,2010))
## remove uneeded columns
docStats = docStats[, !(colnames(docStats) %in% ColsRm), drop = FALSE]
docStats = docStats %>%
  rename(
    DocsPer1000 = Value
  )


##### Nurse Stats
nurseStats = read.csv("nursesStats.csv")
nrow(nurseStats)
ncol(nurseStats)
summary(nurseStats)

#####Filter Nurse Stats from 2000 to 2010
nurseStats = filter(nurseStats, between(TIME, 2000,2010))
## remove uneeded colums
nurseStats = nurseStats[, !(colnames(nurseStats) %in% ColsRm), drop = FALSE]
nurseStats = nurseStats %>%
  rename(
    NursesPer1000 = Value
  )

##### Pharmaeutical Stats 
PharmaStats = read.csv("PharmaStats.csv")
nrow(PharmaStats)
ncol(PharmaStats)
summary(PharmaStats)

#####Filter Pharma Stats from 2000 to 2010
PharmaStats = filter(PharmaStats, between(TIME, 2000,2010))
#### Remove Uneeeded Columns
PharmaStats = PharmaStats[, !(colnames(PharmaStats) %in% ColsRm), drop = FALSE]
PharmaStats = PharmaStats %>%
  rename(
    PharmaSpendPercentGDP = Value
  )



#### rbind all three dataframes into one, can be done as they come from same source and have similar structures
HealthData = merge(docStats,nurseStats, by = c("�..LOCATION","TIME"))
HealthData = merge(HealthData,PharmaStats, by = c("�..LOCATION","TIME"))

write.csv(HealthData, "healthdata.csv")


#### Health, Nutrition, Population Stats
NutritionStats = read.csv("healthNutritionPopulationStats.csv")
nrow(NutritionStats)
ncol(NutritionStats)
summary(NutritionStats)

NutritionStats$�..Country.Name = as.character(NutritionStats$�..Country.Name)

### Filter by OECD Member States
filteredNutStats = filter(NutritionStats, �..Country.Name %in% OECDCountries)
nrow(filteredNutStats)

## Filter by Years
filteredNutStats
filteredNutStatsYear = select(filteredNutStats,�..Country.Name,Indicator.Name,Indicator.Code,c(X2000:X2010))

##Explore One Country
UKNutStats = filter(filteredNutStatsYear, �..Country.Name == "United Kingdom")

##Filter By Certain Indicators
IndicatorCodes = c("SP.POP.80UP.FE", 
                   "SH.XPD.PRIV.ZS",
                   "SH.XPD.PUBL.ZS",
                   "SH.MED.BEDS.ZS",
                   "SM.POP.NETM",
                   "SE.XPD.TOTL.GD.ZS",
                   "SP.RUR.TOTL.ZS",
                   "SE.TER.ENRR",
                   "SL.UEM.TOTL.FE.ZS",
                   "SL.UEM.TOTL.MA.ZS",
                   "SL.UEM.TOTL.ZS",
                   "SP.URB.TOTL.IN.ZS")
filteredNutStatsYear$Indicator.Code  = as.character(filteredNutStatsYear$Indicator.Code)
CompleteFilterStats = subset(filteredNutStatsYear, Indicator.Code %in% IndicatorCodes)

####write the completely filtered stats to a csv file for next stage of KDD
write.csv2(CompleteFilterStats,file = "DemographicFactors.csv")

#### Human Development Index Web Scrape

##### Suicide Rates Data
SuicideRates = read.csv("SuicideRates.csv")
nrow(SuicideRates)
ncol(SuicideRates)
summary(SuicideRates)

## Filter by Years 2000 to 2010
SuicideRates = filter(SuicideRates, between(year, 2000,2010))
## Filter by OECD Countries
SuicideRates = filter(SuicideRates, �..country %in% OECDCountries)

write.csv(SuicideRates,"SuicideRatesYearAndCountries.csv")

#### melt the new dems data and change the column names so that each indicator is a column
NewDems = melt(CompleteFilterStats)
NewDems = NewDems[!(names(NewDems) %in% c("Indicator.Name"))]
NewDems = tidyr::spread(NewDems,Indicator.Code, value)

## Fix Country Name and Year
NewDems = NewDems %>%
  rename(
    Country = �..Country.Name,
    Year = variable
  )
NewDems$Year = sub("X","", NewDems$Year)
NewDems$Year = as.integer(NewDems$Year)

summary(NewDems)

#### change names on Suicide Rates dataset in order for merger with Demographics Dataset
head(SuicideRates)
SuicideRates = SuicideRates %>%
  rename( Country= �..country,
          Year = year
  )
SuicideRates = SuicideRates[!(names(SuicideRates) %in% c("country.year"))]

###Alter Health Data so that it can be merged with Suicide Rates
install.packages("countrycode")
HealthData$Country = countrycode::countrycode(HealthData$�..LOCATION,origin = 'iso3c', destination = 'country.name')
HealthData = HealthData[!(names(HealthData) %in% c("�..LOCATION"))]
HealthData = HealthData %>%
  rename(
    Year = TIME
  )

### Merge the three datasets together
FinalData = merge(SuicideRates,HealthData, by = c("Country","Year"))
FinalDataDem = merge(FinalData, NewDems, by = c("Country","Year"))

write.csv(FinalDataDem, "DataToBeAnalysed.csv")

