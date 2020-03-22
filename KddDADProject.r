install.packages("countrycode")
install.packages("tidyverse")
install.packages("readxl")
install.packages("reshape2")
install.packages("mice")
install.packages("naniar")
install.packages("VIM")
install.packages("devtools")
install.packages("githubinstall")
install.packages("rsdmx")
install.packages("dplyr")
install.packages("DBI")
install.packages("plyr")
install.packages("caTools")
install.packages("scales")
library(tidyverse)
library(rsdmx)
library(readxl)
library(reshape2)
library(mice)
library(VIM)
library(naniar)
library(OECD)
library(dplyr)
library(caTools)
library(scales)

OECDCountries = c("Australia","Austria","Belgium","Canada","Chile","Czech Republic","Denmark","Estonia","Finland","France",
                  "Germany","Greece","Hungary","Iceland","Ireland","Israel","Italy","Japan","Korea","Latvia","Lithuania",
                  "Luxembourg","Mexico","Netherlands","New Zealand","Norway","Poland","Portugal","Slovakia","Slovenia",
                  "Spain","Sweden","Switzerland","Turkey","United Kingdom","United States")


ColsRm = c("INDICATOR","SUBJECT","FREQUENCY","Flag.Codes","MEASURE")
###### Doc Stats
docStats = read.csv("doctorStats.csv", header = T, sep = ",")
nrow(docStats)
ncol(docStats)
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


###### API Call on OECD Data
providers = rsdmx::getSDMXServiceProviders();
providers = as.data.frame(providers)

################# This is the API call (Note this take a long time to run, in the end the data was not used which is discussed in the report)
MentalHealthData = readSDMX(providerId = "OECD", resource = "data", flowRef = "SHA",
                            start = 2000, end = 2010,
                            dsd = TRUE)
MentalHealthDataFrame = as.data.frame(MentalHealthData, labels = TRUE)

FilteredMentalHospitalData = MentalHealthDataFrame[MentalHealthDataFrame$HP == "HP12",]
FilteredMentalHospitalData = FilteredMentalHospitalData[FilteredMentalHospitalData$MEASURE == "PARPIB",]
ColsKeep = c("HF_label.en","HF","HC","HC_label.en","HP_label.en","MEASURE","MEASURE_label.en","LOCATION","obsTime","obsValue","UNIT","TIME_FORMAT_label.en")
FilteredMentalHospitalData = FilteredMentalHospitalData[,which(names(FilteredMentalHospitalData) %in% ColsKeep)]
FilteredMentalHospitalData = FilteredMentalHospitalData[FilteredMentalHospitalData$HC == "HCTOT",]
FilteredMentalHospitalData = FilteredMentalHospitalData[FilteredMentalHospitalData$HF == "HFTOT",]

###### Having examined this data it only has a few countries on mental health hospital data spending
###### I've decided not to add it to analysis

  
  #### rbind all three dataframes into one, can be done as they come from same source and have similar structures
HealthData = merge(docStats,nurseStats, by = c("ï..LOCATION","TIME"))

write.csv(HealthData, "healthdata.csv")


#### Health, Nutrition, Population Stats
NutritionStats = read.csv("healthNutritionPopulationStats.csv")
nrow(NutritionStats)
ncol(NutritionStats)
summary(NutritionStats)

NutritionStats$ï..Country.Name = as.character(NutritionStats$ï..Country.Name)

### Filter by OECD Member States
filteredNutStats = filter(NutritionStats, ï..Country.Name %in% OECDCountries)
nrow(filteredNutStats)

## Filter by Years
filteredNutStats
filteredNutStatsYear = select(filteredNutStats,ï..Country.Name,Indicator.Name,Indicator.Code,c(X2000:X2010))

##Explore One Country
UKNutStats = filter(filteredNutStatsYear, ï..Country.Name == "United Kingdom")

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
SuicideRates = filter(SuicideRates, ï..country %in% OECDCountries)

write.csv(SuicideRates,"SuicideRatesYearAndCountries.csv")

#### melt the new dems data and change the column names so that each indicator is a column
NewDems = melt(CompleteFilterStats)
NewDems = NewDems[!(names(NewDems) %in% c("Indicator.Name"))]
NewDems = tidyr::spread(NewDems,Indicator.Code, value)

## Fix Country Name and Year
NewDems = NewDems %>%
  rename(
    Country = ï..Country.Name,
    Year = variable
  )
NewDems$Year = sub("X","", NewDems$Year)
NewDems$Year = as.integer(NewDems$Year)

summary(NewDems)

#### change names on Suicide Rates dataset in order for merger with Demographics Dataset
head(SuicideRates)
SuicideRates = SuicideRates %>%
  rename( Country= ï..country,
          Year = year
  )
SuicideRates = SuicideRates[!(names(SuicideRates) %in% c("country.year"))]

##
#Alter Health Data so that it can be merged with Suicide Rates
HealthData$Country = countrycode::countrycode(HealthData$ï..LOCATION,origin = 'iso3c', destination = 'country.name')
HealthData = HealthData[!(names(HealthData) %in% c("ï..LOCATION"))]
HealthData = HealthData %>%
  rename(
    Year = TIME
  )

### Merge the three datasets together
FinalData = merge(SuicideRates,HealthData, by = c("Country","Year"))
FinalDataDem = merge(FinalData, NewDems, by = c("Country","Year"))

write.csv(FinalDataDem, "DataToBeAnalysed.csv")


############## Dealing with Missing Data
MissingDataDataSet = read.csv("DataToBeAnalysed.csv")

###### Use Mice to Examine Dataset Missing Values
summary(MissingDataDataSet)
MissingDataDataSet$gdp_for_year.... = as.numeric(MissingDataDataSet$gdp_for_year....)
md.pattern(MissingDataDataSet)

library(VIM)
nrow(MissingDataDataSet)
aggr_plot <- aggr(MissingDataDataSet[4:25], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
missingvals = sapply(MissingDataDataSet, function(x){sum(is.na(x))})
head(sort(missingvals,decreasing = T))
### SM.POP.NETM and HDI have large amount of missing valus, will need to discard these
### we can use imputation for SE.MED.BEDS.ZA, SE.XPD.TOTL.GD.ZS, and SE.TER.ENRR


MissinDataColRm = MissingDataDataSet[, !(names(MissingDataDataSet) %in% c("SM.POP.NETM", "HDI.for.year"))]
library(ggplot2)
aggr_plot <- aggr(MissinDataColRm[4:24], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))
missing.values <- MissingDataDataSet %>%
  tidyr::gather(key = "key", value = "val") %>%
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

missing.values %>%
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity', fill = "black") +
  labs(x='Variable', y="number of missing values", title='Number of missing values by Variable') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

missingvals = sapply(MissinDataColRm, function(x){sum(is.na(x))})
head(sort(missingvals, decreasing = T))

####### Impute Missing Data Using Regression Trees and Classification
miceData = mice(MissinDataColRm, m=5,maxit = 100, method = 'cart', seed = 100)
summary(miceData)
miceData$data$SH.MED.BEDS.ZS
completeData = complete(miceData,1)

####Visualise No More MissingData
aggr_plot <- aggr(completeData[4:24], col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(data), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

write.csv(completeData, "dataNoMissingValues.csv")


##################################################################
#### Preproccessing is now complete, along with dealing of missing values
#### Analysis of Suicide Rates Against various factors now possible
###################################################################
##################      Data Mining KDD Section       #############
###################################################################


AnalysisData = read.csv("dataNoMissingValues.csv")

summary(AnalysisData)
ncol(AnalysisData)
nrow(AnalysisData)
##Get rid of two columns at start
AnalysisData = AnalysisData[,!(names(AnalysisData) %in% c("X.1","X"))]

####Create dataframe to show Suicide Rates by Country

SumDataFrame = aggregate(AnalysisData$suicides_no,
                         by =  list(Year = AnalysisData$Year, Country= AnalysisData$Country),
                         FUN= sum)
head(SumDataFrame)
tail(SumDataFrame)
names(SumDataFrame)
SumDataFrame  = SumDataFrame %>%
  rename(
    SuicideCount = x
  )

###Split Data

write.csv(SumDataFrame,"MapperInput.csv",row.names = F, col.names = F)
##### This CSV was split manually for the purpose of the MapReduce Algorithm
##### Run the Bash File to Obtain the Results in Mapper Output
##### The Reducer Output will be used and Visualised here

ReducerOut = read.csv("MapperOutput/ReducerOutput.csv", header = F)

names(ReducerOut) = c("Year", "Country", "SuicideNum")
ReducerOut = ReducerOut[order(ReducerOut$SuicideNum,decreasing = T ),]
head(ReducerOut)
tail(ReducerOut)
SumDataFrame$Year = as.numeric(SumDataFrame$Year)

###### Visualise Suicde Numbers By Country Over Time
ggplot(data = SumDataFrame,
       aes(x = Year, y = SuicideCount)) +
  geom_line(aes(color = Country), size = 1) +
  theme_classic()


SumDataFrame = aggregate(AnalysisData$suicides.100k.pop,
                         by =  list(Year = AnalysisData$Year, Country= AnalysisData$Country),
                         FUN= sum)

names(SumDataFrame)
SumDataFrame  = SumDataFrame %>%
  rename(
    SuicdesPer100k = x
  )

SumDataFrame= SumDataFrame[order(SumDataFrame$SuicdesPer100k,decreasing = T ),]
head(SumDataFrame)
ggplot(data = SumDataFrame,
       aes(x = Year, y = SuicdesPer100k)) +
  geom_line(aes(color = Country), size = 1) +
  scale_x_discrete() +
  labs(y= "Suicides per 100 Thousand", x = "Time") +
  theme_classic()

CorTestData = merge(SumDataFrame, AnalysisData[,c(1,2,11:22)], by = c("Year", "Country"))
CorTestData = CorTestData[!duplicated(CorTestData[c("Year","Country")]),]

cor.test(x = CorTestData$DocsPer1000, y = CorTestData$SuicdesPer100k)
qqplot(x =CorTestData$DocsPer1000, y = CorTestData$SuicdesPer100k)

cor.test(x = CorTestData$NursesPer1000, y = CorTestData$SuicdesPer100k)
qqplot(x =CorTestData$NursesPer1000, y = CorTestData$SuicdesPer100k)


plot(CorTestData$SuicdesPer100k, CorTestData$SE.TER.ENRR, xlab = "Suicides Per 100k",
     ylab = "Tertiary Education Enrollment (% of Population)",
     main = "Suicide Rates Against College Enrollment",
     col= CorTestData$Country)

ggplot(data = CorTestData,
       aes(x=SuicdesPer100k,y = SE.TER.ENRR, color=Country)) +
  labs(title = "Suicide Rates Against College Attendance") +
  xlab("Suicides per 100 Thousand") + ylab("% Population Who Have College Education") +
  theme(text = element_text(size = 10),
        axis.text.x = element_text(angle = 90, hjust = 1)) +
  geom_point()

ggplot(data = CorTestData,
       aes(x=SuicdesPer100k,y = SE.XPD.TOTL.GD.ZS)) +
  geom_point()


cor.test(x = CorTestData$SE.TER.ENRR, y = CorTestData$SuicdesPer100k)
cor.test(x = CorTestData$SE.XPD.TOTL.GD.ZS, y = CorTestData$SuicdesPer100k)
cor.test(x = CorTestData$SH.MED.BEDS.ZS, y = CorTestData$SuicdesPer100k)


cor.test(x = CorTestData$SH.XPD.PRIV.ZS, y = CorTestData$SuicdesPer100k)
cor.test(x = CorTestData$SH.XPD.PUBL.ZS, y = CorTestData$SuicdesPer100k)

### Unemplploymet Rate Correlation Test (See Conclusions in Report) Cor Value: 0.281
cor.test(x = CorTestData$SL.UEM.TOTL.ZS, y = CorTestData$SuicdesPer100k)
cor.test(x = CorTestData$SP.POP.80UP.FE, y = CorTestData$SuicdesPer100k)
cor.test(x = CorTestData$SP.RUR.TOTL.ZS, y = CorTestData$SuicdesPer100k)

CorTestData$Year = as.factor(CorTestData$Year)
ggplot(data = CorTestData,
       aes(x=SuicdesPer100k,y = SH.MED.BEDS.ZS)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y~x) +
  ggtitle("Suicide RatesAgainst Hospital Beds Per 1000") +
  labs(y="Hospital Beds Per 1000 People", x = "Suicdes Per 100 Thousand People")


##### Aggregate Data Based On Age Bracket
AgeData = aggregate(AnalysisData$suicides_no,
               by = list(Age = AnalysisData$age),
               FUN = sum)
names(AgeData)
AgeData  = AgeData %>%
  rename(
    SuicideTotal = x
  )
AgeData$SuicideTotal = as.numeric(AgeData$SuicideTotal)
AgeData = AgeData[order(AgeData$SuicideTotal),]
ggplot(data = AgeData,
       aes(x=reorder(Age, SuicideTotal), y = SuicideTotal)) +
  geom_bar(stat = "identity", fill = "red", color = "black") +
  scale_y_continuous(labels = comma) +
  ggtitle("Suicide Counts by Age Bracket") +
  xlab("Age Brackets") +
  ylab("Total Suicides") +
  theme(text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, hjust = 1))
  theme_classic()
