#libraries
library(dplyr)
library(censusapi)

#Census API setup
# visit https://api.census.gov/data/key_signup.html
# for your very own apikey!
Sys.setenv(CENSUS_KEY=='Your_Key_Here')
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_KEY")

surveyACS52019 <- "2019/acs/acs5"
acsYearAndTermKey <- function(year, term) {
  return(paste(year,"/acs/acs",term, sep = ""))
}


#tests for censusapi library
apis <- listCensusApis()
View(apis)

geos <- listCensusMetadata(
  name = surveyACS52019, #2019 ACS 5 year community survey
  type = "geography"
)
View(geos)

vars <- listCensusMetadata(
  name = surveyACS52019,
  type = "variables"
)
View(vars)


# years, for acs5, 2019,2014,2011...
# years, for acs1, 2019,2018,2017...

# acs geos
#List of County refIDs
#Santa Fe -- 049
#Los Alamos -- 028
#Rio Arriba -- 039
#San Miguel -- 047
#Sandoval -- 043
#Mora -- 033
#Taos -- 055
#Colfax -- 007

acsCountyFips <- c("049","028","039","047","043","003","055","007")



#functions to retrieve 5 year acs 2019 data from the census 
#   given a variable, or variable group name
getCountyData <- function(countyGeoTag, includedVariables, surveyName) {
  getCensus(
  name = surveyName,
  vars = c("NAME", includedVariables),
  region = paste("county:", countyGeoTag),
  regionin = "state:35"
)
}

getStateData <- function(surveyName, includedVariables) {
  getCensus(
    name = surveyName,
    vars = c("NAME", includedVariables),
    region = "state:35" #NEW MEXICO
  )
}

getNationData <- function(includedVariables, surveyName) {
  passVariables <- c("NAME", includedVariables)
  getCensus(
    name = surveyName,
    vars = passVariables,
    region = "us:*"
  )
}

# variables or groups, following is an earmarked list of groups

aBigListOVariables <- c("group(B01003)",
                        "group(B02001)",
                        "group(B25011)",
                        "group(B25001)",
                        "group(B25003)",
                        "group(B11001)")


# a function to read variables names, given a group name
getGroupVariables <- function(groupName) {
  listCensusMetadata(
    name = "acs/acs5",
    vintage = 2019,
    type = "variables",
    group = groupName)
}

getGroupVariables("B01003")


#ex: getGroupVariables("B19013")

# build a df for a variable group's observations in ALL counties
group_B01003parts <- lapply(countyFips,getCountyData,"group(B01003)")
group_B01003df <- bind_rows(group_B01003parts, .id = "column_label")

concatenateACSRawData <- function(groupVariable,year,term) {
  surveyName <- acsYearAndTermKey(year,term)
  parts <- lapply(acsCountyFips, getCountyData, groupVariable, surveyName)
  natPart <- getNationData(groupVariable, surveyName)
  statPart <- getStateData(surveyName, groupVariable)
  rawData <- bind_rows(parts)
  rawData <- gtools::smartbind(rawData, natPart, statPart , fill = NA)
  return(rawData)
}


group_B01003RawDataTest <- concatenateACSRawData("group(B01003)","2014","5")

# NEXT STEPS
#  - add state and national observations to df,
#  - streamline retrieving another group of variables, 
#     - cbind to build a single dataframe...





# EXPERIMENT ZONE enter at your own risk
#   efforts to retrieve all group variable data for all counties

#combined <- expand.grid(countyFips, aBigListOVariables)
#test <- mapply(getCountyData, combined[,1], combined[,2])


#function testing
#test inclusded variables ""IPRCAT", "IPR_DESC", "PCTUI_PT"" 
#testIncluded <- c("IPRCAT", "IPR_DESC", "PCTUI_PT")
#testIncluded2 <- c("B01001_001E")
#getNationData(testIncluded2)
#getStateData(testIncluded2)
#getCountyData("007",testIncluded2)




