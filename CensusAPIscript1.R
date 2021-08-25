#libraries
library(dplyr)
library(censusapi)

#Census API setup
# visit https://api.census.gov/data/key_signup.html
# for your very own apikey!
Sys.setenv(CENSUS_KEY='')
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_KEY")


### ACS data retrieval ###

#specifics
# survey name: /acs/acs
# survey term: only 5 year has ALL counties
# year: 2009, 2014, 2019
# years, for acs5, 2019,2014,2011...
# years, for acs1, 2019,2018,2017...

# example survey key
#surveyACS52019 <- "2019/acs/acs5"

acsYearAndTermKey <- function(year, term) {
  return(paste(year, "/acs/acs", term, sep = ""))
}


# geos

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

acsCountyFips <- c("049","028","039","047","043","033","055","007")

# variables
# variable names

# variables or groups, following is an earmarked list of groups

#aBigListOVariables <- c("group(B01003)",
#                        "group(B02001)",
#                        "group(B25011)",
#                        "group(B25001)",
#                        "group(B25003)",
#                        "group(B11001)")
#

 
# ear-marked census variables
eMCensusVars <- c("B01003_001E",
                                  "B02001_002E",
                                  "B02001_003E",
                                  "B02001_004E",
                                  "B02001_005E",
                                  "B02001_006E",
                                  "B02001_007E",
                                  "B02001_008E",
                                  "B02001_009E",
                                  "B02001_010E",
                                  "B25011_002E",
                                  "B25011_003E",
                                  "B25011_004E",
                                  "B25011_005E",
                                  "B25011_006E",
                                  "B25011_007E",
                                  "B25011_008E",
                                  "B25011_009E",
                                  "B25011_010E",
                                  "B25011_011E",
                                  "B25011_012E",
                                  "B25011_013E",
                                  "B25011_014E",
                                  "B25011_015E",
                                  "B25011_016E",
                                  "B25011_017E",
                                  "B25011_018E",
                                  "B25011_019E",
                                  "B25011_020E",
                                  "B25011_021E",
                                  "B25011_022E",
                                  "B25011_023E",
                                  "B25011_024E",
                                  "B25011_025E",
                                  "B25011_026E",
                                  "B25011_027E",
                                  "B25011_028E",
                                  "B25011_029E",
                                  "B25011_030E",
                                  "B25011_031E",
                                  "B25011_032E",
                                  "B25011_033E",
                                  "B25011_034E",
                                  "B25011_035E",
                                  "B25011_036E",
                                  "B25011_037E",
                                  "B25011_038E",
                                  "B25011_039E",
                                  "B25011_040E",
                                  "B25011_041E",
                                  "B25011_042E",
                                  "B25011_043E",
                                  "B25011_044E",
                                  "B25011_045E",
                                  "B25011_046E",
                                  "B25011_047E",
                                  "B25011_048E",
                                  "B25011_049E")

groupsForVariables <- c("B01003",
                        "B02001",
                        "B25011",
                        "B25001",
                        "B25003",
                        "B11001")

# a function to read variables names, given a group name
getGroupVariables <- function(groupName, year, term) {
  listCensusMetadata(
    name = paste("acs/acs", term, sep = ""),
    vintage = year,
    type = "variables",
    group = groupName)
}

#getGroupVariables("B01003", 2019, "5")


#switching to the variables labels instead of their names.
#namesMatched <- transfer[transfer$name %in% names(single_B01003_001ETest),]
#names(single_B01003_001ETest)[match(namesMatched[,"name"], 
#     names(single_B01003_001ETest))] = namesMatched[,"label"]

renameVariables <- function(rawData, year, term){
  refList <- bind_rows(lapply(groupsForVariables, getGroupVariables,
                              year, term))
  refListLess <- (refList[,c("name","label")])
  matchVars <- refListLess[refListLess$name %in% names(rawData),]
  names(rawData)[match(matchVars[,"name"],names(rawData))] = matchVars[,"label"]
  return(rawData)
}





#functions to retrieve data from the census 
#   given a variable, variables, or variable group name

# countyGeoTag <- "049"
# includedVariables <- "B01003

getCountyData <- function(countyGeoTag, includedVariables, surveyName) {
  getCensus(
    name = surveyName,
    vars = c("NAME", includedVariables),
    region = paste("county:", countyGeoTag),
    regionin = "state:35"
  )
}

getNMStateData <- function(includedVariables, surveyName) {
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



# The sub-ultimate ACS Retrieval OSS for NCNM DATA  #

aCS5DataRetriever <- function(year) {
  #ACS county data for all NCNM is only available for 5 year surveys
  term = "5"  
  # sets the survey name
  surveyName <- acsYearAndTermKey(year,term)
  
  # Fixed variables and location for now, can vary in the future.
  varsToRetrieve <- eMCensusVars
  localsToRetrieve <- acsCountyFips
  
  tractParts <- lapply(localsToRetrieve, getCountyTractData,
                       varsToRetrieve, surveyName)
  #get the data for each geo level, county, state, nation
  countyParts <- lapply(localsToRetrieve, getCountyData, 
                        varsToRetrieve, surveyName)
  
  natPart <- getNationData(varsToRetrieve, surveyName)
  
  statPart <- getNMStateData(varsToRetrieve, surveyName)
  
  # concatenate the different dfs into one
  rawTractData <- bind_rows(tractParts)
  rawCountyData <- bind_rows(countyParts)
  rawData <- gtools::smartbind(rawTractData, rawCountyData, statPart, natPart,  fill = NA)
  
  # switch census variable names for their corresponding labels
  labeledData <- renameVariables(rawData, year, term)
  
  return(labeledData)
}



acs5Data2009 <- aCS5DataRetriever(2009) 
acs5Data2010 <- aCS5DataRetriever(2010)
acs5Data2014 <- aCS5DataRetriever(2014)
acs5Data2019 <- aCS5DataRetriever(2019)

retrieveAndWriteAcs5YearData <- function(year) {
  df <- aCS5DataRetriever(year)
  write.csv(df,paste("C:\\RStudioProjects\\ncnmeddDataTool\\acs5Data",
                     year,".csv", sep = ""))
}

yearsToRetrieve <- c(2010,
                     2011,
                     2012,
                     2013,
                     2014,
                     2015,
                     2016,
                     2017,
                     2018,
                     2019)

# test run
retrieveAndWriteAcs5YearData(2009)

lapply(yearsToRetreive, retrieveAndWriteAcs5YearData)


# Tract Retrieval #

#acs_income_group <- getCensus(
#  name = "acs/acs5", 
#  vintage = 2017, 
#  vars = c("NAME", "group(B19013)"), 
#  region = "tract:*", 
#  regionin = "state:02")

# I think the move here is an lapply/bind_rows application

# ply our list of counties against the api, calling for all their census tracts

getCountyTractData <- function(countyGeoTag, includedVariables, surveyName) {
  getCensus(
    name = surveyName,
    vars = c("NAME", includedVariables),
    region = "tract:*",
    regionin = paste("state:35+county:",countyGeoTag)
  )
}




#tests for censusapi library

#on the chopping block for non-necessity
#apis <- listCensusApis()
#View(apis)

#geos <- listCensusMetadata(
#  name = surveyACS52019, #2019 ACS 5 year community survey
#  type = "geography"
#)
#View(geos)

#vars <- listCensusMetadata(
#  name = surveyACS52019,
#  type = "variables"
#)
#View(vars)









                                  


#variableNames <- listCensusMetadata("acs/acs5", vintage = 2019, 
#                                    type = "variables", group = NULL)
# retrieves the specific variable names for acs5 2019, ALL variable names...
#   may be overdoing it a bit here, might be the move to go for groups, one at
#   a time.

# build a df for a variable group's observations in ALL counties
#group_B01003parts <- lapply(countyFips,getCountyData,"group(B01003)")
#group_B01003df <- bind_rows(group_B01003parts, .id = "column_label")


#group_B01003RawDataTest <- concatenateACSRawData("group(B01003)","2014","5")
#single_B01003_001ETest <- concatenateACSRawData("B01003_001E","2014", "5")


                         
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




