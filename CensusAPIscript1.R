#title: ACS 5 year survey retrieval for NCNM
#author: Aidan O'Hara
#date: 8/25/2021


#libraries
library(dplyr)
library(censusapi)

#Census API setup
# visit https://api.census.gov/data/key_signup.html
# # # # for your very own apikey!
# Sys.setenv(CENSUS_KEY='')
# readRenviron("~/.Renviron")
# Sys.getenv("CENSUS_KEY")


### ACS data retrieval ###

#specifics
# survey name: /acs/acs
# survey term: only 5 year has ALL counties
# years: 2009-2015

# example survey key
#surveyACS52019 <- "2019/acs/acs5"
# least general code right here.
# this function keeps everything 

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

# variables or groups, following is an earmarked list of groups

#Original list of group Variables for data retrieval
groupsForVariables <- c("B01003", #note out what these are
                        "B02001",
                        "B25011",
                        "B25001",
                        "B25003",
                        "B11001")

# EXTENDING THE CENSUS RETRIEVAL for income etc. 

 # new survey name
 # new variables
 # group B19301
 # group C17002  WILL BE VERY AWK FOR TNUM
 # group B19037
 # group B19326

# getGroupVariableNames("B19301")
# Returns the formal labels of each specific variable in the group
getGroupVariableNames <- function(groupName) {
  listCensusMetadata(
    name = yATKey,
    type = "variables",
    group = groupName)
}


parseGroupVariables <- function(groupVariables) {
  refList <- bind_rows(lapply(groupVariables, getGroupVariableNames))
  print(refList)
  refListLess <- (refList[,c("name","label")])
  refListNoAnno <- refListLess[!grepl("A$", refListLess$name),]
  return(refListNoAnno)
}
 
subGroupVarLabels <- function(rawData, namesAndLabels) {
  matchVars <- namesAndLabels[namesAndLabels$name %in% names(rawData),]
  names(rawData)[match(matchVars[,"name"],names(rawData))] = paste(
    matchVars[,"label"],matchVars[,"name"], sep = "/")
  return (rawData)
} 


#functions to retrieve data from the census 
#   given a variable, variables, or variable group name

# countyGeoTag <- "049"
# includedVariables <- "B01003"
# "state:35" #NEW MEXICO


getCountyTractData2 <- function(countyGeoTag) {
  getCensus(
    name = yATKey,
    vars = c("NAME", singleVariables$name),
    region = "tract:*",
    regionin = paste(stateCode,"+county:",countyGeoTag)
  )
}

getCountyData2 <- function(countyGeoTag) {
  getCensus(
    name = yATKey,
    vars = c("NAME", singleVariables$name),
    region = paste("county:", countyGeoTag),
    regionin = stateCode
  )
}

getNMStateData2 <- function() {
  getCensus(
    name = yATKey,
    vars = c("NAME", singleVariables$name),
    region = stateCode
  )
}

getNationData2 <- function() {
  getCensus(
    name = yATKey,
    vars = c("NAME", singleVariables$name),
    region = "us:*"
  )
}


acsDataRetriever <- function(year, term, stateCode, countyGeos, groupVariables){
  yATKey <<- acsYearAndTermKey(year, term)
  stateCode <<- stateCode
  singleVariables <<- parseGroupVariables(groupVariables)
  
  tractParts <- lapply(countyGeos, getCountyTractData2)
  #get the data for each geo level, county, state, nation
  countyParts <- lapply(countyGeos, getCountyData2)
  
  natPart <- getNationData2()
  statPart <- getNMStateData2()
  
  # concatenate the different dfs into one
  rawTractData <- bind_rows(tractParts)
  rawCountyData <- bind_rows(countyParts)
  rawData <- gtools::smartbind(rawTractData, rawCountyData, 
                               statPart, natPart,  fill = NA)
  
  # switch census variable names for their corresponding labels
  labeledData <- subGroupVarLabels(rawData, singleVariables)
  
  # # add a string key indicating variable codes, and yATKey
  tagTag <- paste(tail(names(labeledData), n=1), 
                  paste("year:", year, sep = ""),
                  paste("survey:", yATKey, sep = ""), 
                  paste("group:", (paste(unlist(groupVariables), 
                                         collapse = ", ")), sep = ""), 
                  sep = ";")
  
  colnames(labeledData)[length(colnames(labeledData))] <- tagTag
  
  # keyTag <- paste(yATKey, "Variable Codes:", 
  #       (paste(unlist(groupVariables), collapse = ", ")),
  #       "(us)", sep = " ")
  # 
  # colnames(labeledData)[length(colnames(labeledData))] <- keyTag
  # 
  
  
  return(labeledData)
  
}

# example use of acsDataRetriever
#Retrieves census tract, county, state, and national data for NCNM
#acs5Data2019 <- acsDataRetriever(2019, 5, "state:35", 
#                                 acsCountyFips, groupsForVariables)

saipeRetriever <- function(stateCode, countyGeos) {
  #set the survey Name
  surveyName <- "timeseries/poverty/saipe"
  #set the state Code
  stateCode <<- stateCode
  #set up Variable scope, done in TNUM DIALOG NOTE
  singleVariables <- c("NAME",
                       "YEAR",
                       "time",
                       "SAEPOVALL_PT",
                       "SAEPOVALL_MOE",
                       "SAEPOV0_4_PT",
                       "SAEPOV0_4_MOE",
                       "SAEPOV5_17R_PT",
                       "SAEPOV5_17R_MOE",
                       "SAEPOVRT0_4_PT",
                       "SAEPOVRT0_4_MOE",
                       "SAEMHI_PT",
                       "SAEMHI_MOE")
  saipeLabels <- listCensusMetadata(name = "timeseries/poverty/saipe", 
                                    type = "variables")
  saipeLabelsRefined <- saipeLabels[ saipeLabels$name %in% singleVariables, ]
  #Call upon getCensus to compile national, state, and county observations 
  countyParts <- lapply(countyGeos, function(countyGeos) {
    getCensus(name = surveyName,
              vars = saipeLabelsRefined$name,
              region = paste("county:", countyGeos),
              regionin = stateCode)
  })
  statePart <- getCensus(name = surveyName,
                         vars = saipeLabelsRefined$name,
                         region = stateCode)
  usPart <- getCensus(name = surveyName,
                      vars = saipeLabelsRefined$name,
                      region = "us:*")
  
    # into a single DF?
  rawData <- gtools::smartbind(bind_rows(countyParts), 
                               statePart, usPart,  fill = NA)
  #switch census variable names for their labels?
  labeledData <- subGroupVarLabels(rawData, saipeLabelsRefined)
  
  #Generate some object that contains the year, survey name, and
  #  "group"
  # Attach it to the DF or pass it along to later
  return(labeledData)
}





### SAIPE Retrieval ###
### https://api.census.gov/data/timeseries/poverty/saipe/variables.html


##### OLD CODE #####
# #Use this function to rapidly retrieve the ACS data for a given year
# retrieveAndWriteAcs5YearData <- function(year) {
#   df <- aCS5DataRetriever(year)
#   write.csv(df,paste("C:\\RStudioProjects\\ncnmeddDataTool\\acs5Data",
#                      year,".csv", sep = ""))
# }

#A list of the available years, minus 2009, to make sure everything is
#    operational before trying to retrieve ALL years
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


### HELPFUL FORMAT FOR COLLECTING YEARS OF DATA ###

# # test run
# retrieveAndWriteAcs5YearData(2009)
# 
# lapply(yearsToRetreive, retrieveAndWriteAcs5YearData)
# 
# 
# # test run using 2009
# retrieveAndWriteAcs5YearData(2009)
# # retrieves and writes to csv for the years in yearsToRetrieve
# lapply(yearsToRetrieve, retrieveAndWriteAcs5YearData)


# # INFORMATIVE CENSUS FUNCTIONS # #
# 
# #apis <- listCensusApis()
# #View(apis)
# 
# #geos <- listCensusMetadata(
# #  name = surveyACS52019, #2019 ACS 5 year community survey
# #  type = "geography")
# #View(geos)
# 
# #vars <- listCensusMetadata(
# #  name = surveyACS52019,
# #  type = "variables")
# #View(vars)
# 
# #variableNames <- listCensusMetadata("acs/acs5", vintage = 2019, 
# #                                    type = "variables", group = NULL)


###   OLD STUFF /// BONE YARD   ###

# a function to read variables names, given a group name
# getGroupVariables <- function(groupName, year, term) {
#   listCensusMetadata(
#     name = paste("acs/acs", term, sep = ""),
#     vintage = year,
#     type = "variables",
#     group = groupName)
# }
# 
# #getGroupVariables("B01003", 2019, "5")
# 
# 
# 
# 
# #switching to the variables labels instead of their names.
# #namesMatched <- transfer[transfer$name %in% names(single_B01003_001ETest),]
# #names(single_B01003_001ETest)[match(namesMatched[,"name"], 
# #     names(single_B01003_001ETest))] = namesMatched[,"label"]
# 
# renameVariables <- function(rawData, year, term){
#   refList <- bind_rows(lapply(groupsForVariables, getGroupVariables,
#                               year, term))
#   refListLess <- (refList[,c("name","label")])
#   matchVars <- refListLess[refListLess$name %in% names(rawData),]
#   names(rawData)[match(matchVars[,"name"],names(rawData))] = matchVars[,"label"]
#   return(rawData)
# }
# 
# 
# 
# 
# 
# #functions to retrieve data from the census 
# #   given a variable, variables, or variable group name
# 
# # countyGeoTag <- "049"
# # includedVariables <- "B01003
# 
# getCountyData <- function(countyGeoTag, includedVariables, surveyName) {
#   getCensus(
#     name = surveyName,
#     vars = c("NAME", includedVariables),
#     region = paste("county:", countyGeoTag),
#     regionin = "state:35"
#   )
# }
# 
# getNMStateData <- function(includedVariables, surveyName) {
#   getCensus(
#     name = surveyName,
#     vars = c("NAME", includedVariables),
#     region = "state:35" #NEW MEXICO
#   )
# }
# 
# getNationData <- function(includedVariables, surveyName) {
#   passVariables <- c("NAME", includedVariables)
#   getCensus(
#     name = surveyName,
#     vars = passVariables,
#     region = "us:*"
#   )
# }
# 
# # Tract Retrieval #
# 
# #acs_income_group <- getCensus(
# #  name = "acs/acs5", 
# #  vintage = 2017, 
# #  vars = c("NAME", "group(B19013)"), 
# #  region = "tract:*", 
# #  regionin = "state:02")
# 
# getCountyTractData <- function(countyGeoTag, includedVariables, surveyName) {
#   getCensus(
#     name = surveyName,
#     vars = c("NAME", includedVariables),
#     region = "tract:*",
#     regionin = paste("state:35+county:",countyGeoTag)
#   )
# }
# 
# 
# # The sub-ultimate ACS Retrieval OSS for NCNM DATA  #
# 
# aCS5DataRetriever <- function(year) {
#   #ACS county data for all NCNM is only available for 5 year surveys
#   term = "5"  
#   # sets the survey name
#   surveyName <- acsYearAndTermKey(year,term)
#   
#   # Fixed variables and location for now, can vary in the future.
#   varsToRetrieve <- eMCensusVars
#   localsToRetrieve <- acsCountyFips
#   
#   tractParts <- lapply(localsToRetrieve, getCountyTractData,
#                        varsToRetrieve, surveyName)
#   #get the data for each geo level, county, state, nation
#   countyParts <- lapply(localsToRetrieve, getCountyData, 
#                         varsToRetrieve, surveyName)
#   
#   natPart <- getNationData(varsToRetrieve, surveyName)
#   
#   statPart <- getNMStateData(varsToRetrieve, surveyName)
#   
#   # concatenate the different dfs into one
#   rawTractData <- bind_rows(tractParts)
#   rawCountyData <- bind_rows(countyParts)
#   rawData <- gtools::smartbind(rawTractData, rawCountyData, statPart, natPart,  fill = NA)
#   
#   # switch census variable names for their corresponding labels
#   labeledData <- renameVariables(rawData, year,                                                                                                                                                                                  )
#   
#   return(labeledData)
# }
# 

# acs5Data2019 <- aCS5DataRetriever(2019)

# ear-marked census variables, I think only part of the whole list...
# eMCensusVars <- c("B01003_001E", #Total Population
#                   "B02001_002E", #Race
#                   "B02001_003E",
#                   "B02001_004E",
#                   "B02001_005E",
#                   "B02001_006E",
#                   "B02001_007E",
#                   "B02001_008E",
#                   "B02001_009E",
#                   "B02001_010E",
#                   "B25011_002E", #Housing Tenure w/extras
#                   "B25011_003E",
#                   "B25011_004E",
#                   "B25011_005E",
#                   "B25011_006E",
#                   "B25011_007E",
#                   "B25011_008E",
#                   "B25011_009E",
#                   "B25011_010E",
#                   "B25011_011E",
#                   "B25011_012E",
#                   "B25011_013E",
#                   "B25011_014E",
#                   "B25011_015E",
#                   "B25011_016E",
#                   "B25011_017E",
#                   "B25011_018E",
#                   "B25011_019E",
#                   "B25011_020E",
#                   "B25011_021E",
#                   "B25011_022E",
#                   "B25011_023E",
#                   "B25011_024E",
#                   "B25011_025E",
#                   "B25011_026E",
#                   "B25011_027E",
#                   "B25011_028E",
#                   "B25011_029E",
#                   "B25011_030E",
#                   "B25011_031E",
#                   "B25011_032E",
#                   "B25011_033E",
#                   "B25011_034E",
#                   "B25011_035E",
#                   "B25011_036E",
#                   "B25011_037E",
#                   "B25011_038E",
#                   "B25011_039E",
#                   "B25011_040E",
#                   "B25011_041E",
#                   "B25011_042E",
#                   "B25011_043E",
#                   "B25011_044E",
#                   "B25011_045E",
#                   "B25011_046E",
#                   "B25011_047E",
#                   "B25011_048E",
#                   "B25011_049E")
# 


