#True Numbers intake test space
# HEADER HERE

library(tnum)
library(stringr)

source("CensusAPIscript1.R")

acs5Data2019 <- aCS5DataRetriever(2019)

#select just the first row, and first 5 columns...
populationTotalTest <- acs5Data2019[2,(1:5)]

evenFirsterTnum <- tnum.makeObject(
  subject = "2019/new_mexico/county:santa_fe/tract:12-05",
  property = "population:estimated:total",
  value = 5719
)

#generalized subject
# year/stateName/county:countyName/tract:tractNumber

firstTnum <- tnum.makeObject(
  subject = "university:state:montclair",
  property = "rate:graduation",
  value = populationTotalTest[,5],
  unit = "%"
)


testSetColfax <- acsDataRetriever(2019, 5, "state:35", 
                                 "007", groupsForVariables)



names <- colnames(testSetColfax, do.NULL = TRUE)
str_split(names[89], "!!", n = Inf, simplify = FALSE)
words <- lapply(names, str_split, "!!", n = Inf, simplify = FALSE)
words2 <- unlist(words, recursive = TRUE)
words3 <- unique(words2, incomparables = FALSE)

uniqueDescriptors <- function(dataSet) {
  names <- colnames(dataSet, do.NULL = TRUE) %>%
    lapply(str_split, "!!", n = Inf, simplify = FALSE) %>%
    unlist(recursive = TRUE) %>%
    unique(incomprables = FALSE)
  return(names)
}

testSFGroupB02001 <- acsDataRetriever(2019, 5, "state:35",
                                      "049", "B02001")

santaFeTnum <- tnum.makeObject(
  subject = "new_mexico/county:santa_fe/tract:000200",
  property = "some_other_race:two_plus_races/population:estimated",
  value = 17,
  tags = c("2019/acs/acs5", "group_B02001")
)


value = 17

tags = c(yATKey, groupVariablesInAList) #also see far right column first row

colonSep <- function(stringsToCombine) {
  paste(stringsToCombine, collapse = ":")
}

slashSep <- function(stringsToCombine) {
  paste(stringsToCombine, collapse = "/")
}

# subject = paste("new_mexico",
#                 paste("county", "santa_fe", sep = ":"),
#                 paste("tract", "000200", sep = ":"), sep = "/")
# #"new_mexico/county:santa_fe/tract:000200"

subject2 = slashSep(c("new_mexico",
                     colonSep(c("county","santa_fe")),
                     colonSep(c("tract", "000200"))))

# property = paste(paste("some_other_race", "two_plus_races", sep = ":"),
#                  paste("population", "estimated", sep = ":"),
#                  sep = "/")

property2 = slashSep(c(colonSep(c("some_other_race", "two_plus_races")),
                       colonSep(c("population", "estimated"))))

#Next up, we try to make a function that will read a column of values and make
# true numbers for each value, In other words with a set property, 
# by reading in the value, 
# and parsing its associated geo's name into a subject. We get true numbers!!!!

#pretty slick...

# Going for Column 7 of the testSFGroupB02001 set
#Set the property:
theProperty = slashSep(c(colonSep(c("some_other_race", "and", "two_plus_races")),
                       colonSep(c("population", "estimated"))))

#Read in the (NAME, NUMBER) tuple from the data set
# EX VALUE # 17 #
testSFGroupB02001[41,7]

theReference = testSFGroupB02001[, c(1:4,7)]

theValues = testSFGroupB02001[,7]

#setTheSubjectValue(testSFGroupB02001[2,])
setTheSubjectValue <- function(valueNameReference) {
  countyTitle <- generateCountyString(valueNameReference['NAME'])
  if (is.na(valueNameReference['tract'])) {
    if (is.na(valueNameReference['county'])) {
      if (is.na(valueNameReference['state'])) {
        subject <- ("united_states")
      }
      subject <- ("new_mexico")
    }
    subject <- (slashSep(c("new_mexico",
                      colonSep(c("county", countyTitle)))))
  }
  subject <- (slashSep(c("new_mexico",
                    colonSep(c("county", countyTitle)),
                    colonSep(c("tract", valueNameReference['tract'])))))
  return(c(subject, valueNameReference[7]))  #HARD
}

# apply(testSFGroupB02001, MARGIN = 1, setTheSubject)
#this call makes a big old list of subject values for TrueNumbers

  # 
  # 
  # 
  # 
  # 
  # if (last(valueNameReference) == "1") {
  #   return "united_states"
  # } else if (isNA.valueNameReference[2]) {
  #   return "new_mexico"
  # }
  # 
# subject = slashSep(c(valueNameReference[],
#                         colonSep(c("county","santa_fe")),
#                         colonSep(c("tract", "000200"))))

#use apply
# 
# str_split(testSFGroupB02001[4,4], ", ")
# 
# 
# nameList <- unlist(str_split(valueNameReference['NAME'], ", "))
# countyName <- nameList[grepl("County", nameList)]

generateCountyString <- function(nameString) {
   parts <- unlist(str_split(nameString, ", "))
   countyName <- parts[grepl("County", parts)]
   partsAgain <- unlist(str_split(countyName, " "))
   noCounty <- partsAgain[!grepl("County", partsAgain)]
   combined <- paste(noCounty, collapse = '_')
   lowered <- str_to_lower(combined)
   return(lowered)
}



oneTrueNumberPlease <- function(subjectValue, property){
  aTNum <- tnum.makeObject(
    subject = unlist(subjectValue[1]),
    property = property,
    value = unname(unlist(subjectValue[2])),
    tags = c("2019/acs/acs5", "group_B02001") # will generalize later
  )
  return(aTNum)
}


