#Set the Census API key in your R environment
Sys.setenv(CENSUS_KEY='Your_Key_Here')

#Re-load the R-Environment with the Key installed
readRenviron("~/.Renviron")

Sys.getenv("CENSUS_KEY")

#load the censusapi library
library(censusapi)

#Code to load the list of available APIs
apis <- listCensusApis()
View(apis)

#How to view the available geos for a dataset
geos <- listCensusMetadata(
  name = "2019/acs/acs5",
  type = "geography"
)
View(geos)

#How to view the available variables for a dataset
vars <- listCensusMetadata(
  name = "2019/acs/acs5",
  type = "variables"
)
View(vars)
