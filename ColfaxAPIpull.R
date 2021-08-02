
#first use of censusapi, tests for one county and one variable group.

#https://api.census.gov/data/2019/acs/acs5
#?get=NAME,B01001_001E&for=county:007&in=state:35

colfax <- getCensus(
  name = "2019/acs/acs5",
  vars = c("NAME", "group(B19013)"),
  region = "county:007",
  regionin = "state:35"
)
colfax
View(colfax)

# See descriptions of the variables in group B19013
group_B19013 <- listCensusMetadata(
  name = "acs/acs5",
  vintage = 2017,
  type = "variables",
  group = "B19013")
group_B19013

#some code to re-name the 'colfax' variable names using the group_B19013
