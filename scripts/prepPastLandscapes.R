# Join state-based landscape files and add fips to 2014-2016 files

library(dplyr)
library(tidyr)
library(stringr)

####################################################################################
# 2014-2016 landscape plans, retrieved from data.healthcare.gov on 04/03/17
# No fips codes :(
####################################################################################

formatColnames <- function(dt) {
	colnames(dt) <- gsub('[.]', '_',tolower(colnames(dt)))
	colnames(dt) <- gsub('___', '_',tolower(colnames(dt)))
	colnames(dt) <- gsub('__', '_',tolower(colnames(dt)))
	# Rating areas
	dt <- dt %>% mutate(rating_area = as.numeric(str_replace_all(rating_area, "Rating Area ", "")))
	return(dt)
}

plans14 <- read.csv("data-original/landscape/2014_QHP_Landscape_Individual_Market_Medical.csv", stringsAsFactors = F)
id14 <- read.csv("data-original/landscape/2014_QHP_Landscape_ID_Individual_Market_Medical.csv", stringsAsFactors = F)
nm14 <- read.csv("data-original/landscape/2014_QHP_Landscape_NM_Individual_Market_Medical.csv", stringsAsFactors = F)
plans14 <- rbind(plans14, id14, nm14)

plans15 <- read.csv("data-original/landscape/2015_QHP_Landscape_Individual_Market_Medical.csv", stringsAsFactors = F)
nm15 <- read.csv("data-original/landscape/2015_QHP_Landscape_NM_Individual_Market_Medical.csv", stringsAsFactors = F)
nv15 <- read.csv("data-original/landscape/2015_QHP_Landscape_NV_Individual_Market_Medical.csv", stringsAsFactors = F)
or15 <- read.csv("data-original/landscape/2015_QHP_Landscape_OR_Individual_Market_Medical.csv", stringsAsFactors = F)
plans15 <- rbind(plans15, nm15, nv15, or15)

plans16 <- read.csv("data-original/landscape/2016_QHP_Landscape_Individual_Market_Medical.csv", stringsAsFactors = F)
hi16 <- read.csv("data-original/landscape/2016_QHP_Landscape_HI_Individual_Market_Medical.csv", stringsAsFactors = F)
nm16 <- read.csv("data-original/landscape/2016_QHP_Landscape_NM_Individual_Market_Medical.csv", stringsAsFactors = F)
nv16 <- read.csv("data-original/landscape/2016_QHP_Landscape_NV_Individual_Market_Medical.csv", stringsAsFactors = F)
or16 <- read.csv("data-original/landscape/2016_QHP_Landscape_OR_Individual_Market_Medical.csv", stringsAsFactors = F)
plans16 <- rbind(plans16, hi16, nm16, nv16, or16)

rm(nm14, nm15, nm16, nv15, nv16, or15, or16, hi16, id14)

plans14 <- formatColnames(plans14)
plans15 <- formatColnames(plans15)
plans16 <- formatColnames(plans16)

####################################################################################
# Add fips codes to names and state abbrevs
####################################################################################
makeNameMatch <- function(dt) {
	dt <- dt %>% mutate(name_match = toupper(str_replace_all(county_name, " County", ""))) %>%
		mutate(name_match = str_replace_all(name_match, " PARISH", ""),
					 name_match = str_replace_all(name_match, " CITY AND BOROUGH", ""),
					 name_match = str_replace_all(name_match, " BOROUGH", ""),
					 name_match = str_replace_all(name_match, " CENSUS AREA", ""),
					 name_match = str_replace_all(name_match, " MUNICIPALITY", ""),
					 name_match = str_replace_all(name_match, " ", ""),
					 name_match = str_replace_all(name_match, "'", ""),
					 name_match = str_replace_all(name_match, "-", ""),
					 name_match = str_replace_all(name_match, "SAINT", "ST"),
					 name_match = str_replace_all(name_match, "ST\\.", "ST"),
					 name_match = str_replace_all(name_match, "STE\\.", "STE"),
					 # Stupid misspellings in 2014 data, other special cases
					 name_match = str_replace_all(name_match, "MANASSUS", "MANASSAS"),
					 name_match = str_replace_all(name_match, "MENOMONEE", "MENOMINEE"),
					 name_match = str_replace_all(name_match, "ARTIC", "ARCTIC"),
					 name_match = str_replace_all(name_match, "STJOHNBAPTIST", "STJOHNTHEBAPTIST"),
					 name_match = str_replace_all(name_match, "RADFORDCITY", "RADFORD"),
					 name_match = str_replace_all(name_match, "BRISTOLCITY", "BRISTOL"),
					 name_match = str_replace_all(name_match, "POQUOSONCITY", "POQUOSON"),
					 name_match = str_replace_all(name_match, "SCOTTBLUFF", "SCOTTSBLUFF"),
					 name_match = str_replace_all(name_match, "SALEMCITY", "SALEM"),
					 name_match = str_replace_all(name_match, "NORTHUMBERLND", "NORTHUMBERLAND"),
					 name_match = str_replace_all(name_match, "E\\.", "EAST"),
					 name_match = str_replace_all(name_match, "W\\.", "WEST"))
	return(dt)
}
# Reference file
fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_codes <- makeNameMatch(fips_codes) %>%
	rename(fips_name = county_name)

# Merge fips codes to names
counties14 <- plans14 %>% group_by(state, county) %>%
	summarize(n = n()) %>%
	select(-n) %>% 
	rename(county_name = county)
counties14 <- makeNameMatch(counties14)

counties15 <- plans15 %>% group_by(state, county) %>%
	summarize(n = n()) %>%
	select(-n) %>% 
	rename(county_name = county)
counties15 <- makeNameMatch(counties15)

counties16 <- plans16 %>% group_by(state_code, county_name) %>%
	summarize(n = n()) %>%
	select(-n)
counties16 <- makeNameMatch(counties16)

counties14 <- left_join(counties14, fips_codes, by = c("state" = "state_code", "name_match" = "name_match"))
counties14[is.na(counties14$fips_name),]

counties15 <- left_join(counties15, fips_codes, by = c("state" = "state_code", "name_match" = "name_match"))
counties15[is.na(counties15$fips_name),]

counties16 <- left_join(counties16, fips_codes, by = c("state_code", "name_match"))
counties16[is.na(counties16$fips_name),]

####################################################################################
# Merge back on to landscape files and save
####################################################################################
counties14 <- counties14 %>% select(-name_match)
counties15 <- counties15 %>% select(-name_match)
counties16 <- counties16 %>% select(-name_match)

plans14 <- left_join(plans14, counties14, by = c("state", "county" = "county_name"))
plans14 <- plans14 %>% select(fips_county, fips_state, fips_name, everything()) %>%
	rename(county_name = fips_name, state_code = state) %>%
	select(-county)

plans15 <- left_join(plans15, counties15, by = c("state", "county" = "county_name"))
plans15 <- plans15 %>% select(fips_county, fips_state, fips_name, everything()) %>%
	rename(county_name = fips_name, state_code = state) %>%
	select(-county)

plans16 <- left_join(plans16, counties16, by = c("state_code", "county_name"))
plans16 <- plans16 %>% select(fips_county, fips_state, fips_name, everything()) %>%
	select(-county_name) %>%
	rename(county_name = fips_name)

write.csv(plans14, "data-original/landscape/2014_QHP_Landscape_Individual_Market_Medical_full.csv", row.names = F, na="")
write.csv(plans15, "data-original/landscape/2015_QHP_Landscape_Individual_Market_Medical_full.csv", row.names = F, na="")
write.csv(plans16, "data-original/landscape/2016_QHP_Landscape_Individual_Market_Medical_full.csv", row.names = F, na="")
