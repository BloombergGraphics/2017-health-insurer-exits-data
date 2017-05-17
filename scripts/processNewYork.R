# New York marketplace data

library(dplyr)
library(tidyr)
library(stringr)

fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_ny <- fips_codes %>% filter(fips_state == "36") %>%
	select(fips_county, county_name)

source("scripts/commonFunctions.R")

# HIOS ids for matching, from makeHiosIds.R
ids_ny <- getHiosFull("NY")

####################################################################################
# Insurers
# https://info.nystateofhealth.ny.gov/PlansMap
# PDF from:
# 2017 https://info.nystateofhealth.ny.gov/sites/default/files/2017%20Individual%20Marketplace%20Plans%20by%20county.pdf
# 2016 https://info.nystateofhealth.ny.gov/sites/default/files/2016%20Individual%20Marketplace%20Plans%20by%20county_0.pdf
# 2015 https://info.nystateofhealth.ny.gov/sites/default/files/2015%20Medical%20Plans%20by%20County%20-%20Individual%2C%20Nov%202015_0.pdf
# 2014 https://info.nystateofhealth.ny.gov/sites/default/files/Certified%20Plans%20by%20Counties%20and%20Boroughs%2C%205-22-14.pdf
# Processed in Tabula
####################################################################################

formatRaw <- function(dt, year) { 
	dt <- dt %>% gather(issuer_name, plans, -county_name) %>%
		filter(!is.na(plans)) %>%
		filter(plans != "") %>%
		select(-plans) %>%
		arrange(county_name) %>%
		mutate(state_code = "NY", year = year,
					 issuer_name = str_replace_all(issuer_name, "[.]", " "))
	return(dt)
}
# 2017
ny_raw17 <- read.csv("data-original/state-based/raw/tabula-2017-ny Individual Marketplace Plans by county.csv", stringsAsFactors = F)
ny_raw17 <- ny_raw17 %>% rename(county_name = X)
ny17 <- formatRaw(ny_raw17, 2017)

ny_raw16 <- read.csv("data-original/state-based/raw/tabula-2016-ny Individual Marketplace Plans by county_0.csv", stringsAsFactors = F)
ny_raw16 <- ny_raw16 %>% rename(county_name = County)
ny16 <- formatRaw(ny_raw16, 2016)

ny_raw15 <- read.csv("data-original/state-based/raw/tabula-2015-ny Medical Plans by County - Individual, Nov 2015_0.csv", stringsAsFactors = F)
ny_raw15 <- ny_raw15 %>% rename(county_name = X)
ny15 <- formatRaw(ny_raw15, 2015)

ny <- rbind(ny15, ny16, ny17)

# Add fips codes (2010 - NY hasn't changed)
fips_ny$name_match <- str_replace_all(fips_ny$county_name, " County", "")
fips_ny <- fips_ny %>% mutate(name_match = ifelse(name_match == "St. Lawrence", "Saint Lawrence", name_match)) %>%
	rename(fips_name = county_name)

ny <- left_join(ny, fips_ny, by = c("county_name" = "name_match"))
ny <- ny %>% select(-county_name) %>% 
	rename(county_name = fips_name) %>%
	select(year, fips_county, everything())


####################################################################################
# 2014 Insurers
# https://info.nystateofhealth.ny.gov/sites/default/files/Certified%20Plans%20by%20Counties%20and%20Boroughs%2C%205-22-14.pdf
# Big PDF list garbage format
####################################################################################
ny_raw14 <- read.csv("data-original/state-based/raw/tabula-2014-ny Certified Plans by Counties and Boroughs, 5-22-14.csv", stringsAsFactors = F, header = F)
# Make a county column
ny_raw14 <- ny_raw14 %>% mutate(county_name = ifelse(str_detect(V1, "Bronx"), "Bronx County",
																										 ifelse(str_detect(V1, "Manhattan"), "New York County",
																										 ifelse(str_detect(V1, "Richmond"), "Richmond County",
																										 ifelse(str_detect(V1, "Kings"), "Kings County",
																										 ifelse(str_detect(V1, "Queens"), "Queens County",
																										 ifelse(str_detect(V1, "County"), V1, NA))))))) %>%
	# remove "Individual marketplace" rows
	filter(V1 != "Individual Marketplace") %>%
	# propogate down county names
	fill(county_name) %>%
	# remove county name rows
	filter(!str_detect(V1, "County") & !str_detect(V1, "Bronx") & !str_detect(V1, "Queens") ) %>%
	rename(issuer_name = V1)

# Join fips
ny14 <- left_join(ny_raw14, fips_ny, by = c("county_name" = "fips_name"))
ny14 <- ny14 %>% mutate(year = 2014, state_code = "NY") %>%
	select(-name_match)

ny <- rbind(ny, ny14)
ny <- ny %>% arrange(year, fips_county)

####################################################################################
# Add HIOS ids and standardized names
# Match from SBM PUF where possible


### NOTES: Verified in NYSOH plan browser, 2017 ###
# Some companies market differently upstate and downstate (but single HIOS co.)
# Empire BlueCross (upstate) and Empire BlueCross Blue Shield (downstate) are
# both HIOS ID 80519 (company name Empire HealthChoice HMO, Inc.)

# Excellus BCBS does business as Univera in some upstate counties but as Excellus downstate
# Both Excellus BCBS and Excellus dba Univera are HIOS ID 78124 ("Excellus Health Plan, Inc.")

# HealthNow dba BCBS of Western NY (49526) and BS of Northeastern NY (36346) are DIFFERENT HIOS ids

# Fidelis care is HIOS id 25303 - company name "New York State Catholic Health Plan, Inc."

# 2014 company Today's Options [American Progressive] full name is
# "American Progressive Life & Health Insurance Company of New York dba Today's Options"
# HIOS id 31808 (via https://www.cms.gov/CCIIO/Programs-and-Initiatives/Premium-Stabilization-Programs/Downloads/RC-Issuer-level-Report.pdf)
####################################################################################
table(ny$issuer_name)
table(ids_ny$issuer_name)
ny <- ny %>% mutate(issuer_name = ifelse(issuer_name == "Capital District Physicians Health Plan", "Capital District Physicians Health Plan, Inc.",
																	ifelse(issuer_name %in% c("CareConnect Insurance Company", "North Shore LIJ"), "North Shore-LIJ CareConnect Insurance Company, Inc.",
																	ifelse(issuer_name == "CDPHP", "Capital District Physicians Health Plan, Inc.",
																	ifelse(issuer_name %in% c("EmblemHealth", "Health Insurance Plan of Greater New York  EmblemHealth "), "Health Insurance Plan of Greater New York",
																	# Empire Blues market differently but are the same company
																	ifelse(issuer_name %in% c("Empire Blue Cross", "Empire BlueCross"),"Empire HealthChoice HMO, Inc. dba Empire BlueCross",
																	ifelse(issuer_name %in% c("Empire Blue Cross Blue Shield", "Empire BlueCross BlueShield"), "Empire HealthChoice HMO, Inc. dba Empire BlueCross BlueShield",
																	# Excellus Blues market differently but are the same company
																	ifelse(issuer_name %in% c("Excellus Blue Cross Blue Shield", "Excellus BlueCross BlueShield"), "Excellus Health Plan, Inc. dba Excellus BlueCross BlueShield",
																	ifelse(issuer_name %in% c("Excellus d b a Univera Healthcare", "Excellus dba Univera Healthcare", "Univera"), "Excellus Health Plan, Inc. dba Univera Healthcare",
																	ifelse(issuer_name == "Fidelis Care", "New York State Catholic Health Plan, Inc. dba Fidelis Care",
																	ifelse(issuer_name == "Health Republic [Freelancers]", "Freelancers Health Service Corporation",
																	ifelse(issuer_name %in% c("Healthfirst", "Healthfirst New York"), "Healthfirst PHSP, Inc.",
																	ifelse(issuer_name %in% c("HealthNow dba BlueCross BlueShield of Western N", "HealthNow dba BlueCross BlueShield of Western New York", 
																														"HealthNow dba BlueCross BlueShield of Western NY"), 
																				 "BlueCross BlueShield of Western New York",
																	ifelse(issuer_name %in% c("HealthNow dba BlueShield of Northeastern New York", "HealthNow dba BlueShield of Northeastern NY"),
																				 "BlueShield of Northeastern New York",
																	ifelse(issuer_name == "Independent Health", "Independent Health Benefits Corporation",
																	ifelse(issuer_name == "MetroPlus Health Plan", "MetroPlus Health Plan, Inc.",
																	ifelse(issuer_name %in% c("MVP", "MVP Health Plan"), "MVP Health Plan, Inc.",
																	ifelse(issuer_name %in% c("Oscar", "Oscar Insurance"), "Oscar Insurance Corporation",
																	ifelse(issuer_name == "Today's Options [American Progressive]", "American Progressive Life & Health Insurance Company of New York dba Today's Options",
																	ifelse(issuer_name %in% c("United", "United Healthcare"), "UnitedHealthcare of New York, Inc.",
																	ifelse(issuer_name == "Wellcare", "WellCare of New York, Inc.",
																				 issuer_name)))))))))))))))))))))

table(ny$issuer_name)

ids_join <- ids_ny %>% group_by(issuer_name, issuer_id) %>%
	summarize(temp = n()) %>%
	select(-temp)
table(ids_join$issuer_name)

ny <- left_join(ny, ids_join, by = "issuer_name")
ny <- ny %>% mutate(issuer_id = ifelse(issuer_name %in% c("Empire HealthChoice HMO, Inc. dba Empire BlueCross", "Empire HealthChoice HMO, Inc. dba Empire BlueCross BlueShield"), 80519,
																		ifelse(issuer_name %in% c("Excellus Health Plan, Inc. dba Excellus BlueCross BlueShield", "Excellus Health Plan, Inc. dba Univera Healthcare"), 78124,
																		ifelse(issuer_name == "New York State Catholic Health Plan, Inc. dba Fidelis Care", 25303,
																		ifelse(issuer_name == "American Progressive Life & Health Insurance Company of New York dba Today's Options", 31808,
																					 issuer_id)))))
summary(ny$issuer_id)

write.csv(ny, "data-original/state-based/ny-insurers.csv", row.names = F, na = "")

####################################################################################
# County enrollment
# 2017 https://info.nystateofhealth.ny.gov/sites/default/files/2017%20OEP%20Number%20of%20Enrollees%2C%20By%20Program%20and%20County_0.pdf
####################################################################################
ny_enroll17 <- read.csv("data-original/state-based/raw/tabula-2017-ny-enrollment OEP Number of Enrollees, By Program and County_0.csv", stringsAsFactors = F)
colnames(ny_enroll17) <- tolower(colnames(ny_enroll17))
ny_enroll17[,c(2:6)] <- lapply(ny_enroll17[,c(2:6)],function(x){as.numeric(gsub(",", "", x))})

# Process for national file
ny_enroll17 <- ny_enroll17 %>%
	rename(county_name = county) %>%
	filter(county_name != "Total") %>%
	mutate(county_name = paste(county_name, "County", sep = " "))

ny_enroll17 <- left_join(ny_enroll17, fips_ny, by = "county_name")

# Fips, etc
ny_enroll17 <- ny_enroll17 %>% mutate(year = 2017, state_code = "NY") %>%
	select(year, fips_county, county_name, state_code, everything()) %>%
	arrange(year, fips_county)

# Only need QHP for now but EP, etc data may be useful for future - save
write.csv(ny_enroll17, "data-original/state-based/2017-ny-county-enrollment-allprograms.csv", row.names = F, na = "")

# Format for national file
ny_enroll17 <- ny_enroll17 %>% select(-medicaid, -chp, -ep, -total) %>%
	rename(plan_selections = qhp)
write.csv(ny_enroll17, "data-original/state-based/ny-county-enrollment.csv", row.names = F, na = "")
