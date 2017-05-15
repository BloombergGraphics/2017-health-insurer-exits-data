# New York marketplace data

library(dplyr)
library(tidyr)
library(stringr)

fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_ny <- fips_codes %>% filter(fips_state == "36") %>%
	select(fips_county, county_name)

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
