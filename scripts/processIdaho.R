# Idaho data

library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_id <- fips_codes %>% filter(state_code == "ID") %>%
	select(fips_county, county_name)

####################################################################################
# Add fips to rating areas
####################################################################################

id_ra <- read.csv("data-original/state-based/2017-id-ratingareas.csv", stringsAsFactors = F)
id_ra$county_name <- paste(str_trim(id_ra$county_name, "both"), "County", sep = " ")

id_ra <- left_join(id_ra, fips_id, by="county_name")

write.csv(id_ra, "data-original/state-based/2017-id-ratingareas.csv", row.names = F, na = "")

####################################################################################
# QHP landscapes from http://www.doi.idaho.gov/company/other/qhp.aspx
# http://www.doi.idaho.gov/Consumer/Docs/ID%202017%20QHP%20Landscape%20Data%20Tables%202016-10-20.zip
# http://www.doi.idaho.gov/Consumer/Docs/ID%202016%20QHP%20Landscape%20Data%20Tables%202015-11-18.zip
# http://www.doi.idaho.gov/Consumer/Docs/ID%202015%20QHP%20Landscape%20Data%20Tables%202014-12-12.zip
# 2014 was in healthcare.gov

# Save processed & prepped for analysis
####################################################################################

id_plans15 <- readWorkbook("documents/id/ID 2015 QHP Landscape Data Tables 2014-12-12.xlsx", sheet = "Medical-Individual")
id_plans16 <- readWorkbook("documents/id/ID 2016 QHP Landscape Data Tables 2015-11-18.xlsx", sheet = "Medical-Individual")
id_plans17 <- readWorkbook("documents/id/ID 2017 QHP Landscape Data Tables 2016-10-20.xlsx", sheet = "Medical-Individual")

prepLandscape <- function(dt) {
	colnames(dt) <- gsub('[.]', '_',tolower(colnames(dt)))
	colnames(dt) <- gsub('___', '_', colnames(dt))
	colnames(dt) <- gsub('__', '_', colnames(dt))
	colnames(dt) <- gsub('\\(', '', colnames(dt))
	colnames(dt) <- gsub('\\)', '', colnames(dt))
	# Rating areas
	dt <- dt %>% mutate(rating_area = as.numeric(str_replace_all(rating_area, "Rating Area ", "")))
	dt <- dt %>% mutate(county_name = paste(county_name, "County", sep = " "))
	return(dt)
}

id_plans15 <- id_plans15 %>% rename(state_code = State, county_name = County)
id_plans15 <- prepLandscape(id_plans15)
id_plans16 <- prepLandscape(id_plans16)
id_plans17 <- prepLandscape(id_plans17)

# Join fips codes to 2015 and 2016
id_plans15 <- left_join(id_plans15, fips_id, by = "county_name") %>%
	select(fips_county, everything())
id_plans16 <- left_join(id_plans16, fips_id, by = "county_name") %>%
	select(fips_county, everything())

# Save full files
write.csv(id_plans15, "data-original/landscape/2015_QHP_Landscape_ID_Individual_Market_Medical.csv", row.names = F, na="")
write.csv(id_plans16, "data-original/landscape/2016_QHP_Landscape_ID_Individual_Market_Medical.csv", row.names = F, na="")
write.csv(id_plans17, "data-original/landscape/2017_QHP_Landscape_ID_Individual_Market_Medical.csv", row.names = F, na="")

####################################################################################
# County enrollment
# December 2016 point-in-time county enrollment sent as pdf table in email, process in Tabula
# Use these shares with state-level OEP enrollment to approximate 2017 OEP county enrollment
# 2017 OEP QHP selections -  100,082 - from state table in 
# https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Marketplace-Products/Plan_Selection_ZIP.html
####################################################################################

id_enroll17 <- read.csv("data-original/state-based/raw/tabula-2016-id Idaho Enrollments by County.csv", stringsAsFactors = F)
colnames(id_enroll17) <- c("county_name", "enroll_dec16")
id_enroll17 <- id_enroll17 %>% mutate(enroll_dec16 = as.numeric(str_replace_all(enroll_dec16, ",", ""))) %>%
	mutate(county_name = str_trim(paste(county_name, "County", " "), "both"))

# Approximate OEP selections using provided data's ratios
id_selections <- 100082
id_dec16 <- sum(id_enroll17$enroll_dec16)
id_enroll17 <- id_enroll17 %>% mutate(plan_selections = round((id_selections * enroll_dec16/id_dec16), 0))
sum(id_enroll17$plan_selections)

# Add fips
id_enroll17 <- left_join(id_enroll17, fips_id, by="county_name")
id_enroll17 <- id_enroll17 %>% mutate(year = 2017) %>%
	select(year, fips_county, county_name, state_code, everything())

# Save with the Dec 16 data for use if needed
write.csv(id_enroll17, "data-original/state-based/2017-id-county-enrollment.csv", row.names = F, na = "")
# And without for joining
id_enroll17 <- id_enroll17 %>% select(-enroll_dec16)
write.csv(id_enroll17, "data-original/state-based/id-county-enrollment.csv", row.names = F, na = "")
