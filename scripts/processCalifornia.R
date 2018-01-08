# California data
# http://hbex.coveredca.com/data-research/

library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

# FIPS
fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_ca <- fips_codes %>% filter(state_code == "CA") %>%
	select(fips_county, county_name)

####################################################################################
# Rating areas
# From https://www.cms.gov/CCIIO/Programs-and-Initiatives/Health-Insurance-Market-Reforms/ca-gra.html
####################################################################################
ca_ra <- read.csv("data-original/state-based/2017-ca-ratingareas.csv", stringsAsFactors = F)

ca_ra <- left_join(ca_ra, fips_ca, by="county_name")
# Save with fips
write.csv(ca_ra, "data-original/state-based/2017-ca-ratingareas.csv", na="", row.names = F)

####################################################################################
# Insurers by rating area
# Not all plans offered in all counties...so not super usable
# From http://hbex.coveredca.com/data-research/library/2017_QHP_Individual_Rates_File_for_Posting_100716.xlsx
####################################################################################
ca_rates <- readWorkbook("documents/ca/2017_QHP_Individual_Rates_File_for_Posting_100716.xlsx", startRow = 6)
colnames(ca_rates) <- gsub('[.]', '_',tolower(colnames(ca_rates)))

# Collapse, don't care about rates
ca_rates <- ca_rates %>% select(-individual_rate, -age)
ca_plans <- unique(ca_rates)
ca_plans$rating_area <- as.numeric(str_replace_all(ca_plans$rating_area_id, "Rating Area ", ""))
ca_plans <- ca_plans %>% select(-rating_area_id)

write.csv(ca_plans, "data-original/state-based/2017-ca-plans.csv", row.names = F, na="")

# Insurers by rating area
ca_areas <- ca_plans %>% group_by(rating_area, hois_id, applicant) %>%
	summarize(plans = n()) %>%
	rename(hios_id = hois_id, issuer_name = applicant) %>%
	mutate(state_code = "CA")
write.csv(ca_areas, "data-original/state-based/2017-ca-ratingarea-insurers.csv", row.names = F, na="")

####################################################################################
# 2017 Insurers by zip code, fun
# From http://hbex.coveredca.com/data-research/library/2017-Products-By-Zip-Code.xlsx
####################################################################################
zips_raw <- readWorkbook("documents/ca/2017-Products-By-Zip-Code.xlsx", startRow = 7)

ca_zips <- zips_raw
# Change X to 1
ca_zips[ca_zips == "X"] <- 1
# Long
ca_zips <- ca_zips %>% gather(issuer_name, offers, -c(1:6))

colnames(ca_zips) <- gsub('[.]', '_',tolower(colnames(ca_zips)))

# Clean up names, fips
ca_zips <- ca_zips %>% mutate(fips_county = paste("06", fips_county_code, sep = "")) %>%
	select(-fips_county_code) %>%
	select(fips_county, everything()) %>%
	rename(rating_area = covered_california_rating_region,
				 county_name = county) %>%
	arrange(zip_code) %>%
	# Filter out non-offered plans
	filter(!is.na(offers))
ca_zips$issuer_name <- str_replace_all(ca_zips$issuer_name, "[.]", " ")

# Issuer names include HMO, PPO, etc which I don't want here
table(ca_zips$issuer_name)
ca_zips <- ca_zips %>%
	mutate(issuer_name = str_replace_all(issuer_name, " EPO", ""),
				 issuer_name = str_replace_all(issuer_name, " HMO", ""),
				 issuer_name = str_replace_all(issuer_name, " HSP", ""),
				 issuer_name = str_replace_all(issuer_name, " PPO", ""),
				 issuer_name = str_replace_all(issuer_name, " 1 Copay", ""),
				 issuer_name = str_replace_all(issuer_name, " 2 Coinsurance", ""))
table(ca_zips$issuer_name)

# Group again no with cleaned up names
ca_zips <- ca_zips %>% group_by(fips_county, rating_area, county_name, zip_code, `zip-county_code`, zip_code_split_across_counties, issuer_name) %>%
	summarize(temp = n()) %>%
	select(-temp) %>%
	arrange(zip_code)

# Group by county
ca17 <- ca_zips %>% group_by(fips_county, county_name, rating_area, issuer_name) %>%
	summarize(temp = n()) %>%
	select(-temp) %>%
	mutate(state_code = "CA", year = 2017) %>%
	select(year, fips_county, everything())

### NOTE!!! LA county contains 2 rating areas. Rating area 16 additionally has Oscar participation (15 doesn't)
write.csv(ca17, "data-original/state-based/2017-ca-insurers.csv", row.names = F, na = "")

####################################################################################
# 2014 insurers by county
# As advised via email from Covered California, use membership profile insurer by
# county enrollment data
####################################################################################
# Get files from http://hbex.coveredca.com/data-research/
# download.file("http://hbex.coveredca.com/data-research/library/active-member-profiles/CC_Membership_Profile_2014_09.xlsx", destfile = "documents/ca/CC_Membership_Profile_2014_09.xlsx")
# download.file("http://hbex.coveredca.com/data-research/library/active-member-profiles/CC_Membership_Profile_2015_09.xlsx", destfile = "documents/ca/CC_Membership_Profile_2015_09.xlsx")
# download.file("http://hbex.coveredca.com/data-research/library/active-member-profiles/CC_Membership_Profile_2016_06.xlsx", destfile = "documents/ca/CC_Membership_Profile_2016_06.xlsx")

ca_raw14 <- readWorkbook("documents/ca/CC_Membership_Profile_2014_09.xlsx", sheet = "County", rows = c(11, 14:24))

# Get rating areas
ca_ra <- read.csv("data-original/state-based/2017-ca-ratingareas.csv", stringsAsFactors = F, colClasses = c("fips_county" = "character"))
ca_ra <- ca_ra %>% select(fips_county, rating_area)
fips_ca <- left_join(fips_ca, ca_ra, by="fips_county")

formatCaInsurers <- function(dt, year) {
	dt <- dt %>% select(-starts_with("X")) %>%
		rename(issuer_name = `COUNTY:`) %>%
		gather(county_name, enrollees, -issuer_name) %>%
		mutate(county_name = str_replace_all(county_name, "[.]", " ")) %>%
		filter(county_name != "TOTAL") %>%
		mutate(year = year) %>%
		mutate(county_name = paste(county_name, "County", sep = " "))
	dt <- left_join(dt, fips_ca, by = "county_name")
	dt <- dt %>% select(year, fips_county, rating_area, everything()) %>%
		arrange(year, rating_area)
	return(dt)
}

# 2014
ca14 <- formatCaInsurers(ca_raw14, 2014)
# enrollees isn't a perfect proxy, supplement with companies by rating area spreadsheet
# Anthem and Blue Shield statewide
table(ca14$issuer_name)
ca14 <- ca14 %>% mutate(offers = ifelse(issuer_name %in% c("Anthem Blue Cross of California", "Blue Shield of California"), 1,
																					ifelse(issuer_name == "Chinese Community Health Plan" & (rating_area == 4 | fips_county == "06081"), 1,
																					ifelse(issuer_name == "Contra Costa Health Plan" & rating_area==5, 1,		
																					ifelse(issuer_name == "Health Net" & (rating_area %in% c(2, 4, 5, 7, 8, 9, 10, 14, 17, 18, 19) | fips_county == "06037"), 1,
																					ifelse(issuer_name == "Kaiser Permanente" & (rating_area %in% c(2, 3, 4, 5, 6, 7, 8, 11, 14, 17, 18, 19) | fips_county == "06037"), 1,
																					# In some RAs, Kaiser only offered in some areas
																					ifelse(issuer_name == "Kaiser Permanente" & rating_area %in% c(1, 10, 12, 13) & enrollees >= 20, 1,
																					ifelse(issuer_name == "L.A. Care Health Plan" & fips_county == "06037", 1,
																					ifelse(issuer_name == "Molina Healthcare" & (rating_area %in% c(17, 19) | fips_county == "06037"), 1,
																					ifelse(issuer_name == "Sharp Health Plan" & rating_area==19, 1,
																					ifelse(issuer_name == "Western Health Advantage" & rating_area %in% c(2, 3), 1,
																					ifelse(issuer_name == "Valley Health Plan" & rating_area == 7, 1,
																								 			 0))))))))))))
temp <- ca14 %>% group_by(rating_area, offers, issuer_name) %>%
	summarize(enrollees = sum(enrollees))

ca14 <- ca14 %>% filter(offers == 1) %>%
	select(-enrollees, -offers) %>%
	mutate(state_code = "CA") %>%
	select(year, fips_county, county_name, state_code, everything())

####################################################################################
# Annual dataset
# 2016 from CMS's SBM PUFs pro cessed in prepSbmPuf.R
####################################################################################
ca14 <- as.data.frame(ca14)
ca17 <- as.data.frame(ca17)
# Join to 2017 data
ca <- bind_rows(ca14, ca17)

# Add HIOS ids
ids <- read.csv("data/hios-ids.csv", stringsAsFactors = F)
ids_ca <- ids %>% filter(state_code == "CA" & market == "Individual") %>%
	mutate(issuer_name = str_replace_all(issuer_name, "  ", ", ")) %>%
	group_by(issuer_name, issuer_id) %>%
	summarize(n = n()) %>%
	select(-n)
table(ids_ca$issuer_name)
table(ca$issuer_name)
# Verify no duplicates
ids_ca[duplicated(ids_ca),]

# Clean up names for matching
# Verified specific Anthem subsidiary for 2014 with https://www.coveredca.com/PDFs/CC-health-plans-booklet-rev4.pdf
# It is Anthem Blue Cross of California, NOT Anthem Blue Cross Life and Health Insurance Company (separate companies & HIOS ids)
ca <- ca %>% mutate(issuer_name = ifelse(issuer_name == "CCHP", "Chinese Community Health Plan",
																	ifelse(issuer_name %in% c("L.A. Care Health Plan", "LA Care"), "Local Initiative Health Authority for Los Angeles County, dba L.A. Care Health Plan",
																	ifelse(issuer_name %in% c("Molina", "Molina Healthcare"), "Molina Healthcare of California",
																	ifelse(issuer_name == "Oscar", "Oscar Health Plan of California",
																	ifelse(issuer_name == "Sharp", "Sharp Health Plan",
																	ifelse(issuer_name == "Valley Health Plan", "County of Santa Clara dba Valley Health Plan",
																	ifelse(issuer_name %in% c("Blue Shield", "Blue Shield of California"), "California Physicians’ Service, dba Blue Shield of California",
																	ifelse(issuer_name %in% c("Anthem", "Anthem Blue Cross of California"), "Blue Cross of California(Anthem BC)",
																	ifelse(issuer_name == "Kaiser Permanente", "Kaiser Foundation Health Plan, Inc.",
																	ifelse(issuer_name == "HealthNet Life", "Health Net Life Insurance Company",
																	ifelse(issuer_name == "HealthNet of CA", "Health Net of California, Inc.",
																		issuer_name))))))))))))

# 2014 Health Net just listed as "Health Net" - tricky to tell if it's Health Net of California or Health Net Life Insurance, which are separate companies
# Use 2016 and 2017 Health Net Life & Health Net CA service areas for 2014 too (unchanged from 2016 to 2017)
# 2017 areas from data above
# 3 counties in 2014 didn't have Health Net in 2016 or 2017: Monterey, San Benito, Mariposa
# Monterey and San Benito were in rating region 9 with Santa Cruz county, which had Health Net Life in 2016 and 2017
# Mariposa in rating region 10 which also had Health Net Life
healthlife <- ca %>% filter(issuer_name == "Health Net Life Insurance Company")
healthca <- ca %>% filter(issuer_name ==  "Health Net of California Inc.")
ca <- ca %>% mutate(issuer_name = ifelse(issuer_name == "Health Net" & fips_county %in% healthlife$fips_county, "Health Net Life Insurance Company",
																	ifelse(issuer_name == "Health Net" & fips_county %in% healthca$fips_county, "Health Net of California, Inc.",
																	ifelse(issuer_name == "Health Net", "Health Net Life Insurance Company",
																				 issuer_name))))
table(ca$issuer_name)
# Join HIOS id to newly cleaned names
ca <- left_join(ca, ids_ca, by="issuer_name")
# Verify no NA ids
summary(ca$issuer_id)
ca <- ca %>% select(year, state_code, rating_area, fips_county, county_name, issuer_name, issuer_id, everything()) %>%
	arrange(year, fips_county)

write.csv(ca, "data-original/state-based/ca-insurers.csv", row.names = F, na = "")

####################################################################################
# Enrollment from 
# 2017 March effectuated http://hbex.coveredca.com/data-research/library/active-member-profiles/12-13-17/CC_Membership_Profile_2017_09.xlsx

# Source notes:
# The Open Enrollment Profile shows counts of enrollees who have selected a plan through Covered California 
# during the open enrollment period. These are gross plan selections, meaning they include any enrollees, 
# regardless of the outcome of effectuation (payment of first premium). Data subject to revision due to 
# reporting data lags and on-going reconciliations between Covered California and health plans. 
# All cells rounded to nearest 10 consistent with privacy policy. Some dimension totals may not sum to the 
# grand total enrollment figures due to occasional discrepancies between reporting data warehouse 
# and CalHEERS, which may be corrected in subsequent updates to the data.					
####################################################################################
# 2017 county enrollment
ca_enroll17 <- readWorkbook("documents/ca/CC_Membership_Profile_2017_03.xlsx", sheet = "Statewide", rows = c(284:343), skipEmptyRows = T)

# Format
ca_enroll17 <- ca_enroll17 %>% rename(county_name = res_county, 
																			plan_selections = all, 
																			plan_selections_aptc = `_c2_`,
																			plan_selections_noaptc = `_c4_`) %>%
	select(county_name, starts_with("plan")) %>%
	mutate(year = 2017, state_code = "CA") %>%
	filter(county_name != "Grand Total") %>%
	mutate(county_name = paste(county_name, "County", sep= " "))

# Add fips
fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_ca <- fips_codes %>% filter(state_code == "CA") %>%
	select(fips_county, county_name)

ca_enroll17 <- left_join(ca_enroll17, fips_ca, by="county_name")
ca_enroll17 <- ca_enroll17 %>% select(year, fips_county, county_name, state_code, everything())
write.csv(ca_enroll17, "data-original/state-based/ca-county-enrollment.csv", row.names = F, na = "")
