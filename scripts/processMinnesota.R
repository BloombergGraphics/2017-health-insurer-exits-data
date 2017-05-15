# Minnesota data

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)

# HIOS ids for matching, from SERFF filings
ids_mn <- read.csv("data-original/issuer-names.csv", stringsAsFactors = F) %>%
	filter(state_code=="MN")

fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_mn <- fips_codes %>% filter(state_code == "MN") %>%
	select(fips_county, county_name) %>%
	mutate(county_name = ifelse(county_name == "Lac qui Parle County", "Lac Qui Parle County", county_name))

####################################################################################
# Plan data from
# * [2017 plan spreadsheet](https://www.mnsure.org/assets/individual-service-areas-provider-lookup-2017-current-customers_tcm34-262838.xls)
# * [2016 plan spreadsheet](https://www.mnsure.org/assets/Individual-ServiceAreas-ProviderLook-up-2016_tcm34-182254.xls)
# * [2015 plan spreadsheet](https://www.mnsure.org/assets/Individual-ServiceAreas-ProviderLook-up_tcm34-182255.xls)
# * [2014 approved plan filings](https://mn.gov/commerce/consumers/your-insurance/health-insurance/rate-review/rate-filings/2014/)
####################################################################################
mn_raw15 <- read_excel("documents/mn/2015-Individual-ServiceAreas-ProviderLook-up_tcm34-182255.xls")
mn_raw16 <- read_excel("documents/mn/Individual-ServiceAreas-ProviderLook-up-2016_tcm34-182254.xls")
mn_raw17 <- read_excel("documents/mn/individual-service-areas-provider-lookup-2017-current-customers_tcm34-262838.xls")

formatPlans <- function(dt, year) {
	# Remove rows that are repeated headers
	dt <- dt %>% rename(plan_name = `Plan Name`, lookup = `Provider Look Up Information`, formulary = `Formulary Information`, metal_level = `Metal Level`) %>%
		filter(plan_name != "Plan Name")
	# Company names are in rows above their plans, only entry in row
	dt <- dt %>% mutate(issuer_name = ifelse(is.na(lookup), plan_name, NA)) %>% 
		select(issuer_name, everything()) %>%
		# propogate down
		fill(issuer_name) %>%
		# dental vs medical column
		mutate(type = ifelse(str_detect(issuer_name, "Dental"), "dental",
												 ifelse(str_detect(plan_name, "Dental"), "dental",
												 ifelse(str_detect(issuer_name, "Total number"), "total","medical")))) %>%
		# remove plan name rows
		filter(plan_name != issuer_name | type == "total") %>%
		# make long
		gather(county_name, offer, -c(plan_name, issuer_name, lookup, formulary, metal_level, type)) %>%
		# remove non-offered plans
		filter(!is.na(offer)) %>%
		filter(type != "total") %>%
		select(-offer) %>%
		mutate(year = year) %>%
		select(year, county_name, everything())
}
mn_plans15 <- formatPlans(mn_raw15, 2015)
mn_plans16 <- formatPlans(mn_raw16, 2016)
mn_plans17 <- formatPlans(mn_raw17, 2017)

# Join
mn_plans <- rbind(mn_plans15, mn_plans16, mn_plans17)
mn_plans <- mn_plans %>% mutate(county_name = paste(county_name, "County", sep = " "),
																state_code = "MN")

# Add fips codes
mn_plans <- left_join(mn_plans, fips_mn, by = "county_name")
mn_plans <- mn_plans %>% select(year, fips_county, everything()) %>%
	arrange(year, fips_county)

# Clean up issuer names, match to HIOS id
table(mn_plans$issuer_name)
# Group Health markets as HealthPartners on exchange, ID 34102
mn_plans <- mn_plans %>% mutate(issuer_name = ifelse(issuer_name == "HealthPartners", "Group Health, Inc. dba HealthPartners",
																			 ifelse(issuer_name == "Blue Plus" | issuer_name == "BluePlus", "HMO Minnesota dba Blue Plus", 
																			 			 ifelse(issuer_name == "Medica", "Medica Health Plans of Wisconsin",
																			 			 			 issuer_name))))
table(mn_plans$issuer_name)
mn_plans <- left_join(mn_plans, ids_mn, by=c("state_code", "issuer_name"))

write.csv(mn_plans, "data-original/state-based/mn-plans.csv", row.names = F, na = "")

####################################################################################
# Number of medical plans by county by year
####################################################################################

mn <- mn_plans %>% filter(type == "medical") %>%
	group_by(year, fips_county, county_name, state_code, issuer_id, issuer_name) %>%
	summarize(plans = n()) %>% 
	arrange(year, fips_county)
write.csv(mn, "data-original/state-based/mn-insurers.csv", row.names = F, na = "")

####################################################################################
# County-level enrollment
# 2017 data provided by email (as a .docx table, I saved as .csv)
####################################################################################

# Clean up non-tidy data
mn_raw <- read.csv("documents/mn/OEP SEP County Data 2017.csv", stringsAsFactors = F)
colnames(mn_raw) <- tolower(colnames(mn_raw))

# Don't need percent column, separate out county and type
mn_raw <- mn_raw %>% select(-percent) %>%
	mutate(type = ifelse(str_detect(county, "APTC"), county, "total")) %>%
	mutate(county = ifelse(str_detect(county, "APTC"), NA, county)) %>%
	fill(county)
	
# Wide
mn_enroll17 <- mn_raw %>%
	spread(type, member.count) %>%
	filter(county != "Grand Total") %>%
	rename(plan_selections = total, plan_selections_aptc = APTC, plan_selections_noaptc = `No APTC`) %>%
	mutate(county_name = paste(county, "County", sep = " ")) %>%
	select(-county) %>%
	mutate(county_name = ifelse(county_name == "Saint Louis County", "St. Louis County", county_name))

# Add fips
mn_enroll17 <- left_join(mn_enroll17, fips_mn, by = "county_name")
mn_enroll17 <- mn_enroll17  %>%
	mutate(year = 2017, state_code = "MN") %>%
	select(year, fips_county, county_name, state_code, plan_selections, everything()) %>%
	arrange(year, fips_county)

write.csv(mn_enroll17, "data-original/state-based/mn-county-enrollment.csv", row.names = F, na = "")

####################################################################################
# NOT USING - they opened up 2014 SERFF access after request
# 2014 insurer service areas
# From https://mn.gov/commerce/consumers/your-insurance/health-insurance/rate-review/rate-filings/2014/
####################################################################################

# processSA <- function(file_path, hios_id, company_name, year, state_code) {
# 	dt <- read.csv(file_path, stringsAsFactors = F)
# 	# Column names
# 	colnames(dt) <- gsub('[.]', '_',tolower(colnames(dt)))
# 	colnames(dt) <- gsub("_$", "",colnames(dt))
# 	dt <- dt %>% rename(cover_entire_state = state) %>%
# 		mutate(issuer_id = hios_id, issuer_name = company_name, year = year, state_code = state_code)
# 	# Separate out county names
# 	dt <- dt %>% separate(county_name, into = c("county_name", "fips_county"), sep = " ‐ ", fill="warn")
# 	
# 	return(dt)
# }
# files <- list.files(path="documents/mn/filings-2014/", pattern="*.csv")
# f1 <- processSA(file_path = "documents/mn/filings-2014/tabula-bcbs-mns-indv-sa.csv", hios_id = 49316, company_name = "Blue Cross Blue Shield", year = 2014, state_code = "MN")
# f2 <- processSA(file_path = "documents/mn/filings-2014/tabula-hp-mns-indv-sa.csv", hios_id = 34102, company_name = "Group Health, Inc.", year = 2014, state_code = "MN")
# f3 <- processSA(file_path = "documents/mn/filings-2014/tabula-medi-mns-indv-sa.csv", hios_id = 65847, company_name = "Medica Health Plans of Wisconsin", year = 2014, state_code = "MN")
# f4 <- processSA(file_path = "documents/mn/filings-2014/tabula-pone-mns-indv-sa.csv", hios_id = 88102, company_name = "PreferredOne Insurance Company", year = 2014, state_code = "MN")
# f5 <- processSA(file_path = "documents/mn/filings-2014/tabula-ucare-mns-indv-sa.csv", hios_id = 85736, company_name = "UCare", year = 2014, state_code = "MN")
# sa14 <- rbind(f1, f2, f3, f4, f5)
# 
# # Get formatted county name
# sa14 <- sa14 %>% select(-county_name)
# sa14 <- left_join(sa14, fips_mn, by="fips_county")
# sa14[sa14 == "Yes"] <- 1
# sa14[sa14 == "No"] <- 0
# write.csv("documents/mn/2014-sa-service-areas.csv", na="", row.names=F)
# 
# # Get insurers by county
# formatServiceAreas <- function(dt, state_code) {
# 	fips_list <- fips_codes[which(fips_codes$state_code == state_code),]$fips_county
# 	# For full state issuers, make one record per county
# 	fullstate <- dt %>% filter(cover_entire_state == 1) %>%
# 		mutate(region ="cover_entire_state") %>%
# 		select(year, state_code, issuer_id, issuer_name, region) %>%
# 		mutate(counties = paste(c(fips_list), collapse=', ' )) %>%
# 		mutate(fips_county = strsplit(counties, ",")) %>% 
# 		unnest(fips_county) %>%
# 		select(-counties) %>%
# 		mutate(fips_county = str_trim(fips_county, side="both"))
# 	
# 	fullstate <- left_join(fullstate, fips_mn, by="fips_county")
# 	# For partial state issuers, collapse by county 
# 	partialstate <- dt %>% filter(cover_entire_state == 0) %>%
# 		mutate(region = ifelse(partial_county == 1, "partial_county", "full_county")) %>%
# 		select(year, state_code, issuer_id, issuer_name, fips_county, county_name, region) %>% 
# 		group_by(year, state_code, issuer_id, issuer_name, fips_county, county_name, region) %>%
# 		summarize(temp = n()) %>%
# 		select(-temp)
# 	
# 	sa <- bind_rows(fullstate, partialstate)
# 	# Remove any duplicate rows
# 	sa <- sa %>% group_by(year, state_code, issuer_id, issuer_name, fips_county, county_name) %>%
# 		summarize(temp = n()) %>%
# 		select(-temp) %>%
# 		arrange(year, fips_county) %>%
# 		select(year, fips_county, county_name, state_code, everything())
# 	return(sa)
# }
# mn14 <- formatServiceAreas(sa14, "MN")
