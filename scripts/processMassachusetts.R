# Massachusetts marketplace data
library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_ma <- fips_codes %>% filter(fips_state == 25) %>%
	select(fips_county, county_name, state_code)

####################################################################################
# Insurers by rating area (don't line up to counties exactly)
# Spreadsheet sent via email
# In 2016, Network Health became Tufts Health Plan - Direct
# Tufts Direct and Premier are listed separately but are the same to consumers
# See also https://betterhealthconnector.com/tufts-health-plan

# Rating regions map https://www.mahealthconnector.org/wp-content/uploads/board_meetings/2016/2016-09-08/ConnectorCare-Placemat-090816.pdf

# ONE CHANGE FROM PROVIDED SPREADSHEET: Corrected Minuteman's 2017 network to exclude regions A and G, as in past years and on the Connector site
# Most plans offered statewide, but 4 substate region combinations used. Mapped to counties as close as possible.
# Approximate county-plan area mappings:
# AB: "Berkshire County, Hampshire County, Hampden County, Franklin County, Worcester County"
# ABC: "Berkshire County, Hampshire County, Hampden County, Franklin County, Worcester County, Middlesex County"
# BCDEF: "Worcester County, Middlesex County, Norfolk County, Suffolk County, Essex County, Plymouth County, Bristol County"
# ABCDEF: "Berkshire County, Hampshire County, Hampden County, Franklin County, Worcester County, Middlesex County, Norfolk County, Suffolk County, Essex County, Plymouth County, Bristol County"
####################################################################################
ma_raw14 <- readWorkbook("documents/ma/Mass. Health Connector Carriers by Region PY 2014-2017.xlsx", rows = c(10:20), cols = c(1:8))
ma_raw15 <- readWorkbook("documents/ma/Mass. Health Connector Carriers by Region PY 2014-2017.xlsx", rows = c(25:36), cols = c(1:8))
ma_raw16 <- readWorkbook("documents/ma/Mass. Health Connector Carriers by Region PY 2014-2017.xlsx", rows = c(41:52), cols = c(1:8))
ma_raw17 <- readWorkbook("documents/ma/Mass. Health Connector Carriers by Region PY 2014-2017.xlsx", rows = c(59:69), cols = c(1:8))

formatMa <- function(dt, year) {
	dt <- dt %>% rename(issuer_name = Issuer) %>%
		mutate(fullstate = ifelse(rowSums(is.na(dt)) == 0, 1, 0)) 
	
	# Get fullstate operators
	full <- dt %>% filter(fullstate == 1) %>% select(issuer_name, fullstate) %>%
		mutate(region = "fullstate") %>%
		select(-fullstate)
	
	# Non-fullstate operators
	dt <- dt %>% filter(fullstate == 0) %>%
		gather(region, offers, -c(issuer_name, fullstate)) %>%
		filter(!is.na(offers)) %>%
		mutate(region = str_replace_all(region, "[.]", " ")) %>%
		mutate(region = str_trunc(region, 8, "right", ellipsis = "")) %>%
		mutate(region = str_trunc(region, 1, "left", ellipsis = "")) %>%
		select(issuer_name, region) %>%
		group_by(issuer_name) %>%
		mutate(region = paste(region, sep=",", collapse = "")) 
	dt <- unique(dt)
	dt <- as.data.frame(dt)
	fulldt <- rbind(dt, full) %>% mutate(year = year)
	return(fulldt)
}
ma_14 <- formatMa(ma_raw14, 2014)
ma_15 <- formatMa(ma_raw15, 2015)
ma_16 <- formatMa(ma_raw16, 2016)
ma_17 <- formatMa(ma_raw17, 2017)

ma_issuers <- rbind(ma_14, ma_15, ma_16, ma_17)

ma_issuers <- ma_issuers %>% mutate(issuer_name = str_replace_all(issuer_name, "\\*", ""))

# Save for future use
write.csv(ma_issuers, "documents/ma/ma-insurers-ratingarea.csv", row.names = F, na = "")

# Get counties for national analysis
table(ma_issuers$region)
ma_names <- paste(c(fips_ma$county_name), collapse=', ' )
ab_names <- paste(c("Berkshire County", "Hampshire County", "Hampden County", "Franklin County", "Worcester County"), collapse=', ' )
abc_names <- paste(c("Berkshire County", "Hampshire County", "Hampden County", "Franklin County", "Worcester County", "Middlesex County"), collapse=', ' )
bcdef_names <- paste(c("Worcester County", "Middlesex County", "Norfolk County", "Suffolk County", "Essex County", "Plymouth County", "Bristol County"), collapse=', ' )
abcdef_names <- paste(c("Berkshire County", "Hampshire County", "Hampden County", "Franklin County", "Worcester County", "Middlesex County", "Norfolk County", "Suffolk County", "Essex County", "Plymouth County", "Bristol County"), collapse=', ' )

ma_issuers <- ma_issuers %>% mutate(counties = ifelse(region == "fullstate", ma_names,
																											ifelse(region == "AB", ab_names,
																											ifelse(region == "ABC", abc_names,
																											ifelse(region == "BCDEF", bcdef_names,
																											ifelse(region == "ABCDEF", abcdef_names,
																														 ""))))))
# One county per line
ma <- ma_issuers %>% mutate(county_name = strsplit(counties, ",")) %>% 
	unnest(county_name) %>%
	select(-counties) %>%
	mutate(county_name = str_trim(county_name, "both")) %>%
	select(-region)

# Join FIPS
ma <- left_join(ma, fips_ma, by="county_name")
ma <- ma %>% arrange(year, fips_county) %>% select(year, fips_county, county_name, state_code, issuer_name, everything())

####################################################################################
# Add HIOS id
####################################################################################
ids <- read.csv("data/hios-ids.csv", stringsAsFactors = F)
ids <- ids %>% mutate(issuer_name = str_replace_all(issuer_name, "  ", ", "))
ids_ma <- ids %>% filter(state_code == "MA" & market == "Individual")
table(ids_ma$issuer_name)
table(ma$issuer_name)

ids_join <- ids_ma %>% group_by(issuer_name, issuer_id) %>%
	summarize(temp = n()) %>%
	select(-temp)

# Using names from the 2016 SBM PUF for matching where possible
# Also for reference https://www.cms.gov/cciio/resources/regulations-and-guidance/downloads/2015-rc-issuer-level-report-11-18-16-final-v2.pdf
# Tufts Health Plan Direct was formerly Network Health
# Used 2016 MA SBM PUF to verify that Tufts Health Public Plans markets as Tufts Direct, Tufts Associated HMO markets as Tufts Premier
ma <- ma %>% mutate(issuer_name = ifelse(issuer_name == "BMC HealthNet Plan", "Boston Medical Center Health Plan",
																	ifelse(issuer_name == "Blue Cross Blue Shield of MA", "Blue Cross and Blue Shield of Massachusetts HMO Blue, Inc",
																	ifelse(issuer_name == "CeltiCare", "CeltiCare Health Plan of Massachusetts",
																	ifelse(issuer_name == "Fallon", "Fallon Community Health Plan, Inc.",
																	ifelse(issuer_name == "Health New England", "Health New England, Inc.",
																	ifelse(issuer_name == "Minuteman", "Minuteman Health, Inc.",
																	ifelse(issuer_name == "United HealthCare", "UnitedHealthcare Insurance Company",	
																	ifelse(issuer_name %in% c("Tufts Health Plan - Direct", "Network Health"), "Tufts Health Public Plans Inc.",
																	ifelse(issuer_name %in% c("Tufts Health Plan - Premier", "Tufts Health Plan"), "Tufts Associated Health Maintenance Organization",
																						issuer_name))))))))))
table(ma$issuer_name)
ma <- left_join(ma, ids_join, by = "issuer_name")
summary(ma$issuer_id)

write.csv(ma, "data-original/state-based/ma-insurers.csv", row.names = F, na = "")

####################################################################################
# County-level enrollment, sent via email
# Source notes: membership by county, as of Feb. 2, 2017. 
# This is the nearest run-date to the end of Open Enrollment. 
# It breaks down each county by ConnectorCare (state and federal subsidy, under 300 FPL), 
# subsidized (tax credits only, 300-400 FPL) and unsubsidized.
####################################################################################
ma_enrollraw <- readWorkbook("documents/ma/Mass. Health Connector membership cntyXcnty 2.17.xlsx", startRow = 5)

# Data has county name = total row, then county column is also subset enrollment
# Make tidy
colnames(ma_enrollraw) <- tolower(colnames(ma_enrollraw))
ma_enrollraw <- ma_enrollraw %>%
	mutate(type = ifelse(str_detect(county, "ConnectorCare") | str_detect(county, "Subsidized") | str_detect(county, "Unsubsidized"), county, "Total")) %>%
	mutate(county_name = ifelse(type=="Total", county, NA)) %>%
	fill(county_name) %>%
	mutate(county_name = paste(county_name, "County", sep =" ")) %>%
	select(-county)

# Verify
temp <- ma_enrollraw %>% filter(county_name != "Total County") %>%
	group_by(type) %>%
	summarize(member.count = sum(member.count))

# Make wide for joining
ma_enroll17 <- ma_enrollraw %>% spread(type, member.count)
ma_enroll17 <- left_join(ma_enroll17, fips_ma, by="county_name")
ma_enroll17 <- ma_enroll17 %>% mutate(year = 2017, state_code = "MA") %>%
	select(year, fips_county, county_name, state_code, everything()) %>%
	arrange(year, fips_county)

# Save for potential later use
write.csv(ma_enroll17, "documents/ma/2017-ma-county-enrollment-full.csv", row.names = F, na = "")

# Format for national file
ma_enroll17 <- ma_enroll17 %>% mutate(plan_selections_aptc = Subsidized + ConnectorCare) %>%
	rename(plan_selections = Total, plan_selections_noaptc = Unsubsidized) %>%
	select(-Subsidized, -ConnectorCare) %>%
	filter(!is.na(fips_county))

# Check that they add up - they do! good
ma_enroll17$temp <- ma_enroll17$plan_selections_noaptc + ma_enroll17$plan_selections_aptc - ma_enroll17$plan_selections
summary(ma_enroll17$temp)

ma_enroll17 <- ma_enroll17 %>% select(-temp)
write.csv(ma_enroll17, "data-original/state-based/ma-county-enrollment.csv", row.names = F, na = "")
