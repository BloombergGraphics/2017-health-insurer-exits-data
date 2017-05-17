# Washington data

library(dplyr)
library(tidyr)
library(stringr)
library(readr)

# fips codes
fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_wa <- fips_codes %>% filter(state_code == "WA") %>%
	select(fips_county, county_name) %>%
	# Remove "County" for matching
	mutate(county_name = str_replace_all(county_name, " County", ""))

####################################################################################
# 2016-2017 Plan data (list form) from
# https://www.wahbexchange.org/new-customers/coverage-basics/plans/#2017
####################################################################################

# 2017
# Read in list data - one row per insurer, with county names listed after
wa_raw17 <- read_lines("data-original/state-based/raw/2017-wa-insurers-raw.txt")
wa_raw17 <- as.data.frame(wa_raw17)

# Separate out county name
wa_raw17 <- wa_raw17 %>% separate(col = wa_raw17, into = c("issuer_name", "counties"), sep = " – ", extra = "merge")
# One county per line
wa17 <- wa_raw17 %>% mutate(county_name = strsplit(counties, ",")) %>% 
	unnest(county_name) %>%
	select(-counties) %>%
	mutate(state_code = "WA",
				 year = 2017,
				 county_name = str_trim(county_name, "both"))

# 2016
wa_raw16 <- read_lines("data-original/state-based/raw/2016-wa-insurers-raw.txt")
wa_raw16 <- as.data.frame(wa_raw16)
wa_raw16 <- wa_raw16 %>% separate(col = wa_raw16, into = c("issuer_name", "counties"), sep = " – ", extra = "merge")

# "All 39 counties" - need to list out
nameswa <- paste(c(fips_wa$county_name), collapse=', ' )
temp <- fips_wa$county_name[fips_wa$county_name != "Clark"]
nameswa_noclark <- paste(c(temp), collapse=', ' )
wa_raw16 <- wa_raw16 %>% mutate(counties = ifelse(counties == "All 39 counties", nameswa,
																									ifelse(counties == "All counties except Clark", nameswa_noclark, counties)))
# One county per line
wa16 <- wa_raw16 %>% mutate(county_name = strsplit(counties, ",")) %>% 
	unnest(county_name) %>%
	select(-counties) %>%
	mutate(state_code = "WA",
				 year = 2016,
				 county_name = str_trim(county_name, "both"))

####################################################################################
# 2014 and 2015 provided via email
####################################################################################

wa_raw14 <- read.csv("data-original/state-based/raw/2014-wa-insurers-raw.csv", stringsAsFactors = F)
wa14 <- wa_raw14 %>% mutate(counties = ifelse(counties == "All counties", nameswa,
																									ifelse(counties == "All counties except Clark", nameswa_noclark, counties)))%>% 
	mutate(county_name = strsplit(counties, ",")) %>% 
	unnest(county_name) %>%
	select(-counties) %>%
	mutate(state_code = "WA",
				 year = 2014,
				 county_name = str_trim(county_name, "both"))

wa_raw15 <- read.csv("data-original/state-based/raw/2015-wa-insurers-raw.csv", stringsAsFactors = F)
wa15 <- wa_raw15 %>% mutate(counties = ifelse(counties == "All counties", nameswa,
																									ifelse(counties == "All counties except Clark", nameswa_noclark, counties))) %>% 
	mutate(county_name = strsplit(counties, ",")) %>% 
	unnest(county_name) %>%
	select(-counties) %>%
	mutate(state_code = "WA",
				 year = 2015,
				 county_name = str_trim(county_name, "both"))

####################################################################################
# Join years, fips
####################################################################################
wa <- rbind(wa14, wa15, wa16, wa17)

# Add "County" back to names
wa <- wa %>% mutate(county_name = paste(county_name, "County", sep = " ")) %>%
	mutate(issuer_name = str_trim(issuer_name, "both"))
fips_wa <- fips_wa %>% mutate(county_name = paste(county_name, "County", sep = " "))

wa <- left_join(wa, fips_wa, by = "county_name")

####################################################################################
# Add HIOS id
####################################################################################
ids <- read.csv("data/hios-ids.csv", stringsAsFactors = F)
ids <- ids %>% mutate(issuer_name = str_replace_all(issuer_name, "  ", ", "))
ids_wa <- ids %>% filter(state_code == "WA" & market == "Individual")
table(ids_wa$issuer_name)
table(wa$issuer_name)

ids_join <- ids_wa %>% group_by(issuer_name, issuer_id) %>%
	summarize(temp = n()) %>%
	select(-temp) %>%
	arrange(issuer_id)

# Using names from the 2016 SBM PUF for matching where possible
# Also for reference https://www.cms.gov/cciio/resources/regulations-and-guidance/downloads/2015-rc-issuer-level-report-11-18-16-final-v2.pdf
# Tufts Health Plan Direct was formerly Network Health
# Used 2016 MA SBM PUF to verify that Tufts Health Public Plans markets as Tufts Direct, Tufts Associated HMO markets as Tufts Premier
# Kaiser of Washington, formerly Group Health: got plan binder from https://fortress.wa.gov/Search.aspx filed under ID 80473
wa <- wa %>% mutate(issuer_name = ifelse(issuer_name == "BridgeSpan", "BridgeSpan Health Company",
																	ifelse(issuer_name == "Columbia United Providers", "Columbia United Providers, Inc.",
																	ifelse(issuer_name %in% c("Community Health Plan of Washington", "Community Health Plans of Washington"), "Community Health Plan of Washington",
																	ifelse(issuer_name == "Coordinated Care", "Coordinated Care Corporation",
																	ifelse(issuer_name %in% c("Kaiser Permanente", "Kaiser Foundation Health Plan of the Northwest"), "Kaiser Foundation Healthplan of the Northwest",
																	ifelse(issuer_name %in% c("LifeWise", "LifeWise Health Plan of Washington"), "LifeWise Health Plan of Washington",			 
																	ifelse(issuer_name == "Moda", "Moda Health Plan, Inc.",		
																	ifelse(issuer_name %in% c("Molina", "Molina Health Care of Washington"), "Molina Healthcare of Washington, Inc.",	
																	ifelse(issuer_name == "UnitedHealthcare", "UnitedHealthcare of Washington, Inc.",
																		issuer_name))))))))))
table(wa$issuer_name)
wa <- left_join(wa, ids_join, by = "issuer_name")

wa <- wa %>% mutate(issuer_id = ifelse(issuer_name == "Kaiser Foundation Health Plan of Washington (formerly Group Health)", 80473,
																				issuer_id))
temp <- wa %>% filter(is.na(issuer_id))
summary(wa$issuer_id)

wa <- wa %>%
	select(year, state_code, fips_county, county_name, issuer_name, issuer_id, everything()) %>%
	arrange(year, fips_county)

write.csv(wa, "data-original/state-based/wa-insurers.csv", row.names = F, na = "")

####################################################################################
# County enrollment
# 2017 data sent via email - represents effectuated enrollment?
####################################################################################
wa_enroll17 <- read.csv("data-original/state-based/raw/2017-wa-county-enrollment-raw.csv", stringsAsFactors = F)
wa_enroll17 <- wa_enroll17 %>% mutate(county_name = paste(county_name, "County", sep = " "))
wa_enroll17 <- left_join(wa_enroll17, fips_wa, by = "county_name")

# Add fields for joining
wa_enroll17 <- wa_enroll17 %>% mutate(state_code = "WA", year = 2017) %>%
	select(year, fips_county, county_name, state_code, everything()) %>%
	rename(plan_selections = effectuated_enrollment)
write.csv(wa_enroll17, "data-original/state-based/wa-county-enrollment.csv", row.names = F, na = "")
