# Maryland data

library(dplyr)
library(tidyr)
library(stringr)
library(openxlsx)

fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_md <- fips_codes %>% filter(state_code == "MD") %>%
	select(fips_county, county_name) %>%
	mutate(county_name = str_replace_all(county_name, "city", "City"))

####################################################################################
# Issuers by county
# 2014, 2015: using SERFF filings obtained by Public Information Act request
# 2016: using state-based marketplace public use files

# 2017 issuers by county
# Went to MD Health connection, entered a 27 year old for each county, entered into CSV
# Verified number by county with KFF map
# https://public.tableau.com/profile/kaiser.family.foundation#!/vizhome/InsurerParticipationinthe2017IndividualMarketplace/2017InsurerParticipation
####################################################################################

md17 <- read.csv("data-original/state-based/raw/2017-md-insurers-raw.csv", stringsAsFactors = F)

md17 <- left_join(md17, fips_md, by = "county_name")
md17 <- md17 %>% mutate(year = 2017,
												state_code = "MD") %>%
	arrange(fips_county) %>%
	select(year, fips_county, county_name, state_code, everything())

#####################################################################################
# Add HIOS ids
#####################################################################################
ids <- read.csv("data/hios-ids.csv", stringsAsFactors = F)
ids <- ids %>% mutate(issuer_name = str_replace_all(issuer_name, "  ", ", "))
ids_md <- ids %>% filter(state_code == "MD" & market == "Individual") %>%
	mutate(issuer_name = str_replace_all(issuer_name, "  ", ", ")) %>%
	group_by(issuer_name, issuer_id) %>%
	summarize(n = n()) %>%
	select(-n)
ids_md_puf <- ids %>% filter(state_code == "MD" & source == "SBM PUF")
table(md17$issuer_name)
table(ids_md_puf$issuer_name)

# CareFirst of Maryland and GHMSI are subsidiaries of CareFirst BlueCross BlueShield
# In Montgomery county and Prince George's county they're GHMSI, all other counties are CareFirst of MD (from 2016 data)
# For 2015, we only know "CareFirst" was in all counties, waiting on state to provide actual subsidiary data, pending request
md17 <- md17 %>% mutate(issuer_name = ifelse(issuer_name == "CareFirst BlueChoice", "CareFirst BlueChoice, Inc.",
																				 	ifelse(issuer_name == "CareFirst BlueCross BlueShield" & fips_county %in% c(24031, 24033), "Group Hospitalization and Medical Services, Inc.",
																				 	ifelse(issuer_name == "CareFirst BlueCross BlueShield" & !(fips_county %in% c(24031, 24033)), "CareFirst of Maryland, Inc.",
																				 	ifelse(str_detect(issuer_name, "Cigna"), "Cigna Health and Life Insurance Company",
																				 	ifelse(issuer_name == "Evergreen", "Evergreen Health Cooperative Inc.",
																				 	ifelse(str_detect(issuer_name, "Kaiser"), "Kaiser Foundation Health Plan of the Mid-Atlantic States, Inc.",
																				 	ifelse(issuer_name == "UnitedHealthCare", "UnitedHealthcare of the Mid-Atlantic, Inc.",
																				 			 	issuer_name))))))))
table(md17$issuer_name)

# Join ids
ids_join <- ids_md_puf %>% select(issuer_name, issuer_id)
md17 <- left_join(md17, ids_join, by = "issuer_name")

md17 <- md17 %>% select(year, state_code, rating_area, fips_county, county_name, issuer_name, issuer_id, everything()) %>%
	arrange(year, fips_county)

write.csv(md17, "data-original/state-based/md-insurers.csv", row.names = F, na = "")

####################################################################################
# County enrollment
# 2017 http://www.marylandhbe.com/wp-content/uploads/2017/01/020117_OE4Count.pdf
####################################################################################

md_enroll17 <- read.csv("data-original/state-based/raw/tabula-2017-md-enrollment-020117_OE4Count.csv", stringsAsFactors = F)
# Interested in QHPs only
md_enroll17 <- md_enroll17[,c(1,2)]
# Format
md_enroll17 <- md_enroll17 %>% rename(plan_selections = PRIVATE.QUALIFIED.HEALTH.PLANS) %>%
	mutate(plan_selections = as.numeric(str_replace_all(plan_selections, ",", ""))) %>%
	filter(county_name != "Out Of State" & county_name != "TOTAL") %>%
	mutate(county_name = ifelse(str_detect(county_name, "City"), county_name,
															paste(county_name, "County", sep = " ")))
md_enroll17 <- left_join(md_enroll17, fips_md, by="county_name")

md_enroll17 <- md_enroll17 %>% mutate(year = 2017, state_code = "MD") %>%
	select(year, fips_county, county_name, everything()) %>%
	arrange(year, fips_county)

write.csv(md_enroll17, "data-original/state-based/md-county-enrollment.csv", row.names = F, na = "")

####################################################################################
# Issuers by county, 2015-2017 provided in spreadsheet via email
# NOT USING - incomplete CareFirst breakdown/known errors with Kaiser 2017
####################################################################################
# md_raw <- readWorkbook("documents/md/QHP Carriers presence in Counties by year - 04-13-2017.xlsx", sheet = "Dataset")
# colnames(md_raw) <- tolower(colnames(md_raw))
# 
# # Need to match fips codes, county names are missing spaces
# md_raw <- md_raw %>% rename(issuer_name = carrier, name_match = county) %>%
# 	mutate(name_match = toupper(name_match))
# 
# fips_md <- fips_md %>% mutate(name_match = toupper(str_replace_all(county_name, "County", ""))) %>%
# 	mutate(name_match = str_replace_all(name_match, "ST\\.", "SAINT")) %>%
# 	mutate(name_match = str_replace_all(name_match, " ", "")) %>%
# 	mutate(name_match = str_replace_all(name_match, "'", "")) %>%
# 	mutate(name_match = str_replace_all(name_match, "[.]", ""))
# 
# md <- left_join(md_raw, fips_md, by="name_match") %>%
# 	select(-name_match) %>%
# 	arrange(year, fips_county) %>%
# 	mutate(state_code = "MD") %>%
# 	select(year, fips_county, county_name, everything())
write.csv(md, "data-original/state-based/md-insurers_fromstate.csv", row.names = F, na = "")
