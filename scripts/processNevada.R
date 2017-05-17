# Nevada pre-healthcare.gov data

library(dplyr)
library(tidyr)
library(stringr)

fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_nv <- fips_codes %>% filter(state_code == "NV") %>%
	select(fips_county, county_name)

####################################################################################
# 2014 plans offered by county
# Source: http://healthrates.doi.nv.gov/Wizard.aspx?type=Individual
# Selected all counties, age 27, all tiers, exchange plans, 2014
# 2015-present data is provided by healthcare.gov
####################################################################################

nv_raw <- read.csv("data-original/state-based/raw/tabula-2014-nv-plansoffered.csv", stringsAsFactors = F)
colnames(nv_raw) <- gsub('[.]', '_',tolower(colnames(nv_raw)))
head(nv_raw)
nv_raw <- nv_raw %>% select(-exchange, -status) %>%
	mutate(cost = as.numeric(str_replace_all(cost, "\\$", ""))) %>%
	rename(cost_age27 = cost)

# Add fips
nv_raw <- nv_raw %>% mutate(county_name = ifelse(str_detect(county, "City"), county, 
														paste(county, "County", sep = " "))) %>%
	select(-county)
# Join and rename fields to match federal data
nv_plans <- left_join(nv_raw, fips_nv, by="county_name") %>%
	mutate(year = 2014, state_code = "NV") %>%
	select(year, fips_county, county_name, state_code, everything()) %>%
	rename(issuer_name = carrier, metal_level = metal)

# Clean up newline chars from PDF output
nv_plans <- nv_plans %>% mutate(plan_name = str_replace_all(plan_name, "\n", " "),
																issuer_name = str_replace_all(issuer_name, "\n", " "))

# Indicate that MSP is anthem
nv_plans <- nv_plans %>% mutate(issuer_name = ifelse(issuer_name == "Multi-State Plan via the OPM (HMO)", "Anthem Multi-State Plan via the OPM (HMO)",
																												 issuer_name))

# Save plan data for future use
write.csv(nv_plans, "data-original/state-based/2014-nv-plans.csv", row.names = F, na = "")

####################################################################################
# 2014 Issuers by county
####################################################################################

# Collapse to issuers by county
nv_issuers <- nv_plans %>% group_by(year, fips_county, county_name, state_code, issuer_name) %>%
	summarize(plans = n()) %>%
	arrange(year, fips_county)

# Add HIOS id by referencing 2015-2017 NV healthcare.gov plans
table(nv_issuers$issuer_name)
nv_issuers <- nv_issuers %>% mutate(issuer_name = ifelse(issuer_name == "Prominence (HMO)", "Prominence HealthFirst",
																									ifelse(issuer_name == "Health Plan of Nevada (HMO)", "Health Plan of Nevada, Inc.",
																									ifelse(issuer_name == "Anthem (HMO)", "HMO Colorado Inc dba HMO NV(Anthem BCBS)",	
																									ifelse(issuer_name == "Anthem Multi-State Plan via the OPM (HMO)", "Rocky Mountain Hospital and Medical Service, Inc., dba Anthem Blue Cross and Blue Shield",		
																												 issuer_name)))))
table(nv_issuers$issuer_name)

nv_issuers <- nv_issuers %>% mutate(issuer_id = ifelse(issuer_name == "Prominence HealthFirst", 16698,
																								ifelse(issuer_name == "Health Plan of Nevada, Inc.", 95865,
																								ifelse(issuer_name == "HMO Colorado Inc dba HMO NV(Anthem BCBS)", 60156,
																								ifelse(issuer_name == "Rocky Mountain Hospital and Medical Service, Inc., dba Anthem Blue Cross and Blue Shield", 33670,
																											 NA)))))
table(nv_issuers$issuer_id)

nv_issuers <- nv_issuers %>%select(year, state_code, fips_county, county_name, issuer_name, issuer_id, everything()) %>%
	arrange(year, fips_county)
write.csv(nv_issuers, "data-original/state-based/2014-nv-insurers.csv", row.names = F, na = "")
