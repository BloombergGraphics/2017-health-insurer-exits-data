# SBM public use files available for 2016
# https://www.cms.gov/CCIIO/Resources/Data-Resources/sbm-puf.html
# Use to supplement for missing states
# Downloaded files with downloadSbmPuf.py

library(dplyr)
library(tidyr)
library(stringr)
#library(readxl)
library(XLConnect)

fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_codes <- fips_codes %>% select(fips_county, fips_state, state_code, county_name)

####################################################################################
# Functions
####################################################################################

readSbmSa <- function(path, state_code) {
	sa <- NULL
	wb <- loadWorkbook(path)
	sa <- readWorksheet(wb, sheet=1)
	
	colnames(sa) <- gsub('[.]', '_',tolower(colnames(sa)))
	colnames(sa) <- gsub('[*]', '_',colnames(sa))
	colnames(sa) <- gsub(' ', '_',colnames(sa))
	colnames(sa) <- gsub('[(]', '',colnames(sa))
	colnames(sa) <- gsub('[)]', '',colnames(sa))
	colnames(sa) <- gsub("_$", "",colnames(sa))
	
	# Get state fips
	fips_state <- fips_codes[fips_codes$state_code == state_code,]$fips_state[1]
	
	sa <- sa %>% rename(year = business_year) %>%
		mutate(fips_county = ifelse(cover_entire_state == 0, paste(fips_state, sprintf("%03s", county), sep=""), NA)) %>%
		select(-county)
	
	# Collapse partial counties
	sa <- sa %>% group_by(year, state_code, issuer_id, fips_county, source_name, version_number, import_date, service_area_id, 
												service_area_name, cover_entire_state, market_coverage, dental_plan_only) %>%
		summarize(temp = n()) %>%
		select(-temp)
	
	sa <- as.data.frame(sa)

	# For full state service areas, make one row per county
	fips_list <- fips_codes[which(fips_codes$state_code == state_code),]$fips_county
	sa <- sa %>% mutate(fips_county = ifelse(cover_entire_state == 1, paste(c(fips_list), collapse=', ' ), fips_county)) %>%
	mutate(fips_county = strsplit(fips_county, ",")) %>% 
	unnest(fips_county) %>%
	mutate(fips_county = str_trim(fips_county, side="both"))
	
	# Add nice county names
	sa <- left_join(sa, fips_codes, by = c("fips_county", "state_code"))
	sa <- sa %>% select(year, state_code, issuer_id, fips_county, county_name, everything()) %>% 
		filter(market_coverage == "Individual" & dental_plan_only != "Yes")
	
	return(sa)
}

readSbmPlan <- function(path) {
	wb <- loadWorkbook(path)
	dt <- readWorksheet(wb, sheet=1)
	
	colnames(dt) <- gsub('[.]', '_',tolower(colnames(dt)))
	colnames(dt) <- gsub('[*]', '_',colnames(dt))
	colnames(dt) <- gsub(' ', '_',colnames(dt))
	colnames(dt) <- gsub('[(]', '',colnames(dt))
	colnames(dt) <- gsub('[)]', '',colnames(dt))
	colnames(dt) <- gsub("_$", "",colnames(dt))
	dt <- dt %>% rename(year = business_year) %>% 
		filter(market_coverage == "Individual" & dental_only_plan != "Yes")

	dt <- dt %>% select(year, state_code, issuer_id, everything()) %>% 
		filter(market_coverage == "Individual" & dental_only_plan != "Yes") %>%
		select(-source_name, -version_number, -import_date)
	
	return(dt)
}

# Single dataset of QHPs by county
makeSbmDataset <- function(path_plans, path_sa, state_code) {
	sa <- readSbmSa(path_sa, state_code)
	plans <- readSbmPlan(path_plans)
	
	qhp <- left_join(plans, sa, by=c("year", "state_code", "issuer_id", "service_area_id", "market_coverage")) %>%
		select(year, state_code, issuer_id, issuer_name, fips_county, county_name, everything()) %>%
		filter(qhp_nonqhp_type_id != "Off the Exchange") %>%
		# Rename plan_brochure to match FFM files
		rename(plan_brochure_url = plan_brochure) %>%
		arrange(year, fips_county, standard_component_id)
	return(qhp)
}

####################################################################################
# Get plans by county for states where data missing
# Combine service areas + plan details to get plans by county
####################################################################################
states_use <- c("CA", "CO", "CT", "KY", "MA", "MD", "MN", "NY", "RI", "WA")
states_nosa <- c("DC", "VT")
path_dir <- "data-original/sbm-puf/"

# States with multiple service areas
sbm_plans16 <- NULL
for (s in states_use) {
	print(s)
	path_folder <- paste(path_dir, tolower(s), sep = "")
	# File names are not consistent
	file_plans = list.files(path = path_folder, pattern = "(plan|Plan).xls$", recursive = TRUE)
	file_sa = list.files(path = path_folder, pattern = "(service|Service).*\\.xls$", recursive = TRUE)
	pathplans = paste(path_folder, file_plans, sep = "/")
	pathsa = paste(path_folder, file_sa, sep = "/")
	
	dt <- makeSbmDataset(pathplans, pathsa, s)
	sbm_plans16 <- rbind(sbm_plans16, dt)

}

# States with a single service area
nosa_plans16 <- NULL
for (s in states_nosa) {
	print(s)
	path_folder <- paste(path_dir, tolower(s), sep = "")
	# File names are not consistent
	file_plans = list.files(path = path_folder, pattern = "(plan|Plan).xls$", recursive = TRUE)
	pathplans = paste(path_folder, file_plans, sep = "/")
	
	dt <- readSbmPlan(pathplans)
	nosa_plans16 <- rbind(nosa_plans16, dt)
}
# Add all counties
fips_nosa <- fips_codes %>% filter(state_code %in% states_nosa)
nosa_plans16 <- left_join(nosa_plans16, fips_nosa, by = "state_code") %>%
	select(year, state_code, issuer_id, issuer_name, fips_county, county_name, everything()) %>%
	filter(qhp_nonqhp_type_id != "Off the Exchange") %>%
	# Rename plan_brochure to match FFM files
	rename(plan_brochure_url = plan_brochure) %>%
	arrange(year, fips_county, standard_component_id)

# Coerce some cols to character to merge
cols <- str_detect(colnames(nosa_plans16), "moop|tehb_ded_inn_tier_1_individual|sbc_having_a_baby_copayment")   
nosa_plans16[,cols] <- lapply(nosa_plans16[,cols], function(x) as.character((x)))
cols2 <- str_detect(colnames(sbm_plans16), "moop|tehb_ded_inn_tier_1_individual|sbc_having_a_baby_copayment")   
sbm_plans16[,cols2] <- lapply(sbm_plans16[,cols2], function(x) as.character((x)))

# Join
sbm_plans16 <- bind_rows(nosa_plans16, sbm_plans16) %>%
	arrange(fips_county, standard_component_id)

write.csv(sbm_plans16, "data-original/landscape/2016_QHP_Landscape_SBM_Individual_Market_Medical_full.csv", na ="", row.names = F)

####################################################################################
# One issue: different AVs for silver plans are separate rows (cost-sharing)
# So don't count # of plans by county unless collapsing
# Collapse by by standard_component_id

#### ISSUES ####
# Kaiser is not included for DC!
####################################################################################
# Check that this works (temp should have same # of rows as temp2)
temp <- as.data.frame(table(sbm_exchplans16$standard_component_id))
temp2<- as.data.frame(table(sbm_plans16$standard_component_id))

# Get standard exchange plans only
sbm_exchplans16 <- sbm_plans16 %>% mutate(plan_id_component = str_trunc(plan_id, 2, side="left", ellipsis = "")) %>%
	filter(plan_id_component == "01")

write.csv(sbm_exchplans16, "data-original/landscape/2016_QHP_Landscape_SBM_Individual_Market_Medical_allstates.csv", na ="", row.names = F)
# States i'm using
sbmuse_exchplans16 <- sbm_exchplans16 %>% filter(state_code %in% c("CA", "CO", "MD"))
write.csv(sbmuse_exchplans16, "data-original/landscape/2016_QHP_Landscape_SBM_Individual_Market_Medical.csv", na ="", row.names = F)
