# Kentucky kynect data

library(dplyr)
library(tidyr)
library(stringr)

####################################################################################
# Marketplace plans
# [2016 plans](http://healthbenefitexchange.ky.gov/Documents/Plans%20offered%20on%20kynect%20in%202016.pdf)
# [2015 plans](http://healthbenefitexchange.ky.gov/Documents/Individual%20Medical%20Plans%20offered%20on%20kynect%20in%202015.pdf)
####################################################################################
formatRaw <- function(dt, year) { 
	dt <- dt %>% gather(issuer_name, plans, -c(county_name, rating_area)) %>%
		filter(!is.na(plans)) %>%
		filter(plans != "") %>%
		select(-plans) %>%
		arrange(county_name) %>%
		mutate(state_code = "KY", year = year,
					 issuer_name = str_replace_all(issuer_name, "[.]", " "),
					 issuer_name = str_replace_all(issuer_name, "HMO", ""),
					 issuer_name = str_replace_all(issuer_name, "PPO", ""),
					 issuer_name = str_replace_all(issuer_name, "OPM", ""),
					 issuer_name = str_trim(issuer_name, "both")) %>%
		# Format full issuer names
		mutate(issuer_name = ifelse(issuer_name == "Anthem", "Anthem Health Plans",
																ifelse(issuer_name == "KYHC", "Kentucky Health Cooperative", issuer_name)))
	# get unique rows, since original source separates out by PPO vs HMO etc
	dt <- dt %>% group_by(county_name, rating_area, issuer_name, state_code, year) %>%
		summarize(temp = n()) %>%
		select(-temp)
	return(dt)
}

ky15_raw <- read.csv("data-original/state-based/raw/tabula-2015-ky Individual Medical Plans offered on kynect in 2015.csv", stringsAsFactors = F)
ky16_raw <- read.csv("data-original/state-based/raw/tabula-2016-ky Plans offered on kynect in 2016.csv", stringsAsFactors = F)

ky15 <- formatRaw(ky15_raw, 2015)
ky16 <- formatRaw(ky16_raw, 2016)
ky <- rbind(ky15, ky16)
ky <- as.data.frame(ky)
ky <- ky %>% mutate(county_name = paste(county_name, "County", sep = " "))

# Single rating area - get county fips and names
fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_ky <- fips_codes %>% filter(state_code == "KY") %>%
	select(fips_county, county_name)

ky <- left_join(fips_ky, ky, by = "county_name")

# Standardize issuer names and add HIOS ids, based on 2016 SBM PUF Ids plus one pre-2016 company
ids <- read.csv("data/hios-ids.csv", stringsAsFactors = F)
ids_ky <- ids %>% filter(state_code == "KY" & (source == "SBM PUF" | year == 2015 & issuer_name == "Kentucky Health Cooperative, Inc." & market == "Individual"))
ids_ky <- ids_ky %>% mutate(issuer_name = str_replace_all(issuer_name, "  ", ", "))

table(ids_ky$issuer_name)
table(ky$issuer_name)

# Full names
ky <- ky %>% mutate(issuer_name = ifelse(issuer_name == "Aetna", "Aetna Health Inc. (a PA corp.)",
										ifelse(issuer_name == "Anthem Health Plans", "Anthem Health Plans of KY(Anthem BCBS)",
										ifelse(issuer_name == "Baptist Health", "Baptist Health Plan, Inc.",
										ifelse(issuer_name == "Care Source", "CareSource Kentucky Co.",
										ifelse(issuer_name == "Humana", "Humana Health Plan, Inc.",
										ifelse(issuer_name == "United", "UnitedHealthcare of Kentucky, Ltd.",
										ifelse(issuer_name == "WellCare", "WellCare Health Plans of Kentucky, Inc",
										ifelse(issuer_name == "Kentucky Health Cooperative", "Kentucky Health Cooperative, Inc.",
													 issuer_name)))))))))
table(ky$issuer_name)

# Match HIOS id
ids_ky <- ids_ky %>% select(issuer_name, issuer_id)
ky <- left_join(ky, ids_ky, by = "issuer_name")
table(ky$issuer_id)

ky <- ky %>% select(year, state_code, fips_county, county_name, issuer_name, issuer_id, everything()) %>%
	arrange(year, fips_county)

write.csv(ky, "data-original/state-based/ky-insurers.csv", row.names = F, na = "")
