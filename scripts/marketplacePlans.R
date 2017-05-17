# Marketplace plan offerings
# Healthcare.gov states from data.healthcare.gov
# 2017: https://data.healthcare.gov/dataset/2017-QHP-Landscape-Individual-Market-Medical/enpz-m4q6

library(dplyr)
library(tidyr)
library(stringr)

####################################################################################
# Plan Formatting functions
# Some plans are marketed under different names
# Blue Cross Blue Shield Healthcare Plan of Georgia is also Anthem but not marked in the landscape files - detect by HIOS ID
# Empire Blue Cross Blue Shield in NY is also Anthem
# Health Net in California was acquired by Centene in 2016
# Mark UHC in Virginia, since they've announced exit
####################################################################################
insurers <- c("Anthem", "Aetna", "Cigna", "Centene", "Highmark", "Humana", "Molina", "Wellmark")

formatLandscape <- function(dt, ffm, year) {
	colnames(dt) <- gsub('[.]', '_',tolower(colnames(dt)))
	colnames(dt) <- gsub('___', '_', colnames(dt))
	colnames(dt) <- gsub('__', '_', colnames(dt))
	colnames(dt) <- gsub('\\(', '', colnames(dt))
	colnames(dt) <- gsub('\\)', '', colnames(dt))
	colnames(dt) <- gsub('plan_id_standard_component_', 'plan_id_standard_component', colnames(dt))
	dt$fips_county <- sprintf("%05s", dt$fips_county)
	
	if (ffm == 1) {
		if ("plan_id_standard_component" %in% colnames(dt)) {
			dt <- dt %>% mutate(issuer_id = str_trunc(plan_id_standard_component, 5, "right", ellipsis = ""))
		}
		if ("rating_area" %in% colnames(dt)) {
			dt <- dt %>% mutate(rating_area = as.numeric(str_replace_all(rating_area, "Rating Area ", "")))
		}
		dt <- dt %>% mutate(year = year,
						humana = ifelse((str_detect(issuer_name, "Humana") | str_detect(plan_marketing_name, "Humana") |
															str_detect(plan_brochure_url, "Humana") | str_detect(plan_brochure_url, "humana")), 1, 0),
						 anthem = ifelse((str_detect(issuer_name, "Anthem") | str_detect(plan_marketing_name, "Anthem") |
						 								 	str_detect(plan_brochure_url, "Anthem") | str_detect(plan_brochure_url, "anthem") | issuer_id == 49046 | issuer_id == "49046"), 1, 0),
						 aetna = ifelse((str_detect(issuer_name, "Aetna") | str_detect(plan_marketing_name, "Aetna") |
						 								 	str_detect(plan_brochure_url, "Aetna") | str_detect(plan_brochure_url, "aetna")), 1, 0),
						 cigna = ifelse((str_detect(issuer_name, "Cigna") | str_detect(plan_marketing_name, "Cigna") |
						 									str_detect(plan_brochure_url, "Cigna") | str_detect(plan_brochure_url, "cigna")), 1, 0),
						 # Centene often markets as Ambetter, acquired Health Net
						 centene  = ifelse(str_detect(issuer_name, "Centene") | str_detect(plan_marketing_name, "centene") |
						 									str_detect(plan_brochure_url, "Centene") | str_detect(plan_brochure_url, "centene") |
						 										str_detect(issuer_name, "Ambetter") | str_detect(plan_marketing_name, "Ambetter") | str_detect(issuer_name, "CeltiCare") |
						 										(str_detect(issuer_name, "HealthNet") & state_code == "CA" & year >= 2016)| (str_detect(issuer_name, "Health Net") & state_code == "CA" & year >= 2016) |
						 										(str_detect(issuer_name, "Health Net") & state_code == "AZ"), 1, 0),
						 highmark = ifelse((str_detect(issuer_name, "Highmark") | str_detect(plan_marketing_name, "highmark") |
						 									str_detect(plan_brochure_url, "Highmark") | str_detect(plan_brochure_url, "highmark")), 1, 0),
						 molina = ifelse((str_detect(issuer_name, "Molina") | str_detect(plan_marketing_name, "molina") |
						 									str_detect(plan_brochure_url, "Molina") | str_detect(plan_brochure_url, "molina")), 1, 0),
						 wellmark = ifelse((str_detect(issuer_name, "Wellmark") | str_detect(plan_marketing_name, "wellmark") |
						 								 	str_detect(plan_brochure_url, "Wellmark") | str_detect(plan_brochure_url, "wellmark")), 1, 0),
						 uhc_va = ifelse(issuer_id == 38599, 1, 0))
	} else {
		dt <- dt %>%
			mutate(humana = ifelse((str_detect(issuer_name, "Humana") | str_detect(issuer_name, "humana")), 1, 0),
						 anthem = ifelse((str_detect(issuer_name, "Anthem") | str_detect(issuer_name, "anthem") | str_detect(issuer_name, "Empire Blue")), 1, 0),
						 aetna = ifelse((str_detect(issuer_name, "Aetna") | str_detect(issuer_name, "aetna")), 1, 0),
						 cigna = ifelse((str_detect(issuer_name, "Cigna") | str_detect(issuer_name, "cigna")), 1, 0),
						 centene = ifelse((str_detect(issuer_name, "Centene") | str_detect(issuer_name, "centene")  | str_detect(issuer_name, "CeltiCare") |
						 								 	str_detect(issuer_name, "Ambetter")  | 
						 									(str_detect(issuer_name, "HealthNet") & state_code == "CA" & year >= 2016)| (str_detect(issuer_name, "Health Net") & state_code == "CA" & year >= 2016)), 1, 0),
						 highmark = ifelse((str_detect(issuer_name, "Highmark") | str_detect(issuer_name, "highmark")), 1, 0),
						 molina = ifelse((str_detect(issuer_name, "Molina") | str_detect(issuer_name, "molina")), 1, 0),
						 wellmark = ifelse((str_detect(issuer_name, "Wellmark") | str_detect(issuer_name, "wellmark")), 1, 0),
						 uhc_va = 0)
	}
	dt <- dt %>% select(year, everything())
	return(dt)
}

####################################################################################
# FFM plan landscapes
####################################################################################
plans14 <- read.csv("data-original/landscape/2014_QHP_Landscape_Individual_Market_Medical_full.csv", stringsAsFactors = F)
plans14 <- formatLandscape(plans14, ffm = 1, 2014)

plans15 <- read.csv("data-original/landscape/2015_QHP_Landscape_Individual_Market_Medical_full.csv", stringsAsFactors = F)
plans15 <- formatLandscape(plans15, ffm = 1, 2015) 

plans16 <- read.csv("data-original/landscape/2016_QHP_Landscape_Individual_Market_Medical_full.csv", stringsAsFactors = F)
plans16 <- formatLandscape(plans16, ffm = 1, 2016) 

plans17 <- read.csv("data-original/landscape/2017_QHP_Landscape_Individual_Market_Medical.csv", stringsAsFactors = F)
plans17 <- plans17 %>% rename(fips_county = FIPS.County.Code)
plans17 <- formatLandscape(plans17, ffm = 1, 2017) 

####################################################################################
# Idaho also provides landscapes! Even for its SBM years
# Rating areas don't match CMS data so don't use
####################################################################################
id_plans15 <- read.csv("data-original/landscape/2015_QHP_Landscape_ID_Individual_Market_Medical.csv", stringsAsFactors = F)
id_plans16 <- read.csv("data-original/landscape/2016_QHP_Landscape_ID_Individual_Market_Medical.csv", stringsAsFactors = F)
id_plans17 <- read.csv("data-original/landscape/2017_QHP_Landscape_ID_Individual_Market_Medical.csv", stringsAsFactors = F)

id_plans15 <- formatLandscape(id_plans15, ffm = 1, 2015) 
id_plans16 <- formatLandscape(id_plans16, ffm = 1, 2016) 
id_plans17 <- formatLandscape(id_plans17, ffm = 1, 2017) 

####################################################################################
# SBM plan landscapes created from SERFF filings for some states
# processed in prepSerff.R
####################################################################################
sbm_plans14 <- read.csv("data-original/landscape/2014_QHP_Landscape_SBM_Individual_Market_Medical.csv", stringsAsFactors = F)
sbm_plans15 <- read.csv("data-original/landscape/2015_QHP_Landscape_SBM_Individual_Market_Medical.csv", stringsAsFactors = F)

sbm_plans14 <- formatLandscape(sbm_plans14, ffm = 1, 2014) 
sbm_plans15 <- formatLandscape(sbm_plans15, ffm = 1, 2015) 

####################################################################################
# SBM plan landscapes created from 2016 SBM PUFs for some states
# Processed in prepSbmPuf.R
####################################################################################
sbm_plans16 <- read.csv("data-original/landscape/2016_QHP_Landscape_SBM_Individual_Market_Medical.csv", stringsAsFactors = F)
sbm_plans16 <- formatLandscape(sbm_plans16, ffm = 1, 2016) 
# For some reason this file is throwing booleans instead of numerics for the created insurer vars
# Coerce to numeric
sbm_plans16 <- sbm_plans16 %>% mutate(humana = ifelse(is.na(humana), 0, humana),
																			anthem = ifelse(is.na(anthem), 0, anthem),
																			aetna = ifelse(is.na(aetna), 0,aetna),
																			cigna = ifelse(is.na(cigna), 0, cigna),
																			centene = ifelse(is.na(centene), 0, centene),
																			highmark = ifelse(is.na(highmark), 0, highmark),
																			molina = ifelse(is.na(molina), 0, molina),
																			wellmark = ifelse(is.na(wellmark), 0, wellmark))

####################################################################################
# Join SBMs, except ID which provides full landscapes like the FFMs and SERFF-only states
####################################################################################

ca <- read.csv("data-original/state-based/ca-insurers.csv", stringsAsFactors = F)
ct <- read.csv("data-original/state-based/ct-insurers.csv", stringsAsFactors = F)
co <- read.csv("data-original/state-based/2017-co-insurers.csv", stringsAsFactors = F)
dc <- read.csv("data-original/state-based/dc-insurers.csv", stringsAsFactors = F)
hi <- read.csv("data-original/state-based/hi-insurers.csv", stringsAsFactors = F)
ky <- read.csv("data-original/state-based/ky-insurers.csv", stringsAsFactors = F)
ma <- read.csv("data-original/state-based/ma-insurers.csv", stringsAsFactors = F)
md <- read.csv("data-original/state-based/md-insurers.csv", stringsAsFactors = F)
mn <- read.csv("data-original/state-based/mn-insurers.csv", stringsAsFactors = F)
ny <- read.csv("data-original/state-based/ny-insurers.csv", stringsAsFactors = F)
nv <- read.csv("data-original/state-based/2014-nv-insurers.csv", stringsAsFactors = F)
ri <- read.csv("data-original/state-based/ri-insurers.csv", stringsAsFactors = F)
vt <- read.csv("data-original/state-based/vt-insurers.csv", stringsAsFactors = F)
wa <- read.csv("data-original/state-based/wa-insurers.csv", stringsAsFactors = F)
issuers_sbm <- bind_rows(ca, ct, co, dc, hi, ky, ma, md, mn, ny, nv, ri, vt, wa)
issuers_sbm <- formatLandscape(issuers_sbm, ffm = 0, NULL) 

####################################################################################
# FFMs and Idaho, group by county & insurer
####################################################################################

makeIssuers <- function(dt, dropra) {
	if (dropra == 0) {
		issuers <- dt %>% group_by(year, fips_county, county_name, state_code, rating_area, issuer_name, issuer_id, humana, anthem, aetna, cigna, centene, highmark, molina, wellmark, uhc_va)
	} else {
		# weird rating areas in idaho data so don't use
		issuers <- dt %>% group_by(year, fips_county, county_name, state_code, issuer_name, issuer_id, humana, anthem, aetna, cigna, centene, highmark, molina, wellmark, uhc_va)
	}
	issuers <- issuers %>%
		summarize(plans = n()) %>%
		arrange(fips_county)
	issuers$issuer_id <- as.numeric(issuers$issuer_id)
	return (issuers)
}

issuers14 <- makeIssuers(plans14, 0)
issuers15 <- makeIssuers(plans15, 0)
issuers16 <- makeIssuers(plans16, 0)
issuers17 <- makeIssuers(plans17, 0)
id_issuers15 <- makeIssuers(id_plans15, 1)
id_issuers16 <- makeIssuers(id_plans16, 1)
id_issuers17 <- makeIssuers(id_plans17, 1)
sbm_issuers14 <- makeIssuers(sbm_plans14, 1)
sbm_issuers15 <- makeIssuers(sbm_plans15, 1)
sbm_issuers16 <- makeIssuers(sbm_plans16, 1)

issuers <- bind_rows(issuers14, issuers15, issuers16, issuers17, id_issuers15, id_issuers16, id_issuers17, sbm_issuers14, sbm_issuers15, sbm_issuers16, issuers_sbm)
issuers <- issuers %>% arrange(year, fips_county)

# Remove county name (non-uniform) and get from fips file
issuers <- as.data.frame(issuers)
issuers <- issuers %>% select(-county_name)
fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_codes <- fips_codes %>% select(fips_county, county_name)
issuers <- left_join(issuers, fips_codes, by = "fips_county") %>% select(year, fips_county, state_code, county_name, everything())

write.csv(issuers, "data/county-insurers.csv", row.names = F, na = "")

####################################################################################
# Number of issuers by county
####################################################################################

# Los Angeles CA has two rating areas, one of which has additional insurer (Oscar)
# Select rating area 16
issuers <- as.data.frame(issuers)
issuers <- issuers %>% filter(!(state_code == "CA" & rating_area == 15 & year == 2017))
issuer_numbers <- issuers %>% group_by(year, fips_county, state_code, county_name, rating_area) %>%
	summarize(issuers = n(),
						anthem = sum(anthem),
						aetna = sum(aetna),
						humana = sum(humana),
						cigna = sum(cigna),
						centene = sum(centene),
						highmark = sum(highmark),
						molina = sum(molina),
						wellmark = sum(wellmark),
						uhc_va = sum(uhc_va)) %>%
	mutate(anthem_only = ifelse(issuers - anthem == 0, 1, 0),
					humana_only = ifelse(issuers - humana == 0, 1, 0))

write.csv(issuer_numbers, "data/county-insurer-totals.csv", row.names = F, na="")

states <- issuer_numbers %>% group_by(state_code, year) %>%
	summarize(anthem = sum(anthem),
						aetna = sum(aetna),
				 humana = sum(humana),
				 cigna = sum(cigna),
				 centene = sum(centene),
				 highmark = sum(highmark),
				 molina = sum(molina),
				 wellmark = sum(wellmark),
				 uhc_va = sum(uhc_va))
write.csv(states, "data/state-insurer-totals.csv", row.names = F, na="")
