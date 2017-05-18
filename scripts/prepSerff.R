# Format plan filings downloaded from SERFF
options(java.parameters = "-Xmx1024m")
library(XLConnect)
library(dplyr)
library(tidyr)
library(stringr)

####################################################################################
# Definition datasets
####################################################################################
# Fips
fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_codes <- fips_codes %>% select(fips_county, state_code, county_name)

# Issuer names
issuer_names <- read.csv("data-original/serff-issuer-names.csv", colClasses = "character")

####################################################################################
# Functions
####################################################################################
# Service areas
readSerffSa <- function(dirpath, files, state_code, year) {
	sa <- NULL
	for (f in seq_along(files)) {
		wb <- loadWorkbook(paste(dirpath, files[f], sep = ""))
		print(files[f])
		
		# metadata
		column_names <- as.vector(readWorksheet(wb, sheet = "Service Areas", header = F, startRow = 11, endRow = 11)[1,])
		issuer_id <- readWorksheet(wb, sheet = "Service Areas", header = F, startRow = 6, endRow = 6, startCol = 2, endCol = 2)[1,1]
		if (!(is.character(issuer_id))) {
		 issuer_id <- as.character(issuer_id)
		}
		
		# Read worksheet, assing colnames
		temp <- readWorksheet(wb, sheet = "Service Areas", header = F, startRow = 13)
		if (dim(temp)[2] < dim(column_names)[2]) {
			num <- dim(temp)[2]
			colnames(temp) <- as.vector(column_names[1, c(1:num)])
		} else {
			colnames(temp) <- as.vector(column_names[1,])
		}
		# Remove empty rows
		temp <- temp[!apply(is.na(temp) | temp == "", 1, all),]
		
		temp <- temp %>% mutate(issuer_id = issuer_id, state_code = state_code, year = year)
		sa <- bind_rows(sa, temp)
	}
	# Clean up column names
	colnames(sa) <- str_replace_all(tolower(colnames(sa)), "[.]|[*]|[ ]|[__]|[___]", "_")
	colnames(sa) <- str_replace_all(colnames(sa), "_$|[(]|[)]", "")
	
	# Binary vars
	sa[sa == "Yes"] <- 1
	sa[sa == "No"] <- 0
	sa <- sa %>% rename(cover_entire_state = state)
	
	#if ("county_name" %in% colnames(sa)) {
	# Separate out county names
	#	sa <- sa %>% separate(county_name, into = c("county_name", "fips_county"), sep = " - ", fill="warn")
	#}
	sa <- sa %>% separate(county_name, into = c("county_name", "fips_county"), sep = " - ", fill="warn")
	
	# For full state service areas, make one row per county
	fips_list <- fips_codes[which(fips_codes$state_code == state_code),]$fips_county
	sa <- sa %>% select(-county_name) %>%
		mutate(fips_county = ifelse(cover_entire_state == 1, paste(c(fips_list), collapse=', ' ), fips_county)) %>%
		mutate(fips_county = strsplit(fips_county, ",")) %>% 
		unnest(fips_county) %>%
		mutate(fips_county = str_trim(fips_county, side="both"))
	
	# Coerce SA id to upper (issue with one MD file)
	sa$service_area_id <- toupper(sa$service_area_id)
	
	# Add nice county names
	sa <- left_join(sa, fips_codes, by = c("fips_county", "state_code"))
	sa <- sa %>% select(year, state_code, issuer_id, fips_county, county_name, everything())
	
	return(sa)
}

# Plan benefits
readSerffBenefits <- function(dirpath, files, year) {
	plans <- NULL
	for (f in seq_along(files)) {
		wb <- loadWorkbook(paste(dirpath, files[f], sep = ""))
		print(files[f])
		
		# For each "Benefits Package" sheet append the data
		benefits_sheets <- getSheets(wb)[grepl("Benefits Package", getSheets(wb))]
		for (s in seq_along(benefits_sheets)) {
			issuer_id <- readWorksheet(wb, sheet = benefits_sheets[s], header = F, startRow = 2, endRow = 2, startCol = 2, endCol = 2)[1,1]
			if (!(is.character(issuer_id))) {
				issuer_id <- as.character(issuer_id)
			}
			
			state_code <- readWorksheet(wb, sheet = benefits_sheets[s], header = F, startRow = 3, endRow = 3, startCol = 2, endCol = 2)[1,1]
			print(benefits_sheets[s])
			
			temp <- readWorksheet(wb, sheet = benefits_sheets[s], header = T, startRow = 8, endRow = 58)
			temp <- temp %>% mutate_all(as.character)
			temp <- temp %>% mutate(state_code = state_code, issuer_id = issuer_id)
			
			plans <- bind_rows(plans, temp)
		}
	}
	# Format joined dataset
	colnames(plans) <- str_replace_all(tolower(colnames(plans)), "[.]|[*]|[ ]|[__]|[___]", "_")
	colnames(plans) <- str_replace_all(colnames(plans), "_$|[(]|[)]", "")
	
	plans <- plans %>% mutate(year = year) %>%
		select(year, state_code, issuer_id, everything())
	return(plans)
}

# Single dataset of QHPs by county
makeSerffDataset <- function(dirpath, state_code, year) {
	benefits_files <- list.files(path = dirpath, pattern = "_DATA_BENEFITS.xlsm$", recursive = TRUE)
	sa_files <- list.files(path = dirpath, pattern = "_SERVICE_AREA.xls$", recursive = TRUE)
	
	plans <- readSerffBenefits(dirpath, benefits_files, year)
	plans <- left_join(plans, issuer_names, by=c("state_code", "issuer_id"))
	
	sa <- readSerffSa(dirpath, sa_files, state_code, year)
	qhp <- left_join(plans, sa, by=c("year", "state_code", "issuer_id", "service_area_id")) %>%
		select(year, state_code, issuer_id, issuer_name, fips_county, county_name, everything()) %>%
		filter(qhp_non_qhp != "Off the Exchange") %>%
		# Rename plan_brochure to match FFM files
		rename(plan_brochure_url = plan_brochure)
	return(qhp)
}

####################################################################################
# Maryland custom wrapper function
# Maryland 2014, 2015 SERFF files received via PIA request
# Non-standard file names and some .xlsx files included
# To avoid renaming files, set file paths custom outside the standard makeSerffDataset
####################################################################################

makeMDSerffDataset <- function(dirpath, benefits_files, sa_files, state_code, year) {

	plans <- readSerffBenefits(dirpath, benefits_files, year)
	plans <- left_join(plans, issuer_names, by=c("state_code", "issuer_id"))
	sa <- readSerffSa(dirpath, sa_files, state_code, year)
	
	qhp <- left_join(plans, sa, by=c("year", "state_code", "issuer_id", "service_area_id")) %>%
		select(year, state_code, issuer_id, issuer_name, fips_county, county_name, everything()) %>%
		filter(qhp_non_qhp != "Off the Exchange") %>%
		# Rename plan_brochure to match FFM files
		rename(plan_brochure_url = plan_brochure)
	return(qhp)
}

# 2014 benefits templates - one is .xlsx rest are .xlsm
benefits1 <- list.files(path = "data-original/serff/md-2014/", pattern = ".xlsm$", recursive = TRUE)
benefits_md14 <- c(benefits1, "72564 Evergreen/Evergreen_MASTER CCIIO TEMPLATE_CGB_10.15.13.xlsx")

# 2014 service area templates
sa1 <- list.files(path = "data-original/serff/md-2014/", pattern = "service_area.xls$", recursive = TRUE)
sa_md14 <- c(sa1, "28137 CareFirst BlueChoice/MD_Service Area_28137_CFBC_plan_management_data_templates_service_area (2).xls",
						 "72564 Evergreen/Evergreen Health Coop Service Area Template_10.9.13.xlsx",
						 "90296 Kaiser/Kaiser_Permanente_MD_Service_Area_Template.xlsx")

# 2015 benefits templates
benefits_md15 <- list.files(path = "data-original/serff/md-2015/", pattern = ".xlsm$", recursive = TRUE)
sa_md15 <- list.files(path = "data-original/serff/md-2015/", pattern = "SERVICE|Service Area|ServiceArea", recursive = TRUE)

md_qhp14 <- makeMDSerffDataset("data-original/serff/md-2014/", benefits_md14, sa_md14, "MD", 2014)
# md 14 is getting an empty col, remove it
md_qhp14 <- md_qhp14 %>% select(-col41)

md_qhp15 <- makeMDSerffDataset("data-original/serff/md-2015/", benefits_md15, sa_md15, "MD", 2015)

####################################################################################
#### HOW TO: Download and unzip files from SERFF public access ####
# Choose "Health Plan Binder Search"
# Select individual medical plans for relevant year(s)
# Then go into each binder, choose "Select current version only" in the attachments
# section and download
# Save in the data-original/serff/ folder in a subfolder named "[state-code]-[year]"
# Unzip each .zip into the state-year folder
# https://filingaccess.serff.com/sfa/home/CO
# https://filingaccess.serff.com/sfa/home/MN
# https://filingaccess.serff.com/sfa/home/OR
####################################################################################

# 2015 dataset
co_qhp14 <- makeSerffDataset("data-original/serff/co-2014/", "CO", 2014)
or_qhp14 <- makeSerffDataset("data-original/serff/or-2014/", "OR", 2014)
mn_qhp14 <- makeSerffDataset("data-original/serff/mn-2014/", "MN", 2014)

qhp14 <- rbind(co_qhp14, or_qhp14, mn_qhp14, md_qhp14)
write.csv(qhp14, "data-original/landscape/2014_QHP_Landscape_SBM_Individual_Market_Medical.csv", row.names = F, na = "")

# 2015 dataset
co_qhp15 <- makeSerffDataset("data-original/serff/co-2015/", "CO", 2015)

qhp15 <- rbind(co_qhp15, md_qhp15)
write.csv(qhp15, "data-original/landscape/2015_QHP_Landscape_SBM_Individual_Market_Medical.csv", row.names = F, na = "")
