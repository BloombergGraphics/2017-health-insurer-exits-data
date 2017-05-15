# Hawaii - SBM in 2014 and 2015

library(dplyr)
library(tidyr)

####################################################################################
# Marketplace plans
# [2015 plans](https://cca.hawaii.gov/ins/files/2015/10/on-exchange-2016-Individual-Premiums.pdf)
# [2014 plans](http://cca.hawaii.gov/ins/files/2013/12/Exchange-Premium-Comparisons-Ind-Rates-2014.pdf)
####################################################################################

hi14 <- data.frame(c("Hawaii Medical Service Association (HMSA)", "Kaiser Permanente Hawaii"), c(11, 14))
colnames(hi14) <- c("issuer_name", "plans")
hi14 <- hi14 %>% mutate(year = 2014)

hi15 <- data.frame(c("Hawaii Medical Service Association (HMSA)", "Kaiser Permanente Hawaii"), c(11, 10))
colnames(hi15) <- c("issuer_name", "plans")
hi15 <- hi15 %>% mutate(year = 2015)

hi <- bind_rows(hi14, hi15)
# Add fips etc
hi <- hi %>% mutate(state_code = "HI", 
										rating_area = 1)

# Add fips
fips_codes <- read.csv("data/fips.csv", colClasses = "character")
fips_hi <- fips_codes %>% filter(state_code == "HI") %>%
	select(fips_county, county_name, state_code)

hi <- left_join(hi, fips_hi, by = "state_code")
hi <- hi %>% select(year, fips_county, county_name, everything()) %>%
	arrange(year, fips_county)

write.csv(hi, "data-original/state-based/hi-insurers.csv", row.names = F, na = "")
