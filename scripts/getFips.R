# County fips
# From 2010, https://www.census.gov/geo/reference/codes/cou.html
library(dplyr)

fips_codes <- read.csv("http://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", header = FALSE)
colnames(fips_codes) <- c("state_code", "fips_state", "fips_county_temp", "county_name", "fips_class")
fips_codes <- fips_codes %>% mutate(fips_county = paste(sprintf("%02s", fips_state), sprintf("%03s", fips_county_temp), sep = "")) %>%
	select(-fips_county_temp, -fips_class) %>%
	mutate(fips_state = sprintf("%02s", fips_state)) %>%
	select(fips_county, everything())
write.csv(fips_codes, "data/fips.csv", na = "", row.names = F)
