# Health insurance marketplace data

This repo contains data and code used in an April 27, 2017 story, [Thousands of Obamacare Customers Left Without Options as Insurers Bolt](https://www.bloomberg.com/graphics/2017-health-insurer-exits/).

The data comes from many sources including [data.healthcare.gov](https://data.healthcare.gov/), state based marketplaces, Centers for Medicare and Medicaid Services, and SERFF filings. For a full list, see [sources.csv](sources.csv).

## Files
The [data](data/) folder includes cleaned national files that list insurers by county and enrollment by county. This folder also includes a [fips.csv](data/fips.csv), a CSV of county fips codes and county names and [hios-ids.csv](data/hios-ids.csv), an incomplete list of HIOS issuer IDs, used for reference in a few states.

The R scripts used to collect and clean source data and generate these files can be found in [scripts](scripts/). The compilation scripts used to create the national datasets in [data](data/), after running initial data downloading and processing scripts, are [processEnrollment.R](scripts/processEnrollment.R) and [marketplacePlans.R](scripts/marketplacePlans.R).

The [documents](documents/) folder includes unmodified source PDFs and a few Excel files downloaded from state marketplaces and the Centers for Medicare and Medicaid Services, organized by marketplace. It also includes files emailed directly by state marketplaces upon request. Some of these files were directly used in the creation of the final datasets, while many were used for reference and crossvalidation.

The [data-original](data-original/) folder includes many datasets used in interim steps, including: county enrollment datasets from data.cms.gov, and interim raw and processed state-based marketplace files. Many interim data files are too large to include on Github. These include: large individual medical marketplace QHP landscape files from data.healthcare.gov, state-based marketplace public use files (used in a few states for 2016), and SERFF filings (used in a few states for 2014 and 2015).

The 2016 state-based marketplace public use files are downloaded via python using (downloadSbmPuf.py)(scripts/downloadSbmPuf.py), or can be downloaded directly from [CMS](https://www.cms.gov/CCIIO/Resources/Data-Resources/sbm-puf.html). Note: these files contain known errors. For example, at publication time, the DC files omit one of their two insurers. The SBM PUFs are *only* used for a few states with heavy data checking when other data sources are unavailable.

## Missing data
At publication time, reliable insurer participation by county was not available for the following states: Maryland in 2014, Kentucky in 2014, California in 2015. We have ongoing public records requests in these states and will update this dataset if the data become available.

## Methods
### Sources
Insurer offerings from states using HealthCare.gov in a given year are calculated from HealthCare.gov QHP Landscape Individual Market Medical files retrieved in April 2017. County-level enrollment in those states and the District of Columbia was provided by the [Centers for Medicare and Medicaid Services](https://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/Marketplace-Products/Plan_Selection_ZIP.html).

For all other states, data were gathered from files posted by state marketplaces and insurance divisions, insurer plan filings and from state agencies in response to data requests. Publicly posted files listing plan offerings or insurer participation by county were used wherever possible. For states that did not post this information, data requests were sent to state marketplaces and/or insurance oversight divisions. When states were unavailable to provide insurer participation by county, plan details were collected from public access approved SERFF health plan binders for 2014 and 2015 in states that made those filings available. Insurer service areas for 2016 for California, Colorado and Maryland were calculated using public use files published by the Centers for Medicare and Medicaid Services.

### Enrollment data
2017 enrollment reflects total enrollment in a given county, regardless of insurer. Rhode Island, Massachusetts and Washington data reflects effectuated enrollment—customers who have paid monthly premiums. California enrollment data is rounded to the nearest 10, as provided by CoveredCA. Colorado enrollment data includes a special enrollment period made available to 2016 customers whose plan was discontinued in 2017. Minnesota enrollment data includes a one-week special enrollment period added after new premium relief subsidies were introduced in that state. In all other states, enrollment reflects total plan selections in the 2017 open enrollment period.

In the 39 states using HealthCare.gov in 2017, enrollment is suppressed in counties with fewer than 50 enrollees—a total of 1,572 plan selections across 38 counties, representing 0.02 percent of all 2017 HealthCare.gov enrollments.

### Insurer identification
Issuers were assigned to their parent companies using a combination of company descriptions and plan data, including issuer name, HIOS identifier, plan name and plan brochure URL.

In some cases, a parent company offers plans under multiple subsidiaries in a given county. For example, some Blue Cross Blue Shield licensees in states that include Florida, Pennsylvania, Michigan and Nevada offer HMO and PPO plans under separate brands. These subsidiaries are provided unique identifying numbers by the Center for Consumer Information and Insurance Oversight. In this analysis, insurers with unique HIOS identifiers are counted as separate companies.

## Attribution
The provided data should be cited as Bloomberg News analysis of data from HealthCare.gov, Centers for Medicare and Medicaid Services and data provided by state marketplaces.

If the data is used in an online story, the source line should include a link to this repository.