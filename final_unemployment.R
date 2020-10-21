#-------------------------------------------------------------------------#
# 1 Project Introduction --------------------------------------------------
#-------------------------------------------------------------------------#

# Identifying population groups critically affected by the economic and health impacts of the Coronavirus Pandemic for Targeted Assistance

#   In the 101 municipalities under the MAPC's jurisdiction, we see large differences in quality of life indicators such as employment, economic security, 
# access to quality housing, health, and education. These disparities have been aggravated by the virus pandemic, and its economic effects as well.
#   Which demographic groups show the most consistent levels of unemployment since March? 
# What identifiers such as race, age, gender, educational levels & occupational types do these individuals exhibit?
#   Knowing, and acknowledging the impact of 20th century urban and regional planning, we can safely assume that populations who were most vulnerable to crises
# (Black & Hispanic persons, women identifying persons, non-college educated persons, service & essential workers (also termed as 'blue-collar' workers)) 
# tend to live in close proximity to people of similar background. Our goal is to identify such locations for a targeted relief program to provide assistance
# to affected persons in order for them to have basic, minimum quality of life standards to weather this, and possibly future crises.


#-------------------------------------------------------------------------#
# 2 Loading Libraries -----------------------------------------------------
#-------------------------------------------------------------------------#

# run this line if packages do not exist:
# install.packages("blsAPI", "rjson", "tidyverse", "lubridate", "sf", "RColorBrewer", "ggridges", "vroom", "waldo") 

library(blsAPI)         # R package to get data from the Bureau of Labor Statistics API
library(tidyverse)      # tidyverse...
library(sf)             # R package for spatial analysis using the 'simple features' geographic standards
library(lubridate)      # R package for temporal data manipulation
library(RColorBrewer)   # R package with expanded color palettes
library(ggridges)       # R package for ridgeplots, (formerly called 'joyplots' - now discontinued)
library(vroom)          # R package to read gzip files
library(waldo)          # R package to find differences between different objects
library(gmapsdistance)  # R package to work with Google's routes API
library(gt)             # R package for generating formatted tables
library(scales)         # R package for aesthetics editing in ggplot
library(paletteer)      # R package: comprehensive collection of palettes

#-------------------------------------------------------------------------#
# 3 Loading Relevant Extrernal Data: --------------------------------------
#-------------------------------------------------------------------------#

# 3.1 Census Bureau Data: -------------------------------------------------
#-------------------------------------------------------------------------#

# 3.1.1 Sub-county Resident Population Estimates: -------------------------
# https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/cities/totals/sub-est2019_25.csv
census_data <- read_csv("https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/cities/totals/sub-est2019_25.csv")

# 3.1.2 Housing Tenure Characteristics 2018 5-year ACS: -------------------
# https://data.census.gov/cedsci/table?q=Owner%2FRenter%20%28Tenure%29&g=0400000US25.060000&tid=ACSDT5Y2018.B25003&hidePreview=true
hh_tenure <- read_csv("owner_renter_occu_2018acs5yr/owner_renter_occu_2018acs5yr.csv")


# 3.1.3 Per Capita Income 2018 5-year ACS: --------------------------------
# https://data.census.gov/cedsci/table?q=ACSDT1Y2019.B19301&g=0400000US25.060000&tid=ACSDT5Y2018.B19301&hidePreview=true
pc_income <- read_csv("percapita_income_2018acs5yr/percapita_income_2018acs5yr.csv")


# 3.2 BLS Local Area Unemployment Statistics: -----------------------------
#-------------------------------------------------------------------------#
# https://download.bls.gov/pub/time.series/la/la.data.28.Massachusetts
laus <- read_tsv("https://download.bls.gov/pub/time.series/la/la.data.28.Massachusetts")


# 3.3 Municipalities Boundary Data Massachusetts: -------------------------
#-------------------------------------------------------------------------#
# https://datacommon.mapc.org/browser/datasets/390
ma_muni <- read_sf("ma_muni_boundary/ma_muni_boundary.shp")


# 3.4 Covid-19 case data Massachusetts 14th October 2020 weekly: ----------
#-------------------------------------------------------------------------#
covid_data <- read_csv("covid_data.csv")


# 3.5 Household crowding data: --------------------------------------------
#-------------------------------------------------------------------------#
# sourced from The Boston Indicators dashboard
crowding <- read_csv("muni_crowding.csv") # percent of households per city with > 1 occupant per room


# 3.5 LEHD LODES data: Employment Characteristics: ------------------------
#-------------------------------------------------------------------------#

# 3.5.1 Residential Area Characteristics (RAC): ---------------------------
# https://lehd.ces.census.gov/data/lodes/LODES7/ma/rac/ma_rac_S000_JT00_2017.csv.gz
ma_rac <- vroom("https://lehd.ces.census.gov/data/lodes/LODES7/ma/rac/ma_rac_S000_JT00_2017.csv.gz", 
                col_types = cols(h_geocode = col_character(),
                                 .default = col_guess()))

# 3.5.2 Origin Destination Data (OD):--------------------------------------
# https://lehd.ces.census.gov/data/lodes/LODES7/ma/od/ma_od_main_JT00_2017.csv.gz
ma_od <- vroom("https://lehd.ces.census.gov/data/lodes/LODES7/ma/od/ma_od_main_JT00_2017.csv.gz", 
               col_types = cols(w_geocode = col_character(),
                                h_geocode = col_character(),
                                .default = col_guess()))
# 3.5.3 Crosswalk File: ---------------------------------------------------
# https://lehd.ces.census.gov/data/lodes/LODES7/ma/ma_xwalk.csv.gz
ma_xw <- vroom("https://lehd.ces.census.gov/data/lodes/LODES7/ma/ma_xwalk.csv.gz",
               col_types = cols(.default = col_character()),
               col_select = c(censusblk_id = tabblk2010,
                              muni_id = ctycsub))

#-------------------------------------------------------------------------#
# 4 Data Cleaning ---------------------------------------------------------
#-------------------------------------------------------------------------#

# certain mismatched variables such as muni_ids (unique 'place' identifiers) are dissimilar, and with some error across data
# converting yyymmdd date variables to readable DATE formats
# renaming irregularly names variables for uniformity across all data sources
# adding 'municipality' name data to certain data sources which were missing


# 4.1 CENSUS DATA: --------------------------------------------------------
#-------------------------------------------------------------------------#


# 4.1.1 Local Area Codes: -------------------------------------------------
# Local Area Codes in the Census are different for two municipalities: Bridgewater & Randolph:

census_data$COUSUB <- str_replace(string = census_data$COUSUB, pattern = "08130", "08085")
census_data$COUSUB <- str_replace(string = census_data$COUSUB, pattern = "56000", "55955")

# padding string to match BLS area codes from Census bureau:

municodes <- census_data %>% 
  filter(FUNCSTAT == 'A',PRIMGEO_FLAG == 1) %>% 
  arrange(NAME) %>% 
  mutate(COUSUB = str_pad(COUSUB, 11, side = "right", pad = "0")) %>% 
  mutate(muni_id = str_c(STATE, COUSUB),
         muni_name = NAME,
         popest_2010 = POPESTIMATE2010,
         popest_2019 = POPESTIMATE2019) %>% 
  select(muni_name, muni_id, popest_2010, popest_2019)
#remove(census_data)

# Cleaning municodes Name field to match corresponding data in the municipalities shapefile for easy joining:

municodes <- municodes %>% 
  mutate(muni_name = str_remove(muni_name, " town| Town")) %>% 
  mutate(muni_name = str_remove(muni_name, " city"))


# 4.1.2 Income & Housing Tenure: ------------------------------------------
hh_chars <- left_join(hh_tenure, pc_income,
                      by = c("muni_id", "muni_name"))

hh_chars <- hh_chars %>% mutate(muni_id = str_remove(string = muni_id, "0600000US"))
str_sub(hh_chars$muni_id, 3, 5) <- ""
hh_chars <- hh_chars %>% mutate(muni_id = str_pad(muni_id, 13, side = "right", pad = "0"))
hh_chars <- left_join(hh_chars %>% select(!muni_name),
                   municodes %>% select(muni_id, muni_name),
                   by = c("muni_id"))
hh_chars <- hh_chars %>% relocate(muni_name, .before = tot_hu) %>% select(!muni_id) %>% 
  select(muni_name,
    hu_tot = tot_hu,
    hu_tot_me = me_tot_hu,
    hu_oo = oo_hu,
    hu_oo_me = me_oo_hu,
    hu_ro = ro_hu,
    hu_ro_me = me_ro_hu,
    inc_pcap = pc_inc,
    inc_pcap_me = me_pc_inc)

# 4.2 Massachusetts State Municipality Shapefile: -------------------------
#-------------------------------------------------------------------------#

# removing unrequired columns and renaming municipality name variable for consistency:
ma_muni <- ma_muni %>% mutate(muni_name = municipal) %>% 
  select(muni_name, geometry)
ma_muni$muni_name <- str_replace(string = ma_muni$muni_name, pattern = "Manchester", "Manchester-by-the-Sea")

ma_muni <- left_join(ma_muni,
                     municodes,
                     by = c("muni_name" = "muni_name"))

ma_muni <- ma_muni %>% relocate(c(muni_id, popest_2010, popest_2019), .after = muni_name) %>%
  arrange(muni_name)


# 4.3 BLS LAUS Data: ------------------------------------------------------
#-------------------------------------------------------------------------#

laus_data <- laus %>%
  # creating municipality codes to match across all data sets
  filter(str_detect(series_id, "LAUCT25|LAUCS25"), year >= 2018) %>%
  mutate(series_id = str_remove(series_id, "LAUCT|LAUCS")) %>%
  mutate(muni_id = str_remove(series_id, "03$|04$|05$|06$")) %>%
  # creating a date object
  filter(str_detect(period, "M13", negate = TRUE)) %>%
  mutate(year_month = ymd(str_c(year, str_remove(period, "M"), "01")),
         year = year(year_month)) %>%
  # adding municipality name info
  left_join(., municodes,
            by = "muni_id")

# 4.4 LEHD LODES Employment Data: -----------------------------------------
#-------------------------------------------------------------------------#


# 4.4.1 Crosswalk File: ---------------------------------------------------
# Matching muni_ids across all three datasets:

str_sub(ma_xw$muni_id, 3, 5) <- ""
ma_xw <- ma_xw %>% 
  mutate(muni_id = str_pad(muni_id, 13, side = "right", pad = "0")) %>% 
  left_join(.,
            municodes %>% select(muni_id, muni_name),
            by = c("muni_id"))

# 
# **********
# NOTE: **** In the OD and RAC data, we join muni_ids, and muni_names to census block groups:
# NOTE: **** Census areas do not necessarily overlap with municipality boundaries
# **********


# 4.4.2 OD Data: ----------------------------------------------------------
# Adding municipality name and id data to work locations:
ma_od <- left_join(ma_od,
                   ma_xw %>% mutate(w_muni_id = muni_id,w_muni_name = muni_name) %>%
                     select(w_muni_id, w_muni_name, censusblk_id),
                   by = c("w_geocode" = "censusblk_id"))
# Adding municipality name and id data to home locations:
ma_od <- left_join(ma_od,
                   ma_xw %>% mutate(h_muni_id = muni_id,h_muni_name = muni_name) %>%
                     select(h_muni_id, h_muni_name, censusblk_id),
                   by = c("h_geocode" = "censusblk_id"))


# 4.4.3 RAC Data ----------------------------------------------------------
# Adding municipality name and id data to home locations:
ma_rac <- left_join(ma_rac,
                   ma_xw %>% select(muni_id, muni_name, censusblk_id),
                   by = c("h_geocode" = "censusblk_id"))
remove(ma_xw)


#-------------------------------------------------------------------------#
# 5 Data Restructuring ----------------------------------------------------
#-------------------------------------------------------------------------#

# making pivot tables
# aggregating data to municipality level
# creating aggregated summary level data


# 5.1 BLS LAUS Data: ------------------------------------------------------
#-------------------------------------------------------------------------#

# BLS data has to be first grouped by series_id, which correspond to what value they have:
# ending with 03: Unemployment Rates
# ending with 04: Unemployment Counts
# ending with 05: Employment Counts
# ending with 06: Total Labor Force Counts

# By doing a pivot_wider, we get these data into columns, which are easier to analyze and visualize

# 5.1.1 Annual Averages: --------------------------------------------------
# Creating subsets for the four different BLS data variables: reduced to ANNUAL averages:

# Unemployment rates: (code = 03)
ue_rate <- laus_data %>% 
  filter(str_detect(series_id, "03$")) %>% 
  select(muni_name, muni_id, year, value, popest_2010, popest_2019) %>% 
  group_by(muni_name, muni_id, year) %>% 
  summarise(ue_rate = mean(value), popest_2010 = mean(as.numeric(popest_2010)), popest_2019 = mean(popest_2019))

# Unemployment count: (code = 04)
ue_count <- laus_data %>%
  filter(str_detect(series_id, "04$")) %>% 
  select(muni_name, muni_id, year, value, popest_2010, popest_2019) %>% 
  group_by(muni_name, muni_id, year) %>%
  summarise(ue_count = mean(value),popest_2010 = mean(as.numeric(popest_2010)),popest_2019 = mean(popest_2019))

# Employment count: (code = 05)
emp_count <- laus_data %>%
  filter(str_detect(series_id, "05$")) %>% 
  select(muni_name, muni_id, year, value, popest_2010, popest_2019) %>% 
  group_by(muni_name, muni_id, year) %>%
  summarise(emp_count = mean(value),popest_2010 = mean(as.numeric(popest_2010)),popest_2019 = mean(popest_2019))

# Labor force count: (code = 06)
labor_force <- laus_data %>% 
  filter(str_detect(series_id, "06$")) %>% 
  select(muni_name, muni_id, year, value, popest_2010, popest_2019) %>% 
  group_by(muni_name, muni_id, year) %>%
  summarise(labor_force = mean(value),popest_2010 = mean(as.numeric(popest_2010)),popest_2019 = mean(popest_2019))

# Joining and pivoting wider to get columns corresponding to each year:
bls_nonaggr_annual <- list(ue_rate, ue_count, emp_count, labor_force) %>% 
  reduce(full_join, by = c("muni_name", "muni_id", "year", "popest_2010", "popest_2019"))

bls_aggr_annual <- list(ue_rate, ue_count, emp_count, labor_force) %>% 
  reduce(full_join, by = c("muni_name", "muni_id", "year", "popest_2010", "popest_2019")) %>%
  pivot_wider(names_from = year, values_from = c(ue_rate, ue_count, emp_count, labor_force))

# Rate of change of unemployment to see which communities were hit hardest:
bls_aggr_annual$ue_rateofchange <- ((bls_aggr_annual$ue_rate_2020/bls_aggr_annual$ue_rate_2019)-1)


# 5.1.2 Monthly Averages: -------------------------------------------------
# Creating subsets for the four different BLS data variables: reduced to MONTHLY averages 

#***********
# NOTE: **** We only take monthly data from 2020 onward to see effects of the Covid-19 Pandemic closely
#***********

# Unemployment rates: (code = 03)
ue_rate_monthly <- laus_data %>% 
  filter(str_detect(series_id, "03$"),year_month >= "2020-01-01") %>% 
  select(muni_name, muni_id, value, year_month, popest_2010, popest_2019) %>% 
  group_by(muni_name, muni_id, year_month) %>% 
  summarise(ue_rate = mean(value),
            popest_2010 = mean(as.numeric(popest_2010)),
            popest_2019 = mean(popest_2019))

# Unemployment count: (code = 04)
ue_count_monthly <- laus_data %>% 
  filter(str_detect(series_id, "04$"),year_month >= "2020-01-01") %>% 
  select(muni_name, muni_id, value, year_month, popest_2010, popest_2019) %>% 
  group_by(muni_name, muni_id, year_month) %>% 
  summarise(ue_count = mean(value),
            popest_2010 = mean(as.numeric(popest_2010)),
            popest_2019 = mean(popest_2019))

# Employment count: (code = 05)
emp_count_monthly <- laus_data %>% 
  filter(str_detect(series_id, "05$"),year_month >= "2020-01-01") %>% 
  select(muni_name, muni_id, value, year_month, popest_2010, popest_2019) %>% 
  group_by(muni_name, muni_id, year_month) %>% 
  summarise(emp_count = mean(value),
            popest_2010 = mean(as.numeric(popest_2010)),
            popest_2019 = mean(popest_2019))

# Labor force count: (code = 06)
labor_force_monthly <-  laus_data %>% 
  filter(str_detect(series_id, "06$"),year_month >= "2020-01-01") %>% 
  select(muni_name, muni_id, value, year_month, popest_2010, popest_2019) %>% 
  group_by(muni_name, muni_id, year_month) %>% 
  summarise(labor_force = mean(value),
            popest_2010 = mean(as.numeric(popest_2010)),
            popest_2019 = mean(popest_2019))

# Joining and pivoting wider to get columns corresponding to each year-month:
bls_nonaggr_monthly <- list(ue_rate_monthly, ue_count_monthly, emp_count_monthly, labor_force_monthly) %>% 
  reduce(full_join, by = c("muni_name", "muni_id", "year_month", "popest_2010", "popest_2019"))

bls_aggr_monthly <- list(ue_rate_monthly, ue_count_monthly, emp_count_monthly, labor_force_monthly) %>% 
  reduce(full_join, by = c("muni_name", "muni_id", "year_month", "popest_2010", "popest_2019")) %>%
  pivot_wider(names_from = year_month, values_from = c(ue_rate, ue_count, emp_count, labor_force))


# 5.2 LEHD LODES Employment Data: -----------------------------------------
#-------------------------------------------------------------------------#


# 5.2.1 Origin Destination Data (OD): -------------------------------------

# Summing all employment variables to origin:
# data table is restructured: 
# * Each row representing count of jobs by type and location town for each home location town

# each 'geography' in the LODES data is at the census block level.
# we are interested in looking at municipal statistics. therefore we summarise at that level

ma_od_summary <- ma_od %>% relocate(c(w_muni_id, w_muni_name), .after = w_geocode) %>% 
  relocate(c(h_geocode, h_muni_id, h_muni_name), .before = w_geocode) %>%
  select(-c(h_geocode, w_geocode, createdate)) %>%
  group_by(h_muni_id,h_muni_name, w_muni_id, w_muni_name) %>%
  summarise(across(.cols = everything(), sum)) %>%
  mutate(lowinc_jobs = SE01+SE02) %>%
  arrange(h_muni_name,desc(lowinc_jobs)) %>%
  group_by(h_muni_name)
remove(ma_od)

# 5.2.2 Residence Area Characteristics (RAC): -----------------------------

# each 'geography' in the LODES data is at the census block level.
# we are interested in looking at municipal statistics. therefore we summarise at that level

ma_rac_summary <- ma_rac %>% relocate(c(muni_id, muni_name), .after = h_geocode) %>%
  select(-c(h_geocode, createdate, muni_id)) %>%
  group_by(muni_name) %>%
  summarise(across(.cols = everything(), sum)) %>%
  mutate(jobs_tot = C000,
         jobs_nocolg = CD02,
         jobs_lowinc = CE01+CE02,
         jobs_lowinc_pc = ((CE01+CE02)/C000)*100,
         jobs_nonwhite = CR02+CR03+CR04+CR05+CT02,
         jobs_nonwhite_pc = ((CR02+CR03+CR04+CR05+CT02)/C000)*100) %>%
  arrange(muni_name)
remove(ma_rac)

#-------------------------------------------------------------------------#
# 6 Data Analysis: --------------------------------------------------------
#-------------------------------------------------------------------------#

# 6.1 Unemployment Volatility: --------------------------------------------
#-------------------------------------------------------------------------#
# Taking only data from between April & August 2020 to measure impact of summer layoffs:
# Standard Deviation from the mean: Gives us how much the unemployment rate varied

emp_volatility <- ue_rate_monthly %>% 
  group_by(muni_name) %>%  
  filter(year_month >= '2020-04-01') %>% 
  summarise(unemp_avg_rate = mean(ue_rate), # average unemployment rate
            sd_ue_rate = sd(log(ue_rate)),  # standard deviation of the normalized unemployment rate
            unemp_rate_change = (last(ue_rate)-first(ue_rate)), # rate of change of unemployment
            emp_vltl = mean(ue_rate)/sd(log(ue_rate))) %>% # employment volatility
  arrange(desc(emp_vltl)) %>% 
  mutate(emp_vltl = percent_rank(emp_vltl)) # percent ranked unemployment volatility

# 6.2 Demographic Data File: ----------------------------------------------
#-------------------------------------------------------------------------#

# The demographic data file gives a detailed information on certain related variables such as:
# * Location: muni_id, muni_name, geometry
# * employment data: population estimates, unemployment rates, volatility, rate of change
# * Covid-19: total_cases, percent_pve, relative_change
# * household characteristics: house_crowding, housing tenure type, income,

# Joining:
# Covid-19 statistics data: total_counts, percent_pve, relative_change_cases
# Household Crowding data: house_crowding
# LODES RAC Data: C000=total_jobs, lowinc_jobs_pc, all_nonwhite_pc
# Unemployment Volatility: unemp_vltl
# household characteristics: income, tenure

demog_data <- list(ma_muni, 
                  covid_data %>% select(muni_name, covid_tot_cases, covid_pc_pv, covid_rel_change),
                  crowding %>% select(muni_name, hu_crowding),
                  hh_chars,
                  ma_rac_summary %>% select(muni_name, jobs_nocolg, jobs_tot, jobs_lowinc_pc, jobs_nonwhite_pc),
                  emp_volatility %>% select(muni_name, emp_vltl, unemp_avg_rate, unemp_rate_change)) %>% 
  reduce(full_join, by = c("muni_name")) %>% 
  select(muni_id, muni_name,popest_2010,popest_2019,
         emp_vltl, unemp_avg_rate, unemp_rate_change,
         jobs_tot, jobs_nocolg, jobs_lowinc_pc, jobs_nonwhite_pc,
         inc_pcap, inc_pcap_me,
         hu_tot, hu_tot_me, hu_oo, hu_oo_me, hu_ro, hu_ro_me, hu_crowding,
         covid_tot_cases, covid_pc_pv, covid_rel_change,
         geometry)

# Write demog_data as a shapefile, geopackage, and csv TO SHARE PUBLICLY:

write_sf(demog_data, "demog_data/demog_data.shp") # ESRI Shapefile
write_sf(demog_data, "demog_data/demog_data.gpkg")# Geopackage
write_csv(demog_data, "demog_data/demog_data.csv") # csv

# metadata file:

demog_meta <-  tibble(variable = colnames(demog_data),
                     meaning = c("Unique Municipality ID", "Municipality Name",
                                 "Census 2010 Population Estimate", "Census 2019 Population Estimate",
                                 "Employment Volatility Score", "Average Summer 2020 Unemployment Rate", 
                                 "Summer 2020 Unemployment Rate Change", "Total Jobs", "Jobs without College Degree", 
                                 "Low Income Jobs, percentage", "Non-White jobs percentage",
                                 "Avg Per Capita Income", "Avg Per Capita Income Margin of Error",
                                 "Total Housing Units", "Total Housing Units, Margin of Error",
                                 "Owner Occupied Housing Units", "Owner Occupied Housing Units, Margin of Error",
                                 "Renter Occupied Housing Units", "Renter Occuped Housing Units, Margin of Error",
                                 "Percent of Crowded Housing Units: >1 persons per habitable room",
                                 "Total Covid-19 cases", "Percent Positive Covid-19 Cases", 
                                 "Relative Change in Covid-19 Cases","Geometry Shape"))

write_csv(demog_meta, "demog_data/demog_meta.csv")

# Visualising Unemployment Volatility:

demog_data %>% 
  # selecting 100 most populous MA cities:
  arrange(desc(popest_2019)) %>% 
  head(100) %>% 
  arrange(desc(emp_vltl)) %>%
  ggplot()+
  # mapping all MA municipalities
  geom_sf(data = demog_data, 
          aes(geometry = geometry), 
          color = NA)+
  # mapping the unemployment volatility index
  geom_sf(aes(geometry = geometry,
              fill = emp_vltl),
          color = NA)+
  # setting palette for scale: highest, median, lowest score
  scale_fill_gradient2(low = muted("#2166ac"), 
                       mid = "#f7f7f7", midpoint = 0.5, 
                       high = "#b2182b")+
  theme_minimal()+
  ggsave("emp_vltl.svg")

# 6.3 Relative Change in Unemployment Rates Summer 2020 -------------------
#-------------------------------------------------------------------------#

# Creating dataset of highest and lowest 10 Summer 2020 Unemployment communities:
ridgedata_top10unemp <- list(demog_data %>% as_tibble() %>% arrange(desc(popest_2019)) %>% head(100) %>% 
                               arrange(desc(unemp_avg_rate)) %>% head(10) %>% 
                               select(muni_name, emp_vltl),
                             bls_nonaggr_monthly %>% select(muni_name, year_month, ue_rate)) %>%
  reduce(left_join ,by = "muni_name") %>% mutate(empvltl_type = "high")

ridgedata_btm10unemp <- list(demog_data %>% as_tibble() %>% arrange(desc(popest_2019)) %>% head(100) %>% 
                               arrange(desc(unemp_avg_rate)) %>% tail(10) %>% 
                               select(muni_name, emp_vltl),
                             bls_nonaggr_monthly %>% select(muni_name, year_month, ue_rate)) %>%
  reduce(left_join ,by = "muni_name") %>% mutate(empvltl_type = "low")

ridgedata_unemp <- bind_rows(ridgedata_top10unemp, ridgedata_btm10unemp)

ridgedata_unemp_order <- ridgedata_unemp %>% filter(year_month == "2020-08-01") %>% 
  arrange(desc(ue_rate))  %>% mutate(rank = rank(ue_rate, ties.method = "first")) %>% select(muni_name, ue_rate, rank)

ridgedata_unemp <- list(ridgedata_unemp, ridgedata_unemp_order %>% select(muni_name, rank)) %>% 
  reduce(left_join, by = "muni_name")

# Visualising through a ridgeplot:
ggplot() + 
  geom_ridgeline(data = ridgedata_unemp, aes(x = year_month,
                                             y = reorder(muni_name, rank),
                                             height = (ue_rate),
                                             color = empvltl_type), fill = NA, size = 1)+
  scale_x_date(date_labels = "%b", date_breaks = "1 months")+
  theme_minimal()#+
  #ggsave("ridgeplot_unemp.svg")

# 6.4 Origin-Destination Job flow Data File: ------------------------------
#-------------------------------------------------------------------------#

# 6.4.1 SF Object Creation: -----------------------------------------------
# Creating a sf object where the geometry corresponds to 'work locations' for a given 'home location'
ma_od_summary_shp <- left_join(ma_od_summary,
                               ma_muni,
                               by = c("w_muni_name" = "muni_name",
                                      "w_muni_id" = "muni_id"))

# Sample Geographic plot: for home location of Lynn, MA:
ma_od_summary_shp %>% filter(h_muni_name == "Lynn") %>% head(n = 5) %>%
  ggplot()+geom_sf(data = ma_muni, aes(geometry = geometry))+
  geom_sf(aes(geometry = geometry, fill = lowinc_jobs))

ma_od_summary_shp %>% filter(h_muni_name == "Lynn") %>% head(n = 5) %>%
  ggplot()+geom_col(aes(x = reorder(w_muni_name, rev(lowinc_jobs)), 
                        y = lowinc_jobs,
                        fill = lowinc_jobs))

# Making subset of top and bottom 10 municipalities to print:
top10emp_vltl <- demog_data %>% as_tibble() %>% 
  select(muni_name, popest_2019, emp_vltl) %>% 
  arrange(desc(popest_2019)) %>% head(100) %>% 
  arrange(desc(emp_vltl)) %>% head(10)
btm10emp_vltl <- demog_data %>% as_tibble() %>% 
  select(muni_name, popest_2019, emp_vltl) %>% 
  arrange(desc(popest_2019)) %>% head(100) %>% 
  arrange(desc(emp_vltl)) %>% tail(10)
top10btm10_empvltl <- bind_rows(top10emp_vltl, btm10emp_vltl)
ma_od_lowinc_top5 <- list(top10btm10_empvltl %>% select(muni_name),
                          ma_od_summary_shp) %>%
  reduce(left_join, by = c("muni_name" = "h_muni_name")) %>%
  group_by(muni_name) %>% slice_head(n = 5)
  
  
  # for (i in ma_od_lowinc_top5$muni_name) 
  #   {
  #   ma_od_lowinc_top5 %>% filter(muni_name == as.character(i))%>%
  #     ggplot()+geom_col(aes(x = reorder(w_muni_name, rev(lowinc_jobs)), 
  #                           y = lowinc_jobs,
  #                           fill = lowinc_jobs))+
  #     ggsave(paste0("lowinc_job_locations/lowinc_job_locations_",i,".svg"))
  # }

ma_od_lowinc_top5 %>% filter(muni_name == "Amherst") %>% head(n = 5) %>%
  ggplot(aes(x = reorder(w_muni_name, rev(lowinc_jobs)), 
             y = lowinc_jobs))+
  geom_col(fill = "#26afff")+
  geom_text(aes(label = lowinc_jobs), vjust = -0.5)+
  theme_minimal()+
  ggsave(paste0("lowinc_job_locations/lowinc_job_locations_","Amherst",".svg"))

# 6.4.2 Driving & Transit Distance Tables: --------------------------------

dist_od_summary <- ma_od_lowinc_top5 %>% 
  mutate(muni_name = gsub(" ", "+", muni_name),
         w_muni_name = gsub(" ", "+", w_muni_name)) %>%
  group_by(muni_name)
# dr_matrix <- gmapsdistance(
#   origin = paste0(dist_od_summary$muni_name, "+MA"),
#   destination = paste0(dist_od_summary$w_muni_name, "+MA"),
#   combinations = "pairwise",
#   mode = "driving",
#   dep_date = "2020-10-20",
#   dep_time = "09:00:00", # 5am UTC = 9am EST
#   key = "AIzaSyBbMZ0c7Rdrs1OGn8_2RgCnhQN1dKNOlYM"
# )
# 
# dr_tbl <- data.frame(dr_matrix)
# dr_tbl <- data.frame("minutes" = dr_tbl$Time.Time/60, # converting time to minutes from seconds
#                      "miles" = dr_tbl$Distance.Distance*0.000621371, # converting distance to miles from meters
#                      "uqlatlongid" = paste(dr_tbl$Time.or, dr_tbl$Time.de, sep = "+")) # we create a unique identifier tag of the o_latlong and d_latlong, on which we can merge later.

tr_matrix <- gmapsdistance(
  origin = paste0(dist_od_summary$muni_name, "+MA"),
  destination = paste0(dist_od_summary$w_muni_name, "+MA"),
  combinations = "pairwise",
  mode = "transit",
  dep_date = "2020-10-20",
  dep_time = "09:00:00", # 5am UTC = 9am EST
  key = "AIzaSyBbMZ0c7Rdrs1OGn8_2RgCnhQN1dKNOlYM"
)

tr_tbl <- data.frame(tr_matrix)
tr_tbl <- data.frame("minutes" = tr_tbl$Time.Time/60, # converting time to minutes from seconds
                     "miles" = tr_tbl$Distance.Distance*0.000621371, # converting distance to miles from meters
                     "odpair" = paste(tr_tbl$Time.or, tr_tbl$Time.de, sep = "+")) # we create a unique identifier tag of the o_latlong and d_latlong, on which we can merge later.
tr_tbl <- tr_tbl %>% filter(minutes > 0)
write_csv(tr_tbl, "tr_tbl.csv")
tr_tbl <- read_csv("tr_tbl.csv")

odpair <- ma_od_lowinc_top5 %>%
  select(muni_name, w_muni_name) %>% 
  mutate(muni_name_1 = paste(gsub(" ", "+", muni_name),"MA", sep = "+"),
         w_muni_name_1 = paste(gsub(" ", "+", w_muni_name),"MA", sep = "+")) %>% 
  mutate(odpair = str_c(muni_name_1, w_muni_name_1, sep = "+")) %>% 
  select(muni_name, w_muni_name, odpair)

ma_od_lowinc_top5 <- left_join(ma_od_lowinc_top5,
                               odpair,
                               by = c("muni_name", "w_muni_name"))

ma_od_lowinc_top5 <- left_join(ma_od_lowinc_top5,
                               tr_tbl,
                               by = "odpair")
# 6.5 Ten Least & Most Vulnerable Municipalities Vital Stats Table: -------
#-------------------------------------------------------------------------#

transittable <- ma_od_lowinc_top5 %>% select(muni_name, minutes, miles) %>% group_by(muni_name) %>% 
  summarise(avg_tr_min = mean(minutes, na.rm = TRUE),
         avg_tr_mi = mean(miles, na.rm = TRUE))

# Most Vulnerable:
vital_stats_top10 <- demog_data %>% as_tibble() %>% 
  select(muni_name, popest_2019, emp_vltl, unemp_avg_rate, inc_pcap, jobs_lowinc_pc,
         jobs_nonwhite_pc, hu_ro, hu_tot, hu_crowding, covid_pc_pv) %>% 
  mutate(hu_ro_pc = (hu_ro/hu_tot)*100,
         covid_p100k_pv = covid_pc_pv*1000)%>% 
  arrange(desc(popest_2019)) %>% head(100) %>% 
  arrange(desc(emp_vltl)) %>% head(10) %>% 
  select(-c(hu_ro, hu_tot, covid_pc_pv))
vital_stats_top10 <- left_join(vital_stats_top10,
                               transittable,
                               by = "muni_name")
write_csv(vital_stats_top10, "vital_stats_top10.csv")

# Least Vulnerable:
vital_stats_btm10 <- demog_data %>% as_tibble() %>% 
  select(muni_name, popest_2019, emp_vltl, unemp_avg_rate, inc_pcap, jobs_lowinc_pc,
         jobs_nonwhite_pc, hu_ro, hu_tot, hu_crowding, covid_pc_pv) %>% 
  mutate(hu_ro_pc = (hu_ro/hu_tot)*100,
         covid_p100k_pv = covid_pc_pv*1000)%>% 
  arrange(desc(popest_2019)) %>% head(100) %>% 
  arrange(emp_vltl) %>% head(10) %>% 
  select(-c(hu_ro, hu_tot, covid_pc_pv)) 
vital_stats_btm10 <- left_join(vital_stats_btm10,
                               transittable,
                               by = "muni_name")
write_csv(vital_stats_btm10, "vital_stats_btm10.csv")

