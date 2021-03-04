

library(tidyverse)
library(sf)
library(tidygeocoder)
library(tidycensus)
library(scales)
library(readxl)
library(viridis)
library(ggplot2)

# Setup steps -------------------------------------------------------------

# Obtain Census API Key here: https://api.census.gov/data/key_signup.html
#census_api_key('API_KEY', install = TRUE) 
readRenviron("~/.Renviron")

# Download data from Box here: https://uchicago.box.com/s/ut4wyfluhtdm3j8b1fd6ek3l05jtf890
# Put it in a local folder and enter path here:
file_path = '/Users/nm/Desktop/projects/work/mansueto/bwsj/bwsj-estab/'

# Read in data from the downloaded BWSJ folder
mwdbe <- read_csv(paste0(file_path,'BWSJ/mwdbe.csv'))
if (file.exists(paste0(file_path,'BWSJ/mwdbe_geocoded.csv'))) { mwdbe_clean <- st_read(paste0(file_path,'BWSJ/mwdbe_geocoded.csv')) }
mapcorps <- read_csv(paste0(file_path,'BWSJ/Map Corps Chicago 2009-2020/Chicago_Data_2019.csv'))
chi_buildings <- st_read(paste0(file_path,'BWSJ/chicago_footprints.geojson'))
infogroup <- read_csv(paste0(file_path,'BWSJ/chicago_establishments.csv'))
afam_biz_licenses <- read_csv(paste0(file_path,'BWSJ/afam_biz_licenses.csv'))

# NAICS -------------------------------------------------------------------

# Download and read in Census NAICS Codes
naics_url <- 'https://www.census.gov/naics/2017NAICS/2-6%20digit_2017_Codes.xlsx'
tmp_filepath <- paste0(tempdir(), '/', basename(naics_url))
download.file(url = paste0(naics_url), destfile = tmp_filepath)
naics <- read_excel(tmp_filepath)

# Clean up columns
naics_clean <- naics %>%
  select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>%
  rename_all(list(tolower)) %>%
  select(`2017_naics_us_code`, `2017_naics_us_title`) %>%
  drop_na() %>%
  rename(naics_code = `2017_naics_us_code`,
         naics_title = `2017_naics_us_title`) 
rm(naics)

# MWDBE Data --------------------------------------------------------------
# Source: https://chicago.mwdbe.com/ 

# Clean up the data
mwdbe <- mwdbe %>% 
  # Removes spaces and non-alphanumeric characters in columns names
  select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>% 
  # Converts all columns names to lower case
  rename_all(list(tolower)) %>% 
  # Removes non-numeric characters from industry columns
  mutate_at(vars(capability, commodity_codes), ~ gsub('[^0-9-]', '', .)) %>% 
  # Substring first 6 characters from each industry columns and coalesces into single column
  mutate(industry_code = coalesce(str_sub(commodity_codes,1,6),str_sub(capability,1,6))) 

# Filter to only African Americans
unique(mwdbe$ethnicity) # Look at values of ethnicity column
mwdbe <- mwdbe %>% filter(ethnicity %in% c("African-American (Black)", "African American"))

# Geocode addresses to lat - lons using a free API service from Census and OSM
# WARNING TAKES 10 minutes - if-else logic to skips this step if you have the file already downloaded from box
if (file.exists(paste0(file_path,'BWSJ/mwdbe_geocoded.csv'))) {
  mwdbe <- st_read(paste0(file_path,'BWSJ/mwdbe_geocoded.csv'))
} else { 
  mwdbe <- mwdbe %>% 
    geocode(street = physical_address, city = city, state = state, postalcode = zip,
            method = "cascade", cascade_order = c("census","osm"))
}

# Select columns with potentially useful data
mwdbe_clean <- mwdbe %>% 
  select(company_name, 
         owner_first, owner_last, 
         physical_address, city, state, zip, phone, email, industry_code, 
         lat, long) 

mwdbe_clean <- mwdbe_clean %>% mutate(source = 'mwdbe') %>%
  # Standardize / rename column names 
  rename(name = company_name, 
         street_address = physical_address,
         naics6 = industry_code,
         latitude = lat,
         longitude = long) %>% 
  # Drop NAs
  drop_na(latitude, longitude) %>%
  filter(latitude != 'NA') %>% 
  mutate(zip = as.numeric(zip)) %>%
  # Convert to lat lons to geometry format
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326, agr = "constant")

# Add in NAICS Code Label
mwdbe_clean <- mwdbe_clean %>%
  mutate(naics4 = str_sub(gsub('-','',naics6),1,4)) %>% 
  left_join(., naics_clean %>% rename(naics4_title = naics_title), by = c('naics4'='naics_code')) %>%
  left_join(., naics_clean %>% rename(naics6_title = naics_title), by = c('naics6'='naics_code')) 
  
mwdbe_clean <- mwdbe_clean %>%
  # Deduplicate identical rows
  distinct(name, street_address, .keep_all = TRUE) %>%
  arrange(phone, email, name, street_address) %>%
  # Create unique ID
  mutate(mwdbe_id = row_number(),
         universe_id = paste0(source,"_",mwdbe_id)) 
rm(mwdbe)

# MapCorps ----------------------------------------------------------------

mapcorps_clean <- mapcorps %>% 
  rename_all(list(tolower)) %>%
  # Drop out of business establishments
  filter(placestatus != "Out of Business") %>% 
  mutate(street_address = paste0(paste0(buildingnumber,' ',street,", ",unit))) %>% 
  # Combine fields into street address
  select(name,
         street_address,city,zip,
         placestatus, placetype, placesubtype, privateresidence, mappedyear, phonenumber, 
         lat, lng) %>%
  arrange(name, street_address, phonenumber, lat, lng) %>%
  # Create unique ID
  mutate(mapcorps_id = row_number()) 

mapcorps_clean <- mapcorps_clean %>% 
  mutate(source = 'mapcorps',
         universe_id = paste0(source,"_",mapcorps_id)) %>% 
  # Standardize / rename column names 
  rename(latitude = lat, 
         longitude = lng) %>% 
  # Drop NAs
  drop_na(latitude, longitude) %>% 
  # Convert to lat lons to geometry format
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326, agr = "constant")
rm(mapcorps)

# InfoGroup ---------------------------------------------------------------

# Select useful columns
infogroup_clean <- infogroup %>%
  select(abi, company,
         address_line_1, city, state, zipcode,
         primary_naics_code, #yellow_page_code, archive_version_year, 
         employee_size_5_location, sales_volume_9_location, year_established, # business_status_code, 
         subsidiary_number, parent_number,
         latitude, longitude) 

infogroup_clean <- infogroup_clean %>% 
  mutate(source = 'infogroup',
         universe_id = paste0(source,"_",abi)) %>%
  # Add in NAICS label
  mutate(naics4 = str_sub(primary_naics_code,1,4)) %>%
  mutate(naics6 = str_sub(primary_naics_code,1,6)) %>%
  left_join(., naics_clean %>% rename(naics4_title = naics_title), by = c('naics4'='naics_code')) %>%
  left_join(., naics_clean %>% rename(naics6_title = naics_title), by = c('naics6'='naics_code')) %>%
  # Standardize / rename column names 
  rename(name = company, 
         street_address = address_line_1,
         zip = zipcode) %>% 
  # Drop NAs
  drop_na(latitude, longitude) %>%  
  # Convert to lat lons to geometry format
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326, agr = "constant")
rm(infogroup)

# Business Licenses -------------------------------------------------------

# Download Business License data from Chicago Data Portal 
biz_licenses <- read_csv('https://data.cityofchicago.org/api/views/uupf-x98q/rows.csv?accessType=DOWNLOAD')
biz_licenses <- biz_licenses %>%
  select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>% 
  rename_all(list(tolower)) %>%
  filter(city == 'CHICAGO')

# Check if file is in folder
if (file.exists(paste0(file_path,'BWSJ/biz_licenses_geocode.csv'))) {
  biz_licenses_geocode <- read_csv(paste0(file_path,'BWSJ/biz_licenses_geocode.csv'))
} else {
  # If not in folder geocode missing lat lons
  biz_licenses_geocode <- biz_licenses %>% 
    filter(is.na(latitude)) %>%
    geocode(street = address, city = city, state = state, postalcode = zip_code,
            method = "cascade", cascade_order = c("census","osm")) %>%  
    # Coalesce missing columns with geocoded columns
    mutate(latitude = coalesce(latitude,lat),
           longitude = coalesce(longitude,long)) %>%
    # Select everything except for these columns
    select(-one_of(c('lat','long','geo_method')))
}
  
biz_licenses <- biz_licenses %>% 
  drop_na(latitude, longitude) %>%  
  # Stack / row bind the dataframes together
  rbind(., biz_licenses_geocode ) %>% 
  drop_na(latitude, longitude) %>%  
  # Convert to lat lons to geometry format
  st_as_sf(coords = c("longitude", "latitude"), 
           crs = 4326, agr = "constant")

biz_licenses_clean <- biz_licenses %>% 
  # Inner join and limit to universe of AfAm owned businesses 
  # Matched business owners names in the Chicago Data Portal to a voterfile within Cook County. There were 7,766 good matches.
  inner_join(., afam_biz_licenses, by=c('account_number'='account_number')) %>%
  # Rename column names 
  rename(street_address = address,
         zip = zip_code) %>%
  # Change from character to numeric
  mutate(zip = as.numeric(zip),
         source = 'licenses',
         universe_id = paste0(source,"_",license_id),
         name = coalesce(legal_name,doing_business_as_name)) %>%
  select(universe_id, id, license_id, account_number, site_number, name, legal_name, doing_business_as_name, 
         street_address, city, state, zip, source, 
         license_code, license_description, business_activity_id, business_activity, match_label, geometry) 
rm(biz_licenses)

# Combine all business files ----------------------------------------------

# Check for duplicates
mwdbe_clean %>%
  group_by(mwdbe_id) %>%
  mutate(dupes = n()) %>%
  filter(dupes >= 2)

biz_licenses_clean  %>%
  group_by(license_id) %>%
  mutate(dupes = n()) %>%
  filter(dupes >= 2)

infogroup_clean  %>% 
  group_by(abi) %>% 
  mutate(dupes = n()) %>%
  filter(dupes >= 2)

mapcorps_clean  %>% 
  group_by(mapcorps_id) %>% 
  mutate(dupes = n()) %>%
  filter(dupes >= 2)

# Combine files into master file: 
full_universe <- bind_rows(mwdbe_clean,biz_licenses_clean,infogroup_clean,mapcorps_clean) %>%
  mutate(black_owned = case_when(source == "licenses" ~ 'likely',
                                 source == "mwdbe" ~ 'confirmed',
                                 source == "mapcorps" ~ 'confirmed',
                                 source == "infogroup" ~ 'unverified',
                                 TRUE ~ as.character('unverified')),
         biz_activity_classification = case_when(source == "licenses" ~ 'License Code',
                                           source == "infogroup" ~ 'NAICS',
                                           source == "mwdbe" ~ "NAICS",
                                           source == "mapcorps" ~ 'PlaceSubType',
                                           TRUE ~ as.character('Other')),
         biz_activity_code = coalesce(as.character(naics6),as.character(license_code)),
         biz_activity_label = coalesce(naics6_title,license_description,placesubtype),
         phone = coalesce(phone,phonenumber))

full_universe  %>% 
  group_by(universe_id) %>% 
  mutate(dupes = n()) %>%
  filter(dupes >= 2)

full_universe <- full_universe %>%
  select(universe_id,
         name,
         street_address,
         city,
         state,
         zip,
         source,
         biz_activity_classification,
         biz_activity_code,
         biz_activity_label,
         black_owned,
         match_label,
         doing_business_as_name,
         owner_first,
         owner_last,
         phone,
         email,
         geometry) %>% 
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])) 

write_csv(full_universe_match_job %>% st_drop_geometry(), paste0(file_path,'match_job.csv'))
st_write(full_universe_match_job, paste0(file_path,'match_job.geojson'))

# Submit Match Job Here: https://shop.safegraph.com/match/

# Read in output from the match job

# Community Areas ---------------------------------------------------------

# Download Community Area geometries
community_areas <- sf::st_read('https://data.cityofchicago.org/api/geospatial/cauq-8yn6?method=export&format=GeoJSON') %>% 
  st_as_sf() %>% 
  select(community)

# Join Community Areas to Universe
full_universe <- full_universe %>%
  st_join(., community_areas)

# Spatial and Census data ---------------------------------------------------------

# Download Chicago tract geometries
chicago_tracts <- sf::st_read('https://data.cityofchicago.org/api/geospatial/5jrd-6zik?method=export&format=GeoJSON') %>% 
  st_as_sf() %>%
  mutate_at(vars(geoid10), list(as.character)) %>%
  mutate(geoid10 = str_pad(geoid10, width=11, side="left", pad="0")) %>%
  select(geoid10) %>%
  rename(geoid = geoid10)

# Investigate Census Variables
acs5_vars <- load_variables(year = 2019, dataset = 'acs5', cache = FALSE)
acs5_vars_subject <- load_variables(year = 2019, dataset = 'acs5/subject', cache = FALSE)
# Variables for Population and Median Household Income for Black and all races
acs5_vars_selected <- c('B02009_001', 'B02001_001', 'B19013_001', 'B19013B_001', 'B25001_001')

# Download Census data by tract for Cook County IL
acs_tract <- get_acs(year = 2019, geography = "tract", survey = 'acs5', variables = acs5_vars_selected, state = '17', county = '031')
# Reshape the table from long to wide
acs_tract <- acs_tract %>%
  rename_all(list(tolower)) %>%
  pivot_wider(id_cols = geoid,
            names_from = c(variable), 
            values_from = c(estimate)) %>%
  rename(total_housing_units = B25001_001, # TOTAL HOUSING UNITS
         total_population = B02001_001, # TOTAL POPULATION
         black_population = B02009_001, # POPULATION OF BLACK OR AFRICAN AMERICAN ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES
         median_household_income = B19013_001, # MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS
         black_median_household_income = B19013B_001) # MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2019 INFLATION-ADJUSTED DOLLARS) BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER

chicago_acs_tract <-chicago_tracts %>%
  left_join(., acs_tract, by = c('geoid'='geoid')) %>% # table join dataframes together on geoid
  st_join(., community_areas, left= TRUE, largest = TRUE) %>% # spatial join data on lat lon
  filter(!is.na(community)) %>% # filter where community is missing
  mutate(black_population_share = black_population/total_population)

# Visually inspect the data
ggplot(chicago_acs_tract, aes(fill = black_population_share, color = black_population_share)) +
  geom_sf() + scale_fill_viridis() + scale_color_viridis() 

ggplot(chicago_acs_tract, aes(fill = median_household_income, color = median_household_income)) +
  geom_sf() + scale_fill_viridis() + scale_color_viridis() 

ggplot(chicago_acs_tract, aes(fill = log(black_median_household_income), color = log(black_median_household_income) )) +
  geom_sf() + scale_fill_viridis() + scale_color_viridis() 

# Aggregate from tract to community area
chicago_acs_community_areas <- chicago_acs_tract %>%
  st_drop_geometry()%>%
  # Create weighted columns for median household income
  mutate(black_median_household_income = coalesce(black_median_household_income, median_household_income),
         median_household_income_weighted = total_housing_units*median_household_income,
         total_black_housing_units = total_housing_units*black_population_share,
         black_median_household_income_weighted = black_median_household_income*total_black_housing_units) %>%
  # Group by community and aggregate
  group_by(community) %>%  
  summarise_at(vars(total_population,
                    black_population,
                    median_household_income_weighted,
                    black_median_household_income_weighted,
                    total_black_housing_units,
                    total_housing_units), list(sum), na.rm = TRUE) %>% 
  ungroup() %>%
  # Adjust median estimates
  mutate(black_median_household_income = median_household_income_weighted / total_black_housing_units,
         median_household_income = median_household_income_weighted / total_housing_units) %>%
  select(community, 
         total_population,
         median_household_income,
         black_population,
         black_median_household_income) %>%
  mutate(black_population_share = black_population/total_population)

# Join community data to community area geometries
chicago_acs_community_areas <- left_join(community_areas, chicago_acs_community_areas, by = c('community'='community')) %>%
  st_transform(crs = st_crs(4326)) %>% 
  st_as_sf()

# Limit Full Universe to AfAm community areas
community_list <- chicago_acs_community_areas %>% filter(black_population_share >= .5) %>% pull(community)
afam_universe <- full_universe %>% filter(community %in% community_list)

write_csv(afam_universe %>% st_drop_geometry(), paste0(file_path,'afam_sites.csv'))
st_write(afam_universe, paste0(file_path,'afam_sites.geojson'),delete_dsn = TRUE )

afam_universe %>% st_drop_geometry() %>% group_by(source ) %>% tally()


# Visually inspect
ggplot(chicago_acs_community_areas, aes(fill = black_population_share, color = black_population_share)) +
  geom_sf() + scale_fill_viridis() + scale_color_viridis() 

ggplot(chicago_acs_community_areas, aes(fill = median_household_income, color = median_household_income)) +
  geom_sf() + scale_fill_viridis() + scale_color_viridis() 

ggplot(chicago_acs_community_areas, aes(fill = log(black_median_household_income), color = log(black_median_household_income) )) +
  geom_sf() + scale_fill_viridis() + scale_color_viridis() 

# Majority Black Community Areas
ggplot(chicago_acs_community_areas %>% filter(black_population_share >= .5), aes(fill = black_population_share, color = black_population_share)) +
  geom_sf() + scale_fill_viridis() + scale_color_viridis() 


# Appendix ----------------------------------------------------------------


chi_bbox <- st_bbox(community_areas) 
chi_bbox_crop <- st_bbox(c(xmin = -87.862226, 
                           xmax = chi_bbox[[3]], 
                           ymax = chi_bbox[[4]], 
                           ymin = chi_bbox[[2]]), crs = st_crs(4326))
community_areas_mod <- st_crop(community_areas, y = chi_bbox_crop) 
  

(p <- ggplot( ) +
  geom_sf(data =st_buffer(st_union(community_areas_mod), joinStyle = "ROUND", dist = .001, endCapStyle="ROUND"), fill = 'white', color = alpha('#333333', 1), size =.5) +
  geom_sf(data = community_areas_mod %>% filter(community %in% community_list), color = '#333333', fill='#333333', size = 1) + #alpha = .06, 
  geom_sf(data = afam_universe %>% filter(source != 'infogroup'), size =.00000000001, 
          color = alpha('white', .5), fill = alpha('#333333', .1)) + 
  ggmap::theme_nothing())

ggsave(plot = p, filename = '/Users/nm/Desktop/b_map.png', device = 'png', 
       width = 6, height = 9, dpi = 500)

