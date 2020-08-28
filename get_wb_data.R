library(wbstats)
library(dplyr)
library(tidyr)
library(jsonlite)
library(stringr)

# Retrieve gender indicators from the World Bank API
cache = wb_cache()
countries = wb_countries()
indicators = 
  c('SP.POP.TOTL',                                       #Population
    'SL.TLF.CACT.FE.NE.ZS',    'SL.TLF.CACT.MA.NE.ZS',   #Labour force participation
    'SP.DYN.LE00.FE.IN',       'SP.DYN.LE00.MA.IN',      #Life expectancy
    'SL.UEM.TOTL.FE.ZS',       'SL.UEM.TOTL.MA.ZS',      #Unemployment
    'SP.DYN.IMRT.FE.IN',       'SP.DYN.IMRT.MA.IN')      #Infant mortality     
gender_data = wb_data(indicator = indicators, country = "countries_only",
                      start_date = 2000, end_date = format(Sys.Date(), "%Y"),
                      return_wide = FALSE, cache = cache)

# Filter by most recent date
wb_data_latest = gender_data %>%
  select(-footnote) %>% 
  filter(complete.cases(value)) %>% 
  group_by(country, indicator_id) %>% 
  filter(date == max(date)) %>% 
  ungroup()

# workaround for indicators missing iso codes
country_names = unique(wb_data_latest$country)
indicators_pt2 = 
  c('SP.DYN.SMAM.FE',          'SP.DYN.SMAM.MA',         #Age at first marriage
    'SH.DTH.INJR.1534.FE.ZS',  'SH.DTH.INJR.1534.MA.ZS') #Injury
url_body = 'https://api.worldbank.org/v2/en/country/all/indicator/'
gender_data_pt2 = NULL
for (indicator in indicators_pt2){
  gender_data_pt2 =
    fromJSON(paste0(url_body, indicator, '?format=json&per_page=20000')) %>% 
    as.data.frame() %>% 
    filter(complete.cases(value)
           & country$value %in% country_names
           & date >= 2000) %>%
    group_by(country) %>% 
    filter(date == max(date)) %>% 
    ungroup() %>% 
    bind_rows(gender_data_pt2)
}
gender_data_pt2 = gender_data_pt2 %>% 
  mutate(indicator_id = indicator$id,
         indicator = indicator$value,
         iso3c = country$id,
         country = country$value) %>% 
  select(indicator_id, indicator, country, iso3c, date, value)
wb_data_latest$date = as.character(wb_data_latest$date)
wb_data_latest = bind_rows(wb_data_latest, gender_data_pt2)

# Reshape so male and female are columns
# If years are different for male and female data, paste together with a slash
population_latest  = wb_data_latest %>% 
  filter(indicator_id == 'SP.POP.TOTL') %>% 
  select(iso3c, date, value)
gender_latest = filter(wb_data_latest, indicator_id != 'SP.POP.TOTL')
gender_reshape = gender_latest %>% 
  left_join(select(cache$indicators, indicator_id, indicator_desc)) %>% 
  mutate(Sex = case_when(
    str_detect(indicator, 'female') ~ "female",
    str_detect(indicator, ' male') ~ "male"
  )) %>% mutate_at(c("indicator", "indicator_desc"),
                   str_remove_all, " males| females|, female|, male| female| male") %>%
  mutate(indicator = str_replace_all(indicator, "  ", " ")) %>% 
  group_by(country, indicator) %>% 
  mutate(indicator_year
          = ifelse(n_distinct(date) == 1, date, paste0(date, collapse='/'))) %>% 
  group_by(country) %>%
  filter(n() == length(indicators) + length(indicators_pt2) - 1) %>% 
  ungroup() %>% 
  select(-indicator_id, -date) %>%
  spread(Sex, value)

# Join gender data, population data and indicator descriptions;
# add short names for indicators
bubble_data = population_latest %>% 
  rename(population_year = date, population = value) %>% 
  right_join(gender_reshape) %>% 
  left_join(select(countries, iso3c, region, income_level)) %>% 
  mutate_at(c("male", "female"), round, 1) %>% 
  mutate(short_name = 
           recode(indicator,
                  "Mortality rate, infant (per 1,000 live births)"
                    = "Infant mortality rate",
                  "Cause of death, by injury, ages 15-34 (% of relevant age group)"
                    = "Deaths from injury, ages 15-34",
                  "Unemployment (% of labor force) (modeled ILO estimate)"
                    = "Unemployment rate",
                  "Life expectancy at birth (years)"
                    = "Life expectancy",
                  "Labor force participation rate (% of population ages 15+) (national estimate)"
                    = "Labor force participation rate")
  ) %>% 
  arrange(short_name)

# Split off indicator descriptions
descriptions = bubble_data %>% 
  select(short_name, indicator_desc) %>% 
  distinct()

# Add a column for the dotted parity line - the limits 
# of the parity line for each indicator set the plot limits
bubble_countries  = unique(bubble_data$country)
bubble_indicators = unique(bubble_data$short_name)
axes_lims = matrix(rep(c(0,100), length(bubble_indicators)),
                   ncol=2, byrow=T, dimnames = list(bubble_indicators,
                                                    c("min", "max")))
axes_lims["Age at first marriage", ] = c(16, 36)
axes_lims["Infant mortality rate", 2] = 90
axes_lims["Life expectancy", ] = c(50, 90)
axes_lims["Unemployment rate", 2] = 40
line_column = NULL
for(indicator in bubble_indicators){
  line_column = c(line_column, 
                  seq(axes_lims[indicator, 1], axes_lims[indicator, 2],
                      length.out = length(bubble_countries)))
}
bubble_data = bubble_data %>% 
  mutate(parity_line = line_column) %>% 
  select(-indicator_desc)

saveRDS(bubble_data, 'data/bubble_data.rds')
saveRDS(descriptions, 'data/descriptions.rds')
