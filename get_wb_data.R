library(wbstats)
library(dplyr)
library(tidyr)
library(stringr)

# Retrieve gender indicators from the World Bank API
wb_cache = wbcache()
countries = wbcountries()
indicators = 
  c('SP.POP.TOTL',                                       #Population
    'SP.DYN.SMAM.FE',          'SP.DYN.SMAM.MA',         #Age at first marriage
    'SL.TLF.CACT.FE.NE.ZS',    'SL.TLF.CACT.MA.NE.ZS',   #Labour force participation
    'SP.DYN.LE00.FE.IN',       'SP.DYN.LE00.MA.IN',      #Life expectancy
    'SL.UEM.TOTL.FE.ZS',       'SL.UEM.TOTL.MA.ZS',      #Unemployment
    'SH.DTH.INJR.1534.FE.ZS',  'SH.DTH.INJR.1534.MA.ZS', #Injury
    'SP.DYN.IMRT.FE.IN',       'SP.DYN.IMRT.MA.IN')      #Infant mortality     
wb_data = wb(indicator = indicators, country = "countries_only", startdate = 2000, enddate = format(Sys.Date(), "%Y"), cache = wb_cache)

# Filter by most recent date
wb_data_latest = wb_data %>%
  group_by(country, indicatorID) %>% 
  filter(date == max(date)) %>% 
  ungroup()

# Reshape so male and female are columns
# If years are different for male and female data, paste together with a slash
population_latest  = filter(wb_data_latest, indicatorID == 'SP.POP.TOTL')
gender_latest = filter(wb_data_latest, indicatorID != 'SP.POP.TOTL')
gender_reshape = gender_latest %>% 
  left_join(select(wb_cache$indicators, indicatorID, indicatorDesc)) %>% 
  mutate(Sex = case_when(
    str_detect(indicator, 'female') ~ "female",
    str_detect(indicator, ' male') ~ "male"
  )) %>% mutate_at(c("indicator", "indicatorDesc"),
                   str_remove_all, " males| females|, female|, male| female| male") %>%
  mutate(indicator = str_replace_all(indicator, "  ", " ")) %>% 
  group_by(country, indicator) %>% 
  mutate(indicator_year = ifelse(n_distinct(date) == 1, date, paste0(date, collapse='/'))) %>% 
  group_by(country) %>%
  filter(n() == length(indicators) - 1) %>% 
  ungroup() %>% 
  select(-indicatorID, -date) %>%
  spread(Sex, value)

# Join gender data, population data and indicator descriptions; add short names for indicators
bubble_data = population_latest %>% 
  select(-indicatorID, -indicator) %>% 
  rename(population_year = date, population = value) %>% 
  right_join(gender_reshape) %>% 
  left_join(select(countries, iso3c, region, income)) %>% 
  mutate_at(c("male", "female"), round, 1) %>% 
  mutate(short_name = 
           recode(indicator,
                  "Mortality rate, infant (per 1,000 live births)" = "Infant mortality rate",
                  "Cause of death, by injury, ages 15-34 (% of relevant age group)" = "Deaths from injury, ages 15-34",
                  "Unemployment (% of labor force) (modeled ILO estimate)" = "Unemployment rate",
                  "Life expectancy at birth (years)" = "Life expectancy",
                  "Labor force participation rate (% of population ages 15+) (national estimate)" = "Labor force participation rate")
  ) %>% 
  arrange(short_name)

# Split off indicator descriptions
descriptions = bubble_data %>% 
  select(short_name, indicatorDesc) %>% 
  distinct()

# Add a column for the dotted parity line - the limits 
# of the parity line for each indicator set the plot limits
bubble_countries  = unique(bubble_data$country)
bubble_indicators = unique(bubble_data$short_name)
axes_lims = matrix(rep(c(0,100), length(bubble_indicators)),
                   ncol=2, byrow=T, dimnames = list(bubble_indicators, c("min", "max")))
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
  select(-indicatorDesc)

saveRDS(bubble_data, 'shiny-app/data/bubble_data.rds')
saveRDS(descriptions, 'shiny-app/data/descriptions.rds')
