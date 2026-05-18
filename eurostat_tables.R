# Loading used libraries
library(tidyverse)
library(kableExtra)
library(eurostat)

# Before running the code make sure that the working directory is the same as this R script
# OpenMusE tables
## Getting the filtered data for OpenMusE tables
yf <- get_eurostat("sbs_ovw_act",
                   filters = list(nace_r2 = c("C18", "C322", "G4763", "J59", "J60","R90"),
                                  indic_sbs = c("ENT_NR", "SAL_NR", "NETTUR_MEUR",
                                                "AV_MEUR", "AV_SAL_TEUR","EMP_ENT_NR")))
## Calculating max value
max_value <- yf %>%
  label_eurostat() %>%
  group_by(indic_sbs, geo, time) %>%
  summarise(max = sum(values, na.rm = TRUE))

## Calculating min value 
min_value <- yf %>%
  filter(nace_r2 == "C322") %>%
  label_eurostat() %>%
  group_by(indic_sbs, geo, time) %>%
  summarise(min = values)

## Combining the values
combined_values <- left_join(min_value, max_value) %>%
  mutate(year = lubridate::year(time)) %>%
  select(-time) %>%
  rename(country = geo, indicator = indic_sbs)

## Getting specific datasets for year 2022 
wanted_years <- c(2022) # You can change the year or add additional years
### Employees - number
emp_num <- combined_values %>% 
  filter(indicator %in% c("Employees - number"),
         year %in% wanted_years) 
emp_num %>%
  kableExtra::kable()

### Enterprises - number
ent_num <- combined_values %>% 
  filter(indicator %in% c("Enterprises - number"),
         year %in% wanted_years)
ent_num %>%
  kableExtra::kable()

### Net turnover - million euro
net_turn <- combined_values %>% 
  filter(indicator %in% c("Net turnover - million euro"),
         year %in% wanted_years)
net_turn %>%
  kableExtra::kable()

### Persons employed per enterprise - number
pers_emp <- combined_values %>% 
  filter(indicator %in% c("Persons employed per enterprise - number"),
         year %in% wanted_years)
pers_emp %>%
  kableExtra::kable()

### Value added - million euro
val_add <- combined_values %>% 
  filter(indicator %in% c("Value added - million euro"),
         year %in% wanted_years)
val_add %>%
  kableExtra::kable()

### Value added per employee - thousand euro
val_add_emp <- combined_values %>% 
  filter(indicator %in% c("Value added per employee - thousand euro"),
         year %in% wanted_years)
val_add_emp %>%
  kableExtra::kable()

## Saving the tables as csv files
### Creating the directory for the files
if (!file.exists("tables")) {
  dir.create("tables")
  dir.create("tables/openmuse")
  dir.create("tables/cicerone")
} 
### Employees - number
write.csv(emp_num, file = "tables/openmuse/employees_number", row.names = FALSE)

### Enterprises - number
write.csv(ent_num, file = "tables/openmuse/enterprises_number", row.names = FALSE)

### Net turnover - million euro
write.csv(net_turn, file = "tables/openmuse/net_turnover", row.names = FALSE)

### Persons employed per enterprise - number
write.csv(pers_emp, file = "tables/openmuse/persons_employed_per_enterprise", row.names = FALSE)

### Value added - million euro
write.csv(val_add, file = "tables/openmuse/value_added", row.names = FALSE)

### Value added per employee - thousand euro
write.csv(val_add_emp, file = "tables/openmuse/value_added_per_employee", row.names = FALSE)


# Cicerone tables
## Getting the filtered data for Cicerone tables
yf_ci <- get_eurostat("sbs_ovw_act",
                   filters = list(nace_r2 = c("C18", "C322", "G4763", "J5811", "J5814", "J59",
                                              "J60", "J6010", "J6020","R90", "R9001", "R9002",
                                              "R9003"),
                                  indic_sbs = c("ENT_NR", "SAL_NR", "NETTUR_MEUR",
                                                "AV_MEUR", "AV_SAL_TEUR","EMP_ENT_NR")))
## Calculating max value
max_value_ci <- yf_ci %>%
  filter(nace_r2 %in% c("C18", "G4763", "J5811", "J5814", "J59", "J60", "R90",
                        "J9001", "J9002")) %>%
  label_eurostat() %>%
  group_by(indic_sbs, geo, time) %>%
  summarise(max = sum(values, na.rm = TRUE))

## Calculating min value 
min_value_ci <- yf_ci %>%
  filter(nace_r2 %in% c("C18", "G4763", "J59", "R90")) %>%
  label_eurostat() %>%
  group_by(indic_sbs, geo, time) %>%
  summarise(min = sum(values, na.rm = TRUE))

## Combining the values
combined_values_ci <- left_join(min_value_ci, max_value_ci) %>%
  mutate(year = lubridate::year(time)) %>%
  select(-time) %>%
  rename(country = geo, indicator = indic_sbs)

## Getting specific datasets for year 2022 
wanted_years <- c(2022) # You can change the year or add additional years
### Employees - number
emp_num_ci <- combined_values_ci %>% 
  filter(indicator %in% c("Employees - number"),
         year %in% wanted_years) 
emp_num_ci %>%
  kableExtra::kable()

### Enterprises - number
ent_num_ci <- combined_values_ci %>% 
  filter(indicator %in% c("Enterprises - number"),
         year %in% wanted_years)
ent_num_ci %>%
  kableExtra::kable()

### Net turnover - million euro
net_turn_ci <- combined_values_ci %>% 
  filter(indicator %in% c("Net turnover - million euro"),
         year %in% wanted_years)
net_turn_ci %>%
  kableExtra::kable()

### Persons employed per enterprise - number
pers_emp_ci <- combined_values_ci %>% 
  filter(indicator %in% c("Persons employed per enterprise - number"),
         year %in% wanted_years)
pers_emp_ci %>%
  kableExtra::kable()

### Value added - million euro
val_add_ci<- combined_values_ci %>% 
  filter(indicator %in% c("Value added - million euro"),
         year %in% wanted_years)
val_add_ci %>%
  kableExtra::kable()

### Value added per employee - thousand euro
val_add_emp_ci <- combined_values_ci %>% 
  filter(indicator %in% c("Value added per employee - thousand euro"),
         year %in% wanted_years)
val_add_emp_ci %>%
  kableExtra::kable()

## Saving the tables as csv files
### Employees - number
write.csv(emp_num_ci, file = "tables/cicerone/employees_number")

### Enterprises - number
write.csv(ent_num_ci, file = "tables/cicerone/enterprises_number")

### Net turnover - million euro
write.csv(net_turn_ci, file = "tables/cicerone/net_turnover")

### Persons employed per enterprise - number
write.csv(pers_emp_ci, file = "tables/cicerone/persons_employed_per_enterprise")

### Value added - million euro
write.csv(val_add_ci, file = "tables/cicerone/value_added")

### Value added per employee - thousand euro
write.csv(val_add_emp_ci, file = "tables/cicerone/value_added_per_employee")


# Weights calculations
## Getting values from the last four years
y <- get_eurostat("cult_emp_art",
                  filters = list(lastTimePeriod = 4)) 
## Labeling dataset
y <- label_eurostat(y) %>%
  mutate(year = as.character(lubridate::year(time)))
## Reading the data from excel file (Make sure that the file is on the same directory 
## or change the file path accordingly)
music_employment_data <- read_tsv("music_employment.csv")
## Combining data
weight_data <- music_employment_data %>% 
  pivot_longer(!Country, names_to = "year", values_to = "employment") %>%
  left_join(y, by = join_by(Country == geo, year)) %>%
  select(-c(freq, unit, time)) %>%
  filter(!Country == "EU")
## Calculating weights
weight_table <- weight_data %>%
  group_by(Country, year) %>%
  summarise(weight = (employment * 100) / values) %>%
  pivot_wider(names_from = year, values_from = weight) 
weight_table %>%
  kableExtra::kable()

# Saving the table as a csv file
write.csv(weight_table, file = "tables/weight_table.csv", row.names = FALSE)
