#Preliminaries
cat('\014')
options(scipen = 999, mc.cores = parallel::detectCores(), stringsAsFactors = F)
graphics.off()
rm(list = ls())

#Libraries
library(tidyverse)
library(readxl)
library(glue)
library(stargazer)
library(plm)

#Importing data and cleaning
#========================================================================
#path 1 = home, 2 = office. Remove hash sign to select the option
pth <- "F:/Home Documents/Post-Biannual II/Analysis/"
#pth = "C:/Users/K00008795/Documents/Documents/Research/2019-20 Research/Financial Inclusion/Post-Biannual II/Analysis/"
import_excel <- function(path, file) {
  read_excel(glue("{path}{file}"), skip = 1, na = c("-", " ", "", "NULL")) %>% 
    mutate_at(vars(-c("Remittance-sending country", "Sending country code")), as.numeric, parse_number) %>% 
    pivot_longer(., cols = -c("Remittance-sending country", "Sending country code"), 
                 names_to = "Remittance-receiving country", 
                 values_to = "Remittance Amount") %>% 
    filter(!is.na(`Sending country code`))
  }
rem_dt <- tibble(path = pth,
                 file = c("bilateralremittancematrix2014.xlsx", "bilateralremittancematrix2015Oct2016.xlsx", "bilateralremittancematrix2016_Nov2017.xlsx", "bilateralremittancematrix2017Apr2018.xlsx"))
migr_dt <- tibble(path = pth,
                  file = c("bilateralmigrationmatrix20130.xlsx", "bilateralmigrationmatrix20170_Apr2018.xlsx"))
Rec_countries <- import_excel(pth, rem_dt$file[[4]]) %>% 
  select(c("Remittance-sending country", "Sending country code")) %>% 
  unique() %>% dplyr::rename("Receiving country code" = "Sending country code",
                             "Remittance-receiving country" = "Remittance-sending country")

rem_dt <- rem_dt %>% 
  mutate(data = map2(.x = path, .y = file, ~import_excel(.x, .y))) %>% 
  mutate(Year = file %>% sub("bilateralremittancematrix", "", .) %>% substr(.,1,4) %>% as.numeric()) 
rem_dt <- rem_dt %>% unnest(data) %>% 
  select(c("Remittance-sending country", "Sending country code", "Remittance-receiving country","Year", "Remittance Amount")) %>% 
  left_join(Rec_countries, "Remittance-receiving country")

migr_dt <- migr_dt %>% 
  mutate(data = map2(.x = path, .y = file, ~import_excel(.x, .y))) %>% 
  mutate(Year = file %>% sub("bilateralmigrationmatrix", "", .) %>% substr(.,1,4) %>% as.numeric(),
         Year = if_else(Year == 2013,2014,Year)) 
migr_dt <- migr_dt %>% unnest(data) %>%
  mutate(`Migrant Stock` = `Remittance Amount`) %>% 
  select(c("Remittance-sending country", "Sending country code", "Remittance-receiving country","Year", "Migrant Stock")) 

dt <- rem_dt %>% left_join(migr_dt, by = c("Remittance-sending country",
                                           "Sending country code",
                                           "Remittance-receiving country",
                                           "Year"))
rm(migr_dt, rem_dt)

#WORLD DEVELOPMENT INDICATORS
#========================================================================
# Downloading and loading data
input <- if (file.exists("WDI_csv.zip")) {
  "WDI_csv.zip"
} else {
  download.file("http://databank.worldbank.org/data/download/WDI_csv.zip", destfile = paste0(add,"/WDI_csv.zip"), mode = "wb")
}
WDI <- read_delim(input, ",") 
WDI <- WDI %>% 
  select(-c("X65", starts_with("1"))) %>% 
  filter(`Indicator Name` %in% c("GDP growth (annual %)", 
                                 "Domestic credit to private sector (% of GDP)",
                                 "Official exchange rate (LCU per US$, period average)",
                                 "Real effective exchange rate index (2010 = 100)",
                                 "Account ownership at a financial institution or with a mobile-money-service provider (% of population ages 15+)",
                                 "Inflation, consumer prices (annual %)",
                                 "Deposit interest rate (%)",
                                 "Population, total",
                                 "GDP (current US$)")) 
WDI <- WDI %>% pivot_longer(cols = starts_with("2"), names_to = "Year", values_to = "Values") %>% 
  mutate(Year = as.numeric(Year, parse_number)) %>% 
  select(-c(`Indicator Code`)) %>% 
  pivot_wider(names_from = `Indicator Name`, values_from = Values)

#MERGING REMITTANCE AND MIGRANT STOCK DATA WITH WDI 
dt <- dt %>% left_join(WDI, by = c("Sending country code" = "Country Code",
                                   "Year" = "Year")) %>% 
  dplyr::rename("Account ownership - Source" = "Account ownership at a financial institution or with a mobile-money-service provider (% of population ages 15+)",
                "Interest rate - Source" = "Deposit interest rate (%)",
                "Credit to GDP - Source" = "Domestic credit to private sector (% of GDP)",
                "GDP (US$) - Source" = "GDP (current US$)", 
                "GDP growth - Source" = "GDP growth (annual %)", 
                "Inflation - Source" = "Inflation, consumer prices (annual %)",
                "Official exchange rate - Source" = "Official exchange rate (LCU per US$, period average)",
                "Population - Source" = "Population, total",
                "REER - Source" = "Real effective exchange rate index (2010 = 100)",
                "Source Name" = "Country Name")
dt <- dt %>% left_join(WDI, by = c("Receiving country code" = "Country Code",
                                   "Year" = "Year")) %>%
  dplyr::rename("Account ownership - Receiving" = "Account ownership at a financial institution or with a mobile-money-service provider (% of population ages 15+)",
                "Interest rate - Receiving" = "Deposit interest rate (%)",
                "Credit to GDP - Receiving" = "Domestic credit to private sector (% of GDP)",
                "GDP (US$) - Receiving" = "GDP (current US$)", 
                "GDP growth - Receiving" = "GDP growth (annual %)", 
                "Inflation - Receiving" = "Inflation, consumer prices (annual %)",
                "Official exchange rate - Receiving" = "Official exchange rate (LCU per US$, period average)",
                "Population - Receiving" = "Population, total",
                "REER - Receiving" = "Real effective exchange rate index (2010 = 100)",
                "Destination Name" = "Country Name")

#LOADING REMITTANCE FEE DATA
#========================================================================
df <- tibble(fl = c(paste0(pth,"Dataset (up to Q1 2016).csv"), paste0(pth,"Dataset (from Q2 2016).csv"))) %>% 
  mutate(data = fl %>% map(read_csv, skip=0)) %>% unnest(data) %>% select(-c(fl))

df <- df %>% separate(period, c("Year", "Quarter"), sep = "_") %>% 
  mutate(Year = as.numeric(Year, parse_number),
         Acc_pt = `access point`,
         Acc_pt = fct_collapse(Acc_pt, 
                      "Mobile Phone" = c("Mobile phone", "on mobile phone"),
                      "Multiple access Point incl. mobile phone" = c("Agent,Internet,Mobile phone"),
                      "Others" = c("Agent", "Agent,Call Center", "Agent,Call Center,Internet",
                                   "Agent,Internet", "at Branch", "at Branch,Call Center", 
                                   "at Branch,Call Center,On-line", "at Branch,On-line", "ATM Network,Internet", 
                                   "Bank branch", "Bank branch,Call Center", "Bank branch,Call Center,Internet", 
                                   "Bank branch,Internet", "Call Center", "Call Center,Internet", 
                                   "Call Center,Internet,Post Office", "Call Center,On-line",
                                   "Call Center,Post Office", "Internet", "Internet,Post Office", 
                                   "Internet,Post Office branch", "Not available", "On-line", "Post Office", 
                                   "Post Office branch")),
         `Mobile Phone` = if_else(`access point` %in% c("Mobile phone", "on mobile phone"), 1,0),
         `Multiple access Point incl. mobile phone` = if_else(`access point` %in% c("Agent,Internet,Mobile phone"), 1,0)) %>% 
  select(c("Year", "source_code", "source_name", "destination_code", "destination_name", "firm_type", "payment instrument", "access point", "Acc_pt", "Mobile Phone", "Multiple access Point incl. mobile phone", "speed actual", "cc2 lcu amount", "cc2 denomination amount", "cc2 lcu code", "cc2 lcu fee", "cc2 lcu fx rate", "cc2 fx margin", "cc2 total cost %", "inter lcu bank fx", "transparent", "receiving network coverage", "pickup method", "corridor"))
#df <- df %>% pivot_wider(names_from = "Acc_pt", values_from = `access point`)


#Aggregating data to annual series from quarterly
df <- df %>% group_by(Year, source_code, destination_code) %>% 
  summarise(`Firm type` = first(firm_type), 
            `Payment instrument` = first(`payment instrument`),
            `Speed actual` = first(`speed actual`),
            `Amount` = mean(`cc2 lcu amount`, na.rm = TRUE),
            `Denomination amount` = mean(`cc2 denomination amount`, na.rm = TRUE),
            `LCU code` = first(`cc2 lcu code`),
            `Remittance Fee` = mean(`cc2 lcu fee`, na.rm = TRUE),
            `FX rate` = mean(`cc2 lcu fx rate`, na.rm = TRUE),
            `FX margin` = mean(`cc2 fx margin`, na.rm = TRUE),
            `Total Rem. cost %` = mean(`cc2 total cost %`, na.rm = TRUE),
            `Bank FX Rate` = mean(`inter lcu bank fx`, na.rm = TRUE),
            Transparent = first(transparent),
            `Receiving network coverage` = first(`receiving network coverage`),
            `Pickup method` = first(`pickup method`),
            `Corridor` = first(corridor),
            `access point` = first(`access point`),
            Acc_pt = last(Acc_pt),
            `Mobile Phone` = mean(`Mobile Phone`, na.rm = T),
            `Multiple access Point incl. mobile phone` = mean(`Multiple access Point incl. mobile phone`, na.rm = T)) %>% 
  ungroup()

dt <- dt %>% left_join(df, by = c("Sending country code" = "source_code",
                                  "Receiving country code" = "destination_code",
                                  "Year" = "Year")) 

rm(WDI, input, import_excel)

#FILTERING DATA - SUB-SAHARAN AFRICA
SSA <- c('AGO', 'BDI', 'BEN', 'BFA','BWA', 'CAF', 'CIV', 'CMR', 'COD', 'COG', 'COM', 'CPV', 'ERI', 'ETH', 'GAB', 'GHA', 'GIN', 'GMB', 'GNB', 'GNQ', 'KEN', 'LBR', 'LSO', 'MDG', 'MLI', 'MOZ', 'MRT', 'MUS', 'MWI', 'NAM', 'NER', 'NGA', 'RWA', 'SDN', 'SEN', 'SLE', 'SOM', 'SSD', 'STP', 'SWZ', 'SYC', 'TCD', 'TGO', 'TZA', 'UGA', 'ZAF', 'ZMB', 'ZWE')
dt <- dt %>% filter(`Receiving country code` %in% SSA) %>% 
  mutate(`Source of Remittance` = if_else(`Sending country code` %in% SSA, 0, 1),
         `Source of Remittance` = factor(`Source of Remittance`, 
                      levels = c(0,1), 
                      labels = c("Sub-Saharan Africa",
                                 "Rest of the World")))

#SUMMARY STATISTICS
#======================================================================== 
#Missing values
dt %>% 
  purrr::map_df(~class(.)) %>% pivot_longer(cols = dput(names(.)), names_to = "Variables", values_to = "Type") %>%
  right_join(dt %>% purrr::map_df(~sum(is.na(.))) %>% 
               pivot_longer(cols = dput(names(.)), names_to = "Variables", values_to = "Missing") %>% 
               mutate(`Total` = dim(dt)[[1]],
                      `%-age missing` = round(100*Missing/`Total`, digits = 1))) %>% 
  arrange(desc(`%-age missing`)) %>% 
  formattable::formattable(align = c('l','r','r','r', 'r'),
                                                              caption = "Distribution of missing values")
  
#Plots
dt %>% filter(Year == 2017) %>% 
  group_by(`Remittance-receiving country`, `Source of Remittance`) %>% 
  summarise(`Remittance Received` = sum(`Remittance Amount`)) %>% 
  ggplot(aes(x = `Remittance Received`, y = reorder(`Remittance-receiving country`, `Remittance Received`), fill = `Source of Remittance`)) + 
  geom_col() + theme_classic() +
  ylab("Remittance-receiving country") + scale_x_continuous(labels = scales::comma) +
  xlab("Remittances Received (US$ Million)") +
  theme(legend.position = c(0.8, 0.15))
dt %>% filter(Year == 2017) %>% 
  group_by(`Remittance-receiving country`, `Source of Remittance`) %>% 
  summarise(`Remittance Received` = sum(`Remittance Amount`, na.rm = T),
            `GDP (US$) - Receiving` = mean(`GDP (US$) - Receiving`, na.rm = T)) %>% 
  mutate(`Remittance Received` = `Remittance Received`*1000000/`GDP (US$) - Receiving`) %>% 
  filter(!is.na(`Remittance Received`)) %>% 
  ggplot(aes(x = `Remittance Received`, y = reorder(`Remittance-receiving country`, `Remittance Received`), fill = `Source of Remittance`)) + 
  geom_col() + theme_classic() +
  ylab("Remittance-receiving country") + scale_x_continuous(labels = scales::percent) +
  xlab("Remittances Received (% of GDP)") +
  theme(legend.position = c(0.8, 0.15))
dt %>% group_by(`Source of Remittance`, Year) %>% 
  summarise(`Remittance Received` = sum(`Remittance Amount`, na.rm = T)) %>% 
  ggplot(aes(x = Year, y = `Remittance Received`, group = `Source of Remittance`, col = `Source of Remittance`)) + geom_line() + theme_classic() + theme(legend.position = c(0.8,0.5))

dt %>% aggregate(`Mobile Phone`~Year + `Source of Remittance`,.,mean) %>% 
  ggplot(aes(x = Year, y = `Mobile Phone`, group = `Source of Remittance`, color = `Source of Remittance`)) + 
  geom_line() + theme_classic() + theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_y_continuous(labels = scales::percent)

dt %>% group_by(Acc_pt, Year) %>% 
  summarise(`Total Rem. cost %` = mean(`Total Rem. cost %`, na.rm = T)) %>% 
  ggplot(aes(x = Year, y = `Total Rem. cost %`, group = Acc_pt, col = Acc_pt)) + geom_line() + theme_classic() + theme(legend.position = "bottom")

#Remittance Received by SSA Countries
dt %>%  filter(Year == 2017) %>%
  aggregate(`Remittance Amount`~`Remittance-receiving country`+`Source of Remittance`,.,sum) %>%
  pivot_wider(names_from = `Source of Remittance`, values_from = `Remittance Amount`) %>% 
  mutate(Total = `Sub-Saharan Africa`+`Rest of the World`,
         `Main Source` = factor(if_else(`Sub-Saharan Africa` > `Rest of the World`,1,0), levels = c(0,1), labels = c("ROW", "SSA"))) %>% 
  arrange(desc(Total)) %>% 
  formattable::formattable(align = c('l', rep('r',3)), digits=1)

dt %>% ungroup() %>% as.data.frame() %>%
  stargazer::stargazer(., 
                       type = "text",  
                       title="Descriptive statistics", 
                       digits=1, 
                       out = "summ_stats.txt") 

#REGRESSION ANALYSIS
#======================================================================== 
dt <- dt %>% mutate(`Remittance Fee (%)` = `Remittance Fee`/Amount,
                    `FX Premium` = log(`Bank FX Rate`/`FX rate`),
                    ID = factor(paste0(`Sending country code`,`Receiving country code`)))
# n and N
#df %>% arrange(destination_code) %>% mutate(id = ave(1:dim(df)[[1]],destination_code,FUN = seq_along), N = ave(1:dim(df)[[1]], destination_code, FUN = length)) %>% select(destination_code, Year, id, N) %>% formattable::formattable()
#Declaring data as panel
dt <- plm::pdata.frame(dt, index=c("ID", "Year"))
pdim(dt) #checking the panel dimensions
attach(dt)

#Model Specification and Executions
macro <- cbind(`GDP growth - Source`, `GDP growth - Receiving`, `Inflation - Receiving`, `Inflation - Source`, `Interest rate - Receiving`, `Interest rate - Source`)
mod1 <- plm(log(1+`Remittance Amount`) ~ 1 + `Remittance Fee (%)` + `FX margin` + `Account ownership - Source` + log(1+`Migrant Stock`), data = dt, model = "within")
mod2 <- plm(log(1+`Remittance Amount`) ~ 1 + `Remittance Fee (%)` + `FX Premium` + `Account ownership - Source` + log(1+`Migrant Stock`), data = dt, model = "within")
mod3 <- plm(log(1+`Remittance Amount`) ~ 1 + `Total Rem. cost %` + `Account ownership - Source` + log(1+`Migrant Stock`), data = dt, model = "within")
mod4 <- plm(log(1+`Remittance Amount`) ~ 1 + `Remittance Fee (%)` + `FX margin` + `Account ownership - Source` + log(1+`Migrant Stock`) + as.factor(Acc_pt) + macro, data = dt, model = "within")
mod5 <- plm(log(1+`Remittance Amount`) ~ 1 + `Remittance Fee (%)` + `FX Premium` + `Account ownership - Source` + log(1+`Migrant Stock`) + log(1+`Migrant Stock`) + as.factor(Acc_pt) + macro, data = dt, model = "within")
mod6 <- plm(log(1+`Remittance Amount`) ~ 1 + `Total Rem. cost %` + `Account ownership - Source` + log(1+`Migrant Stock`) + as.factor(Acc_pt) + macro, data = dt, model = "within")

detach(dt)
stargazer::stargazer(mod1, mod2, mod3, type = "text", title = "Table 1: Regression Results")
stargazer::stargazer(mod4, mod5, mod6, type = "text", title = "Table 2: Regression Results")

#Robustness tests
