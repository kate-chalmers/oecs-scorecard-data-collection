# Load libraries
library(RSelenium)
library(rvest)
library(rjson)
library(httr)
library(readxl)
library(tidyverse)
library(janitor)
library(countrycode)
library(comtradr)
library(curl)
library(RCurl)

# ------------ Before running ----------------
# 1. Ensure working directory is same as project directory
# 2. Validate the port in rsDriver() is viable (not already in use)
# 3. Run script
# --------------------------------------

# Country filter lists
clist_avail <- c("Antigua and Barbuda","Dominica","Grenada", "St. Kitts and Nevis", "St. Lucia","St. Vincent and the Grenadines", "Montserrat", "Anguilla") %>% countrycode(., "country.name", "country.name")
clist_iso2c <- countrycode(clist_avail, "country.name", "iso2c")
clist_iso3c <- countrycode(clist_avail, "country.name", "iso3c")

# missing data filter
dat <- readRDS("./data/oecs.long.RDS")
oecs_dat_prior <- dat %>%
  filter(iso3c %in% clist_iso3c) %>%
  select(variable, iso3c, year, value) %>%
  drop_na()

# set time frame for collection
current_year <- as.numeric(format(Sys.Date(), format="%Y")) - 1
end_date <- paste0(current_year, "-12-31")

# open selenium driver
rD <- rsDriver(browser = "firefox",
               chromever = NULL,
               # por = 1015L,
               port = netstat::free_port(), # use this when running manually
               check = TRUE,
               verbose = TRUE,
               extraCapabilities = list("moz:firefoxOptions" = list(args = list('--headless'))))

remDr <- rD[["client"]]

# Pull currency conversion

currency_convert <- WDI::WDI(indicator="PA.NUS.FCRF", country=c("ATG","GRD","DMA","VCT","LCA","KNA", "MSR", "AIA"))

currency_convert <- currency_convert %>%
  group_by(country) %>%
  mutate(PA.NUS.FCRF = zoo::na.locf(PA.NUS.FCRF, fromLast=T)) %>%
  ungroup() %>%
  mutate(country = countrycode(country, "country.name", "country.name")) %>%
  select(country, year, "xr_rate" = PA.NUS.FCRF)

currency_convert_temp <- currency_convert %>%
  filter(country=="Grenada") %>%
  mutate(country = "Montserrat")

currency_convert_temp2 <- currency_convert %>%
  filter(country=="Grenada") %>%
  mutate(country = "Anguilla")

currency_convert_fin <- rbind(currency_convert, currency_convert_temp, currency_convert_temp2)

print("currency finished")

rm(currency_convert, currency_convert_temp, currency_convert_temp2)

# GDP -------------------------------

url <- "https://www.eccb-centralbank.org/statistics/gdp-datas/comparative-report/1"

remDr$navigate(url)

remDr$findElement(using = "id", value = "categories-ids-ngdp_mc")$clickElement()

remDr$findElement(using = "id", value = "start-date")$sendKeysToElement(list("2019-12-31"))
remDr$findElement(using = "id", value = "end-date")$sendKeysToElement(list(end_date))

remDr$findElement(using = "id", value = "statistics-select-all-countries")$clickElement()

remDr$findElements("css", ".form-buttom")[[1]]$clickElement()

# Give the page time to fully load
Sys.sleep(5)

html <- remDr$getPageSource()[[1]]

gdp_dat <- read_html(html) %>% # parse HTML
  html_nodes(".dataTables_scroll") %>%
  html_table(fill = TRUE)

gdp_dat <- gdp_dat %>% as.data.frame()

gdp_dat_tidy <- gdp_dat %>%
  clean_names() %>%
  .[-1,] %>%
  mutate_if(is.numeric, as.character) %>%
  pivot_longer(!c(country, category, units), names_to="year") %>%
  mutate(year = parse_number(year),
         country = countrycode(country, "country.name", "country.name"),
         category = paste0("GDP in market prices (current EC$) (millions)"),
         value = parse_number(value)) %>%
  select(-units) %>%
  drop_na()

print("gdp (not lca) finished")


# St Lucia gdp
url <- "https://www.eccb-centralbank.org/files/Statistics%20Documents/SLU%20Rebased%20GDP_Saint%20Lucia_2007%20to%202023%20(posted%2016%20February%202022).XLSX"

GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")), config = httr::config(ssl_verifypeer = FALSE))
gdp_lca <- readxl::read_excel(tf, 1)

# download.file(url,"./data/st-lucia-gdp.xlsx",
#               method="curl",
#               mode="wb")
#
# gdp_lca <- readxl::read_excel("./data/st-lucia-gdp.xlsx")

# Keep only table 1 (GDP in current prices)
gdp_lca <- gdp_lca[1:82,]
colnames(gdp_lca) <- gdp_lca[7,]
gdp_lca <- gdp_lca[-c(1:7),]

lca_breakdown <- gdp_lca %>%
  mutate_if(is.numeric, as.character) %>%
  pivot_longer(!INDUSTRIES, names_to="year", names_transform = list(year = as.numeric), values_transform = list(value = as.numeric)) %>%
  drop_na() %>%
  filter(INDUSTRIES %in% c("GDP at Market Prices", "Agriculture, Forestry and Fishing", "Fishing"))

gdp_lca <- lca_breakdown %>%
  filter(INDUSTRIES == "GDP at Market Prices") %>%
  mutate(country = "St. Lucia") %>%
  rename("category" = "INDUSTRIES") %>%
  mutate(category = "GDP in market prices (current EC$) (millions)")

ag_fishing_lca <- lca_breakdown %>%
  filter(INDUSTRIES %in% c("Agriculture, Forestry and Fishing", "Fishing")) %>%
  pivot_wider(names_from = "INDUSTRIES") %>%
  rename("ag_tot" = 2, "fishing" = 3) %>%
  mutate(ag_no_fish = ag_tot - fishing) %>%
  select(-ag_tot) %>%
  pivot_longer(!year, names_to = "variable") %>%
  mutate(category = ifelse(variable == "fishing", "Fishing, value added (millions)",  "Agriculture, livestock and forestry value added (millions)"),
         country = "St. Lucia") %>%
  select(-variable) %>%
  arrange(category, year)

print("gdp (lca) finished")


# St Vincent gdp
url <- "https://www.eccb-centralbank.org/files/Statistics%20Documents/Rebased%20GDP_Saint%20Vincent%20and%20the%20Grenadines_2000%20to%202023%20(posted%2016%20February%202022).XLSX"

GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")), config = httr::config(ssl_verifypeer = FALSE))
gdp_vct <- readxl::read_excel(tf, 1)

# download.file(url,"./data/st-vincent-gdp.xlsx",
#               method="curl",
#               mode="wb")
#
# gdp_vct <- readxl::read_excel("./data/st-vincent-gdp.xlsx")

# Keep only table 1 (GDP in current prices)
gdp_vct <- gdp_vct[1:76,]
colnames(gdp_vct) <- gdp_vct[6,]
gdp_vct <- gdp_vct[-c(1:6),]

vct_breakdown <- gdp_vct %>%
  mutate_if(is.numeric, as.character) %>%
  pivot_longer(!Industry, names_to="year", names_transform = list(year = as.numeric), values_transform = list(value = as.numeric)) %>%
  drop_na() %>%
  filter(Industry %in% c("GDP at Market Prices", "Agriculture, forestry and fishing", "Fishing and aquaculture"))

gdp_vct <- vct_breakdown %>%
  filter(Industry == "GDP at Market Prices") %>%
  mutate(country = "St. Vincent & Grenadines") %>%
  rename("category" = "Industry") %>%
  mutate(category = "GDP in market prices (current EC$) (millions)")

ag_fishing_vct <- vct_breakdown %>%
  filter(Industry %in% c("Agriculture, forestry and fishing", "Fishing and aquaculture")) %>%
  pivot_wider(names_from = "Industry") %>%
  rename("ag_tot" = 2, "fishing" = 3) %>%
  mutate(ag_no_fish = ag_tot - fishing) %>%
  select(-ag_tot) %>%
  pivot_longer(!year, names_to = "variable") %>%
  mutate(category = ifelse(variable == "fishing", "Fishing, value added (millions)",  "Agriculture, livestock and forestry value added (millions)"),
         country = "St. Vincent & Grenadines") %>%
  select(-variable) %>%
  arrange(category, year)

gdp_dat_full <- rbind(gdp_dat_tidy, gdp_vct, gdp_lca)

print("gdp (vct) finished")


# Agriculture LCA, VCT
agri_dat_small <- rbind(ag_fishing_vct, ag_fishing_lca)

agri_dat_small <- agri_dat_small %>%
  mutate(value = value*(1/2.7),
         category = ifelse(category == "Fishing, value added (millions)", "Fishing, value added (current US$) (millions)", "Agriculture, Livestock and Forestry, value added (current US$) (millions)"))


# Tourism expenditure ------------------
url2 <- "https://www.eccb-centralbank.org/statistics/tourisms/comparative-report"

remDr$navigate(url2)

remDr$findElement(using = "id", value = "categories-ids-ave")$clickElement()
remDr$findElement(using = "id", value = "categories-ids-ava")$clickElement()

remDr$findElement(using = "xpath", value = "//*/option[@value = 'A']")$clickElement()

remDr$findElement(using = "id", value = "start-date")$sendKeysToElement(list("2019-12-31"))
remDr$findElement(using = "id", value = "end-date")$sendKeysToElement(list(end_date))

remDr$findElement(using = "id", value = "statistics-select-all-countries")$clickElement()

remDr$findElements("css", ".form-buttom")[[1]]$clickElement()

# Give the page time to fully load
Sys.sleep(5)

html <- remDr$getPageSource()[[1]]

tourism_dat <- read_html(html) %>% # parse HTML
  html_nodes(".dataTables_scroll") %>%
  html_table(fill = TRUE)

tourism_dat <- tourism_dat %>% as.data.frame()

tourism_dat_fin <- tourism_dat %>%
  clean_names() %>%
  .[-1,] %>%
  mutate_if(is.numeric, as.character) %>%
  pivot_longer(!c(country, category, units), names_to="year") %>%
  mutate(year = parse_number(year),
         country = countrycode(country, "country.name", "country.name"),
         category = ifelse(category == "Total Visitor Expenditure - EC$m", "International tourism expenditure (US$) (millions)", "Tourism arrivals"),
         value = parse_number(value),
         value = ifelse(!category =="Tourism arrivals", value*(1/2.7), value)) %>%
  select(-units) %>%
  drop_na() %>%
  pivot_wider(names_from="category") %>%
  rename("expend" = 3, "arrival" = 4) %>%
  mutate(expend_arrival = (expend*1e6)/arrival) %>%
  select(-arrival) %>%
  pivot_longer(!c(country, year), names_to="category") %>%
  mutate(category = ifelse(category == "expend", "International tourism, receipts (current US$) (millions)", "International tourism, receipts per arrival (current US$)")) %>%
  arrange(category, country, year)

print("tourism finished")

# Population projections ------------------

url3 <- "https://www.eccb-centralbank.org/statistics/population-datas/comparative-report"

remDr$navigate(url3)

remDr$findElement(using = "xpath", value = "//*/option[@value = 'A']")$clickElement()

remDr$findElement(using = "id", value = "start-date")$sendKeysToElement(list("2019-12-31"))
remDr$findElement(using = "id", value = "end-date")$sendKeysToElement(list(end_date))

remDr$findElement(using = "id", value = "statistics-select-all-countries")$clickElement()

remDr$findElements("css", ".form-buttom")[[1]]$clickElement()

# Give the page time to fully load
Sys.sleep(5)

html <- remDr$getPageSource()[[1]]

pop_dat <- read_html(html) %>% # parse HTML
  html_nodes(".dataTables_scroll") %>%
  html_table(fill = TRUE)

pop_dat <- pop_dat %>% as.data.frame()

pop_dat <- pop_dat %>%
  clean_names() %>%
  .[-1,] %>%
  mutate_if(is.numeric, as.character) %>%
  pivot_longer(!c(country, category, units), names_to="year") %>%
  mutate(year = parse_number(year),
         country = countrycode(country, "country.name", "country.name"),
         category = paste0("Population projection"),
         value = parse_number(value)) %>%
  select(-units) %>%
  drop_na() %>%
  select(country, year, "pop" = "value")

new_dat_pop <- pop_dat %>% filter(year == current_year)

if(nrow(new_dat_pop) < 1) {

  # UN population projections
  # url <- "https://population.un.org/wpp/Download/Files/1_Indicators%20(Standard)/CSV_FILES/WPP2022_TotalPopulationBySex.zip"
  #
  # temp <- tempfile()
  # download.file(url, temp)
  # un_dat <- read.csv(unz(temp, "WPP2022_TotalPopulationBySex.csv"))
  # unlink(temp)
  #
  # un_tidy <- un_dat %>%
  #   filter(Variant == "Medium") %>%
  #   as.data.frame() %>%
  #   filter(Time > 2010) %>%
  #   select(Location, Time, PopTotal) %>%
  #   mutate(country = countrycode(Location, "country.name", "country.name"),
  #          iso2c = countrycode(Location, "country.name", "iso2c"),
  #          PopTotal = PopTotal*1e3) %>%
  #   drop_na() %>%
  #   filter(iso2c %in% clist_iso2c) %>%
  #   select(-Location, -iso2c) %>%
  #   rename("year" = "Time", "pop" = "PopTotal")
  #
  # pop_dat <- un_tidy %>% filter(year > 2020 & year < (current_year + 1))

  pop_dat <- readRDS("./data/population_data.RDS")

}

gdp_per_cap <- gdp_dat_full %>%
  filter(category == "GDP in market prices (current EC$) (millions)") %>%
  merge(., pop_dat, by=c("country", "year")) %>%
  mutate(value = (value * 1e6) / pop,
         category = "GDP per capita (current EC$)") %>%
  select(-pop)

print("gdp pc finished")


# Exports of goods ------------------

url4 <- "https://www.eccb-centralbank.org/statistics/trades/comparative-report"

remDr$navigate(url4)

remDr$findElement(using = "id", value = "categories-ids-tx")$clickElement()

remDr$findElement(using = "xpath", value = "//*/option[@value = 'A']")$clickElement()

remDr$findElement(using = "id", value = "start-date")$sendKeysToElement(list("2019-12-31"))
remDr$findElement(using = "id", value = "end-date")$sendKeysToElement(list(end_date))

remDr$findElement(using = "id", value = "statistics-select-all-countries")$clickElement()

remDr$findElements("css", ".form-buttom")[[1]]$clickElement()

# Give the page time to fully load
Sys.sleep(5)

html <- remDr$getPageSource()[[1]]

goods_dat <- read_html(html) %>%
  html_nodes(".dataTables_scroll") %>%
  html_table(fill = TRUE)

goods_dat <- goods_dat %>% as.data.frame()

goods_dat <- goods_dat %>%
  clean_names() %>%
  .[-1,] %>%
  mutate_if(is.numeric, as.character) %>%
  pivot_longer(!c(country, category, units), names_to="year") %>%
  mutate(year = parse_number(year),
         country = countrycode(country, "country.name", "country.name"),
         category = paste0("Exports of goods (current US$) (millions)"),
         value = parse_number(value),
         value = value*(1/2.7)) %>%
  select(-units) %>%
  drop_na()

print("goods finished")

# Trade in services

url <- "https://www.eccb-centralbank.org/p/external-sector-statistics"

links_avail <- read_html(url) %>%
  html_nodes("a") %>%
  html_attr("href")

link_needed <- sub(".*/files/", "", links_avail[grepl("Trade", links_avail)])

url <- paste0("https://www.eccb-centralbank.org/files/", link_needed)
GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")), config = httr::config(ssl_verifypeer = FALSE))

# download.file(url,"./data/export_services.xlsx",
#               method="curl",
#               mode="wb")

iso2c_list <- c("AI", "DM", "GD", "KN", "AG", "MS", "VC", "LC")

exports_services <- c()

for(iso2c in iso2c_list) {

  temp <- readxl::read_excel(tf, sheet=iso2c)
  temp <- temp[7:11,]

  colnames(temp) <- temp[2,]

  temp_fin <- temp %>%
    clean_names() %>%
    select(contains("exports")) %>%
    .[-c(1:4),] %>%
    mutate(category = "Exports of services (current US$) (millions)") %>%
    pivot_longer(!category, names_to = "year", values_transform = list(value = as.numeric)) %>%
    mutate(year = parse_number(year), year = ifelse(is.na(year), 1, year),
           year = year - 1, year = 2014 + year, value = value * (1/2.7),
           country = countrycode(iso2c, "iso2c", "country.name"))

  exports_services <- rbind(exports_services, temp_fin)

}

print("services finished")


# Public debt ------------------

url5 <- "https://www.eccb-centralbank.org/statistics/debt-datas/comparative-report/2"

remDr$navigate(url5)

remDr$findElement(using = "id", value = "categories-ids-dgdpcg")$clickElement()

remDr$findElement(using = "id", value = "start-date")$sendKeysToElement(list("2019-12-31"))
remDr$findElement(using = "id", value = "end-date")$sendKeysToElement(list(end_date))

remDr$findElement(using = "id", value = "statistics-select-all-countries")$clickElement()

remDr$findElements("css", ".form-buttom")[[1]]$clickElement()

# Give the page time to fully load
Sys.sleep(5)

html <- remDr$getPageSource()[[1]]

debt_dat <- read_html(html) %>%
  html_nodes(".dataTables_scroll") %>%
  html_table(fill = TRUE)

debt_dat <- debt_dat %>% as.data.frame()

debt_dat <- debt_dat %>%
  clean_names() %>%
  .[-1,] %>%
  mutate_if(is.numeric, as.character) %>%
  pivot_longer(!c(country, category, units), names_to="year") %>%
  mutate(year = parse_number(year),
         country = countrycode(country, "country.name", "country.name"),
         category = paste0("Public Sector Debt to GDP (%)"),
         value = parse_number(value)) %>%
  select(-units) %>%
  drop_na()

print("debt finished")


# Agriculture & fishing value added

url6 <- "https://www.eccb-centralbank.org/statistics/gdp-datas/country-report/5"

# No LCA or VCT in database
iso2c <- c("AI", "DM", "GD", "KN", "AG", "MS")

agri_dat_complet <- c()

for(iso2c_input in iso2c) {

  remDr$navigate(url6)

  selected_path <- paste0("//*/option[@value = '", iso2c_input, "']")
  country_name <- countrycode(iso2c_input, "iso2c", "country.name")

  remDr$findElement(using = "xpath", value = selected_path)$clickElement()

  remDr$findElement(using = "id", value = "start-date")$sendKeysToElement(list("2019-12-31"))
  remDr$findElement(using = "id", value = "end-date")$sendKeysToElement(list(end_date))

  remDr$findElements("css", ".form-buttom")[[1]]$clickElement()

  Sys.sleep(5)

  html <- remDr$getPageSource()[[1]]

  agri_dat <- read_html(html) %>%
    html_nodes(".dataTables_scroll") %>%
    html_table(fill = TRUE)

  agri_dat <- agri_dat %>% as.data.frame()

  agri_dat_fin <- agri_dat %>%
    clean_names() %>%
    .[-1,] %>%
    mutate(
      var_1 = trimws(var_1, which="left"),
      var_1 = str_remove(var_1, "' -")) %>%
    filter(var_1 %in% c("Agriculture, Livestock and Forestry", "Fishing")) %>%
    rename("category" = "var_1") %>%
    mutate(country = country_name) %>%
    mutate_if(is.numeric, as.character) %>%
    pivot_longer(!c(country, category), names_to="year") %>%
    mutate(year = parse_number(year),
           country = countrycode(country, "country.name", "country.name"),
           value = parse_number(value),
           value = value*(1/2.7)) %>%
    drop_na() %>%
    merge(., gdp_dat_tidy[c(1,3,4)] %>% rename("gdp" = "value"), by = c("country", "year")) %>%
    mutate(value = (value/100) * (gdp*1e6),
           value = value/1e6,
           category = ifelse(category == "Fishing", "Fishing, value added (current US$) (millions)", "Agriculture, Livestock and Forestry, value added (current US$) (millions)")) %>%
    select(-gdp)

  agri_dat_complet <- rbind(agri_dat_complet, agri_dat_fin)

}

agri_dat_complet <- agri_dat_complet %>% arrange(category, country, year)

scraped_eccb_dat <- rbind(gdp_dat_full, gdp_per_cap, tourism_dat_fin, goods_dat, exports_services, debt_dat, agri_dat_small, agri_dat_complet)

unique(scraped_eccb_dat$category)

scraped_eccb_dat <- scraped_eccb_dat %>% arrange(category, country, year)

eccb_values <- scraped_eccb_dat

print("agri finished")


# Comtradr -------------------------------------------------
# This API can be finicky may have to run twice

comtrade_names <- c("Saint Vincent and the Grenadines", "Saint Kitts and Nevis", "Anguilla",
                    "Montserrat", "Dominica", "Grenada", "Antigua and Barbuda", "Saint Lucia")

un_codes <- c(662,
              28,
              308,
              212,
              500,
              659,
              660,
              670)

past_years <- paste0(c(2010:2018), collapse=",")
future_years <- paste0(c(2019:2030), collapse=",")

regional_exports <- c()
for(i in 1:length(un_codes)) {

  reporter <- un_codes[i]
  partners <-paste0(c(0,un_codes[!un_codes %in% reporter]), collapse = ",")

  url <- paste0("https://comtradeapi.un.org/data/v1/get/C/A/HS?reporterCode=", reporter, "&period=", past_years, "&partnerCode=", partners, "&cmdCode=TOTAL&flowCode=X&customsCode=C01&motCode=1000&aggregateBy=cmdCode&breakdownMode=classic&includeDesc=true")
  dat <- GET(url, config = add_headers('Cache-Control' = 'no-cache', 'Ocp-Apim-Subscription-Key' = '6e39939d67b64504ab45e2610a82a679'))

  if(jsonlite::fromJSON(rawToChar(dat$content))$error == "" & !jsonlite::fromJSON(rawToChar(dat$content))$count == 0) {
    dat_tidy1 <- jsonlite::fromJSON(rawToChar(dat$content)) %>% as.data.frame()
  } else {
    dat_tidy1 <- NULL
  }

  Sys.sleep(30)

  url <- paste0("https://comtradeapi.un.org/data/v1/get/C/A/HS?reporterCode=", reporter, "&period=", future_years, "&partnerCode=", partners, "&cmdCode=TOTAL&flowCode=X&customsCode=C01&motCode=1000&aggregateBy=cmdCode&breakdownMode=classic&includeDesc=true")
  dat <- GET(url, config = add_headers('Cache-Control' = 'no-cache', 'Ocp-Apim-Subscription-Key' = '6e39939d67b64504ab45e2610a82a679'))

  if(jsonlite::fromJSON(rawToChar(dat$content))$error == "" & !jsonlite::fromJSON(rawToChar(dat$content))$count == 0) {
    dat_tidy2 <-  jsonlite::fromJSON(rawToChar(dat$content)) %>% as.data.frame()
  } else {
    dat_tidy2 <- NULL
  }

  regional_exports <- rbind(regional_exports, dat_tidy1, dat_tidy2)

  Sys.sleep(30)

}

regional_exports %>%
  clean_names() %>%
  select(reporter = data_reporter_desc, partner = data_partner_desc, year = data_period, trade_value_usd = data_fobvalue) %>%
  arrange(reporter, year) %>%
  filter(reporter == "Antigua and Barbuda")
  distinct()

regional_tidy <- regional_exports %>%
  clean_names() %>%
  select(reporter = data_reporter_desc, partner = data_partner_desc, year = data_period, trade_value_usd = data_fobvalue) %>%
  arrange(reporter, year) %>%
  filter(!partner == "World") %>%
  group_by(reporter, year) %>%
  mutate(value = sum(trade_value_usd),
         partner = "OECS") %>%
  slice(1) %>%
  ungroup() %>%
  select(-trade_value_usd) %>%
  mutate(country = countrycode(reporter, "country.name", "country.name"),
         category = "Intra-regional exports (current US$) (millions)",
         year = as.numeric(year), value = as.numeric(value)) %>%
  select(country, category, year, value) %>%
  mutate(value = value/1e6)

comtrade_values <- regional_tidy

print("comtrader finished")

# WDI -------------------------------------------------

indic.codes <- c("SH.STA.OWAD.ZS",
                 "SH.MED.BEDS.ZS",
                 "SH.XPD.CHEX.GD.ZS",
                 "EN.FSH.THRD.NO",
                 "ER.FSH.PROD.MT",
                 "IT.NET.USER.ZS",
                 "SE.XPD.TOTL.GD.ZS",
                 "NV.AGR.EMPL.KD",
                 "SL.EMP.TOTL.SP.NE.ZS",
                 "NE.EXP.GNFS.ZS",
                 "BX.KLT.DINV.WD.GD.ZS",
                 "AG.LND.FRST.ZS",
                 "ER.MRN.PTMR.ZS",
                 "SH.DYN.NCOM.ZS",
                 "SI.POV.UMIC",
                 "SE.PRE.ENRR",
                 "SE.TER.ENRR",
                 "SE.PRM.TCAQ.ZS",
                 "SE.SEC.TCAQ.ZS",
                 "SL.UEM.TOTL.NE.ZS",
                 "ST.INT.RCPT.CD",
                 "NV.AGR.TOTL.CD",
                 "SL.UEM.TOTL.FE.NE.ZS",
                 "SL.EMP.TOTL.SP.FE.NE.ZS",
                 "SH.DYN.NMRT",
                 "SH.XPD.OOPC.CH.ZS",
                 "SH.STA.MMRT",
                 "EN.ATM.GHGT.KT.CE",
                 "ST.INT.ARVL")

vals <- WDI::WDI(indicator = indic.codes, country = clist_iso2c, start=2015, end=(current_year + 1)) %>% select(-iso3c)

labs <- lapply(vals, attr, "label")
colnames(vals)[4:ncol(vals)] <- c(as.character(labs[c(4:ncol(vals))]))

wdi_indicators <- vals %>%
  pivot_longer(!c(iso2c, country, year), names_to="indicator",
               values_to="VALUE") %>%
  mutate(iso3c = countrycode(iso2c, origin="iso2c", destination="iso3c"),
         country = countrycode(iso2c, origin="iso2c", destination="country.name")) %>%
  filter(!is.na(iso3c)) %>%
  select(iso3c, indicator, year, VALUE, country) %>%
  arrange(indicator, iso3c, year) %>%
  drop_na()

wdi_values <- wdi_indicators %>%
  select("category"="indicator", year, "value" = "VALUE", country)

print("WDI finished")

# UNODC ------------------

print("UNODC started")

url <- "http://dataunodc.un.org/sites/dataunodc.un.org/files/data_cts_violent_and_sexual_crime.xlsx"

GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")), config = httr::config(ssl_verifypeer = FALSE))
unodc_dat <- readxl::read_excel(tf, 1)

colnames(unodc_dat) <- unodc_dat[2,]
unodc_dat <- unodc_dat %>% .[-c(1:2),]


unodc_tidy <- unodc_dat %>%
  clean_names() %>%
  filter(iso3_code %in% countrycode(clist_iso2c, "iso2c", "iso3c") & category == "Robbery") %>%
  mutate(country = countrycode(country, "country.name", "country.name"),
         category = "Rates of police-recorded offenses (robbery) (per 100,000 population)") %>%
  select(country, year, value, category)

# Intention homicides
url2 <- "http://dataunodc.un.org/sites/dataunodc.un.org/files/homicide_country_download.xlsx"

GET(url, write_disk(tf <- tempfile(fileext = ".xlsx")), config = httr::config(ssl_verifypeer = FALSE))
homicide <- readxl::read_excel(tf, 1)

colnames(homicide) <- homicide[2,]
homicide <- homicide %>% .[-c(1:2),]

homicide_tidy <- homicide %>%
  clean_names() %>%
  filter(unit_of_measurement == "Rate per 100,000 population" & sex == "Total" & age == "Total") %>%
  select(country, year, value) %>%
  mutate(country = countrycode(country, "country.name", "country.name"),
         category = "Intentional homicides (per 100,000 people)") %>%
  filter(country %in% countrycode(clist_avail, "country.name", "country.name"))

unodc_values <- rbind(unodc_tidy, homicide_tidy)

print("UNODC finished")

unodc_values <- c()

# Crime statistics for St. Lucia

lca_crime <- c()
for(yoi in 2021:2030) {

  crime_url <- paste0("https://www.stats.gov.lc/subjects/society/crime/crime-statistics-by-type-and-outcome-", yoi, "/")

  test <- RCurl::url.exists(crime_url)

  if(test == T) {
    tidy_crime <- read_html(crime_url) %>%
      html_nodes("table") %>%
      html_table(fill = TRUE) %>%
      as.data.frame() %>%
      row_to_names(2) %>%
      clean_names() %>%
      select(offences, crimes_reported) %>%
      filter(offences %in% c("Robbery", "Capital Murder", "Non Capital Murder")) %>%
      pivot_wider(names_from="offences", values_from="crimes_reported") %>%
      clean_names() %>%
      mutate_if(is.character, as.numeric) %>%
      mutate(murder = capital_murder + non_capital_murder,
             year = yoi,
             country = "St. Lucia") %>%
      select(robbery, murder, year, country) %>%
      pivot_longer(!c(year, country)) %>%
      mutate(category = ifelse(name == "Robbery", "Rates of police-recorded offenses (robbery) (per 100,000 population)",
                               "Intentional homicides (per 100,000 people)")) %>%
      select(-name)

    tidy_crime <- un_tidy %>%
      filter(country == "St. Lucia") %>%
      merge(., tidy_crime) %>%
      mutate(value = (value/pop)*1e5)

    lca_crime <- rbind(lca_crime, tidy_crime)

  }

}

unodc_values <- rbind(unodc_values, lca_crime)

print("crime finished")


# WHO ---------------------------------------
# Finicky, have to run twice sometimes

# library(rgho)
# obesity <- get_gho_data(dimension = "GHO", code = "NCD_BMI_30A")
# obesity_teen <- get_gho_data(dimension = "GHO", code = "NCD_BMI_PLUS2C")

url_adult <- "https://ghoapi.azureedge.net/api/NCD_BMI_30A"
url_teen <- "https://ghoapi.azureedge.net/api/NCD_BMI_PLUS2C"

# Adult value
obesity <- jsonlite::fromJSON(url_adult)
obesity <- obesity[[2]]

obesity <- obesity %>%
  clean_names() %>%
  filter(spatial_dim_type == "COUNTRY" & dim1=="BTSX") %>%
  select(country = spatial_dim, year = time_dim, value = numeric_value) %>%
  mutate(country = countrycode(country, "iso3c", "country.name"),
         category = "Prevalence of obesity (BMI > 30) (% of adults)",
         value = as.numeric(value)) %>%
  filter(country %in% clist_avail) %>%
  arrange(country, year)

# Teen value
obesity_teen <- jsonlite::fromJSON(url_teen)
obesity_teen <- obesity_teen[[2]]

teen_tidy <- obesity_teen %>%
  clean_names() %>%
  filter(spatial_dim_type == "COUNTRY" & dim1=="BTSX" & dim2 == "YEARS10-19") %>%
  select(country = spatial_dim, year = time_dim, value = numeric_value) %>%
  mutate(country = countrycode(country, "iso3c", "country.name"),
         category = "Prevalence of obesity in children/adolescents aged 10-19 years (% total)",
         value = as.numeric(value)) %>%
  filter(country %in% clist_avail) %>%
  arrange(country, year)

# obesity <- obesity %>%
#   clean_names() %>%
#   filter(sex == "BTSX") %>%
#   select(country, year, "value" = numeric) %>%
#   drop_na() %>%
#   mutate(country = countrycode(country, "iso3c", "country.name"),
#          category = "Prevalence of obesity (BMI > 30) (% of adults)") %>%
#   filter(country %in% clist_avail) %>%
#   arrange(country, year)
#
# teen_tidy <- obesity_teen %>%
#   clean_names() %>%
#   filter(sex == "BTSX") %>%
#   select(country, year, "value" = numeric) %>%
#   drop_na() %>%
#   mutate(country = countrycode(country, "iso3c", "country.name"),
#          category = "Prevalence of obesity in children/adolescents aged 10-19 years (% total)") %>%
#   filter(country %in% clist_avail) %>%
#   arrange(country, year)

who_values <- rbind(obesity, teen_tidy)

print("obesity finished")


# IRENA --------------

url_which <- c()
for(yoi in 2021:2030) {
  for(x in 1:2) {

    df <- data.frame(
      "year" = yoi,
      "exists" =  RCurl::url.exists(paste0("http://pxweb.irena.org/pxweb/en/IRENASTAT/IRENASTAT__Power%20Capacity%20and%20Generation/ELECGEN_", yoi,"_cycle", x, ".px/")),
      "url" = paste0("http://pxweb.irena.org/pxweb/en/IRENASTAT/IRENASTAT__Power%20Capacity%20and%20Generation/ELECGEN_", yoi,"_cycle", x, ".px/")
    )

    url_which <- rbind(url_which, df)

  }
}

url <- url_which %>% filter(exists == T) %>% pull(url)

remDr$navigate(url)

remDr$findElement(using = "xpath", value = "//*/option[@value = 'ATG']")$clickElement()
remDr$findElement(using = "xpath", value = "//*/option[@value = 'AIA']")$clickElement()
remDr$findElement(using = "xpath", value = "//*/option[@value = 'DMA']")$clickElement()
remDr$findElement(using = "xpath", value = "//*/option[@value = 'GRD']")$clickElement()
remDr$findElement(using = "xpath", value = "//*/option[@value = 'MSR']")$clickElement()
remDr$findElement(using = "xpath", value = "//*/option[@value = 'VCT']")$clickElement()
remDr$findElement(using = "xpath", value = "//*/option[@value = 'LCA']")$clickElement()
remDr$findElement(using = "xpath", value = "//*/option[@value = 'KNA']")$clickElement()

remDr$findElement(using = "css", value = "#ctl00_ContentPlaceHolderMain_VariableSelector1_VariableSelector1_VariableSelectorValueSelectRepeater_ctl02_VariableValueSelect_VariableValueSelect_SelectAllButton")$clickElement()

remDr$findElement(using = "css", value = "#ctl00_ContentPlaceHolderMain_VariableSelector1_VariableSelector1_VariableSelectorValueSelectRepeater_ctl03_VariableValueSelect_VariableValueSelect_SelectAllButton")$clickElement()
remDr$findElement(using = "css", value = "#ctl00_ContentPlaceHolderMain_VariableSelector1_VariableSelector1_VariableSelectorValueSelectRepeater_ctl04_VariableValueSelect_VariableValueSelect_SelectAllButton")$clickElement()

remDr$findElement(using = "css", value = "#ctl00_ContentPlaceHolderMain_VariableSelector1_VariableSelector1_ButtonViewTable")$clickElement()

remDr$findElement(using = "css", value = "#ctl00_ctl00_ContentPlaceHolderMain_cphSettings_rblZeroOption_4")$clickElement()

remDr$findElement(using = "css", value = "#ctl00_ctl00_ContentPlaceHolderMain_CommandBar1_CommandBar1_ShortcutFileFileTypePX")$clickElement()

df <- file.info(list.files("~/Downloads", full.names = T))
irena_file <- rownames(df)[which.max(df$mtime)]

dat <- as.data.frame(pxR::read.px(irena_file))

unique(dat$Technology)

irena_tidy <- dat %>%
  clean_names() %>%
  mutate_if(is.factor, as.character) %>%
  mutate(year = as.numeric(year),
         value = ifelse(is.na(value), 0, value),
         value = as.numeric(value)) %>%
  mutate(renewable = ifelse(technology %in% c("Fossil fuels", "Nuclear", "Other non-renewable energy", "Coal and peat",
                                              "Natural gas", "Fossil fuels n.e.s.", "Oil"), "no", "yes")) %>%
  group_by(country_area, year) %>%
  mutate(tot = sum(value),
         renewable_tot = sum(value[renewable == "yes"]),
         value = (renewable_tot/tot)*100) %>%
  slice(1) %>%
  ungroup() %>%
  select(country_area, year, value) %>%
  mutate(country = countrycode(country_area, "country.name", "country.name", custom_match = c("Kosovo*" = "Kosovo"))) %>%
  select(-country_area) %>%
  mutate(category = "Renewable electricity output (% of total electricity output)")

irena_values <- irena_tidy

print("renewable finished")


# -----------------------

# Combine collects dfs
final_values <- rbind(eccb_values, comtrade_values, unodc_values, who_values, irena_values) %>%
  mutate(country = countrycode(country, "country.name", "country.name"),
         value = as.numeric(value))

final_values$category[final_values$category == "Intra-regional exports (current US$) (millions)"] <- "Intra-regional exports of goods (current US$) (millions)"

new_dat_priors <- oecs_dat_prior %>%
  mutate(country = countrycode(iso3c, "iso3c", "country.name"),
         value = as.numeric(value)) %>%
  select(-iso3c) %>%
  rename(category = variable) %>%
  anti_join(final_values, ., by = c("category", "country", "year")) %>%
  filter(year < 2020, year >=2000)

# Give preference to primary databases rather than WDI
wdi_values <- anti_join(wdi_values, final_values) %>%
  rbind(., wdi_values)

final_values_cleaned <- final_values %>%
  filter(year > 2020 & year < (current_year + 1)) %>%
  pivot_wider(names_from="year") %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c")) %>%
  select("variable" = "category", iso3c, starts_with("20"))

final_values_cleaned$variable[final_values_cleaned$variable == "Intra-regional exports (current US$) (millions)"] <- "Intra-regional exports of goods (current US$) (millions)"

write_csv(final_values_cleaned, paste0("data/", format(Sys.Date(), "%B_%d_%Y"), ".csv"))
write_csv(final_values_cleaned, paste0("data/", "updated_data", ".csv"))
write_csv(new_dat_priors, paste0("data/", "new_prior_dat", ".csv"))



