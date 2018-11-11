#setwd("C:/Users/ooska_000/Desktop/r/bostad")
setwd("D:/dokument/Google Drive/projekt/swe_housing_prices")
library(readxl)
library(tidyverse)
library(Quandl)
library(ggplot2)
library(zoo)
library(pxweb)
library(gdata)

### HOX

# http://dataservice.valueguard.se/ExcelServlet/hoxIndex/index
# https://valueguard.se/beskrivning

download.file("http://dataservice.valueguard.se/ExcelServlet/hoxIndex/index", destfile = "hox.xls")
hox <- read_excel("hox.xls")

hox <- read_excel("HOX_index.xls")
hox_info <- read_excel("HOX_index.xls", sheet = "info")

hox <- hox %>% 
  filter(HOXSWE > 0)

hox$Month <- as.Date(paste0(hox$Month, "01"), format = "%Y%m%d")

### OMRX Mortgage Bond Index (OMRXMORT)
# https://www.quandl.com/data/NASDAQOMX/OMRXMORT-OMRX-Mortgage-Bond-Index-OMRXMORT

OMRXMORT <- as.tibble(Quandl("NASDAQOMX/OMRXMORT", order = "asc", start_date = first(hox$Month)))

### Riksbanken
# https://www.riksbank.se/sv/statistik/sok-rantor--valutakurser/?g2-SECBREPOEFF=on&g9-SEMB2YCACOMB=on&g9-SEMB5YCACOMB=on&g99-EMGVB5Y=on&g151-SEKKIX92=on&from=2005-01-03&to=2018-04-27&f=Month&c=cAverage&s=Comma

rates <- read_excel("rb.xlsx")
rates$Month <- as.Date(rates$Month, format = "%Y%m%d")

### SCB
# http://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__BO__BO0101__BO0101A/LagenhetNyKv16/?rxid=6e700fe7-ea14-4571-ad06-5b716d7f5319

bygge <- read_excel("bygge.xlsx")
bygge$Month <- as.Date(bygge$Month, format = "%Y%m%d")

### SCB api
# Get data from SCB (Statistics Sweden)
scb <- interactive_pxweb(api = "api.scb.se")

# Fetching data from the swedish SCB (Statistics Sweden) pxweb API:
scb2 <- interactive_pxweb(api = "api.scb.se", version = "v1", lang = "sv")

pxweb_test_data <- 
  get_pxweb_data(url = "http://api.scb.se/OV0104/v1/doris/sv/ssd/PR/PR0101/PR0101E/Basbeloppet", 
                 dims = list(ContentsCode = c('PR0101A1'), 
                 Tid = c('*')),
                 clean = FALSE)

test <- 
  get_pxweb_data(url = "http://api.scb.se/OV0104/v1/doris/en/ssd/BO/BO0101/BO0101A/LghReHtypUfAr",
                 dims = list(Region = c('01'),
                             Hustyp = c('FLERBO'),
                             Upplatelseform = c('*'),
                             ContentsCode = c('0000005O'),
                             Tid = c('2005', '2006', '2007', '2008', '2009', '2010', '2011', '2012', '2013', '2014', '2015', '2016')),
                             clean = TRUE)

### Merge

data <- hox %>%
  select(Month, HOXSWE, HOXFLATSTO)

data <- left_join(data, rates, by = "Month")

data <- left_join(data, bygge, by = "Month")

data$byggda_lgh_ts <- NA
data[first(which(!is.na(data$byggda_lgh))):last(which(!is.na(data$byggda_lgh))), ]$byggda_lgh_ts <- na.approx(data$byggda_lgh)

data$startade_lgh_ts <- NA
data[first(which(!is.na(data$startade_lgh))):last(which(!is.na(data$startade_lgh))), ]$startade_lgh_ts <- na.approx(data$startade_lgh)

data %>% ggplot(aes(x = Month)) +
  geom_line(aes(y = HOXFLATSTO, color = "HOXFLATSTO"), size = 1.2) +
  geom_line(aes(y = Repo * 100, color = "Repo")) +
  geom_line(aes(y = BoObl2Y * 100, color = "BoObl2Y")) +
  geom_line(aes(y = BoObl5Y * 100, color = "BoObl5Y")) +
  geom_line(aes(y = EU5Y * 100, color = "EU5Y")) + 
  geom_line(aes(y = EU5Y * 100, color = "EU5Y")) +
  geom_line(aes(y = byggda_lgh_ts / 10, color = "byggda_lgh_ts"))

data %>% ggplot(aes(x = Month)) +
  geom_line(aes(y = HOXFLATSTO, color = "HOXFLATSTO"), size = 1.2) +
  geom_line(aes(y = BoObl2Y * 100, color = "BoObl2Y")) +
  geom_line(aes(y = byggda_lgh_ts / 10, color = "byggda_lgh_ts"))

summary(lm(data = data, HOXFLATSTO ~ Repo + BoObl2Y + BoObl5Y + EU5Y))

summary(lm(data = data, HOXFLATSTO ~ BoObl5Y + byggda_lgh_ts))

