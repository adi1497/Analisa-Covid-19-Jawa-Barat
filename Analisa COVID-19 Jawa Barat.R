# Load Packages
library(ggplot2)
library(httr)
library(dplyr)
library(hrbrthemes)
library(lubridate)

set_config(config(ssl_verifypeer = 0L))
resp_jabar <- GET("https://storage.googleapis.com/dqlab-dataset/prov_detail_JAWA_BARAT.json")
cov_jabar_raw <- content(resp_jabar, as = "parsed", simplifyVector = TRUE)

names(cov_jabar_raw)
cov_jabar_raw$kasus_total
cov_jabar_raw$meninggal_persen
cov_jabar_raw$sembuh_persen

cov_jabar <- cov_jabar_raw$list_perkembangan
str(cov_jabar)
head(cov_jabar)

new_cov_jabar <-
  cov_jabar %>% 
  select(-contains("DIRAWAT_OR_ISOLASI")) %>% 
  select(-starts_with("AKUMULASI")) %>% 
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>% 
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin = "1970-01-01"),
    tanggal = as.Date(tanggal)
  )
str(new_cov_jabar)  

ggplot(data = new_cov_jabar, aes(x = tanggal, y = kasus_baru)) + geom_col()

ggplot(new_cov_jabar, aes(tanggal, kasus_baru)) + geom_col(fill = "salmon") + 
  labs(
    x = NULL, 
    y = "Jumlah Kasus", 
    title = "Kasus Harian Positif COVID-19 di Jawa Barat", 
    subtitle = "Terjadi pelonjakan kasus di awal bulan Juli akibat klaster secapa AD Bandung", 
    caption = "Sumber data: covid.19.go.id"
  ) + 
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21, 
    grid = "Y", 
    ticks = TRUE
  ) + 
  theme(plot.title.position = "plot")

ggplot(new_cov_jabar, aes(tanggal, sembuh)) +
  geom_col(fill = "olivedrab2") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Sembuh Dari COVID-19 di Jawa Barat",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

ggplot(new_cov_jabar, aes(tanggal, meninggal)) +
  geom_col(fill = "darkslategray4") +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Meninggal Akibat COVID-19 di Jawa Barat",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13, 
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")