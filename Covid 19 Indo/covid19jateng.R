library(httr)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(lubridate)
library(tidyr)
library(zoo)


resp_jateng <- GET("https://data.covid19.go.id/public/api/prov_detail_JAWA_TENGAH.json")
status_code(resp)
headers(resp)

# Analisis data
cov_jateng_raw <- content(resp_jateng, as = "parsed", simplifyVector = TRUE)
length(cov_jateng_raw)
names(cov_jateng_raw)

cov_jateng_raw$kasus_total
cov_jateng_raw$meninggal_persen
cov_jateng_raw$sembuh_persen

cov_jateng <- cov_jateng_raw$list_perkembangan
str(cov_jateng)
head(cov_jateng)


new_cov_jateng <-
  cov_jateng %>%
  select(-contains("DIRAWAT_OR_ISOLASI")) %>%
  select(-starts_with("AKUMULASI")) %>%
  rename(
    kasus_baru = KASUS,
    meninggal = MENINGGAL,
    sembuh = SEMBUH
  ) %>%
  mutate(
    tanggal = as.POSIXct(tanggal / 1000, origin ="1970-01-01"),
    tanggal = as.Date(tanggal)
  )

str(new_cov_jateng)

# Kasus Harian Positif
ggplot(new_cov_jateng, aes(tanggal, kasus_baru)) +
  geom_col(fill = "salmon") +
  labs(
    x = NULL,
    y = "Jumlah Kasus",
    title = "Kasus Harian Positif COVID-19 di Jawa Tengah",
    caption = "Sumber data: covid.19.go.id"
   ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

# 
new_cov_jateng %>%
  ggplot(aes(tanggal, kasus_baru)) +
  geom_col(fill = "firebrick3") +
  scale_x_date(
    breaks = "2 weeks",
    guide = guide_axis(check.overlap = TRUE, n.dodge = 2),
    labels = scales::label_date(format = "%e %b"),
    expand = c(0.005, 0.005)
  ) +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Harian Positif COVID-19 di Jawa Tengah",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum_tw(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

#rata 3 hari
new_cov_jateng%>%
  mutate(
    across(kasus_baru:meninggal, ~ rollmean(.x, k = 3, fill = NA))
  ) %>%
  pivot_longer(
    cols = kasus_baru:meninggal,
    names_to = "status",
    values_to = "rollmean3day"
  ) %>%
  mutate(
    status = factor(status, levels = c("kasus_baru", "sembuh", "meninggal"), labels = c("Positif", "Sembuh", "Meninggal"))
  ) %>%
  ggplot(aes(tanggal, rollmean3day, colour = status)) +
  facet_wrap(~status, ncol = 1, scales = "free_y") +
  geom_line(size = 1.1, show.legend = FALSE) +
  scale_x_date(
    breaks = "7 days",
    guide = guide_axis(check.overlap = TRUE, n.dodge = 2),
    labels = scales::label_date(format = "%e %b"),
    expand = c(0.005, 0.005)
  ) +
  scale_y_continuous(position = "right") +
  scale_colour_manual(
    values = c("firebrick3", "seagreen2", "darkslategray4")
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Kasus Harian COVID-19 di Jawa Tengah Menggunakan Rerata Bergerak 3 hari",
    subtitle = "Jumlah kasus pada sumbu-y tidak dibuat seragam antar panel status kasus",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum_tw(
    base_size = 13,
    plot_title_size = 21,
    strip_text_face = "italic",
    grid = FALSE,
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")


# Kasus Harian Sembuh
ggplot(new_cov_jateng, aes(tanggal, sembuh)) +
  geom_col(fill = "olivedrab2") +
  labs(
    x = NULL,
    y = "Jumlah Kasus",
    title = "Kasus Harian Sembuh Covid-19 di Jawa Tengah",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

# Kasus Harian Meninggal
ggplot(new_cov_jateng, aes(tanggal, meninggal)) +
  geom_col(fill="darkslategray4") +
  labs(
    x = NULL,
    y = "Jumlah Kasus",
    title = "Kasus Harian Meninggal COvid-19 di Jawa Tengah",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")

# Penambahan Kasus Mingguan
cov_jateng_pekanan <- new_cov_jateng %>% 
  count(
    tahun = year(tanggal),
    pekan_ke = week(tanggal),
    wt = kasus_baru,
    name = "jumlah"
  )

glimpse(cov_jateng_pekanan)

# Apakah pekan ini lebih baik dari pekan kemarin?
cov_jateng_pekanan <-
  cov_jateng_pekanan %>%
  mutate(
    jumlah_pekanlalu = dplyr::lag(jumlah,1),
    jumlah_pekanlalu = ifelse(is.na(jumlah_pekanlalu), 0, jumlah_pekanlalu),
    lebih_baik = jumlah < jumlah_pekanlalu
  )
glimpse(cov_jateng_pekanan)

ggplot(cov_jateng_pekanan, aes(pekan_ke, jumlah, fill= lebih_baik)) +
  geom_col(show.legend = FALSE) +
  scale_x_continuous(breaks = 9:40, expand = c(0,0)) +
  scale_fill_manual(values = c("TRUE" = "seagreen3", "FALSE" = "salmon")) +
  labs(
    x = NULL,
    y = "Jumlah kasus",
    title = "Kasus Pekanan Positif COVID-19 di Jawa Tengah",
    subtitle = "Kolom hijau menunjukan penambahan kasus baru lebih sedikit dibandingkan pekan sebelumnya",
    caption = "Sumber data: covid.19.go.id"
  ) +
  theme_ipsum(
    base_size = 13,
    plot_title_size = 21,
    grid = "Y",
    ticks = TRUE
  ) +
  theme(plot.title.position = "plot")


# Hingga saat ini, berapa banyak kasus yang masih aktif?
 cov_jateng_akumulasi <-
   new_cov_jateng %>%
   transmute(
     tanggal,
     akumulasi_aktif = cumsum(kasus_baru) - cumsum(sembuh) - cumsum(meninggal),
     akumulasi_sembuh = cumsum(sembuh),
     akumulasi_meninggal = cumsum(meninggal)
   )
 
 tail(cov_jateng_akumulasi)

 ggplot(data = cov_jateng_akumulasi, aes(x = tanggal, y = akumulasi_aktif)) +
   geom_line()
 
 # Seventh-Day Amplification Factor
 cov_jateng_pekanan_akumulasi <- cov_jateng_akumulasi %>% 
   count(
     tahun = year(tanggal),
     pekan_ke = week(tanggal),
     wt = akumulasi_aktif,
     name = "jumlah"
   )
 
 glimpse(cov_jateng_pekanan_akumulasi)
 
 tail(cov_jateng_pekanan_akumulasi)
 
 cov_jateng_pekanan_akumulasi <-
   cov_jateng_pekanan_akumulasi %>%
   mutate(
     jumlah_pekanlalu = dplyr::lag(jumlah,1),
     jumlah_pekanlalu = ifelse(is.na(jumlah_pekanlalu), 0, jumlah_pekanlalu),
     lebih_baik = jumlah < jumlah_pekanlalu,
     a7 = jumlah / jumlah_pekanlalu,
     
   )
 glimpse(cov_jateng_pekanan_akumulasi)
 
 ggplot(cov_jateng_pekanan_akumulasi, aes(pekan_ke, a7, fill= lebih_baik, label = a7)) +
   geom_col(show.legend = FALSE) +
   scale_x_continuous(breaks = 9:40, expand = c(0,0)) +
   scale_fill_manual(values = c("TRUE" = "seagreen3", "FALSE" = "salmon")) +
   labs(
     x = "Pekan Ke",
     y = "A7",
     title = "Seventh-Day Amplification Factor dari Kasus Terkofirmasi",
     subtitle = "Update : 16/09.20",
     caption = "Sumber data: covid.19.go.id"
   ) +
   theme_ipsum(
     base_size = 12,
     plot_title_size = 21,
     grid = "Y",
     ticks = TRUE
   ) +
   theme(plot.title.position = "plot")
 
 # Transformasi Data
 cov_jateng_akumulasi_pivot <-
   cov_jateng_akumulasi %>%
   gather(
     key = "kategori",
     value = "jumlah",
     -tanggal
   ) %>%
   mutate(
     kategori = sub(pattern = "akumulasi_", replacement = "", kategori)
   )
 glimpse(cov_jateng_akumulasi_pivot)
 
 # Membuat Grafik Komparasi Kasus Aktif, Sembuh, dan Meninggal
 ggplot(cov_jateng_akumulasi_pivot, aes(tanggal, jumlah, colour=(kategori))) +
   geom_line(size=0.9) +
   scale_y_continuous(sec.axis = dup_axis(name = NULL)) +
   scale_colour_manual(
     values = c(
       "aktif" = "salmon",
       "meninggal" = "darkslategray4",
       "sembuh" = "olivedrab2"
     ),
     labels = c("Aktif","Meninggal","Sembuh")
   ) +
   labs(
     x = NULL,
     y = "Jumlah kasus akumulasi",
     colour = NULL,
     title = "Dinamika Kasus COVID-19 di Provinsi Jawa Tengah",
     caption = "Sumber data: covid.19.go.id"
   ) +
   theme_ipsum(
     base_size = 13,
     plot_title_size = 21,
     grid = "Y",
     ticks = TRUE
   ) +
   theme(
     plot.title = element_text(hjust = 0.5),
     legend.position = "top"
   )
 