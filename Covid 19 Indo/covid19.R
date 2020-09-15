library(httr)
# Mengakses API
resp <- GET("https://data.covid19.go.id/public/api/update.json")
# Status Kode
status_code(resp)

# Headers API
headers(resp)

# Mengekstrak Isi Respon
cov_id_raw <- content(resp, as = "parsed", simplifyVector = TRUE)
length(cov_id_raw)
names(cov_id_raw)
cov_id_update <- cov_id_raw$update

# Analisis Data
lapply(cov_id_update,names)
# Kapan pembaharuan data penambahan kasus?
cov_id_update$penambahan$tanggal
# Berapa jumlah penambahan kasus sembuh?
cov_id_update$penambahan$jumlah_sembuh
# Berapa jumlah penambahan kasus meninggal?
cov_id_update$penambahan$jumlah_meninggal
# Berapa jumlah total kasus positif hingga saat ini?
cov_id_update$total$jumlah_positif
# Berapa jumlah total kasus meninggal hingga saat ini?
cov_id_update$total$jumlah_meninggal