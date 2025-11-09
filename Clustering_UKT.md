---
Nama: Biyan Daniswara
NRP: '3323600042'
Kelas: S.Tr. SDT B
Title: Clustering UKT
output: 
  md_document:
    variant: gfm
    preserve_yaml: true
---

``` r
library(dbscan)
```

    ## 
    ## Attaching package: 'dbscan'

    ## The following object is masked from 'package:stats':
    ## 
    ##     as.dendrogram

``` r
library(factoextra)
```

    ## Loading required package: ggplot2

    ## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa

``` r
library(cluster)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyr)
library(stringr)
library(purrr)
library(Matrix)
```

    ## 
    ## Attaching package: 'Matrix'

    ## The following objects are masked from 'package:tidyr':
    ## 
    ##     expand, pack, unpack

``` r
library(readxl)
```

``` r
# 1. Load data
df_raw <- read_excel("E:/Semester 5/Workshop Analitika Data Terapan/Data Asli simandiri 2024.xlsx")          
df <- df_raw
df
```

    ## # A tibble: 340 × 26
    ##    NOMOR TAHUN  UMMB MAHASISWA_JALUR_PENERIMAAN JALUR_PENERIMAAN
    ##    <dbl> <dbl> <dbl>                      <dbl> <chr>           
    ##  1 72497  2024    25                          4 SIMANDIRI       
    ##  2 73378  2024    25                          4 SIMANDIRI       
    ##  3 72161  2024    25                          4 SIMANDIRI       
    ##  4 74162  2024    25                          4 SIMANDIRI       
    ##  5 72572  2024    25                          4 SIMANDIRI       
    ##  6 73473  2024    25                          4 SIMANDIRI       
    ##  7 72134  2024    25                          4 SIMANDIRI       
    ##  8 74062  2024    25                          4 SIMANDIRI       
    ##  9 73909  2024    25                          4 SIMANDIRI       
    ## 10 73952  2024    25                          4 SIMANDIRI       
    ## # ℹ 330 more rows
    ## # ℹ 21 more variables: PROGRAM_STUDI_DITERIMA <chr>, NODAFTAR <chr>,
    ## #   NISN <chr>, NAMA <chr>, PENGHASILAN <dbl>, JUMLAH_TANGGUNGAN <dbl>,
    ## #   KEPEMILIKAN_RUMAH <chr>, JUMLAH_RUMAH <chr>, PUNYA_TANAH <chr>,
    ## #   PUNYA_TAMBAK_SAWAH <chr>, PUNYA_GEDUNG <chr>, PUNYA_APARTEMEN <chr>,
    ## #   JUMLAH_MOTOR <chr>, JUMLAH_MOBIL <chr>, DAYA_LISTRIK <chr>, SPI <dbl>,
    ## #   KOTA <chr>, ASAL_SEKOLAH <chr>, AGAMA <dbl>, KOTA_TEMPAT_TINGGAL <chr>, …

``` r
# 2. Drop kolom non-ekonomi
drop_cols <- c("TAHUN", "UMMB", "MAHASISWA_JALUR_PENERIMAAN",
               "JALUR_PENERIMAAN", "NODAFTAR", "NISN", "KOTA",
               "NAMA", "ASAL_SEKOLAH", "AGAMA")

df <- df %>% dplyr::select(-dplyr::any_of(drop_cols))
df
```

    ## # A tibble: 340 × 16
    ##    NOMOR PROGRAM_STUDI_DITERIMA  PENGHASILAN JUMLAH_TANGGUNGAN KEPEMILIKAN_RUMAH
    ##    <dbl> <chr>                         <dbl>             <dbl> <chr>            
    ##  1 72497 D3-Teknik Telekomunika…     2000000                 2 Sendiri          
    ##  2 73378 D3PSDKU-SM-Teknik Info…     2000000                 5 Kontrak/Sewa     
    ##  3 72161 D3-Teknik Telekomunika…     6020000                 2 Menumpang        
    ##  4 74162 STr-Teknologi Game          5000000                 2 Sendiri          
    ##  5 72572 STrPJJ-Teknik Telekomu…     1500000                 2 Sendiri          
    ##  6 73473 STr-Sistem Pembangkit …     4000000                 1 Kontrak/Sewa     
    ##  7 72134 STr-Teknik Informatika     20000000                 2 Sendiri          
    ##  8 74062 D3PJJ-Teknik Informati…     3500000                 4 Sendiri          
    ##  9 73909 D3-Teknik Elektro Indu…     2500000                 1 Menumpang        
    ## 10 73952 D3-Teknik Telekomunika…     2500000                 3 Menumpang        
    ## # ℹ 330 more rows
    ## # ℹ 11 more variables: JUMLAH_RUMAH <chr>, PUNYA_TANAH <chr>,
    ## #   PUNYA_TAMBAK_SAWAH <chr>, PUNYA_GEDUNG <chr>, PUNYA_APARTEMEN <chr>,
    ## #   JUMLAH_MOTOR <chr>, JUMLAH_MOBIL <chr>, DAYA_LISTRIK <chr>, SPI <dbl>,
    ## #   KOTA_TEMPAT_TINGGAL <chr>, PROPINSI_TEMPAT_TINGGAL <chr>

``` r
# 3. Normalisasi string
df <- df %>% dplyr::mutate(across(where(is.character), ~str_trim(.)))
df
```

    ## # A tibble: 340 × 16
    ##    NOMOR PROGRAM_STUDI_DITERIMA  PENGHASILAN JUMLAH_TANGGUNGAN KEPEMILIKAN_RUMAH
    ##    <dbl> <chr>                         <dbl>             <dbl> <chr>            
    ##  1 72497 D3-Teknik Telekomunika…     2000000                 2 Sendiri          
    ##  2 73378 D3PSDKU-SM-Teknik Info…     2000000                 5 Kontrak/Sewa     
    ##  3 72161 D3-Teknik Telekomunika…     6020000                 2 Menumpang        
    ##  4 74162 STr-Teknologi Game          5000000                 2 Sendiri          
    ##  5 72572 STrPJJ-Teknik Telekomu…     1500000                 2 Sendiri          
    ##  6 73473 STr-Sistem Pembangkit …     4000000                 1 Kontrak/Sewa     
    ##  7 72134 STr-Teknik Informatika     20000000                 2 Sendiri          
    ##  8 74062 D3PJJ-Teknik Informati…     3500000                 4 Sendiri          
    ##  9 73909 D3-Teknik Elektro Indu…     2500000                 1 Menumpang        
    ## 10 73952 D3-Teknik Telekomunika…     2500000                 3 Menumpang        
    ## # ℹ 330 more rows
    ## # ℹ 11 more variables: JUMLAH_RUMAH <chr>, PUNYA_TANAH <chr>,
    ## #   PUNYA_TAMBAK_SAWAH <chr>, PUNYA_GEDUNG <chr>, PUNYA_APARTEMEN <chr>,
    ## #   JUMLAH_MOTOR <chr>, JUMLAH_MOBIL <chr>, DAYA_LISTRIK <chr>, SPI <dbl>,
    ## #   KOTA_TEMPAT_TINGGAL <chr>, PROPINSI_TEMPAT_TINGGAL <chr>

``` r
# 4. Cek missing values
missing_summary <- df %>% 
  dplyr::summarise(across(everything(), ~ sum(is.na(.)))) %>% 
  tidyr::pivot_longer(cols = everything(), names_to = "column", values_to = "missing_count")

# Tambahan: cek khusus 0
zero_penghasilan <- sum(df$PENGHASILAN == 0 | df$PENGHASILAN == "0", na.rm = TRUE)
zero_spi <- sum(df$SPI == 0 | df$SPI == "0", na.rm = TRUE)

missing_summary <- missing_summary %>%
  dplyr::mutate(missing_count = dplyr::case_when(
    column == "PENGHASILAN" ~ missing_count + zero_penghasilan,
    column == "SPI" ~ missing_count + zero_spi,
    TRUE ~ missing_count
  ))
print(missing_summary)
```

    ## # A tibble: 16 × 2
    ##    column                  missing_count
    ##    <chr>                           <int>
    ##  1 NOMOR                               0
    ##  2 PROGRAM_STUDI_DITERIMA              0
    ##  3 PENGHASILAN                         9
    ##  4 JUMLAH_TANGGUNGAN                   2
    ##  5 KEPEMILIKAN_RUMAH                   1
    ##  6 JUMLAH_RUMAH                        1
    ##  7 PUNYA_TANAH                         0
    ##  8 PUNYA_TAMBAK_SAWAH                  0
    ##  9 PUNYA_GEDUNG                        0
    ## 10 PUNYA_APARTEMEN                     0
    ## 11 JUMLAH_MOTOR                        1
    ## 12 JUMLAH_MOBIL                        6
    ## 13 DAYA_LISTRIK                        1
    ## 14 SPI                                61
    ## 15 KOTA_TEMPAT_TINGGAL                 0
    ## 16 PROPINSI_TEMPAT_TINGGAL             0

``` r
# 4b: Mengganti nilai 0 menjadi NA
df <- df %>%
  mutate(
    PENGHASILAN = ifelse(PENGHASILAN == 0, NA, PENGHASILAN),
    SPI = ifelse(SPI == 0, NA, SPI)
  )
```

Penjelasan: Pada kolom SPI, terdapat value 0 yang bukan missing value.
Tetapi value 0 tersebut tetap harus diimputasi, sehingga perlu dijadikan
NA terlebih dahulu agar terdeteksi sebagai missing value.

``` r
# Helper untuk numerik
safe_num <- function(x){
  x2 <- as.character(x)
  x2 <- str_replace_all(x2, ",", "")
  x2 <- str_replace_all(x2, "^\\s+|\\s+$", "")
  suppressWarnings(as.numeric(x2))
}

num_cols_try <- c("PENGHASILAN", "JUMLAH_TANGGUNGAN", "JUMLAH_RUMAH",
                  "JUMLAH_MOTOR", "JUMLAH_MOBIL", "SPI")
for (col in intersect(num_cols_try, names(df))){
  if (!is.numeric(df[[col]])){
    df[[col]] <- safe_num(df[[col]])
  }
}
```

``` r
# 5.a Imputasi PENGHASILAN berdasarkan UMR kota tempat tinggal
if ("KOTA_TEMPAT_TINGGAL" %in% names(df_raw)){
  df$KOTA_TEMPAT_TINGGAL <- df_raw$KOTA_TEMPAT_TINGGAL
}

umr_map <- c(
  "kab sidoarjo" = 4638582,
  "kota surabaya" = 4725479,
  "kab bojonegoro" = 2371016,
  "kab blitar" = 2256050,
  "kota blitar" = 2330000,
  "kab gresik" = 4642031,
  "kota cilegon" = 4815102,
  "kab lamongan" = 2828323
)

df <- df %>% mutate(KOTA_TEMPAT_TINGGAL = str_trim(str_to_lower(KOTA_TEMPAT_TINGGAL)))

umr_map_lower <- str_to_lower(names(umr_map))
names(umr_map_lower) <- umr_map

overall_penghasilan_median <- median(df$PENGHASILAN, na.rm = TRUE)

df <- df %>% dplyr::mutate(PENGHASILAN = dplyr::case_when(
  !is.na(PENGHASILAN) ~ PENGHASILAN,
  is.na(PENGHASILAN) & !is.na(KOTA_TEMPAT_TINGGAL) & KOTA_TEMPAT_TINGGAL %in% names(umr_map) ~ umr_map[KOTA_TEMPAT_TINGGAL],
  TRUE ~ overall_penghasilan_median
))
```

``` r
# 5.b JUMLAH_TANGGUNGAN
if ("JUMLAH_TANGGUNGAN" %in% names(df)){
  df <- df %>% dplyr::mutate(JUMLAH_TANGGUNGAN = ifelse(is.na(JUMLAH_TANGGUNGAN),
    dplyr::case_when(
      PENGHASILAN <= 3000000 ~ 3,
      PENGHASILAN >= 10000000 ~ 2,
      TRUE ~ 1
    ), JUMLAH_TANGGUNGAN))
}
```

Penjelasan: Imputasi dilakukan dengan prinsip semakin sedikit
penghasilan, maka semakin banyak tanggungan. Prinsip ini digunakan demi
menandai data yang memiliki jumlah tanggungan lebih banyak berarti
kurang “mampu”.

``` r
# 5.c KEPEMILIKAN_RUMAH & JUMLAH_RUMAH
if ("JUMLAH_RUMAH" %in% names(df)){
  df <- df %>% dplyr::mutate(JUMLAH_RUMAH = as.numeric(JUMLAH_RUMAH))
}

if (all(c("KEPEMILIKAN_RUMAH","JUMLAH_RUMAH") %in% names(df))){
  df <- df %>% dplyr::mutate(
    KEPEMILIKAN_RUMAH = ifelse(!is.na(JUMLAH_RUMAH) & JUMLAH_RUMAH >= 1, "Sendiri", KEPEMILIKAN_RUMAH),
    JUMLAH_RUMAH = ifelse(is.na(JUMLAH_RUMAH) & !is.na(KEPEMILIKAN_RUMAH) & KEPEMILIKAN_RUMAH=="Sendiri", 1, JUMLAH_RUMAH),
    JUMLAH_RUMAH = ifelse(is.na(JUMLAH_RUMAH), 0, JUMLAH_RUMAH),
    KEPEMILIKAN_RUMAH = ifelse(is.na(KEPEMILIKAN_RUMAH), "Menumpang", KEPEMILIKAN_RUMAH)
  )
}
```

``` r
# 5.d Motor & Mobil
if ("JUMLAH_MOTOR" %in% names(df_raw) && !("JUMLAH_MOTOR" %in% names(df))){
  df$JUMLAH_MOTOR <- df_raw$JUMLAH_MOTOR
}

if ("JUMLAH_MOTOR" %in% names(df)){
  df <- df %>% dplyr::mutate(JUMLAH_MOTOR_raw = as.character(JUMLAH_MOTOR))
  df <- df %>% dplyr::mutate(JUMLAH_MOTOR_num = dplyr::case_when(
    JUMLAH_MOTOR_raw == "diatas 4" ~ 5,
    TRUE ~ suppressWarnings(as.numeric(JUMLAH_MOTOR_raw))
  ))
  df <- df %>% dplyr::mutate(JUMLAH_MOTOR_num = ifelse(is.na(JUMLAH_MOTOR_num) & !is.na(JUMLAH_MOBIL) & JUMLAH_MOBIL>0, 1, JUMLAH_MOTOR_num))
  df <- df %>% dplyr::mutate(JUMLAH_MOTOR_num = ifelse(is.na(JUMLAH_MOTOR_num) & PENGHASILAN <= 2000000, 0, JUMLAH_MOTOR_num))
  df <- df %>% dplyr::mutate(JUMLAH_MOTOR_num = ifelse(is.na(JUMLAH_MOTOR_num), 1, JUMLAH_MOTOR_num))
  df$JUMLAH_MOTOR <- as.integer(df$JUMLAH_MOTOR_num)
  df <- df %>% dplyr::select(-JUMLAH_MOTOR_raw, -JUMLAH_MOTOR_num)
}

if ("JUMLAH_MOBIL" %in% names(df)){
  df <- df %>% dplyr::mutate(JUMLAH_MOBIL = as.numeric(JUMLAH_MOBIL))
  df <- df %>% dplyr::mutate(JUMLAH_MOBIL = ifelse(is.na(JUMLAH_MOBIL) & JUMLAH_MOTOR==0, 0, JUMLAH_MOBIL))
  df <- df %>% dplyr::mutate(JUMLAH_MOBIL = ifelse(is.na(JUMLAH_MOBIL) & (JUMLAH_MOTOR>=3 | PENGHASILAN>10000000), 1, JUMLAH_MOBIL))
  df <- df %>% dplyr::mutate(JUMLAH_MOBIL = ifelse(is.na(JUMLAH_MOBIL), 0, JUMLAH_MOBIL))
}
```

``` r
# 5.e Listrik
if ("DAYA_LISTRIK" %in% names(df)){
  df <- df %>% dplyr::mutate(DAYA_LISTRIK = ifelse(is.na(DAYA_LISTRIK),
    dplyr::case_when(
      PENGHASILAN < 2000000 ~ "450 watt",
      PENGHASILAN <= 5000000 ~ "900 watt",
      PENGHASILAN <= 10000000 ~ "1300 watt",
      TRUE ~ "diatas 1300 watt"
    ), DAYA_LISTRIK))
}
```

``` r
# 5.f SPI
if ("SPI" %in% names(df)){
  df <- df %>% dplyr::mutate(SPI = ifelse(is.na(SPI),
    dplyr::case_when(
      PENGHASILAN < 2000000 ~ 2000000,
      PENGHASILAN <= 5000000 ~ 5000000,
      PENGHASILAN <= 10000000 ~ 10000000,
      PENGHASILAN > 10000000 ~ 40000000
    ), SPI))
}
```

``` r
# 6. Final missing check
final_missing <- df %>%
  dplyr::summarise(across(everything(), ~ sum(is.na(.)))) %>%
  tidyr::pivot_longer(cols = everything(), names_to = "column", values_to = "missing_count")
print(final_missing)
```

    ## # A tibble: 16 × 2
    ##    column                  missing_count
    ##    <chr>                           <int>
    ##  1 NOMOR                               0
    ##  2 PROGRAM_STUDI_DITERIMA              0
    ##  3 PENGHASILAN                         0
    ##  4 JUMLAH_TANGGUNGAN                   0
    ##  5 KEPEMILIKAN_RUMAH                   0
    ##  6 JUMLAH_RUMAH                        0
    ##  7 PUNYA_TANAH                         0
    ##  8 PUNYA_TAMBAK_SAWAH                  0
    ##  9 PUNYA_GEDUNG                        0
    ## 10 PUNYA_APARTEMEN                     0
    ## 11 JUMLAH_MOTOR                        0
    ## 12 JUMLAH_MOBIL                        0
    ## 13 DAYA_LISTRIK                        0
    ## 14 SPI                                 0
    ## 15 KOTA_TEMPAT_TINGGAL                 0
    ## 16 PROPINSI_TEMPAT_TINGGAL             0

``` r
# Cek data missing value setelah imputasi
df %>% filter(NOMOR == 73809)
```

    ## # A tibble: 1 × 16
    ##   NOMOR PROGRAM_STUDI_DITERIMA PENGHASILAN JUMLAH_TANGGUNGAN KEPEMILIKAN_RUMAH
    ##   <dbl> <chr>                        <dbl>             <dbl> <chr>            
    ## 1 73809 D3-Teknik Elektronika      4815102                 6 Sendiri          
    ## # ℹ 11 more variables: JUMLAH_RUMAH <dbl>, PUNYA_TANAH <chr>,
    ## #   PUNYA_TAMBAK_SAWAH <chr>, PUNYA_GEDUNG <chr>, PUNYA_APARTEMEN <chr>,
    ## #   JUMLAH_MOTOR <int>, JUMLAH_MOBIL <dbl>, DAYA_LISTRIK <chr>, SPI <dbl>,
    ## #   KOTA_TEMPAT_TINGGAL <chr>, PROPINSI_TEMPAT_TINGGAL <chr>

Penjelasan: Mengecek data yang sebelumnya memiliki missing value,
sekarang sudah tidak ada missing value.

``` r
# 7. Convert types
to_integer_cols <- intersect(c("JUMLAH_TANGGUNGAN","JUMLAH_RUMAH","JUMLAH_MOTOR","JUMLAH_MOBIL"), names(df))
for (cname in to_integer_cols){
df[[cname]] <- as.integer(round(as.numeric(df[[cname]])))
}
df
```

    ## # A tibble: 340 × 16
    ##    NOMOR PROGRAM_STUDI_DITERIMA  PENGHASILAN JUMLAH_TANGGUNGAN KEPEMILIKAN_RUMAH
    ##    <dbl> <chr>                         <dbl>             <int> <chr>            
    ##  1 72497 D3-Teknik Telekomunika…     2000000                 2 Sendiri          
    ##  2 73378 D3PSDKU-SM-Teknik Info…     2000000                 5 Sendiri          
    ##  3 72161 D3-Teknik Telekomunika…     6020000                 2 Sendiri          
    ##  4 74162 STr-Teknologi Game          5000000                 2 Sendiri          
    ##  5 72572 STrPJJ-Teknik Telekomu…     1500000                 2 Sendiri          
    ##  6 73473 STr-Sistem Pembangkit …     4000000                 1 Sendiri          
    ##  7 72134 STr-Teknik Informatika     20000000                 2 Sendiri          
    ##  8 74062 D3PJJ-Teknik Informati…     3500000                 4 Sendiri          
    ##  9 73909 D3-Teknik Elektro Indu…     2500000                 1 Menumpang        
    ## 10 73952 D3-Teknik Telekomunika…     2500000                 3 Menumpang        
    ## # ℹ 330 more rows
    ## # ℹ 11 more variables: JUMLAH_RUMAH <int>, PUNYA_TANAH <chr>,
    ## #   PUNYA_TAMBAK_SAWAH <chr>, PUNYA_GEDUNG <chr>, PUNYA_APARTEMEN <chr>,
    ## #   JUMLAH_MOTOR <int>, JUMLAH_MOBIL <int>, DAYA_LISTRIK <chr>, SPI <dbl>,
    ## #   KOTA_TEMPAT_TINGGAL <chr>, PROPINSI_TEMPAT_TINGGAL <chr>

``` r
# 8. Encoding punya/tidak punya
bin_cols <- intersect(c("PUNYA_TANAH","PUNYA_TAMBAK_SAWAH","PUNYA_GEDUNG","PUNYA_APARTEMEN"), names(df))
for (cname in bin_cols){
df[[cname]] <- tolower(str_trim(as.character(df[[cname]])))
df[[cname]] <- ifelse(df[[cname]] %in% c("punya", "ya", "1"), 1, 0)
}
```

``` r
# 9. Encoding ordinal columns
# DAYA_LISTRIK: "450 watt"=1, "900 watt"=2, "1300 watt"=3, "diatas 1300 watt"=4
if ("DAYA_LISTRIK" %in% names(df)){
df <- df %>% mutate(DAYA_LISTRIK = tolower(str_trim(DAYA_LISTRIK)))
df <- df %>% mutate(DAYA_LISTRIK_enc = case_when(
DAYA_LISTRIK %in% c("450 watt","450") ~ 1,
DAYA_LISTRIK %in% c("900 watt","900") ~ 2,
DAYA_LISTRIK %in% c("1300 watt","1300") ~ 3,
TRUE ~ 4
))
}

# JUMLAH_MOTOR already numeric with 'diatas 4' -> 5
# KEPEMILIKAN_RUMAH: 'menumpang'=0, 'sewa/kontrak'=1, 'sendiri'=2
if ("KEPEMILIKAN_RUMAH" %in% names(df)){
  df <- df %>% mutate(KEPEMILIKAN_RUMAH = tolower(str_trim(KEPEMILIKAN_RUMAH)))
  df <- df %>% mutate(KEPEMILIKAN_RUMAH_enc = case_when(
    KEPEMILIKAN_RUMAH %in% c("menumpang","tidak punya","tidak") ~ 0,
    KEPEMILIKAN_RUMAH %in% c("sewa","kontrak","sewa/kontrak") ~ 1,
    KEPEMILIKAN_RUMAH %in% c("sendiri","milik sendiri") ~ 2,
    TRUE ~ 0
  ))
}
```

``` r
# 10. Binning rasio -> ordinal
# PENGHASILAN: <2M=1, <=5M=2, <=10M=3, >10M=4
if ("PENGHASILAN" %in% names(df)){
df <- df %>% mutate(PENGHASILAN_bin = case_when(
PENGHASILAN < 2000000 ~ 1,
PENGHASILAN <= 5000000 ~ 2,
PENGHASILAN <= 10000000 ~ 3,
TRUE ~ 4
))
}
# SPI same bins
if ("SPI" %in% names(df)){
df <- df %>% mutate(SPI_bin = case_when(
SPI < 2000000 ~ 1,
SPI <= 5000000 ~ 2,
SPI <= 10000000 ~ 3,
TRUE ~ 4
))
}
# JUMLAH_TANGGUNGAN: >=5=1, 3-4=2, <=2=3 (sebagai ordinal where higher means more 'mampu')
if ("JUMLAH_TANGGUNGAN" %in% names(df)){
df <- df %>% mutate(JUMLAH_TANGGUNGAN_bin = case_when(
JUMLAH_TANGGUNGAN >= 5 ~ 1,
JUMLAH_TANGGUNGAN >= 3 & JUMLAH_TANGGUNGAN <=4 ~ 2,
JUMLAH_TANGGUNGAN <= 2 ~ 3,
TRUE ~ 2
))
}
```

Penjelasan: Semua binning dan encoding dilakukan dengan prinsip semakin
tinggi nilai code dan bin berarti semakin tinggi tingkat ekonominya.

``` r
df
```

    ## # A tibble: 340 × 21
    ##    NOMOR PROGRAM_STUDI_DITERIMA  PENGHASILAN JUMLAH_TANGGUNGAN KEPEMILIKAN_RUMAH
    ##    <dbl> <chr>                         <dbl>             <int> <chr>            
    ##  1 72497 D3-Teknik Telekomunika…     2000000                 2 sendiri          
    ##  2 73378 D3PSDKU-SM-Teknik Info…     2000000                 5 sendiri          
    ##  3 72161 D3-Teknik Telekomunika…     6020000                 2 sendiri          
    ##  4 74162 STr-Teknologi Game          5000000                 2 sendiri          
    ##  5 72572 STrPJJ-Teknik Telekomu…     1500000                 2 sendiri          
    ##  6 73473 STr-Sistem Pembangkit …     4000000                 1 sendiri          
    ##  7 72134 STr-Teknik Informatika     20000000                 2 sendiri          
    ##  8 74062 D3PJJ-Teknik Informati…     3500000                 4 sendiri          
    ##  9 73909 D3-Teknik Elektro Indu…     2500000                 1 menumpang        
    ## 10 73952 D3-Teknik Telekomunika…     2500000                 3 menumpang        
    ## # ℹ 330 more rows
    ## # ℹ 16 more variables: JUMLAH_RUMAH <int>, PUNYA_TANAH <dbl>,
    ## #   PUNYA_TAMBAK_SAWAH <dbl>, PUNYA_GEDUNG <dbl>, PUNYA_APARTEMEN <dbl>,
    ## #   JUMLAH_MOTOR <int>, JUMLAH_MOBIL <int>, DAYA_LISTRIK <chr>, SPI <dbl>,
    ## #   KOTA_TEMPAT_TINGGAL <chr>, PROPINSI_TEMPAT_TINGGAL <chr>,
    ## #   DAYA_LISTRIK_enc <dbl>, KEPEMILIKAN_RUMAH_enc <dbl>, PENGHASILAN_bin <dbl>,
    ## #   SPI_bin <dbl>, JUMLAH_TANGGUNGAN_bin <dbl>

``` r
# Buat dataframe untuk analisis
df_cluster <- df %>%
  dplyr::select(
    PENGHASILAN_bin,
    JUMLAH_TANGGUNGAN_bin,
    JUMLAH_RUMAH,
    JUMLAH_MOTOR,
    JUMLAH_MOBIL,
    SPI_bin,
    PUNYA_TANAH,
    PUNYA_TAMBAK_SAWAH,
    PUNYA_GEDUNG,
    PUNYA_APARTEMEN,
    DAYA_LISTRIK_enc,
    KEPEMILIKAN_RUMAH_enc
  )

# Hapus kolom yang tidak bervariasi (standar deviasi 0) untuk menghindari NaN saat scaling
# Ini penting karena banyak variabel 'PUNYA_*' mungkin memiliki nilai yang sama
non_varying_cols <- df_cluster %>%
  dplyr::select(where(is.numeric)) %>%
  purrr::map_lgl(~sd(., na.rm = TRUE) == 0) %>%
  names(.)[.]

df_cluster <- df_cluster %>% dplyr::select(-dplyr::all_of(non_varying_cols))
df_cluster
```

    ## # A tibble: 340 × 10
    ##    PENGHASILAN_bin JUMLAH_TANGGUNGAN_bin JUMLAH_RUMAH JUMLAH_MOTOR JUMLAH_MOBIL
    ##              <dbl>                 <dbl>        <int>        <int>        <int>
    ##  1               2                     3            1            1            0
    ##  2               2                     1            1            2            0
    ##  3               3                     3            1            2            0
    ##  4               2                     3            1            3            1
    ##  5               1                     3            1            1            0
    ##  6               2                     3            1            1            1
    ##  7               4                     3            1            3            1
    ##  8               2                     2            1            2            1
    ##  9               2                     3            0            1            0
    ## 10               2                     2            0            1            0
    ## # ℹ 330 more rows
    ## # ℹ 5 more variables: SPI_bin <dbl>, PUNYA_TANAH <dbl>,
    ## #   PUNYA_TAMBAK_SAWAH <dbl>, DAYA_LISTRIK_enc <dbl>,
    ## #   KEPEMILIKAN_RUMAH_enc <dbl>

``` r
# 11. Mahalanobis distance
numeric_for_maha <- df_cluster

# Hitung Mahalanobis
cov_mat <- cov(numeric_for_maha)
center <- colMeans(numeric_for_maha)

# Jika covariance singular → perbaiki dengan nearPD
if (det(cov_mat) == 0) {
  cov_mat <- as.matrix(Matrix::nearPD(cov_mat)$mat)
}

maha_dist <- mahalanobis(numeric_for_maha, center, cov_mat)

# Threshold pakai distribusi Chi-square
p <- ncol(numeric_for_maha)  # jumlah variabel
alpha <- 0.01                # tingkat signifikansi 1%
threshold <- qchisq(1 - alpha, df = p)

# Tandai outlier
is_outlier <- maha_dist > threshold

# Hitung persentase outlier
outlier_pct <- mean(is_outlier) * 100

# Output hasil
head(maha_dist)
```

    ## [1]  3.559990 17.798858  6.164111  8.729510  4.057264 14.616511

``` r
outlier_pct
```

    ## [1] 5.882353

Penjelasan: Terdapat 5% outlier pada dataset, karena masih termasuk
wajar maka tidak dilakukan handling.

``` r
# 12. Scaling
scaled_df_cluster <- as.data.frame(scale(df_cluster))
head(scaled_df_cluster)
```

    ##   PENGHASILAN_bin JUMLAH_TANGGUNGAN_bin JUMLAH_RUMAH JUMLAH_MOTOR JUMLAH_MOBIL
    ## 1      -0.2299698             0.9050927    0.2977861   -1.0787956   -0.5689643
    ## 2      -0.2299698            -2.4765723    0.2977861    0.2647594   -0.5689643
    ## 3       0.9198793             0.9050927    0.2977861    0.2647594   -0.5689643
    ## 4      -0.2299698             0.9050927    0.2977861    1.6083144    1.3655144
    ## 5      -1.3798189             0.9050927    0.2977861   -1.0787956   -0.5689643
    ## 6      -0.2299698             0.9050927    0.2977861   -1.0787956    1.3655144
    ##      SPI_bin PUNYA_TANAH PUNYA_TAMBAK_SAWAH DAYA_LISTRIK_enc
    ## 1 -0.9419654  -0.3382552         -0.2290781       -0.3714583
    ## 2 -0.9419654   2.9476525         -0.2290781       -0.3714583
    ## 3 -0.9419654  -0.3382552         -0.2290781       -0.3714583
    ## 4  0.2945915  -0.3382552         -0.2290781       -0.3714583
    ## 5 -0.9419654  -0.3382552         -0.2290781       -0.3714583
    ## 6  0.2945915   2.9476525         -0.2290781        0.8088764
    ##   KEPEMILIKAN_RUMAH_enc
    ## 1             0.3849824
    ## 2             0.3849824
    ## 3             0.3849824
    ## 4             0.3849824
    ## 5             0.3849824
    ## 6             0.3849824

``` r
names(scaled_df_cluster)
```

    ##  [1] "PENGHASILAN_bin"       "JUMLAH_TANGGUNGAN_bin" "JUMLAH_RUMAH"         
    ##  [4] "JUMLAH_MOTOR"          "JUMLAH_MOBIL"          "SPI_bin"              
    ##  [7] "PUNYA_TANAH"           "PUNYA_TAMBAK_SAWAH"    "DAYA_LISTRIK_enc"     
    ## [10] "KEPEMILIKAN_RUMAH_enc"

``` r
# 13. Lakukan PCA pada data yang sudah di-scaling
pca_res <- prcomp(scaled_df_cluster, scale. = FALSE)
summary(pca_res)
```

    ## Importance of components:
    ##                           PC1    PC2    PC3    PC4     PC5     PC6     PC7
    ## Standard deviation     1.6741 1.2910 1.0672 1.0136 0.97246 0.91747 0.76724
    ## Proportion of Variance 0.2803 0.1667 0.1139 0.1027 0.09457 0.08418 0.05887
    ## Cumulative Proportion  0.2803 0.4469 0.5608 0.6635 0.75811 0.84228 0.90115
    ##                            PC8     PC9    PC10
    ## Standard deviation     0.69935 0.64835 0.28116
    ## Proportion of Variance 0.04891 0.04204 0.00791
    ## Cumulative Proportion  0.95006 0.99209 1.00000

``` r
pca_data <- as.matrix(pca_res$x[, 1:5])
```

CLUSTERING - DBSCAN

``` r
library(dbscan)
library(factoextra)
```

``` r
# Coba cari nilai eps pakai k-NN distance plot
kNNdistplot(pca_data, k = 8)
abline(h = 1.5, lty = 2)
```

![](Clustering_UKT_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

``` r
# Jalankan DBSCAN
set.seed(123)
dbscan_res <- dbscan(pca_data, eps = 1.3, minPts = 3)
table(dbscan_res$cluster)
```

    ## 
    ##   0   1   2   3   4   5   6   7   8 
    ##  21 248  18  19  14   5   5   7   3

``` r
# Visualisasi 2D PCA plot
fviz_cluster(dbscan_res, data = pca_data,
             geom = "point",
             stand = FALSE,
             show.clust.cent = FALSE) +
  ggtitle("DBSCAN Clustering Result")
```

![](Clustering_UKT_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

``` r
# Evaluasi DBSCAN
library(cluster) # untuk silhouette

# Ambil cluster selain noise (0)
cluster_labels <- dbscan_res$cluster
valid_idx <- which(cluster_labels != 0)

# --- Silhouette Score ---
sil <- silhouette(cluster_labels[valid_idx], dist(pca_data[valid_idx, ]))
dbscan_silhouette <- mean(sil[, "sil_width"])
cat("Average Silhouette Width:", dbscan_silhouette, "\n")
```

    ## Average Silhouette Width: 0.3690352

``` r
# --- BSS/TSS Ratio ---
# Total Sum of Squares (TSS)
overall_center <- colMeans(pca_data)
TSS <- sum(rowSums((pca_data - overall_center)^2))

# Between Sum of Squares (BSS)
BSS <- 0
for (cl in unique(cluster_labels[cluster_labels != 0])) {
  cl_idx <- which(cluster_labels == cl)
  cl_center <- colMeans(pca_data[cl_idx, , drop = FALSE])
  BSS <- BSS + length(cl_idx) * sum((cl_center - overall_center)^2)
}

BSS_TSS_ratio <- BSS / TSS
cat("BSS/TSS Ratio:", BSS_TSS_ratio, "\n")
```

    ## BSS/TSS Ratio: 0.4437761

Preparing FCM - PCM - FPCM - MFPCM

``` r
library(fclust)
library(inaparc)
library(ppclust)
library(cluster)

# Fungsi untuk menghitung BSS/TSS Ratio
calculate_bss_tss <- function(data, labels) {
  overall_center <- colMeans(data)
  TSS <- sum(rowSums((data - overall_center)^2))
  BSS <- 0
  for (cl in unique(labels)) {
    cl_idx <- which(labels == cl)
    cl_data <- data[cl_idx, , drop = FALSE]
    cl_center <- colMeans(cl_data)
    BSS <- BSS + nrow(cl_data) * sum((cl_center - overall_center)^2)
  }
  return(BSS / TSS)
}

x <- as.matrix(pca_data)
c <- 8
vu <- inaparc::imembrand(nrow(x), k = c)$u
```

Clustering - FCM

``` r
res.fcm <- fcm(x, centers = c, m = 2)

# --- Konversi hasil FCM ke format fclust ---
res.fcm1 <- ppclust2(res.fcm, "fclust")

# --- Label crisp (hard clustering) dari membership matrix ---
fcm_labels <- apply(res.fcm$u, 1, which.max)

# --- Evaluasi silhouette (crisp) ---
if (length(unique(fcm_labels)) > 1) {
  sil_fcm <- silhouette(fcm_labels, dist(x))
  avg_sil_fcm <- mean(sil_fcm[, "sil_width"])
} else {
  avg_sil_fcm <- NA
}

# --- Evaluasi BSS/TSS ---
bss_tss_ratio_fcm <- calculate_bss_tss(x, fcm_labels)

# --- Evaluasi fuzzy silhouette index (pakai fclust) ---
fsi <- SIL.F(res.fcm1$Xca, res.fcm1$U, alpha = 1)

# --- Print hasil ---
cat(sprintf("FCM | Avg. Silhouette: %.4f | BSS/TSS Ratio: %.4f | Fuzzy SI: %.4f\n",
            avg_sil_fcm, bss_tss_ratio_fcm, fsi))
```

    ## FCM | Avg. Silhouette: 0.3367 | BSS/TSS Ratio: 0.7528 | Fuzzy SI: 0.5733

Clustering - PCM

``` r
res.pcm <- pcm(x, centers = c, memberships = vu)
res.pcm1 <- ppclust2(res.pcm, "fclust")

pcm_labels <- apply(res.pcm1$U, 1, which.max)

if (length(unique(pcm_labels)) > 1) {
  sil_pcm <- silhouette(pcm_labels, dist(x))
  avg_sil_pcm <- mean(sil_pcm[, "sil_width"])
  bss_tss_ratio_pcm <- calculate_bss_tss(x, pcm_labels)
  fsi_pcm <- SIL.F(res.pcm1$Xca, res.pcm1$U, alpha = 1)
  
  cat(sprintf("PCM | Avg. Silhouette: %.4f | BSS/TSS Ratio: %.4f | FSI: %.4f\n",
              avg_sil_pcm, bss_tss_ratio_pcm, fsi_pcm))
}
```

    ## PCM | Avg. Silhouette: -0.2353 | BSS/TSS Ratio: 0.0072 | FSI: -0.4623

Clustering - FPCM

``` r
res.fpcm <- fpcm(x, centers = c, memberships = vu)
res.fpcm1 <- ppclust2(res.fpcm, "fclust")
fpcm_labels <- apply(res.fpcm1$U, 1, which.max)
sil_fpcm <- silhouette(fpcm_labels, dist(x))
avg_sil_fpcm <- mean(sil_fpcm[, "sil_width"])
bss_tss_ratio_fpcm <- calculate_bss_tss(x, fpcm_labels)
fsi_fpcm <- SIL.F(res.fpcm1$Xca, res.fpcm1$U, alpha = 1)

cat(sprintf("FPCM | Avg. Silhouette: %.4f | BSS/TSS Ratio: %.4f | FSI: %.4f\n",
            avg_sil_fpcm, bss_tss_ratio_fpcm, fsi_fpcm))
```

    ## FPCM | Avg. Silhouette: 0.2638 | BSS/TSS Ratio: 0.5957 | FSI: 0.5787

Clustering - MFPCM

``` r
res.mfpcm <- mfpcm(x, centers = c, memberships = vu)
res.mfpcm1 <- ppclust2(res.mfpcm, "fclust")
mfpcm_labels <- apply(res.mfpcm1$U, 1, which.max)
sil_mfpcm <- silhouette(mfpcm_labels, dist(x))
avg_sil_mfpcm <- mean(sil_mfpcm[, "sil_width"])
bss_tss_ratio_mfpcm <- calculate_bss_tss(x, mfpcm_labels)
fsi_mfpcm <- SIL.F(res.mfpcm1$Xca, res.mfpcm1$U, alpha = 1)

cat(sprintf("MFPCM | Avg. Silhouette: %.4f | BSS/TSS Ratio: %.4f | FSI: %.4f\n",
            avg_sil_mfpcm, bss_tss_ratio_mfpcm, fsi_mfpcm))
```

    ## MFPCM | Avg. Silhouette: 0.3194 | BSS/TSS Ratio: 0.6954 | FSI: 0.6560

Plot Perbandingan Silhouette

``` r
# --- Library untuk Dumbbell Plot ---
library(ggplot2)
```

``` r
# --- 1. Kumpulkan semua hasil evaluasi ---
# Dapatkan skor dari setiap metode
scores_dbscan <- c(dbscan_silhouette, BSS_TSS_ratio)
scores_fcm <- c(avg_sil_fcm, bss_tss_ratio_fcm)
scores_fpcm <- c(avg_sil_fpcm, bss_tss_ratio_fpcm)
scores_mfpcm <- c(avg_sil_mfpcm, bss_tss_ratio_mfpcm)

# Satukan semua skor ke dalam dataframe yang benar
results_combined <- data.frame(
  Method = rep(c("DBSCAN", "FCM", "FPCM", "MFPCM"), each = 2),
  Metric = rep(c("Silhouette Score", "BSS/TSS Ratio"), 4),
  Score = c(scores_dbscan, scores_fcm, scores_fpcm, scores_mfpcm)
)

print(results_combined)
```

    ##   Method           Metric     Score
    ## 1 DBSCAN Silhouette Score 0.3690352
    ## 2 DBSCAN    BSS/TSS Ratio 0.4437761
    ## 3    FCM Silhouette Score 0.3366676
    ## 4    FCM    BSS/TSS Ratio 0.7527584
    ## 5   FPCM Silhouette Score 0.2638178
    ## 6   FPCM    BSS/TSS Ratio 0.5957290
    ## 7  MFPCM Silhouette Score 0.3194333
    ## 8  MFPCM    BSS/TSS Ratio 0.6953665

``` r
# --- 2. Buat Plot Perbandingan (Grouped Bar Chart) ---
# Gunakan dataframe yang sudah dikoreksi
ggplot(results_combined, aes(x = Method, y = Score, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.4f", Score)),
            position = position_dodge(width = 0.9), vjust = -0.5) +
  geom_hline(yintercept = 0.3, linetype = "dashed", color = "red", size = 1) +
  geom_hline(yintercept = 0.7, linetype = "dashed", color = "blue", size = 1) +
  labs(
    title = "Perbandingan Metode Clustering",
    subtitle = "Garis merah: target Silhouette (0.3). Garis biru: target BSS/TSS (0.7)",
    y = "Score",
    x = "Metode Clustering",
    fill = "Metrik"
  ) +
  ylim(0, 1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](Clustering_UKT_files/figure-gfm/unnamed-chunk-38-1.png)<!-- -->

Centroid FCM

``` r
# --- Label cluster dari FCM ---
df$FCM_cluster <- fcm_labels

# --- Mendefinisikan Bobot yang Disepakati ---
weights <- c(
  PENGHASILAN_bin = 0.40,
  JUMLAH_TANGGUNGAN_bin = 0.15,
  JUMLAH_RUMAH = 0.05,
  JUMLAH_MOTOR = 0.05,
  JUMLAH_MOBIL = 0.03,
  SPI_bin = 0.15,
  PUNYA_TANAH = 0.01,
  PUNYA_TAMBAK_SAWAH = 0.01,
  DAYA_LISTRIK_enc = 0.10,
  KEPEMILIKAN_RUMAH_enc = 0.05
)

# --- Hitung rata-rata setiap variabel per cluster FCM ---
cluster_means <- df %>%
  group_by(FCM_cluster) %>%
  summarise(across(all_of(names(weights)), mean, .names = "mean_{.col}"))

# --- Hitung Skor UKT ---
relevant_means <- cluster_means %>%
  select(starts_with("mean_"))

names(relevant_means) <- gsub("mean_", "", names(relevant_means))
relevant_means <- relevant_means[, names(weights)]  # urutkan sesuai bobot

ukt_scores <- as.matrix(relevant_means) %*% as.matrix(weights)

ukt_final_scores <- data.frame(
  FCM_cluster = cluster_means$FCM_cluster,
  UKT_Score = as.vector(ukt_scores)
) %>%
  arrange(UKT_Score) %>%
  mutate(UKT_Level = row_number())

print(ukt_final_scores)
```

    ##    FCM_cluster UKT_Score UKT_Level
    ## 10           2  1.693889         1
    ## 8            4  1.710139         2
    ## 9            7  1.879167         3
    ## 31           3  1.879412         4
    ## 1            5  1.917761         5
    ## 2            8  2.454643         6
    ## 4            1  2.523065         7
    ## 7            6  2.673077         8

``` r
# --- Range UKT 500 ribu - 8 juta ---
ukt_min_price <- 500000
ukt_max_price <- 8000000

min_score <- min(ukt_final_scores$UKT_Score)
max_score <- max(ukt_final_scores$UKT_Score)
score_range <- max_score - min_score

ukt_final_scores <- ukt_final_scores %>%
  mutate(
    UKT_Price = round(
      ukt_min_price + 
      (UKT_Score - min_score) / score_range * (ukt_max_price - ukt_min_price)
    )
  ) %>%
  mutate(UKT_Price = ifelse(UKT_Level == 1, ukt_min_price, UKT_Price))

print(ukt_final_scores)
```

    ##    FCM_cluster UKT_Score UKT_Level UKT_Price
    ## 10           2  1.693889         1    500000
    ## 8            4  1.710139         2    624465
    ## 9            7  1.879167         3   1919118
    ## 31           3  1.879412         4   1920995
    ## 1            5  1.917761         5   2214729
    ## 2            8  2.454643         6   6326925
    ## 4            1  2.523065         7   6850994
    ## 7            6  2.673077         8   8000000

``` r
final_df <- df %>%
  left_join(ukt_final_scores, by = "FCM_cluster")

# Cek hasil akhir
head(final_df)
```

    ## # A tibble: 6 × 25
    ##   NOMOR PROGRAM_STUDI_DITERIMA   PENGHASILAN JUMLAH_TANGGUNGAN KEPEMILIKAN_RUMAH
    ##   <dbl> <chr>                          <dbl>             <int> <chr>            
    ## 1 72497 D3-Teknik Telekomunikasi     2000000                 2 sendiri          
    ## 2 73378 D3PSDKU-SM-Teknik Infor…     2000000                 5 sendiri          
    ## 3 72161 D3-Teknik Telekomunikasi     6020000                 2 sendiri          
    ## 4 74162 STr-Teknologi Game           5000000                 2 sendiri          
    ## 5 72572 STrPJJ-Teknik Telekomun…     1500000                 2 sendiri          
    ## 6 73473 STr-Sistem Pembangkit E…     4000000                 1 sendiri          
    ## # ℹ 20 more variables: JUMLAH_RUMAH <int>, PUNYA_TANAH <dbl>,
    ## #   PUNYA_TAMBAK_SAWAH <dbl>, PUNYA_GEDUNG <dbl>, PUNYA_APARTEMEN <dbl>,
    ## #   JUMLAH_MOTOR <int>, JUMLAH_MOBIL <int>, DAYA_LISTRIK <chr>, SPI <dbl>,
    ## #   KOTA_TEMPAT_TINGGAL <chr>, PROPINSI_TEMPAT_TINGGAL <chr>,
    ## #   DAYA_LISTRIK_enc <dbl>, KEPEMILIKAN_RUMAH_enc <dbl>, PENGHASILAN_bin <dbl>,
    ## #   SPI_bin <dbl>, JUMLAH_TANGGUNGAN_bin <dbl>, FCM_cluster <int>,
    ## #   UKT_Score <dbl>, UKT_Level <int>, UKT_Price <dbl>

``` r
# Hitung total pendapatan
sum(final_df$UKT_Price, na.rm = TRUE)
```

    ## [1] 1298979598

``` r
sum(final_df$UKT_Price, na.rm = TRUE)
```

    ## [1] 1298979598

``` r
data_ukt <- final_df %>%
  select(
    NOMOR,
    PROGRAM_STUDI_DITERIMA,
    PENGHASILAN,
    JUMLAH_TANGGUNGAN,
    JUMLAH_MOTOR,
    JUMLAH_MOBIL,
    DAYA_LISTRIK,
    SPI,
    KOTA_TEMPAT_TINGGAL,
    PROPINSI_TEMPAT_TINGGAL,
    KEPEMILIKAN_RUMAH,
    FCM_cluster,
    UKT_Level,
    UKT_Price
  )

data_ukt
```

    ## # A tibble: 340 × 14
    ##    NOMOR PROGRAM_STUDI_DITERIMA       PENGHASILAN JUMLAH_TANGGUNGAN JUMLAH_MOTOR
    ##    <dbl> <chr>                              <dbl>             <int>        <int>
    ##  1 72497 D3-Teknik Telekomunikasi         2000000                 2            1
    ##  2 73378 D3PSDKU-SM-Teknik Informati…     2000000                 5            2
    ##  3 72161 D3-Teknik Telekomunikasi         6020000                 2            2
    ##  4 74162 STr-Teknologi Game               5000000                 2            3
    ##  5 72572 STrPJJ-Teknik Telekomunikasi     1500000                 2            1
    ##  6 73473 STr-Sistem Pembangkit Energi     4000000                 1            1
    ##  7 72134 STr-Teknik Informatika          20000000                 2            3
    ##  8 74062 D3PJJ-Teknik Informatika         3500000                 4            2
    ##  9 73909 D3-Teknik Elektro Industri       2500000                 1            1
    ## 10 73952 D3-Teknik Telekomunikasi         2500000                 3            1
    ## # ℹ 330 more rows
    ## # ℹ 9 more variables: JUMLAH_MOBIL <int>, DAYA_LISTRIK <chr>, SPI <dbl>,
    ## #   KOTA_TEMPAT_TINGGAL <chr>, PROPINSI_TEMPAT_TINGGAL <chr>,
    ## #   KEPEMILIKAN_RUMAH <chr>, FCM_cluster <int>, UKT_Level <int>,
    ## #   UKT_Price <dbl>

``` r
# Ekspor ke CSV
write.csv(data_ukt, "data_ukt_simandiri.csv", row.names = FALSE)
```
