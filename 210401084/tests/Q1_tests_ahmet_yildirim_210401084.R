# YC<klC< kC<tC<phaneleri kontrol et
library(testthat)


## Q3 Q3Q 3 Q3 Q3 Q3
## Q3 Q3Q 3 Q3 Q3 Q3
## Q3 Q3Q 3 Q3 Q3 Q3

# Test senaryolarD1nD1 iC'eren bir dosya oluEturun (C6rneDin, test_veri_keEfi.R)
context("Q3 AdD1mD1 iC'in Test SenaryolarD1: Veri seti keEifsel analizi")

# Test: Veri setinin satD1r ve sC<tun sayD1larD1 doDru
test_that("Veri setinin satD1r ve sC<tun sayD1larD1 doDru", {
  # Beklenen deDerler
  beklenen_satir_sayisi <- 1326
  beklenen_sutun_sayisi <- 6
  
  # Test
  expect_equal(nrow(veriseti), beklenen_satir_sayisi)
  expect_equal(ncol(veriseti), beklenen_sutun_sayisi)
})

# Test: Veri setindeki sC<tun isimleri doDru tanD1mlandD1
test_that("Veri setindeki sC<tun isimleri doDru tanD1mlandD1", {
  # Beklenen sC<tun isimleri
  beklenen_sutun_isimleri <- c("bodyPart", "equipment", "gifUrl", "id", "name", "target")
  
  # Test
  expect_true(all(colnames(veriseti) == beklenen_sutun_isimleri))
})

# Test: Her sC<tundaki veri tipleri doDru
test_that("Her sC<tundaki veri tipleri doDru", {
  # Beklenen veri tipleri
  beklenen_veri_tipleri <- c("character", "character", "character", "character", "character", "character")
  
  # Test
  expect_true(all(sapply(veriseti, class) == beklenen_veri_tipleri))
})

# Q4 Q4 Q4 Q4 Q4 Q4
# Q4 Q4 Q4 Q4 Q4 Q4
# Q4 Q4 Q4 Q4 Q4 Q4
# Q4 Q4 Q4 Q4 Q4 Q4
# Q4 Q4 Q4 Q4 Q4 Q4
# Q4 Q4 Q4 Q4 Q4 Q4
# Q4 Q4 Q4 Q4 Q4 Q4
# Q4 Q4 Q4 Q4 Q4 Q4

# Test senaryolarD1nD1 tanD1mla
testthat::test_that("Veri seti C6n iElemesi doDru mu?", {
  
  # Kategori bilgisi iC'eren sC<tunu factor tipine C'evirme testi
  expect_is(veriseti$bodyPart, "factor")
  
  # Czet rapor testi
  summary_df <- data.frame(
    NAs_before = sum(is.na(veriseti)),
    NAs_after = sum(is.na(veriseti[complete.cases(veriseti), ])),
    unique_values_bodyPart = length(unique(veriseti$bodyPart)),
    unique_values_equipment = length(unique(veriseti$equipment))
  )
  
  expect_equal(summary_df$NAs_before, 0, info = "NA deDerleri kontrolC<")
  expect_equal(summary_df$NAs_after, 0, info = "NA deDerleri kontrolC< (veri setinden eksik gC6zlemleri C'D1karma)")
  expect_gt(summary_df$unique_values_bodyPart, 0, info = "bodyPart sC<tununda benzersiz deDerler kontrolC<")
  expect_gt(summary_df$unique_values_equipment, 0, info = "equipment sC<tununda benzersiz deDerler kontrolC<")
})

## Q5 Q5 Q5 
## Q5 Q5 Q5 
## Q5 Q5 Q5 
## Q5 Q5 Q5 
## Q5 Q5 Q5 
## Q5 Q5 Q5 
## Q5 Q5 Q5 
## Q5 Q5 Q5 

library(dplyr)

## Q5 Q5 Q5 # Test senaryolarD1nD1 tanD1mla
testthat::test_that("Uygulama D0statistikleri doDru mu?", {
  
  # VC<cut bC6lgelerine gC6re egzersiz sayD1sD1 grafiDi testi
  expect_true(file.exists("vucut_bolgeleri_grafik.png"), info = "VC<cut bC6lgelerine gC6re egzersiz sayD1sD1 grafiDi dosyasD1 kontrolC<")
  
  # Ekipmanlara gC6re egzersiz sayD1sD1 daDD1lD1mD1 grafiDi testi
  expect_true(file.exists("ekipmanlar_grafik.png"), info = "Ekipmanlara gC6re egzersiz sayD1sD1 daDD1lD1mD1 grafiDi dosyasD1 kontrolC<")
  
  # Kategori frekans grafiDi testi
  expect_true(file.exists("kategori_frekans_grafik.png"), info = "Kategori frekans grafiDi dosyasD1 kontrolC<")
})


