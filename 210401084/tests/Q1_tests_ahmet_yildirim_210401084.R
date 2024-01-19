# Yüklü kütüphaneleri kontrol et
library(testthat)


## Q3 Q3Q 3 Q3 Q3 Q3
## Q3 Q3Q 3 Q3 Q3 Q3
## Q3 Q3Q 3 Q3 Q3 Q3

# Test senaryolarını içeren bir dosya oluşturun (örneğin, test_veri_keşfi.R)
context("Q3 Adımı için Test Senaryoları: Veri seti keşifsel analizi")

# Test: Veri setinin satır ve sütun sayıları doğru
test_that("Veri setinin satır ve sütun sayıları doğru", {
  # Beklenen değerler
  beklenen_satir_sayisi <- 1326
  beklenen_sutun_sayisi <- 6
  
  # Test
  expect_equal(nrow(veriseti), beklenen_satir_sayisi)
  expect_equal(ncol(veriseti), beklenen_sutun_sayisi)
})

# Test: Veri setindeki sütun isimleri doğru tanımlandı
test_that("Veri setindeki sütun isimleri doğru tanımlandı", {
  # Beklenen sütun isimleri
  beklenen_sutun_isimleri <- c("bodyPart", "equipment", "gifUrl", "id", "name", "target")
  
  # Test
  expect_true(all(colnames(veriseti) == beklenen_sutun_isimleri))
})

# Test: Her sütundaki veri tipleri doğru
test_that("Her sütundaki veri tipleri doğru", {
  # Beklenen veri tipleri
  beklenen_veri_tipleri <- c("character", "character", "character", "character", "character", "character")
  
  # Test
  expect_true(all(sapply(veriseti, class) == beklenen_veri_tipleri))
})



