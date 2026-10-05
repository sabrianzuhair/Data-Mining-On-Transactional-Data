##BAGIAN 1: INTRODUCTION TO R & DATA IMPORT (SESI 1)
------------------------------------------------------------------------
##1. Buat sebuah vector bernama `promo_discount`
promo_discount <- c(10,5)
##2. Buat sebuah fungsi kustom bernama `hitung_total(harga, pajak)`
hitung_total <- function(harga,pajak) {
  total <- harga + (pajak * harga)
}
print(hitung_total(100000,0.5))
##3. Import dataset 'items.csv' ke variabel `items` dan 'transactions.csv' ke variabel `trx`.\
trx <- read.csv('transactions.csv')
items <- read.csv('items.csv')


##------------------------------------------------------------------------
  ##BAGIAN 2: DATA DESCRIPTION & DATA CLEANING (SESI 3)
##------------------------------------------------------------------------
##1. Tampilkan ringkasan statistik deskriptif dari tabel `items` menggunakan fungsi `summary()`.
summary(items)
##2. Lakukan pengecekan data tidak valid (invalid data): a. Tampilkan baris mana saja yang nilai kolom 'Stock'-nya di bawah 0.
print(items[items$Stock <= 0,])
##3. Bersihkan tabel `items`:
##- Buang baris yang memiliki nilai NA.
clean_item <- na.omit(items)
clean_trx <- na.omit(trx)
##- Buang baris yang memiliki nilai Stock negatif (< 0).
##- Simpan data yang sudah bersih ke dalam variabel bernama `items_clean`.
clean_items <- clean_item[clean_item$Stock >= 0, ] 
print(clean_items)
##4. Gunakan fungsi `aggregate()` pada `items_clean` untuk menghitung:
##a. Rata-rata nilai 'Price' berdasarkan masing-masing 'Category'.
avg_price <- aggregate(Price ~ Category , data=clean_items , FUN = mean)
avg_price
##b. Total akumulasi 'Stock' berdasarkan masing-masing 'Category'.
total_stock <- aggregate(Stock ~ Category , data = clean_item, FUN = sum)
print(total_stock)



# ------------------------------------------------------------------------
#   BAGIAN 3: TRANSFORMATION, GROUPING & VISUALIZATION (SESI 2)
# ------------------------------------------------------------------------
#   1. Dari tabel `items_clean`:
#   - Ambil hanya kolom 'ItemName', 'Category', dan 'Price'.
print(clean_items[,c("ItemName","Category","Price")])
# - Urutkan barisnya dari produk dengan harga termahal ke termurah.
ordered_item <-  clean_items[order(clean_items$Price,decreasing=TRUE),]
ordered_item
# 2. Lakukan grouping nilai (count by value) untuk menghitung berapa 
# jumlah item produk pada setiap kategori menggunakan fungsi `table()`.
category_item <-  table(ordered_item$Category)
print(category_item)
# 3. Buat grafik visualisasi sederhana (diagram batang / barplot) untuk 
# menampilkan distribusi jumlah item per kategori yang sudah dihitung.
barplot(category_item,main = "niggerballs",xlab = "auauau",ylab = "iaiaia")
barplot(avg_price$Price,names.arg = avg_price$Category,main = "rafahitam",xlab = "andreas hitam",ylab = "arip cipeng")
barplot(total_stock$Stock,names.arg = total_stock$Category)

# ------------------------------------------------------------------------
#   BAGIAN 4: FREQUENT PATTERNS & ASSOCIATION RULES (SESI 4)
# ------------------------------------------------------------------------
#   1. Gabungkan (merge) data transaksi `trx` dengan `items_clean`.
joined <-  merge(clean_items,clean_trx , by = "ItemID")
print(joined)
# 2. Ubah format data `trx_detail` menjadi format keranjang belanja 
# - Pisahkan kolom 'ItemName' berdasarkan 'TransactionID' menggunakan `split()`.
basket <- split(joined$ItemName , joined$TransactionID)
print(basket)
# - Pastikan tidak ada nama item yang terduplikasi dalam satu keranjang belanja.
basket_clean<- lapply(basket, unique)
print(basket_clean)
# - Ubah objek list tersebut menjadi objek berkelas `transactions` 
# (gunakan library `arules`).
R.version.string
install.packages("arules")
library(arules)
transaksi <- as(basket_clean,"transactions")
print(transaksi)
# 3. Jalankan algoritma Apriori untuk menemukan Association Rules dengan kriteria:
#   - Minimum Support    : 0.20 (20%)
# - Minimum Confidence : 0.50 (50%)
rules <-  apriori(
  transaksi,
  parameter = list(
    supp = 0.20 , 
    conf = 0.50
  )
  
)

# 4. Tampilkan aturan (rule) menggunakan fungsi `inspect()`!
inspect(rules)
#   ========================================================================
  

