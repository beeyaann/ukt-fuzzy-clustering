# 🎓 Klasterisasi UKT Berbasis Data dengan Pemodelan Statistik Fuzzy

Proyek ini dikembangkan sebagai bagian dari mata kuliah **Workshop Analitika Data Terapan** untuk merancang sistem klasifikasi Uang Kuliah Tunggal (UKT) yang adil dan berbasis data menggunakan indikator sosial ekonomi mahasiswa.

---

## 📊 Gambaran Proyek

Tujuan proyek ini adalah mengelompokkan mahasiswa ke dalam kelompok UKT yang setara berdasarkan kapasitas finansial, menggunakan pendekatan *fuzzy* dan *density-based clustering*. Analisis ini memadukan berbagai atribut sosial ekonomi untuk memastikan pembagian UKT yang adil sekaligus menjaga keseimbangan pendapatan institusi.

---

## 🧮 Data & Pra-pemrosesan

* **Jumlah data:** 340 mahasiswa
* **Variabel utama:**

  * Penghasilan
  * SPI (Sumbangan Pengembangan Institusi)
  * Jumlah tanggungan
  * Kepemilikan properti
  * Daya listrik
  * Jenis hunian

Tahapan pra-pemrosesan data mencakup:

* Pembersihan dan penanganan data hilang atau penghasilan nol
* Imputasi menggunakan data UMR regional dan logika ekonomi
* Normalisasi serta pengkodean variabel kategorik dan ordinal
* Deteksi outlier menggunakan *Mahalanobis Distance*

---

## 🧠 Metode Klasterisasi

Lima algoritma dibandingkan dalam analisis ini:

* **DBSCAN** (*Density-Based Spatial Clustering*)
* **FCM** – *Fuzzy C-Means*
* **PCM** – *Possibilistic C-Means*
* **FPCM** – *Fuzzy Possibilistic C-Means*
* **MFPCM** – *Modified Fuzzy Possibilistic C-Means*

### 📈 Metrik Evaluasi

* **Silhouette Coefficient**
* **BSS/TSS Ratio**
* **Fuzzy Silhouette Index (FSI)**

**Model terbaik:** FCM

* **BSS/TSS:** 0.75
* **FSI:** 0.57
* **Menghasilkan 8 klaster sosial ekonomi (Rp 500 ribu – Rp 8 juta)**

---

## 💡 Sistem Pembobotan

Penentuan skor klaster tiap mahasiswa dilakukan berdasarkan bobot berikut:

| Faktor             | Bobot |
| ------------------ | ----- |
| Penghasilan        | 40%   |
| Tanggungan         | 15%   |
| SPI                | 15%   |
| Properti & Listrik | 25%   |
| Lain-lain          | 5%    |

---

## 📊 Dashboard Analitik

Dashboard interaktif dikembangkan untuk menampilkan:

* Distribusi klaster berdasarkan tingkat sosial ekonomi
* Analisis keadilan penetapan UKT
* Estimasi total pendapatan institusi ≈ **Rp 1,29 miliar**

---

## 🛠️ Bahasa & Pustaka

* **Bahasa:** R
* **Pustaka:** tidyverse, dbscan, fclust, ppclust, ggplot2, cluster

---

## Oleh:

**Biyan Daniswara**
Mahasiswa S1 Terapan – Data Science
Politeknik Elektronika Negeri Surabaya
