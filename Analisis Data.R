#=============================== Read Data =====================================#library(readxl)
library(readxl)
Tumor = read_xlsx("C:/Users/LENOVO/OneDrive/Documents/TA FIKS SEMHAS/DATA_TA.xlsx")
head(Tumor)
View(Tumor)

#Cek dimensi data (Baris & kolom)
dim(Tumor)

#Cek type data
str(Tumor)

#Cek Missing data
sum(is.na(Tumor))

#Ubah Nama data
Tumor$id <- factor(Tumor$id) 
Tumor$Status <- factor(Tumor$Status) 
Tumor$`Jenis Kelamin` <- factor(Tumor$`Jenis Kelamin`) 
Tumor$`Jenis Operasi` <- factor(Tumor$`Jenis Operasi`) 
Tumor$`Lokasi Tumor` <- factor(Tumor$`Lokasi Tumor`) 
Tumor$ASA <- factor(Tumor$ASA) 
Tumor$`Jenis Anestesi` <- factor(Tumor$`Jenis Anestesi`) 
Tumor$`Status Hipertensi` <- factor(Tumor$`Status Hipertensi`) 
Tumor$`Status DM` <- factor(Tumor$`Status DM`) 
Tumor$`Status Pernikahan` <- factor(Tumor$`Status Pernikahan`) 
Tumor$`Hu-Care Window SGRN` <- factor(Tumor$`Hu-Care Window SGRN`)

#=============================== Kaplan Meier =====================================#
library(survival)
library(ggplot2) 
library(survminer) 

Y = Surv(Tumor$`Waktu Survival`, Tumor$Status == 1) 
kmfit <- survfit(Y~1) 
km = ggsurvplot(kmfit, data=Tumor, xlab="Waktu (hari)", 
                ylab=expression(bold(hat(S)(t))), legend="none", surv.median.line = "hv",  
                palette="pink1",break.x.by=11) 
ggpar(km, font.main=c(12,"bold"), font.x=c(12,"bold"), font.y=c(12,"bold"), 
      font.tickslab=c(11))

#Usia (x1) 
Tumor$Usia_Kat <- ifelse(Tumor$Usia >= 10 & Tumor$Usia <= 18,"Remaja", 
                         ifelse(Tumor$Usia >= 19 & Tumor$Usia <= 59,"Dewasa","Lansia")) 
kmfit1 <- survfit(Y~Tumor$Usia_Kat) 
km1 = ggsurvplot(kmfit1, data=Tumor, pval=TRUE, pval.method=TRUE, pval.size=3.5, 
                 pval.coord=c(70,0.36), pval.method.coord=c(70,0.42), 
                 legend.title="Usia (x1)",  
                 legend.labs=c("Remaja","Dewasa","Lansia"), 
                 xlab="Waktu Pemulihan (hari)", ylab=expression(bold(hat(S)(t))), 
                 palette=c("cyan","brown1","seagreen"),break.x.by=11) 
ggpar(km1, font.main=c(12,"bold"), font.x=c(12,"bold"), font.y=c(12,"bold"), 
      font.tickslab=c(11), font.legend=c(10,"bold"), legend=c(0.7,0.80))

#Jenis Kelamin (x2) 
kmfit2 <- survfit(Y~Tumor$`Jenis Kelamin`) 
km2 = ggsurvplot(kmfit2, data=Tumor, pval=TRUE, pval.method=TRUE, pval.size=3.5, 
                 pval.coord=c(70,0.47), pval.method.coord=c(70,0.53), 
                 legend.title="Jenis Kelamin (x2)",legend.labs=c("Laki-Laki","Perempuan"), 
                 palette=c("cyan","brown1"),break.x.by=11, 
                 xlab="Waktu Pemulihan (hari)", ylab=expression(bold(hat(S)(t)))) 
ggpar(km2, font.main=c(12,"bold"), font.x=c(12,"bold"), font.y=c(12,"bold"), 
      font.tickslab=c(11), font.legend=c(10,"bold"), legend=c(0.7,0.80))

#Jenis Operasi (x3) 
kmfit3 <- survfit(Y~Tumor$`Jenis Operasi`) 
km3 = ggsurvplot(kmfit3, data=Tumor, pval=TRUE, pval.method=TRUE, pval.size=3.5, 
                 pval.coord=c(70,0.36), pval.method.coord=c(70,0.42), 
                 legend.title="Jenis Operasi (x3)",  
                 legend.labs=c("Ringan","Tinggi","Khusus"), 
                 xlab="Waktu Pemulihan (hari)", ylab=expression(bold(hat(S)(t))), 
                 palette=c("cyan1","brown1","seagreen"),break.x.by=11) 
ggpar(km3, font.main=c(12,"bold"), font.x=c(12,"bold"), font.y=c(12,"bold"), 
      font.tickslab=c(11), font.legend=c(10,"bold"), legend=c(0.7,0.80))

#Lama Operasi (x4) 
Tumor$Lama_Operasi_Kat <- ifelse(Tumor$`Lama Operasi`<=30,"Pendek","Panjang") 
kmfit4 <- survfit(Y~Tumor$Lama_Operasi_Kat) 
km4 = ggsurvplot(kmfit4, data=Tumor,pval=TRUE, pval.method=TRUE, pval.size=3.5, 
                 pval.coord=c(70,0.47), pval.method.coord=c(70,0.53),  
                 legend.title="Lama Operasi (x4)",  
                 legend.labs=c("Pendek","Panjang"), 
                 palette=c("cyan1","brown1"),break.x.by=11, 
                 xlab="Waktu Pemulihan (hari)", ylab=expression(bold(hat(S)(t)))) 
ggpar(km4, font.main=c(12,"bold"), font.x=c(12,"bold"), font.y=c(12,"bold"), 
      font.tickslab=c(11), font.legend=c(10,"bold"), legend=c(0.7,0.80))

#Lokasi Tumor (x5) 
kmfit5 <- survfit(Y~Tumor$`Lokasi Tumor`) 
km5 = ggsurvplot(kmfit5,data=Tumor, pval=TRUE, pval.method=TRUE, pval.size=3.5, 
                 pval.coord=c(70,0.47), pval.method.coord=c(70,0.53), 
                 legend.title="Lokasi Tumor yang Dioperasi (x5)",  
                 legend.labs=c("Sistem Integumen","Sistem Muskuloskeletal", 
                               "Sistem Reproduksi","Sistem Lain"), xlab="Waktu Pemulihan (hari)", 
                 ylab=expression(bold(hat(S)(t))), break.x.by=11, 
                 palette=c("cyan1","brown2","seagreen","purple1")) 
ggpar(km5, font.main=c(12,"bold"), font.x=c(12,"bold"), font.y=c(12,"bold"), 
      font.tickslab=c(11), font.legend=c(10,"bold"), legend=c(0.7,0.80))

#Ukuran Tumor (x6) 
Tumor$Berat_Tumor <- ifelse(Tumor$`Berat Tumor`<= 5,"kecil", 
                            ifelse(Tumor$`Berat Tumor`>5 & Tumor$`Berat Tumor`<=20,"sedang", 
                                   ifelse(Tumor$`Berat Tumor`>20 & Tumor$`Berat Tumor`<=50,"besar","sangat besar"))) 
kmfit6 <- survfit(Y~Tumor$Berat_Tumor) 
km6 = ggsurvplot(kmfit6, data=Tumor,pval=TRUE, pval.method=TRUE, pval.size=3.5, 
                 pval.coord=c(70,0.4), pval.method.coord=c(70,0.46),  
                 legend.title="Berat Tumor (x6)", 
                 legend.labs=c("Kecil","Sedang","Besar","Sangat Besar"), 
                 palette=c("cyan1","brown2","seagreen","purple1"), 
                 break.x.by=11, 
                 xlab="Waktu Pemulihan (hari)", ylab=expression(bold(hat(S)(t)))) 
ggpar(km6, font.main=c(12,"bold"), font.x=c(12,"bold"), font.y=c(12,"bold"), 
      font.tickslab=c(11), font.legend=c(10,"bold"), legend=c(0.7,0.80))

#ASA (x7) 
kmfit7 <- survfit(Y~Tumor$ASA) 
km7 = ggsurvplot(kmfit7, data=Tumor, pval=TRUE, pval.method=TRUE, pval.size=3.5, 
                 pval.coord=c(70,0.47), pval.method.coord=c(70,0.53), 
                 legend.title="ASA (x7)", legend.labs=c("I","II"), 
                 palette=c("cyan1","brown2"),break.x.by=11, 
                 xlab="Waktu Pemulihan (hari)", ylab=expression(bold(hat(S)(t)))) 
ggpar(km7, font.main=c(12,"bold"), font.x=c(12,"bold"), font.y=c(12,"bold"), 
      font.tickslab=c(11), font.legend=c(10,"bold"), legend=c(0.7,0.80))

#Jenis Anestesi (x8) 
kmfit8 <- survfit(Y~Tumor$`Jenis Anestesi`) 
km8 = ggsurvplot(kmfit8, data=Tumor, pval=TRUE, pval.method=TRUE, pval.size=3.5, 
                 pval.coord=c(70,0.47), pval.method.coord=c(70,0.53), 
                 legend.title="Jenis Anestesi (x8)",  
                 legend.labs=c("Umum","Spinal"), 
                 palette=c("cyan1","brown2"),break.x.by=11, 
                 xlab="Waktu Pemulihan (hari)", ylab=expression(bold(hat(S)(t)))) 
ggpar(km8, font.main=c(12,"bold"), font.x=c(12,"bold"), font.y=c(12,"bold"), 
      font.tickslab=c(11), font.legend=c(10,"bold"), legend=c(0.7,0.80))

#Kadar Hemoglobin (x9) 
Tumor$Hemoglobin_Kat <- ifelse(Tumor$HB >= 12 & Tumor$HB <= 16, "normal", "abnormal") 
kmfit9 <- survfit(Y~Tumor$Hemoglobin_Kat) 
km9 = ggsurvplot(kmfit9, data=Tumor,pval=TRUE, pval.method=TRUE, pval.size=3.5, 
                 pval.coord=c(70,0.47), pval.method.coord=c(70,0.53),  
                 legend.title="Kadar Hemoglobin (x9)",  
                 legend.labs=c("Normal","Abnormal"), 
                 palette=c("cyan1","brown2"),break.x.by=11, 
                 xlab="Waktu Pemulihan (hari)", ylab=expression(bold(hat(S)(t)))) 
ggpar(km9, font.main=c(12,"bold"), font.x=c(12,"bold"), font.y=c(12,"bold"), 
      font.tickslab=c(11), font.legend=c(10,"bold"), legend=c(0.7,0.80))

#Status Diabetes Melitus (x10) 
kmfit10 <- survfit(Y~Tumor$`Status DM`) 
km10 = ggsurvplot(kmfit10, data=Tumor,pval=TRUE, pval.method=TRUE, pval.size=3.5, 
                  pval.coord=c(70,0.47), pval.method.coord=c(70,0.53), 
                  legend.title="Status Riwayat Diabetes Melitus (x10)",  
                  legend.labs=c("Tidak Ada","Ada"), 
                  palette=c("cyan1","brown2"),break.x.by=11, 
                  xlab="Waktu Pemulihan (hari)", ylab=expression(bold(hat(S)(t)))) 
ggpar(km10, font.main=c(12,"bold"), font.x=c(12,"bold"), font.y=c(12,"bold"), 
      font.tickslab=c(11), font.legend=c(10,"bold"), legend=c(0.7,0.80))

#Status Hipertensi (x11) 
kmfit11 <- survfit(Y~Tumor$`Status Hipertensi`) 
km11 = ggsurvplot(kmfit11, data=Tumor,pval=TRUE, pval.method=TRUE, pval.size=3.5, 
                  pval.coord=c(70,0.47), pval.method.coord=c(70,0.53), 
                  legend.title="Status Hipertensi (x11)", legend.labs=c("Tidak Ada","Ada"), 
                  palette=c("cyan1","brown2"),break.x.by=11, 
                  xlab="Waktu Pemulihan (hari)", ylab=expression(bold(hat(S)(t)))) 
ggpar(km11, font.main=c(12,"bold"), font.x=c(12,"bold"), font.y=c(12,"bold"), 
      font.tickslab=c(11), font.legend=c(10,"bold"), legend=c(0.7,0.80))

#Status Pernikahan (x12) 
kmfit12 <- survfit(Y~Tumor$`Status Pernikahan`) 
km12 = ggsurvplot(kmfit12, data=Tumor,pval=TRUE, pval.method=TRUE, pval.size=3.5, 
                  pval.coord=c(70,0.47), pval.method.coord=c(70,0.53),  
                  legend.title="Status Pernikahan (x12)",  
                  legend.labs=c("Belum Menikah","Pernah Menikah"), 
                  palette=c("cyan1","brown2"),break.x.by=11, 
                  xlab="Waktu Pemulihan (hari)", ylab=expression(bold(hat(S)(t)))) 
ggpar(km12, font.main=c(12,"bold"), font.x=c(12,"bold"), font.y=c(12,"bold"), 
      font.tickslab=c(11), font.legend=c(10,"bold"), legend=c(0.7,0.80))

#Hu-Care Window SGRN (x13) 
kmfit13 <- survfit(Y~Tumor$`Hu-Care Window SGRN`) 
km13 = ggsurvplot(kmfit13, data=Tumor,pval=TRUE, pval.method=TRUE, pval.size=3.5, 
                  pval.coord=c(70,0.47), pval.method.coord=c(70,0.53),  
                  legend.title="Hu-Care SGRN (x13)",  
                  legend.labs=c("Nyaman","Gamang"), 
                  palette=c("cyan1","brown2"),break.x.by=11, 
                  xlab="Waktu Pemulihan (hari)", ylab=expression(bold(hat(S)(t)))) 
ggpar(km13, font.main=c(12,"bold"), font.x=c(12,"bold"), font.y=c(12,"bold"), 
      font.tickslab=c(11), font.legend=c(10,"bold"), legend=c(0.7,0.80))

#=============================== LOG-RANK =====================================#
LR1 <- survdiff(Y~Tumor$Usia_Kat); LR1; LR1$pvalue; LR1$chisq
LR2 <- survdiff(Y~Tumor$`Jenis Kelamin`); LR2; LR2$pvalue; LR2$chisq
LR3 <- survdiff(Y~Tumor$`Jenis Operasi`); LR3; LR3$pvalue; LR3$chisq
LR4 <- survdiff(Y~Tumor$Lama_Operasi_Kat); LR4; LR4$pvalue; LR4$chisq
LR5 <- survdiff(Y~Tumor$`Lokasi Tumor`); LR5; LR5$pvalue; LR5$chisq
LR6 <- survdiff(Y~Tumor$Berat_Tumor);LR6; LR6$pvalue; LR6$chisq
LR7 <- survdiff(Y~Tumor$ASA); LR7; LR7$pvalue; LR7$chisq
LR8 <- survdiff(Y~Tumor$`Jenis Anestesi`); LR8; LR8$pvalue; LR8$chisq 
LR9 <- survdiff(Y~Tumor$Hemoglobin_Kat);LR9; LR9$pvalue; LR9$chisq
LR10 <- survdiff(Y~Tumor$`Status DM`); LR10; LR10$pvalue; LR10$chisq
LR11 <- survdiff(Y~Tumor$`Status Hipertensi`); LR11; LR11$pvalue; LR11$chisq 
LR12 <- survdiff(Y~Tumor$`Status Pernikahan`); LR12; LR12$pvalue; LR12$chisq 
LR13 <- survdiff(Y~Tumor$`Hu-Care Window SGRN`); LR13; LR13$pvalue; LR13$chisq

#=============================== Asumsi Proportional Hazard =====================================#
#GOF
m1 = coxph(Y~Usia+`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+`Lokasi Tumor`+
             `Berat Tumor`+ASA+`Jenis Anestesi`+HB+`Status DM`+`Status Hipertensi`+
             `Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor)
summary(m1)

# Uji dengan pendekatan GOF
uji_ph <- cox.zph(m1, transform = rank)
uji_ph$table

# Dari output di atas, dapat diketahui bahwa pada variabel Usia, Jenis Kelamin, Jenis Operasi, Lama Operasi, Lokasi Tumor, Ukuran Tumor, ASA, Jenis Anestesi, HB, Status DM, Status Hipertensi, Status Pernikahan, dan Hu-Care Window SGRN memiliki nilai signifikansi (p-value) lebih dari α (0,05). Hal ini menandakan bahwa kita gagal menolak H0 untuk semua variabel tersebut. 
# Dengan demikian, dapat disimpulkan bahwa semua variabel yang digunakan dalam penelitian ini telah memenuhi asumsi proportional hazard (PH).

#=============================== Estimasi parameter Breslow =====================================#
m2 = coxph(Y~Usia+`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+`Lokasi Tumor`+
             `Berat Tumor`+ASA+`Jenis Anestesi`+HB+`Status DM`+`Status Hipertensi`+
             `Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "breslow")
summary(m2)

# Uji dengan pendekatan GOF
uji_ph <- cox.zph(m2, transform = rank)
uji_ph$table
#=============================== Eliminasi Backward =====================================#
em1 = coxph(Y~Usia+`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+`Lokasi Tumor`+
              `Berat Tumor`+ASA+`Jenis Anestesi`+HB+`Status DM`+`Status Hipertensi`+
              `Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "breslow")
summary(em1)
#Hapus `Status Hipertensi`

em2 = coxph(Y~Usia+`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+`Lokasi Tumor`+
              `Berat Tumor`+ASA+`Jenis Anestesi`+HB+`Status DM`+
              `Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "breslow")
summary(em2)
#Hapus `Status Hipertensi` dan HB

em3 = coxph(Y~Usia+`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+`Lokasi Tumor`+
              `Berat Tumor`+ASA+`Jenis Anestesi`+`Status DM`+
              `Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "breslow")
summary(em3)
#Hapus `Status Hipertensi`, HB,  `Berat Tumor`

em4 = coxph(Y~Usia+`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+`Lokasi Tumor`+ ASA+`Jenis Anestesi`+`Status DM`+
              `Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "breslow")
summary(em4)
#Hapus `Status Hipertensi`, HB,  `Berat Tumor`, ASA

em5 = coxph(Y~Usia+`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+`Lokasi Tumor`+`Jenis Anestesi`+`Status DM`+
              `Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "breslow")
summary(em5)
#Hapus `Status Hipertensi`, HB,  `Berat Tumor`, ASA, `Lokasi Tumor`

em6 = coxph(Y~Usia+`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+`Jenis Anestesi`+`Status DM`+
              `Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "breslow")
summary(em6)
#Hapus `Status Hipertensi`, HB,  `Berat Tumor`, ASA, `Lokasi Tumor`,usia

em7 = coxph(Y~`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+`Jenis Anestesi`+`Status DM`+
              `Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "breslow")
summary(em7)
#Hapus `Status Hipertensi`, HB,  `Berat Tumor`, ASA, `Lokasi Tumor`, usia , `Status DM`

em8 = coxph(Y~`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+`Jenis Anestesi`+
              `Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "breslow")
summary(em8)
#Hapus `Status Hipertensi`, HB,  `Berat Tumor`, ASA, `Lokasi Tumor`, usia , `Status DM`, `Hu-Care Window SGRN`

em9 = coxph(Y~`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+`Jenis Anestesi`+
              `Status Pernikahan`, data=Tumor, ties = "breslow")
summary(em9)
#Hapus `Status Hipertensi`, HB,  `Berat Tumor`, ASA, `Lokasi Tumor`, usia , `Status DM`, `Hu-Care Window SGRN`, `Jenis Kelamin`

em10 = coxph(Y~`Jenis Operasi`+`Lama Operasi`+`Jenis Anestesi`+
               `Status Pernikahan`, data=Tumor, ties = "breslow")
summary(em10)
#Hapus `Status Hipertensi`, HB,  `Berat Tumor`, ASA, `Lokasi Tumor`, usia , `Status DM`, `Hu-Care Window SGRN`, `Jenis Kelamin`, `Lama Operasi`

em11 = coxph(Y~`Jenis Operasi`+`Jenis Anestesi`+
               `Status Pernikahan`, data=Tumor, ties = "breslow")
summary(em11)
#Hapus `Status Hipertensi`, HB,  `Berat Tumor`, ASA, `Lokasi Tumor`, usia , `Status DM`, `Hu-Care Window SGRN`, `Jenis Kelamin`, `Lama Operasi`, `Jenis Operasi`

# Uji dengan pendekatan GOF
uji_ph <- cox.zph(em11, transform = rank)
uji_ph$table
#=============================== Estimasi parameter Efron =====================================#
m3 = coxph(Y~Usia+`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+`Lokasi Tumor`+
             `Berat Tumor`+ASA+`Jenis Anestesi`+HB+`Status DM`+`Status Hipertensi`+
             `Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "efron")
summary(m3)

# Uji dengan pendekatan GOF
uji_ph <- cox.zph(m3, transform = rank)
uji_ph$table
#=============================== Eliminasi Backward =====================================#
em21 = coxph(Y~Usia+`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+`Lokasi Tumor`+
               `Berat Tumor`+ASA+`Jenis Anestesi`+HB+`Status DM`+`Status Hipertensi`+
               `Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "efron")
summary(em21)
#Hapus Status Hipertensi

em22 = coxph(Y~Usia+`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+`Lokasi Tumor`+
               `Berat Tumor`+ASA+`Jenis Anestesi`+HB+`Status DM`+
               `Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "efron")
summary(em22)
#Hapus Status Hipertensi, Ukuran Tumor

em23 = coxph(Y~Usia+`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+`Lokasi Tumor`+
               ASA+`Jenis Anestesi`+HB+`Status DM`+
               `Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "efron")
summary(em23)
#Hapus Status Hipertensi, Berat Tumor, HB

em24 = coxph(Y~Usia+`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+`Lokasi Tumor`+
               ASA+`Jenis Anestesi`+`Status DM`+
               `Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "efron")
summary(em24)
#Hapus Status Hipertensi, Berat Tumor, HB, ASA

em25 = coxph(Y~Usia+`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+`Lokasi Tumor`+
               `Jenis Anestesi`+`Status DM`+
               `Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "efron")
summary(em25)
#Hapus Status Hipertensi, Berat Tumor, HB, ASA, Lokasi Tumor

em26 = coxph(Y~Usia+`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+
               `Jenis Anestesi`+`Status DM`+
               `Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "efron")
summary(em26)
#Hapus Status Hipertensi, Berat Tumor, HB, ASA, Lokasi Tumor, Usia

em27 = coxph(Y~`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+
               `Jenis Anestesi`+`Status DM`+
               `Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "efron")
summary(em27)
#Hapus Status Hipertensi, Berat Tumor, HB, ASA, Lokasi Tumor, Usia, Status DB

em28 = coxph(Y~`Jenis Kelamin`+`Jenis Operasi`+`Lama Operasi`+
               `Jenis Anestesi`+`Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "efron")
summary(em28)
#Hapus Status Hipertensi, Berat Tumor, HB, ASA, Lokasi Tumor, Usia, Status DB, Lama Operasi

em29 = coxph(Y~`Jenis Kelamin`+`Jenis Operasi`+
               `Jenis Anestesi`+`Status Pernikahan`+`Hu-Care Window SGRN`, data=Tumor, ties = "efron")
summary(em29)
#Hapus Status Hipertensi, Berat Tumor, HB, ASA, Lokasi Tumor, Usia, Status DB, Lama Operasi, Hu-Care Window

em30 = coxph(Y~`Jenis Kelamin`+`Jenis Operasi`+
               `Jenis Anestesi`+`Status Pernikahan`, data=Tumor, ties = "efron")
summary(em30)
#Hapus Status Hipertensi, Berat Tumor, HB, ASA, Lokasi Tumor, Usia, Status DB, Lama Operasi, Hu-Care Window, Jenis Kelamin

em31 = coxph(Y~`Jenis Operasi`+`Jenis Anestesi`+`Status Pernikahan`, data=Tumor, ties = "efron")
summary(em31)
#Signifikan
AIC(em31)
#=============================== Nilai AIC Breslow =====================================#
AIC (em1)
AIC (em2)
AIC (em3)
AIC (em4) 
AIC (em5)
AIC (em6)
AIC (em7)
AIC (em8)
AIC (em9)
AIC (em10)
AIC (em11) #Nilai Paling Bagus

#=============================== Nilai AIC Efron =====================================#
AIC (em21)
AIC (em22)
AIC (em23)
AIC (em24)
AIC (em25)
AIC (em26)
AIC (em27)
AIC (em28)
AIC (em29)
AIC (em30)
AIC (em31) # Paling Bagus

#=============================== Nilai Baseline Breslow =====================================#
#Library
library(survival)
library(ggplot2)
library(dplyr)

#Signifikansi Efron
em11 = coxph(Y~`Jenis Operasi`+`Jenis Anestesi`+`Status Pernikahan`, data=Tumor, ties = "breslow")
summary(em11)

# Menghitung baseline hazard
bh <- basehaz(em11, centered = FALSE)

# Hitung baseline hazard per waktu t
basehaz <- bh %>%
  arrange(time) %>%
  mutate(
    hazard = c(NA, diff(hazard)),
    hazard = ifelse(is.na(hazard), 0, hazard)  # ganti NA dengan 0
  )

print(basehaz)

# Plot baseline hazard terhadap waktu
ggplot(basehaz, aes(x = time, y = hazard)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "Baseline Hazard Breslow",
    x = "Waktu",
    y = "Baseline Hazard h₀(t)"
  ) +
  theme_minimal()
#=============================== Nilai Baseline Efron =====================================#
#Library
library(survival)
library(ggplot2)
library(dplyr)

#Signifikansi Efron
em31 = coxph(Y~`Jenis Operasi`+`Jenis Anestesi`+`Status Pernikahan`, data=Tumor, ties = "efron")
summary(em31)

# Menghitung baseline hazard
bh <- basehaz(em31, centered = FALSE)

# Hitung baseline hazard per waktu t
basehaz <- bh %>%
  arrange(time) %>%
  mutate(
    hazard = c(NA, diff(hazard)),
    hazard = ifelse(is.na(hazard), 0, hazard)  # ganti NA dengan 0
  )

print(basehaz)

# Plot baseline hazard terhadap waktu
ggplot(basehaz, aes(x = time, y = hazard)) +
  geom_line(color = "steelblue", size = 1) +
  labs(
    title = "Baseline Hazard Efron",
    x = "Waktu",
    y = "Baseline Hazard h₀(t)"
  ) +
  theme_minimal()
#===================================== Grafik h(t) Efron Rata-rata Hazard============================================#
#Library
library(survival)
library(ggplot2)
library(dplyr)

#Signifikansi Efron
em31 = coxph(Y~`Jenis Operasi`+`Jenis Anestesi`+`Status Pernikahan`, data=Tumor, ties = "efron")
summary(em31)

# Menghitung baseline hazard
bh <- basehaz(em31, centered = FALSE)

# Hitung baseline hazard per waktu t
basehaz <- bh %>%
  arrange(time) %>%
  mutate(
    hazard = c(NA, diff(hazard)),
    hazard = ifelse(is.na(hazard), 0, hazard)  # ganti NA dengan 0
  )

# Buat data pasien
pasien <- data.frame(
  check.names = FALSE,  # agar nama kolom bisa pakai spasi
  "Jenis Operasi" = factor("2", levels = levels(Tumor$`Jenis Operasi`)),
  "Jenis Anestesi" = factor("1", levels = levels(Tumor$`Jenis Anestesi`)),
  "Status Pernikahan" = factor("1", levels = levels(Tumor$`Status Pernikahan`))
)

# Hitung exp(β^T x)
exp_bx <- exp(predict(em31, newdata = pasien))

# Kalikan dengan exp(β^T x)
hazard_df <- basehaz %>%
  mutate(
    h_tx = hazard * exp_bx,     # hazard di sini adalah hasil diff(), yaitu h(t)
    H_tx = cumsum(hazard) * exp_bx  # cumulative hazard H(t;x)
  )

#Kurva h(t;x)
ggplot(hazard_df, aes(x = time, y = h_tx)) +
  geom_line(color = "blue") +
  labs(
    title = "Kurva Pemulihan h(t;x)",
    x = "Waktu (t)",
    y = expression(h(t,x))
  ) +
  theme_minimal()

#Kurva H(t;x)
ggplot(hazard_df, aes(x = time, y = H_tx)) +
  geom_line(color = "darkgreen") +
  labs(
    title = "Kurva Cumulative Hazard H(t;x)",
    x = "Waktu (t)",
    y = expression(H(t,x))
  ) +
  theme_minimal()