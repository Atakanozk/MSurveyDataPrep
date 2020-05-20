

#Data Preparation

#install.packages(readxl)
library(readxl)
Big_Data <- read_excel("C:/Users/ataka/OneDrive/Masaüstü/My_R_Project/Data.xlsx")
View(Big_Data)


str(Big_Data)#Strcuture of the data 

summary(Big_Data)

Big_Data$...1 <- NULL #deleting first column because of wrong xlsx format

#head(Big_Data)
#is.data.frame(Big_Data)
#Big_Data[!complete.cases(Big_Data),]
#Big_Data[is.na(Big_Data$YAŞ[rownames = "A Mahallesi"]),]
#Big_Data[!complete.cases(Big_Data$YAŞ[rownames="A Mahallesi"]),]

#Big_Data#CİNSİYET SHOULD BE A factor with 2 level(ERKEK==MALE, KADIN== FEMALE)
#Cinsiyet column has 4 levels they are:"kadın" "Kadın" "Erkek" "erkek"
#It should be changed into 2 levels with upper cases.
Big_Data$CİNSİYET <- factor(Big_Data$CİNSİYET)
?gsub
str(Big_Data$CİNSİYET)
Big_Data$CİNSİYET <- gsub("erkek","Erkek",Big_Data$CİNSİYET)#deleting "erkek" from 
Big_Data$CİNSİYET <- factor(Big_Data$CİNSİYET)

Big_Data[which(Big_Data$CİNSİYET=="kadın"),]
Big_Data$CİNSİYET <- gsub("kadın","Kadın",Big_Data$CİNSİYET)
Big_Data$CİNSİYET <- factor(Big_Data$CİNSİYET)
str(Big_Data$CİNSİYET)

colnames(Big_Data) 
names(Big_Data)[names(Big_Data)=="CİNSİYET"] <- "Cinsiyet(Gender)"

Big_Data[is.na(Big_Data$`Cinsiyet(Gender)`),1]
Big_Data[is.na(Big_Data$`Cinsiyet(Gender)`) & Big_Data$MAHALLE=="A Mahallesi",] # there are 64 NA's in Cinsiyet(Gender), not assingable a new value 

#to identify which nighborhood has NA gender on it------

library(tidyverse)

mah_gendf <- Big_Data %>% select(MAHALLE,`Cinsiyet(Gender)`) #subsetting columns"MAHALLE AND CİNSİYET(GENDER)"
mah_gendf[is.na(mah_gendf$`Cinsiyet(Gender)`),]#findig na values of gender column
#str(mah_gendf)
mah_gendf$MAHALLE <- factor(mah_gendf$MAHALLE)
#summary(mah_gendf)
mah_gendf[is.na(mah_gendf$MAHALLE),] # there is no NA neighborhood(good)

# Computing Neighborhoods with NA gender------------
mah_gendf[is.na(mah_gendf$`Cinsiyet(Gender)`),]#Total of 64 rows wit NA 
mah_gendf[is.na(mah_gendf$`Cinsiyet(Gender)`) & Big_Data$MAHALLE==paste("A Mahallesi"),]#NA genders in A Mahallesi(Nieghborhood)
mah_gendf$cnt <- 1:length(mah_gendf$MAHALLE)
mah_gendf <- mah_gendf[c(3,1,2)]
view(mah_gendf)
?matrix

#In order to find special neighborhood I created a function to make it more intractive Default<-"A Mahallesi"
fun_mah_gendf <- function(x="A"){
  if(x=="ALL"){
    print(mah_gendf[is.na(mah_gendf$`Cinsiyet(Gender)`),])
    NoNa <-c("Number of NA's:",length(!complete.cases(mah_gendf[is.na(mah_gendf$`Cinsiyet(Gender)`),])))
    NoNa
  }else{
  y <- paste(x,"Mahallesi")
print(mah_gendf[is.na(mah_gendf$`Cinsiyet(Gender)`) & mah_gendf$MAHALLE==y,])
NoNa <-c("Number of NA's:",length(!complete.cases(mah_gendf[is.na(mah_gendf$`Cinsiyet(Gender)`) & mah_gendf$MAHALLE==y,])))
NoNa
}}
fun_mah_gendf("ALL")#ALL neigborhoods with NA values and total number
fun_mah_gendf("B")#exapmle of "B Mahallesi"
fun_mah_gendf("C")#exapmle of "C Mahallesi"
str(mah_gendf)
levels(mah_gendf$MAHALLE)
#Data Prepa continues------

str(Big_Data)  
Big_Data$MAHALLE <- factor(Big_Data$MAHALLE) # Mahalle(Nieghborhood) is chr because of library(readxl) it should be factor with levels
names(Big_Data)[names(Big_Data)=="MAHALLE"] <- "Mahalle(District)" 
#In the Mahalle(District) column word of "Mahallesi" is not important because we already know that they are districts. I can delete them to look more clean
Big_Data$`Mahalle(District)` <- gsub("Mahallesi","",Big_Data$`Mahalle(District)`)
#str(Big_Data$`Mahalle(District)`)
Big_Data$`Mahalle(District)` <- factor(Big_Data$`Mahalle(District)`) 
#length(levels(Big_Data$`Mahalle(District)`))

Big_Data$Number <- 1:length(Big_Data$`Mahalle(District)`)
#Big_Datayedek <-  Big_Data 
Big_Data <- Big_Data[c(14,1,2,3,4,5,6,7,8,9,10,11,12,13)] 
Big_Data  
names(Big_Data)[names(Big_Data)=="YAŞ"] <- "Yas(Age)"  #Name change of YAŞ

#YAS(Age)----------------
Big_Data[!complete.cases(Big_Data$`Yas(Age)`),]#NA Age
mah_A_Erkek <- Big_Data[Big_Data$`Mahalle(District)`=="A "& Big_Data$`Cinsiyet(Gender)`=="Erkek",]
#Function for finding NA yas(age) for each district
fun_age <- function(x){
  if(x=="ALL"){
    print(Big_Data[!complete.cases(Big_Data$`Yas(Age)`),])
  }else{
    Z <- paste(x,"")
    print(Big_Data[Big_Data$`Mahalle(District)`==Z & is.na(Big_Data$`Yas(Age)`),])
  }}
fun_age("ALL")

fun_age("A")
#ADJUSTMENTS PARTICIPANTS AGE---------
#for District A there are only male(Erkek) participants so we need only erkek median to fill the NA values
fun_age("A")
#mah_A_Erkek <- Big_Data[Big_Data$`Mahalle(District)`=="A "& Big_Data$`Cinsiyet(Gender)`=="Erkek",] #data frame of District A's with "Erkek" gender
#median(mah_A_Erkek$`Yas(Age)`,na.rm = T)#Median of district A for male participants MEDIAN=="45"
Big_Data[Big_Data$Number==c(49,56),3] <-45 
fun_age("C")
mah_C_Erkek <- Big_Data[Big_Data$`Mahalle(District)`=="C "& Big_Data$`Cinsiyet(Gender)`=="Erkek",] #data frame of District A's with "Erkek" gender
median(mah_C_Erkek$`Yas(Age)`,na.rm = T)#Median of district C for male participants MEDIAN=="44"
#Big_Datayedek2 <- Big_Data
mah_c_Kadın <- Big_Data[Big_Data$`Mahalle(District)`=="C "& Big_Data$`Cinsiyet(Gender)`=="Kadın",] #data frame of District A's with "Erkek" gender
median(mah_c_Kadın$`Yas(Age)`,na.rm = T)#Median of district C for male participants MEDIAN=="39.5"
Big_Data[Big_Data$`Mahalle(District)`=="C " & is.na(Big_Data$`Yas(Age)`)& Big_Data$`Cinsiyet(Gender)`=="Kadın",3] <- 39

fun_age("M")
mah_M_Erkek <- Big_Data[Big_Data$`Mahalle(District)`=="M "& Big_Data$`Cinsiyet(Gender)`=="Erkek",] #data frame of District A's with "Erkek" gender
median(mah_M_Erkek$`Yas(Age)`,na.rm = T)#Median of district M for male participants MEDIAN=="35.5"
Big_Data[Big_Data$`Mahalle(District)`=="M " & is.na(Big_Data$`Yas(Age)`)& Big_Data$`Cinsiyet(Gender)`=="Erkek",3] <- 35


fun_age("R")
mah_R_Erkek <- Big_Data[Big_Data$`Mahalle(District)`=="R "& Big_Data$`Cinsiyet(Gender)`=="Erkek",] #data frame of District A's with "Erkek" gender
median(mah_R_Erkek$`Yas(Age)`,na.rm = T)#Median of district M for male participants MEDIAN=="52"
Big_Data[Big_Data$Number==1824,3] <- 52
Big_Data[Big_Data$Number==1870,3] <- 52
mah_R_Kadın <- Big_Data[Big_Data$`Mahalle(District)`=="R "& Big_Data$`Cinsiyet(Gender)`=="Kadın",] #data frame of District A's with "Erkek" gender
median(mah_R_Kadın$`Yas(Age)`,na.rm = T)#Median of district M for male participants MEDIAN=="47"
Big_Data[Big_Data$Number==1748,3] <- 47

fun_age("T")
mah_T_Erkek <- Big_Data[Big_Data$`Mahalle(District)`=="T "& Big_Data$`Cinsiyet(Gender)`=="Erkek",] #data frame of District A's with "Erkek" gender
median(mah_T_Erkek$`Yas(Age)`,na.rm = T)#Median of district M for male participants MEDIAN=="42.5"
Big_Data[Big_Data$Number==1937,3] <- 42 
mah_K_Erkek <- Big_Data[Big_Data$`Mahalle(District)`=="T "& Big_Data$`Cinsiyet(Gender)`=="Kadın",] #data frame of District A's with "Erkek" gender
median(mah_K_Erkek$`Yas(Age)`,na.rm = T)#Median of district M for male participants MEDIAN=="37.5"
Big_Data[Big_Data$Number==1950,3] <- 37 

fun_age("S")
mah_S_Erkek <- Big_Data[Big_Data$`Mahalle(District)`=="S "& Big_Data$`Cinsiyet(Gender)`=="Erkek",] #data frame of District A's with "Erkek" gender
median(mah_S_Erkek$`Yas(Age)`,na.rm = T)#Median of district M for male participants MEDIAN=="50"
Big_Data[Big_Data$`Mahalle(District)`=="S " & is.na(Big_Data$`Yas(Age)`)& Big_Data$`Cinsiyet(Gender)`=="Erkek",3] <- 50
mah_S_Kadın <- Big_Data[Big_Data$`Mahalle(District)`=="S "& Big_Data$`Cinsiyet(Gender)`=="Kadın",] #data frame of District A's with "Erkek" gender
median(mah_S_Kadın$`Yas(Age)`,na.rm = T)#Median of district M for male participants MEDIAN=="45"
Big_Data[Big_Data$`Mahalle(District)`=="S " & is.na(Big_Data$`Yas(Age)`)& Big_Data$`Cinsiyet(Gender)`=="Kadın",3] <- 45

fun_age("U")
mah_U_Erkek <- Big_Data[Big_Data$`Mahalle(District)`=="U "& Big_Data$`Cinsiyet(Gender)`=="Erkek",] #data frame of District A's with "Erkek" gender
median(mah_U_Erkek$`Yas(Age)`,na.rm = T)#Median of district M for male participants MEDIAN=="47
Big_Data[Big_Data$`Mahalle(District)`=="U " & is.na(Big_Data$`Yas(Age)`)& Big_Data$`Cinsiyet(Gender)`=="Erkek",3] <- 47

fun_age("ALL")# There are 6 participant we dont know their gender so we can not assign age (Districts:A,G,P,R,T)
#Big_Datayedek3 <- Big_Data

#4 columns are ready for analysis
# Adjustment to "Meslek" column--------------------------
str(Big_Data)
#str(Big_Data$MESLEK)
names(Big_Data)[names(Big_Data)=="MESLEK"] <- "Meslek(Profession)"#name change
Big_Data$`Meslek(Profession)` <- factor(Big_Data$`Meslek(Profession)`)
#levels(Big_Data$`Meslek(Profession)`)
#str(Big_Data)
#summary(Big_Data$`Meslek(Profession)`)
#Big_Data_yedek <- Big_Data
#There are upper case words and lower case word for the same word. I need to change them into 1 lower case version of the same word.
Big_Data$`Meslek(Profession)` <- as.character(Big_Data$`Meslek(Profession)`)#They need to be character
#Big_Data$`Meslek(Profession)`[Big_Data$`Meslek(Profession)`=="Emekli"]

#install.packages("stringi")
#library(stringi)
#stri_trans_totitle(Big_Data$`Meslek(Profession)`)
Big_Data$`Meslek(Profession)`
Big_Data$`Meslek(Profession)` <- tolower(Big_Data$`Meslek(Profession)`)
Big_Data$`Meslek(Profession)` <- factor(Big_Data$`Meslek(Profession)`)
str(Big_Data$`Meslek(Profession)`)
#now every category has only lower case letters and they are different. total of 453 levels
#NA proffesion should be found
Big_Data[!complete.cases(Big_Data$`Meslek(Profession)`),]
Big_Data[is.na(Big_Data$`Meslek(Profession)`),]#There are total 25 rows with NA profession but I cant find a way to fill them so they will be stay as NA

#Question 1:Belediyemiz gelcekte olmassa olmaz dediğiniz konu nedir?--------
#Big_Datayedek <- Big_Data
names(Big_Data)[names(Big_Data)=="Belediyemizin gelecek planları için olmazsa olmaz dediğiniz konu nedir ?"] <- "Q1"#name change(What is the most imporant subject that our municipality must fix?)
summary(Big_Data$Q1)
str(Big_Data)

#This question is multiple response question like other questions. But in the data they are written in the same cell so I have to create another data frame for each question
q1_raw <- Big_Data %>% select(Number,Q1) #selecting q1
q1_raw$Q1 <- factor(q1_raw$Q1)
levels(q1_raw$Q1)
str(q1_raw)
newvectorq1 <- c("Altyapı ve Üstyapı Çalışmaları","Daha İyi Hizmet","Kültür,Sanat ve Spor","Teknolojik Altyapı","Çevre Düzenleme",
                 "Esnafa Destek","İmar,İskan ve Tapu","Veterinerlik Hiz","Çevre Temizliği ve İlaçlama","Eğitim ve Kreş İmkanları",
                 "Huzur ve Güven Ortamı","Yeşil Alan","İş İmkanları","Sosyal Sorumluluk","Yaşlı ve Engelli","Ulaşım",
                 "Hastane","Otopark","Kentsel Dönüşüm","Doğal Afetler")

q1df[,newvectorq1]<-NA
q1df$Q1 <- q1_raw$Q1
#SAME names columns fix
#q1df$`Daha İyi Hizmet.1` <- NULL
#q1df$`Teknolojik Altyapı.1` <- NULL
#q1df$`Esnafa Destek.1` <- NULL


#There should be a loop for assing correct answers to df
#--1
for (i in 1:2245) {
  if(grepl("Altyapı ve Üstyapı Çalışmaları",q1df$Q1[i])==T){
    q1df$`Altyapı ve Üstyapı Çalışmaları`[i] <- 1
  }else if(grepl("Altyapı ve Üstyapı Çalışmaları",q1df$Q1[i])==F){
    q1df$`Altyapı ve Üstyapı Çalışmaları`[i] <- 0
  }
}
#--2
for (i in 1:2245) {
  if(grepl("Daha İyi Hizmet",q1df$Q1[i])==T){
    q1df$`Daha İyi Hizmet` [i] <- 1
  }else if(grepl("Daha İyi Hizmet",q1df$Q1[i])==F){
    q1df$`Daha İyi Hizmet` [i] <- 0
  }
}
#q1dfyedek <- q1df
#--3
for (i in 1:2245) {
  if(grepl("Kültür,Sanat ve Spor",q1df$Q1[i])==T){
    q1df$`Kültür,Sanat ve Spor` [i] <- 1
  }else if(grepl("Kültür,Sanat ve Spor",q1df$Q1[i])==F){
    q1df$`Kültür,Sanat ve Spor` [i] <- 0
  }
}
#--4
for (i in 1:2245) {
  if(grepl("Teknolojik Altyapı",q1df$Q1[i])==T){
    q1df$`Teknolojik Altyapı` [i] <- 1
  }else if(grepl("Teknolojik Altyapı",q1df$Q1[i])==F){
    q1df$`Teknolojik Altyapı` [i] <- 0
  }
}
#--5
for (i in 1:2245) {
  if(grepl("Çevre Düzenleme",q1df$Q1[i])==T){
    q1df$`Çevre Düzenleme` [i] <- 1
  }else if(grepl("Çevre Düzenleme",q1df$Q1[i])==F){
    q1df$`Çevre Düzenleme` [i] <- 0
  }
}
#--6
for (i in 1:2245) {
  if(grepl("Esnafa Destek",q1df$Q1[i])==T){
    q1df$`Esnafa Destek` [i] <- 1
  }else if(grepl("Esnafa Destek",q1df$Q1[i])==F){
    q1df$`Esnafa Destek` [i] <- 0
  }
}
#--7
for (i in 1:2245) {
  if(grepl("İmar,İskan ve Tapu",q1df$Q1[i])==T){
    q1df$`İmar,İskan ve Tapu` [i] <- 1
  }else if(grepl("İmar,İskan ve Tapu",q1df$Q1[i])==F){
    q1df$`İmar,İskan ve Tapu` [i] <- 0
  }
}
#--8
for (i in 1:2245) {
  if(grepl("Veterinerlik Hiz",q1df$Q1[i])==T){
    q1df$`Veterinerlik Hiz`[i] <- 1
  }else if(grepl("Veterinerlik Hiz",q1df$Q1[i])==F){
    q1df$`Veterinerlik Hiz` [i] <- 0
  }
}
#--9
for (i in 1:2245) {
  if(grepl("Çevre Temizliği ve İlaçlama",q1df$Q1[i])==T){
    q1df$`Çevre Temizliği ve İlaçlama` [i] <- 1
  }else if(grepl("Çevre Temizliği ve İlaçlama",q1df$Q1[i])==F){
    q1df$`Çevre Temizliği ve İlaçlama` [i] <- 0
  }
}
#--10
for (i in 1:2245) {
  if(grepl("Eğitim ve Kreş İmkanları",q1df$Q1[i])==T){
    q1df$`Eğitim ve Kreş İmkanları` [i] <- 1
  }else if(grepl("Eğitim ve Kreş İmkanları",q1df$Q1[i])==F){
    q1df$`Eğitim ve Kreş İmkanları`[i] <- 0
  }
}
#--11
for (i in 1:2245) {
  if(grepl("Huzur ve Güven Ortamı",q1df$Q1[i])==T){
    q1df$`Huzur ve Güven Ortamı` [i] <- 1
  }else if(grepl("Huzur ve Güven Ortamı",q1df$Q1[i])==F){
    q1df$`Huzur ve Güven Ortamı` [i] <- 0
  }
}
#--12
for (i in 1:2245) {
  if(grepl("Yeşil Alan",q1df$Q1[i])==T){
    q1df$`Yeşil Alan` [i] <- 1
  }else if(grepl("Yeşil Alan",q1df$Q1[i])==F){
    q1df$`Yeşil Alan` [i] <- 0
  }
}
#--13
for (i in 1:2245) {
  if(grepl("İş İmkanları",q1df$Q1[i])==T){
    q1df$`İş İmkanları` [i] <- 1
  }else if(grepl("İş İmkanları",q1df$Q1[i])==F){
    q1df$`İş İmkanları` [i] <- 0
  }
}
#--14
for (i in 1:2245) {
  if(grepl("Sosyal Sorumluluk",q1df$Q1[i])==T){
    q1df$`Sosyal Sorumluluk`[i] <- 1
  }else if(grepl("Sosyal Sorumluluk",q1df$Q1[i])==F){
    q1df$`Sosyal Sorumluluk` [i] <- 0
  }
}
#--15
for (i in 1:2245) {
  if(grepl("Yaşlı ve Engelli",q1df$Q1[i])==T){
    q1df$`Yaşlı ve Engelli` [i] <- 1
  }else if(grepl("Yaşlı ve Engelli",q1df$Q1[i])==F){
    q1df$`Yaşlı ve Engelli` [i] <- 0
  }
}
#--16
for (i in 1:2245) {
  if(grepl("Ulaşım",q1df$Q1[i])==T){
    q1df$Ulaşım [i] <- 1
  }else if(grepl("Ulaşım",q1df$Q1[i])==F){
    q1df$Ulaşım [i] <- 0
  }
}
#--17
for (i in 1:2245) {
  if(grepl("Hastane",q1df$Q1[i])==T){
    q1df$Hastane [i] <- 1
  }else if(grepl("Hastane",q1df$Q1[i])==F){
    q1df$Hastane [i] <- 0
  }
}
#--18
for (i in 1:2245) {
  if(grepl("Otopark",q1df$Q1[i])==T){
    q1df$Otopark [i] <- 1
  }else if(grepl("Otopark",q1df$Q1[i])==F){
    q1df$Otopark [i] <- 0
  }
}
#--19
for (i in 1:2245) {
  if(grepl("Kentsel Dönüşüm",q1df$Q1[i])==T){
    q1df$`Kentsel Dönüşüm` [i] <- 1
  }else if(grepl("Kentsel Dönüşüm",q1df$Q1[i])==F){
    q1df$`Kentsel Dönüşüm` [i] <- 0
  }
}
#--20
for (i in 1:2245) {
  if(grepl("Doğal Afetler",q1df$Q1[i])==T){
    q1df$`Doğal Afetler` [i] <- 1
  }else if(grepl("Doğal Afetler",q1df$Q1[i])==F){
    q1df$`Doğal Afetler` [i] <- 0
  }
}
#The structure is fixed of the first question
#str(q1df)
#sum(q1df$`Altyapı ve Üstyapı Çalışmaları`)
#summary(q1df)
#q1df2 <- q1df yedekleme
names(q1df2)[names(q1df2)=="q1_raw.Number"] <- "Number"
Big_Data_With_Questions <- merge(Big_Data,q1df2, by = "Number") #merging subsetted data frmae into big data set

#Question 2:Belediyemizden EN büyük beklentiniz nedir?--------
#Q2 has multiple reposnse so I have to store them in another df with responses(like Q1)
names(Big_Data)[names(Big_Data)=="Belediyemizden EN büyük beklentiniz nedir ?"] <- "Q2"#name change(What is your biggest expactation from our municipality?)
names(Big_Data_With_Questions)[names(Big_Data_With_Questions)=="Belediyemizden EN büyük beklentiniz nedir ?"] <- "Q2"
Big_Data$Q2 <- factor(Big_Data$Q2)
Big_Data$Q2
levels(Big_Data$Q2)
#selecting into another data frame
q2df<- Big_Data %>% select(Number,Q2)
q2df
newvectorq2 <- c("Adil Yönetim ve Şeffaf Bir Yönetim","Altyapı ve Üstyapı Çalışmları","Daha İyi Hizmet","Ulaşım","Oyun Parkı ve Eğlence Alanları",
                 "Çevre Düzenleme","Eğitim ve Kreşler","İmar,İskan ve Tapu","İş İmkanları","Otopark Alanları","Sokak Hayvanlarına Destek",
                 "Yaşlı ve Engellilere Hizmet","Yeşil Alan","Çevre Temizliği","Huzur ve Güven","Sağlık İmkanları","Sosyal Sorumluluk Projeleri",
                 "Sosyal Yardım","Esnafa Destek","Kültür,Sanat ve Spor","Sosyal ve Duyarlı Bir Belediye")

q2df[,newvectorq2]<-NA
q2df$Q2 <- Big_Data$Q2
#I need to fill q2df corespond to answers from participant.
#--1
for (i in 1:2245) {
  if(grepl("Adil Yönetim ve Şeffaf Bir Yönetim",q2df$Q2[i])==T){
    q2df[i,2] <- 1
  }else if(grepl("Adil Yönetim ve Şeffaf Bir Yönetim",q2df$Q2[i])==F){
    q2df[i,2] <- 0
  }
}
#--2
for (i in 1:2245) {
  if(grepl("Altyapı ve Üstyapı Çalışmları",q2df$Q2[i])==T){
    q2df[i,match("Altyapı ve Üstyapı Çalışmları",names(q2df))] <- 1
  }else if(grepl("Altyapı ve Üstyapı Çalışmları",q2df$Q2[i])==F){
    q2df[i,match("Altyapı ve Üstyapı Çalışmları",names(q2df))] <- 0
  }
}
#--3
for (i in 1:2245) {
  if(grepl("Daha İyi Hizmet",q2df$Q2[i])==T){
    q2df[i,match("Daha İyi Hizmet",names(q2df))] <- 1
  }else if(grepl("Daha İyi Hizmet",q2df$Q2[i])==F){
    q2df[i,match("Daha İyi Hizmet",names(q2df))] <- 0
  }
}
#--4
for (i in 1:2245) {
  if(grepl("Ulaşım",q2df$Q2[i])==T){
    q2df[i,5] <- 1
  }else if(grepl("Ulaşım",q2df$Q2[i])==F){
    q2df[i,5] <- 0
  }
}
#--5
for (i in 1:2245) {
  if(grepl("Oyun Parkı ve Eğlence Alanları",q2df$Q2[i])==T){
    q2df[i,6] <- 1
  }else if(grepl("Oyun Parkı ve Eğlence Alanları",q2df$Q2[i])==F){
    q2df[i,6] <- 0
  }
}
#--6
for (i in 1:2245) {
  if(grepl("Çevre Düzenleme",q2df$Q2[i])==T){
    q2df[i,7] <- 1
  }else if(grepl("Çevre Düzenleme",q2df$Q2[i])==F){
    q2df[i,7] <- 0
  }
}
#--7
for (i in 1:2245) {
  if(grepl("Eğitim ve Kreşler",q2df$Q2[i])==T){
    q2df[i,8] <- 1
  }else if(grepl("Eğitim ve Kreşler",q2df$Q2[i])==F){
    q2df[i,8] <- 0
  }
}
#--8
for (i in 1:2245) {
  if(grepl("İmar,İskan ve Tapu",q2df$Q2[i])==T){
    q2df[i,9] <- 1
  }else if(grepl("İmar,İskan ve Tapu",q2df$Q2[i])==F){
    q2df[i,9] <- 0
  }
}
#--9
for (i in 1:2245) {
  if(grepl("İş İmkanları",q2df$Q2[i])==T){
    q2df[i,10] <- 1
  }else if(grepl("İş İmkanları",q2df$Q2[i])==F){
    q2df[i,10] <- 0
  }
}
#--10
for (i in 1:2245) {
  if(grepl("Otopark Alanları",q2df$Q2[i])==T){
    q2df[i,11] <- 1
  }else if(grepl("Otopark Alanları",q2df$Q2[i])==F){
    q2df[i,11] <- 0
  }
}
#--11
for (i in 1:2245) {
  if(grepl("Sokak Hayvanlarına Destek",q2df$Q2[i])==T){
    q2df[i,12] <- 1
  }else if(grepl("Sokak Hayvanlarına Destek",q2df$Q2[i])==F){
    q2df[i,12] <- 0
  }
}
#--12
for (i in 1:2245) {
  if(grepl("Yaşlı ve Engellilere Hizmet",q2df$Q2[i])==T){
    q2df[i,13] <- 1
  }else if(grepl("Yaşlı ve Engellilere Hizmet",q2df$Q2[i])==F){
    q2df[i,13] <- 0
  }
}
#--13
for (i in 1:2245) {
  if(grepl("Yeşil Alan",q2df$Q2[i])==T){
    q2df[i,14] <- 1
  }else if(grepl("Yeşil Alan",q2df$Q2[i])==F){
    q2df[i,14] <- 0
  }
}
#--14
for (i in 1:2245) {
  if(grepl("Çevre Temizliği",q2df$Q2[i])==T){
    q2df[i,15] <- 1
  }else if(grepl("Çevre Temizliği",q2df$Q2[i])==F){
    q2df[i,15] <- 0
  }
}
#--15
for (i in 1:2245) {
  if(grepl("Huzur ve Güven",q2df$Q2[i])==T){
    q2df[i,16] <- 1
  }else if(grepl("Huzur ve Güven",q2df$Q2[i])==F){
    q2df[i,16] <- 0
  }
}
#--16
for (i in 1:2245) {
  if(grepl("Sağlık İmkanları",q2df$Q2[i])==T){
    q2df[i,17] <- 1
  }else if(grepl("Sağlık İmkanları",q2df$Q2[i])==F){
    q2df[i,17] <- 0
  }
}
#--17
for (i in 1:2245) {
  if(grepl("Sosyal Sorumluluk Projeleri",q2df$Q2[i])==T){
    q2df[i,18] <- 1
  }else if(grepl("Sosyal Sorumluluk Projeleri",q2df$Q2[i])==F){
    q2df[i,18] <- 0
  }
}
#--18
for (i in 1:2245) {
  if(grepl("Sosyal Yardım",q2df$Q2[i])==T){
    q2df[i,19] <- 1
  }else if(grepl("Sosyal Yardım",q2df$Q2[i])==F){
    q2df[i,19] <- 0
  }
}
#--19
for (i in 1:2245) {
  if(grepl("Esnafa Destek",q2df$Q2[i])==T){
    q2df[i,20] <- 1
  }else if(grepl("Esnafa Destek",q2df$Q2[i])==F){
    q2df[i,20] <- 0
  }
}
#--20
for (i in 1:2245) {
  if(grepl("Kültür,Sanat ve Spor",q2df$Q2[i])==T){
    q2df[i,21] <- 1
  }else if(grepl("Kültür,Sanat ve Spor",q2df$Q2[i])==F){
    q2df[i,21] <- 0
  }
}
#--21
for (i in 1:2245) {
  if(grepl("Sosyal ve Duyarlı Bir Belediye",q2df$Q2[i])==T){
    q2df[i,22] <- 1
  }else if(grepl("Sosyal ve Duyarlı Bir Belediye",q2df$Q2[i])==F){
    q2df[i,22] <- 0
  }
}
#q2dfyedek <- q2df
Big_Data_With_Questions2 <- merge(Big_Data_With_Questions,q2df, by = "Number") #merging subsetted data frmae into big data set
#change to logical for the first 2 question----
#they need to be logical 
#Identifying col numbers
names(Big_Data_With_Questions2[15])
names(Big_Data_With_Questions2[34])
names(Big_Data_With_Questions2[15:34])
#q1 logical convertion
Big_Data_With_Questions2$`Altyapı ve Üstyapı Çalışmaları`<-as.logical(Big_Data_With_Questions2$`Altyapı ve Üstyapı Çalışmaları`)
Big_Data_With_Questions2$`Daha İyi Hizmet.x` <-as.logical(Big_Data_With_Questions2$`Daha İyi Hizmet.x`)
Big_Data_With_Questions2$`Kültür,Sanat ve Spor.x` <- as.logical(Big_Data_With_Questions2$`Kültür,Sanat ve Spor.x`)
Big_Data_With_Questions2$`Teknolojik Altyapı` <- as.logical(Big_Data_With_Questions2$`Teknolojik Altyapı`)
Big_Data_With_Questions2$`Çevre Düzenleme.x` <- as.logical(Big_Data_With_Questions2$`Çevre Düzenleme.x`)
Big_Data_With_Questions2$`Esnafa Destek.x`<- as.logical(Big_Data_With_Questions2$`Esnafa Destek.x`)
Big_Data_With_Questions2$`İmar,İskan ve Tapu.x` <- as.logical(Big_Data_With_Questions2$`İmar,İskan ve Tapu.x`)
Big_Data_With_Questions2$`Veterinerlik Hiz` <- as.logical(Big_Data_With_Questions2$`Veterinerlik Hiz`)
Big_Data_With_Questions2$`Çevre Temizliği ve İlaçlama` <- as.logical(Big_Data_With_Questions2$`Çevre Temizliği ve İlaçlama`)
Big_Data_With_Questions2$`Eğitim ve Kreş İmkanları` <- as.logical(Big_Data_With_Questions2$`Eğitim ve Kreş İmkanları`)
Big_Data_With_Questions2$`Huzur ve Güven Ortamı` <- as.logical(Big_Data_With_Questions2$`Huzur ve Güven Ortamı`)
Big_Data_With_Questions2$`Yeşil Alan.x` <- as.logical(Big_Data_With_Questions2$`Yeşil Alan.x`)
Big_Data_With_Questions2$`İş İmkanları.x` <- as.logical(Big_Data_With_Questions2$`İş İmkanları.x`)
Big_Data_With_Questions2$`Sosyal Sorumluluk` <- as.logical(Big_Data_With_Questions2$`Sosyal Sorumluluk`)
Big_Data_With_Questions2$`Yaşlı ve Engelli` <- as.logical(Big_Data_With_Questions2$`Yaşlı ve Engelli`)
Big_Data_With_Questions2$Ulaşım.x <- as.logical(Big_Data_With_Questions2$Ulaşım.x)
Big_Data_With_Questions2$Hastane <- as.logical(Big_Data_With_Questions2$Hastane)
Big_Data_With_Questions2$Otopark <- as.logical(Big_Data_With_Questions2$Otopark)
Big_Data_With_Questions2$`Kentsel Dönüşüm` <- as.logical(Big_Data_With_Questions2$`Kentsel Dönüşüm`)
Big_Data_With_Questions2$`Doğal Afetler`<-as.logical(Big_Data_With_Questions2$`Doğal Afetler`)
#q2 logical conversion
Big_Data_With_Questions2$`Adil Yönetim ve Şeffaf Bir Yönetim`<-as.logical(Big_Data_With_Questions2$`Adil Yönetim ve Şeffaf Bir Yönetim`)
Big_Data_With_Questions2$`Altyapı ve Üstyapı Çalışmları` <- as.logical(Big_Data_With_Questions2$`Altyapı ve Üstyapı Çalışmları`)
Big_Data_With_Questions2$`Daha İyi Hizmet.y` <- as.logical(Big_Data_With_Questions2$`Daha İyi Hizmet.y`)
Big_Data_With_Questions2$Ulaşım.y <- as.logical(Big_Data_With_Questions2$Ulaşım.y)
Big_Data_With_Questions2$`Oyun Parkı ve Eğlence Alanları` <- as.logical(Big_Data_With_Questions2$`Oyun Parkı ve Eğlence Alanları`)
Big_Data_With_Questions2$`Çevre Düzenleme.y` <- as.logical(Big_Data_With_Questions2$`Çevre Düzenleme.y`)
Big_Data_With_Questions2$`Eğitim ve Kreşler` <- as.logical(Big_Data_With_Questions2$`Eğitim ve Kreşler`)
Big_Data_With_Questions2$`İmar,İskan ve Tapu.y` <- as.logical(Big_Data_With_Questions2$`İmar,İskan ve Tapu.y`)
Big_Data_With_Questions2$`İş İmkanları.y` <- as.logical(Big_Data_With_Questions2$`İş İmkanları.y`)
Big_Data_With_Questions2$`Otopark Alanları` <- as.logical(Big_Data_With_Questions2$`Otopark Alanları`)
Big_Data_With_Questions2$`Sokak Hayvanlarına Destek`<-as.logical(Big_Data_With_Questions2$`Sokak Hayvanlarına Destek`)
Big_Data_With_Questions2$`Yaşlı ve Engellilere Hizmet` <- as.logical(Big_Data_With_Questions2$`Yaşlı ve Engellilere Hizmet`)
Big_Data_With_Questions2$`Yeşil Alan.y` <- as.logical(Big_Data_With_Questions2$`Yeşil Alan.y`)
Big_Data_With_Questions2$`Çevre Temizliği` <- as.logical(Big_Data_With_Questions2$`Çevre Temizliği`)
Big_Data_With_Questions2$`Huzur ve Güven` <- as.logical(Big_Data_With_Questions2$`Huzur ve Güven`)
Big_Data_With_Questions2$`Sağlık İmkanları`<- as.logical(Big_Data_With_Questions2$`Sağlık İmkanları`)
Big_Data_With_Questions2$`Sosyal Sorumluluk Projeleri`<-as.logical(Big_Data_With_Questions2$`Sosyal Sorumluluk Projeleri`)
Big_Data_With_Questions2$`Sosyal Yardım` <- as.logical(Big_Data_With_Questions2$`Sosyal Yardım`)
Big_Data_With_Questions2$`Esnafa Destek.y`<-as.logical(Big_Data_With_Questions2$`Esnafa Destek.y`)
Big_Data_With_Questions2$`Kültür,Sanat ve Spor.y`<-as.logical(Big_Data_With_Questions2$`Kültür,Sanat ve Spor.y`)
Big_Data_With_Questions2$`Sosyal ve Duyarlı Bir Belediye` <- as.logical(Big_Data_With_Questions2$`Sosyal ve Duyarlı Bir Belediye`)
#str(Big_Data_With_Questions2)
#q3 Adjustments----

names(Big_Data)[names(Big_Data)=="Belediye ile yaşadığınız EN büyük sorun nedir?"] <- "Q3"#name change(What is the biggest problem with municipality?)
Big_Data$Q3 <- factor(Big_Data$Q3)
levels(Big_Data$Q3)
newvectorq3 <- c("Yaşamadım","Altyapı ve Üstyapı","Ulaşım ve Otopark","Çevre Düzenleme","Zabıta ve Denetim","Çevre Temizliği",
                 "İletişim ve Adil Yönetim","Zayıf Hizmet","Huzur ve Güvenlik","İmar,İskan ve Tapu","Kentsel Dönüşüm","Personel Problemleri",
                 "Sokak Hayvanları","Sosyal Tesis Fiyatlarında Düzenleme","Vergi Düzenlemeri","Yaşlılar ve Engelliler",
                 "Yeşil Alan","Yetersiz Kreş Sayısı","Bütçe")
q3df<- Big_Data %>% select(Number)
q3df[,newvectorq3]<-NA
q3df$`Zabıta ve Denetim.1`<-NULL
q3df$Q3 <- Big_Data$Q3
#--1
for (i in 1:2245) {
  if(grepl("Yaşamadım",q3df$Q3[i])==T){
    q3df[i,2] <- 1
  }else if(grepl("Yaşamadım",q3df$Q3[i])==F){
    q3df[i,2] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Altyapı ve Üstyapı",q3df$Q3[i])==T){
    q3df[i,3] <- 1
  }else if(grepl("Altyapı ve Üstyapı",q3df$Q3[i])==F){
    q3df[i,3] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Ulaşım ve Otopark",q3df$Q3[i])==T){
    q3df[i,4] <- 1
  }else if(grepl("Ulaşım ve Otopark",q3df$Q3[i])==F){
    q3df[i,4] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Çevre Düzenleme",q3df$Q3[i])==T){
    q3df[i,5] <- 1
  }else if(grepl("Çevre Düzenleme",q3df$Q3[i])==F){
    q3df[i,5] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Zabıta ve Denetim",q3df$Q3[i])==T){
    q3df[i,6] <- 1
  }else if(grepl("Zabıta ve Denetim",q3df$Q3[i])==F){
    q3df[i,6] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Çevre Temizliği",q3df$Q3[i])==T){
    q3df[i,7] <- 1
  }else if(grepl("Çevre Temizliği",q3df$Q3[i])==F){
    q3df[i,7] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("İletişim ve Adil Yönetim",q3df$Q3[i])==T){
    q3df[i,8] <- 1
  }else if(grepl("İletişim ve Adil Yönetim",q3df$Q3[i])==F){
    q3df[i,8] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Zayıf Hizmet",q3df$Q3[i])==T){
    q3df[i,9] <- 1
  }else if(grepl("Zayıf Hizmet",q3df$Q3[i])==F){
    q3df[i,9] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Huzur ve Güvenlik",q3df$Q3[i])==T){
    q3df[i,10] <- 1
  }else if(grepl("Huzur ve Güvenlik",q3df$Q3[i])==F){
    q3df[i,10] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("İmar,İskan ve Tapu",q3df$Q3[i])==T){
    q3df[i,11] <- 1
  }else if(grepl("İmar,İskan ve Tapu",q3df$Q3[i])==F){
    q3df[i,11] <- 0
  }
}

for (i in 1:2245) {
  if(grepl("Kentsel Dönüşüm",q3df$Q3[i])==T){
    q3df[i,12] <- 1
  }else if(grepl("Kentsel Dönüşüm",q3df$Q3[i])==F){
    q3df[i,12] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Personel Problemleri",q3df$Q3[i])==T){
    q3df[i,13] <- 1
  }else if(grepl("Personel Problemleri",q3df$Q3[i])==F){
    q3df[i,13] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Sokak Hayvanları",q3df$Q3[i])==T){
    q3df[i,14] <- 1
  }else if(grepl("Sokak Hayvanları",q3df$Q3[i])==F){
    q3df[i,14] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Sosyal Tesis Fiyatlarında Düzenleme",q3df$Q3[i])==T){
    q3df[i,15] <- 1
  }else if(grepl("Sosyal Tesis Fiyatlarında Düzenleme",q3df$Q3[i])==F){
    q3df[i,15] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Vergi Düzenlemeri",q3df$Q3[i])==T){
    q3df[i,16] <- 1
  }else if(grepl("Vergi Düzenlemeri",q3df$Q3[i])==F){
    q3df[i,16] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Yaşlılar ve Engelliler",q3df$Q3[i])==T){
    q3df[i,17] <- 1
  }else if(grepl("Yaşlılar ve Engelliler",q3df$Q3[i])==F){
    q3df[i,17] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Yeşil Alan",q3df$Q3[i])==T){
    q3df[i,18] <- 1
  }else if(grepl("Yeşil Alan",q3df$Q3[i])==F){
    q3df[i,18] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Yetersiz Kreş Sayısı",q3df$Q3[i])==T){
    q3df[i,19] <- 1
  }else if(grepl("Yetersiz Kreş Sayısı",q3df$Q3[i])==F){
    q3df[i,19] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Bütçe",q3df$Q3[i])==T){
    q3df[i,20] <- 1
  }else if(grepl("Bütçe",q3df$Q3[i])==F){
    q3df[i,20] <- 0
  }
}
Big_Data_With_Questions3 <- merge(Big_Data_With_Questions2,q3df, by = "Number")
Question_List <- list("Q1"=q1df,"Q2"=q2df,"Q3"=q3df) #for holding data frames as binary with 1 and 0, I created a list
#Question_List

#Q4 Adjustments----
names(Big_Data)[names(Big_Data)=="Sizce Belediyemizin değerlendirmesi gereken EN büyük fırsat nedir ?"] <- "Q4"#name change(What is the most important oppurtunity that municipality should fix?)
Big_Data$Q4 <- factor(Big_Data$Q4)
levels(Big_Data$Q4)

newvectorq4 <- c("Bakım Evleri","Başkanın Genç ve Dinamik Olması","Gençlik ve Çocuklar","Büyükşehir ile Koordinasyon",
                 "Halk Desteği","Hizmet İçin Zaman","Coğrafi Konum","Değerlendirilecek Meydanlar",
                 "Vaatler","Deniz Ulaşım İmkanları","Eğitim ve Kreşlere Verilecek Önem")
q4df<- Big_Data %>% select(Number)
q4df[,newvectorq4]<-NA
q4df$`Gençlik ve Çocuklar.1`<-NULL
q4df$Q4 <- Big_Data$Q4

for (i in 1:2245) {
  if(grepl("Bakım Evleri",q4df$Q4[i])==T){
    q4df[i,2] <- 1
  }else if(grepl("Bakım Evleri",q4df$Q4[i])==F){
    q4df[i,2] <- 0
  }
}

for (i in 1:2245) {
  if(grepl("Başkanın Genç ve Dinamik Olması",q4df$Q4[i])==T){
    q4df[i,3] <- 1
  }else if(grepl("Başkanın Genç ve Dinamik Olması",q4df$Q4[i])==F){
    q4df[i,3] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Gençlik ve Çocuklar",q4df$Q4[i])==T){
    q4df[i,4] <- 1
  }else if(grepl("Gençlik ve Çocuklar",q4df$Q4[i])==F){
    q4df[i,4] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Büyükşehir ile Koordinasyon",q4df$Q4[i])==T){
    q4df[i,5] <- 1
  }else if(grepl("Büyükşehir ile Koordinasyon",q4df$Q4[i])==F){
    q4df[i,5] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Halk Desteği",q4df$Q4[i])==T){
    q4df[i,6] <- 1
  }else if(grepl("Halk Desteği",q4df$Q4[i])==F){
    q4df[i,6] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Hizmet İçin Zaman",q4df$Q4[i])==T){
    q4df[i,7] <- 1
  }else if(grepl("Hizmet İçin Zaman",q4df$Q4[i])==F){
    q4df[i,7] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Coğrafi Konum",q4df$Q4[i])==T){
    q4df[i,8] <- 1
  }else if(grepl("Coğrafi Konum",q4df$Q4[i])==F){
    q4df[i,8] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Değerlendirilecek Meydanlar",q4df$Q4[i])==T){
    q4df[i,9] <- 1
  }else if(grepl("Değerlendirilecek Meydanlar",q4df$Q4[i])==F){
    q4df[i,9] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Vaatler",q4df$Q4[i])==T){
    q4df[i,10] <- 1
  }else if(grepl("Vaatler",q4df$Q4[i])==F){
    q4df[i,10] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Deniz Ulaşım İmkanları",q4df$Q4[i])==T){
    q4df[i,11] <- 1
  }else if(grepl("Deniz Ulaşım İmkanları",q4df$Q4[i])==F){
    q4df[i,11] <- 0
  }
}
for (i in 1:2245) {
  if(grepl("Eğitim ve Kreşlere Verilecek Önem",q4df$Q4[i])==T){
    q4df[i,12] <- 1
  }else if(grepl("Eğitim ve Kreşlere Verilecek Önem",q4df$Q4[i])==F){
    q4df[i,12] <- 0
  }
}
Question_List[["Q4"]]<-q4df#modifying list
Big_Data_With_Questions4 <- merge(Big_Data_With_Questions3,q4df, by = "Number")










#Q5 Adjustments-------
#Q5(Belediyemizin çalışmalarını genel olarak nasıl değerlendirirsiniz ?) is question with likert scale
#Q5: What is the score that you give to our municipality from 1 to 5- 1,2,3,4,5 
#This question is measuring the satisfaction levels of citizens from service quality of municipality
#For that question I dont need to make it logical data frame because its categorical data with likert scale I just need to conver t it to a factor
names(Big_Data)[names(Big_Data)=="Belediyemizin çalışmalarını genel olarak nasıl değerlendirirsiniz ?"] <- "Q5(Satisfaction_level)"#name change
Big_Data[(!complete.cases(Big_Data$`Q5(Satisfaction_level)`)),]#there are total of 233 NA's in the data set 
Big_Data$`Q5(Satisfaction_level)`= factor(Big_Data$`Q5(Satisfaction_level)`)
summary(Big_Data$`Q5(Satisfaction_level)`)
q5df <- data.frame("Q5"=Big_Data$`Q5(Satisfaction_level)`) #q5 creating data frame for question list
q5df$Number <- Big_Data$Number
q5df <- q5df[c(2,1)]
names(Big_Data_With_Questions4)[names(Big_Data_With_Questions4)=="Belediyemizin çalışmalarını genel olarak nasıl değerlendirirsiniz ?"] <- "Q5(Satisfaction_level)"
Big_Data_With_Questions4$`Q5(Satisfaction_level)` <- factor(Big_Data_With_Questions4$`Q5(Satisfaction_level)`)
str(Big_Data_With_Questions4)














#Q6 & Q9 Adjustments----
#Q6 is not necessary for analysis because they did not colleted the way that they should
#I will Delete them from data set to be more clear
Big_Data$`Hangi alanda kendimizi geliştirmeliyiz ?...11`<-NULL
Big_Data$`Hangi alanda kendimizi geliştirmeliyiz ?...14`<-NULL
#They are still in the big_Datayedek data frame
Big_Data_With_Questions4$`Hangi alanda kendimizi geliştirmeliyiz ?...11`<-NULL
Big_Data_With_Questions4$`Hangi alanda kendimizi geliştirmeliyiz ?...14`<-NULL
#other data frames are not important
#Q7 Adjustment----
Big_Data$`Bölgede sizi ne mutlu eder?`
#q7 is Bölgede sizi ne mutlu eder? (What's make you happy in the area?)
#This question is also multiple response question I need to create logical data frame
names(Big_Data)[names(Big_Data)=="Bölgede sizi ne mutlu eder?"] <- "Q7"
names(Big_Data_With_Questions4)[names(Big_Data_With_Questions4)=="Bölgede sizi ne mutlu eder?"] <- "Q7"
#str(Big_Data)
Big_Data$Q7 <- factor(Big_Data$Q7)
levels(Big_Data$Q7)
#There are 23 answers for that question
newvectorq7 <- c("Altyapı ve Üstyapı","Bölgenin Gelişimi","Eğitim ve Kreşler","Gençlere ve Çocuklara Yatırım",
                 "Çevre Düzenlemesi","Çevre Temizliği","Daha İyi Hizmet ve İletişim","Eğlence ve Oyun Alanları",
                 "Hastane ve Rehabilitasyon","Otopark","Huzur ve Güven Ortamı","Kültür, Sanat ve Spor",
                 "İmar , İskan ve Tapu","İstihdam","Bölgenin Gelişimi","Plaj Yapımı",
                 "Yeşil Alan ve Park","Sosyal Tesis Fiyat Düzenlemesi","Yaşlı ve Engelliler","Esnafa Destek","Sokak Hayvanları Kontrolü",
                 "Ulaşım ve Trafik","Uyuşturucu ile Mücadele","Nostaljik Yapılanma")
q7df<- Big_Data %>% select(Number)
q7df[,newvectorq7]<-NA
#str(q7df)
q7df$`Bölgenin Gelişimi.1`<-NULL#deleting double col
q7df$Q7 <- Big_Data$Q7
b=2
for (i in 1:2245) {
  if(grepl("Altyapı ve Üstyapı",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Altyapı ve Üstyapı",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Bölgenin Gelişimi",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Bölgenin Gelişimi",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Eğitim ve Kreşler",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Eğitim ve Kreşler",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Gençlere ve Çocuklara Yatırım",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Gençlere ve Çocuklara Yatırım",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Çevre Düzenlemesi",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Çevre Düzenlemesi",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Çevre Temizliği",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Çevre Temizliği",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Daha İyi Hizmet ve İletişim",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Daha İyi Hizmet ve İletişim",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Eğlence ve Oyun Alanlar",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Eğlence ve Oyun Alanlar",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Hastane ve Rehabilitasyon",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Hastane ve Rehabilitasyon",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Otopark",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Otopark",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Huzur ve Güven Ortamı",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Huzur ve Güven Ortamı",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Kültür, Sanat ve Spor",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Kültür, Sanat ve Spor",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("İmar , İskan ve Tapu",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("İmar , İskan ve Tapu",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("İstihdam",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("İstihdam",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Plaj Yapımı",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Plaj Yapımı",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Yeşil Alan ve Park",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Yeşil Alan ve Park",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Sosyal Tesis Fiyat Düzenlemesi",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Sosyal Tesis Fiyat Düzenlemesi",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Yaşlı ve Engelliler",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Yaşlı ve Engelliler",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Esnafa Destek",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Esnafa Destek",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Sokak Hayvanları Kontrolü",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Sokak Hayvanları Kontrolü",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Ulaşım ve Trafik",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Ulaşım ve Trafik",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Uyuşturucu ile Mücadele",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Uyuşturucu ile Mücadele",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
b=b+1
for (i in 1:2245) {
  if(grepl("Nostaljik Yapılanma",q7df$Q7[i])==T){
    q7df[i,b] <- 1
  }else if(grepl("Nostaljik Yapılanma",q7df$Q7[i])==F){
    q7df[i,b] <- 0
  }
}
Question_List[["Q7"]] <- q7df
Big_Data_With_Questions5 <- merge(Big_Data_With_Questions4,q7df, by="Number")






























#Q8 Adjustments----
#q8 is like q5 measures communucation quality of municipality.
#This question is also likert scale question from 1 to 5 
#Need to convert it to factor 
Big_Data$`Belediyemizin Halk ile iletişimini nasıl buluyorsunuz ?`
names(Big_Data)[names(Big_Data)=="Belediyemizin Halk ile iletişimini nasıl buluyorsunuz ?"] <- "Q8(Communication Level)"
names(Big_Data_With_Questions5)[names(Big_Data_With_Questions5)=="Belediyemizin Halk ile iletişimini nasıl buluyorsunuz ?"] <- "Q8(Communication Level)"
Big_Data$`Q8(Communication Level)`<-factor(Big_Data$`Q8(Communication Level)`)
Big_Data_With_Questions5$`Q8(Communication Level)`<-factor(Big_Data_With_Questions5$`Q8(Communication Level)`)
str(Big_Data)
#aaa <- Big_Data_With_Questions3
#aaa[58:76]
#str(aaa)
#c<-names(aaa[58:76])
#aaa[,c] = data.frame(apply(aaa[c], 2, as.logical))


#General Adjustments and Cleaning for the final state-----
str(Big_Data)
str(Big_Data_With_Questions5)
#q3,q4 and q7 answers are binary they need to be logical to analyze
Final_data <- Big_Data_With_Questions5
colnamesq3_Final_data <- colnames(Final_data[56:74])
Final_data[,colnamesq3_Final_data] = data.frame(apply(Final_data[colnamesq3_Final_data], 2, as.logical))#Converting all q3 answers to logical
colnamesq4_Final_data <- colnames(Final_data[76:86])
Final_data[,colnamesq4_Final_data] = data.frame(apply(Final_data[colnamesq4_Final_data], 2, as.logical))#Converting all q4 answers to logical
#colnames(Final_data[88:110])
colnamesq7_Final_data <- colnames(Final_data[88:110])
Final_data[,colnamesq7_Final_data] = data.frame(apply(Final_data[colnamesq7_Final_data], 2, as.logical))#Converting all q7 answers to logical

#str(Final_data,list.len=ncol(Final_data))
#Number column should be factor
#summary(Final_data$`Q5(Satisfaction_level)`)
#summary(Final_data$Number)
Final_data$Number <- factor(Final_data$Number)
str(Final_data,list.len=111)




















