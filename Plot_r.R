
#Plotting Basic Graphs----
summary(Final_data)
head(Big_Data)
#Mahalle column summary--
sum_Mahalle<- matrix(summary(Big_Data$`Mahalle(District)`))
rownames(sum_Mahalle) <- c(levels(Big_Data$`Mahalle(District)`))

#I will conver this method for subbestting as function
Subesttingcol_as_matrix <- function(x,y) { 
  x<- t(matrix(summary(Big_Data[[y]])))
  colnames(x) <- c(levels(Big_Data[[y]]))
}
sum_Mahalle[,max.col(sum_Mahalle)]
min(sum_Mahalle)
#lets see Mahalle in a basic bar graph
ggplot(data=dat, aes(x=Fruit, y=Count, fill=Bug)) + geom_bar(stat="identity")
str(Big_Data)
Big_Data_plot <- Big_Data
str(Big_Data_plot)
names(Big_Data_plot)[names(Big_Data_plot) == "Mahalle(District)"] <- "Mahalle"#name change for plotting
names(Big_Data_plot)[names(Big_Data_plot) == "Yas(Age)"] <- "Yas"
names(Big_Data_plot)[names(Big_Data_plot) == "Cinsiyet(Gender)"] <- "Cinsiyet"
names(Big_Data_plot)[names(Big_Data_plot) == "Meslek(Profession)"] <- "Meslek"
names(Big_Data_plot)[names(Big_Data_plot) == "Q5(Satisfaction_level)"] <- "Q5"
names(Big_Data_plot)[names(Big_Data_plot) == "Q8(Communication Level)"] <- "Q8"

#plot of citizen in each district
plotofMahalle<-ggplot(data = na.omit(Big_Data_plot),aes(x = Mahalle,fill=Cinsiyet, size = Yas)) +
geom_bar() 
plotofMahalle +
  ggtitle("Citizens Distribution in Each District")+
  xlab("Districts(A to U)")+
  ylab("Number of Citizens")+
  facet_grid(Cinsiyet~.)+
  labs(fill="District")

summary(Big_Data_plot)

Big_Data_plot$Number<-factor(Big_Data_plot$Number)  
summary(Big_Data_plot)
p <- ggplot(data = Big_Data_plot, mapping = aes(x = Yas, y = Mahalle))
p+geom_jitter(aes(color=Cinsiyet))

summary(Big_Data_plot$Meslek)

ggplot(data = Big_Data_plot, mapping = aes(x = Meslek, y = Yas))+
  geom_boxplot(alpha=0.5)+
  geom_jitter(mapping = aes(color=Cinsiyet))+
  ylab("Age")+
  xlab("Profession")+
  ggtitle("Effect of Age & Gender on Professions")+
  scale_y_continuous(limits=c(20,80))+
  scale_color_discrete(name  ="Gender",
                          breaks=c("Erkek", "Kadın","NA"),
                          labels=c("Man", "Woman","NA"))+
  scale_x_discrete(breaks=c("esnaf","ev hanımı","emekli",
                            "öğrenci","serbest meslek"),
    labels=c("Artisan","Housewife","Retired","Student","Self-employment"),
    limits=c("esnaf","ev hanımı","emekli","öğrenci","serbest meslek"))+
  theme( axis.line = element_line(colour = "darkblue",  size = 1, linetype = "solid"))+
  facet_wrap(Mahalle~.)+
  theme(axis.text.x = element_text(face="bold",color = "lightblue4"),
        axis.text.y = element_text(face="bold",color = "lightblue4"),
        plot.title = element_text(color = "lightblue4",size=16,face = "bold",hjust = 0.5),
        panel.background = element_rect(fill = "snow3", colour = "snow3",
                                        size = 2, linetype = "solid"))
  
  

ggplot(data = Big_Data_plot, mapping = aes(x = Meslek, y = Yas))+
  geom_boxplot(alpha=0.5,fill="cornsilk1")+
  geom_jitter(mapping = aes(color=Cinsiyet))+
  ylab("Age")+
  xlab("Profession")+
  ggtitle("Effect of Age & Gender on Professions")+
  scale_color_discrete(name  ="Gender",
                       breaks=c("Erkek", "Kadın","NA"),
                       labels=c("Man", "Woman","NA"))+
  scale_y_continuous(limits=c(20,80))+
  scale_x_discrete(breaks=c("işçi","kuaför","serbest meslek","işsiz","öğretmen"),
                   labels=c("Worker","Hairdresser","Self-employment","Unemployed","Teacher"),
                   limits=c("işçi","kuaför","serbest meslek","işsiz","öğretmen"))+
  theme( axis.line = element_line(colour = "cornsilk1",  size = 1, linetype = "solid"))+
  theme(axis.text.x = element_text(face="bold",color = "lightblue4"),
        axis.text.y = element_text(face="bold",color = "lightblue4"),
        plot.title = element_text(color = "lightblue4",size=16,face = "bold",hjust = 0.5),
        panel.background = element_rect(fill = "grey98", colour = "grey98",
                                        size = 2, linetype = "solid"))



ggplot(data = Big_Data_plot, mapping = aes(x = Meslek, y = Yas))+
  geom_boxplot(alpha=0.5)+
  geom_jitter(mapping = aes(color=Cinsiyet))+xlim("işçi","kuaför","serbest","işsiz","öğretmen")+
  ylab("Age")+
  xlab("Profession")

ggplot(data = na.omit(Big_Data_plot), mapping = aes(x = Meslek, y = Yas))+
  geom_jitter(mapping = aes(color=Mahalle),size=2)+xlim("esnaf","ev hanımı","emekli","öğrenci","serbest meslek")+
  facet_grid(Q5~Q8)

str(Big_Data_plot)
ggplot(data = na.omit(Big_Data_plot),mapping=aes(x=Yas,y=Q5)) +geom_boxplot(aes(color=Cinsiyet),alpha=0.5)+
geom_jitter(color="lightyellow4")  



ggplot(data = Big_Data_plot,aes(y=Q5))+geom_bar()+facet_wrap(.~Mahalle)


#Question plots--------------
Questios_plot <- Final_data

names(Questios_plot)[names(Questios_plot) == "Mahalle(District)"] <- "Mahalle"#name change for plotting
names(Questios_plot)[names(Questios_plot) == "Yas(Age)"] <- "Yas"
names(Questios_plot)[names(Questios_plot) == "Cinsiyet(Gender)"] <- "Cinsiyet"
names(Questios_plot)[names(Questios_plot) == "Meslek(Profession)"] <- "Meslek"
names(Questios_plot)[names(Questios_plot) == "Q5(Satisfaction_level)"] <- "Q5"
names(Questios_plot)[names(Questios_plot) == "Q8(Communication Level)"] <- "Q8"

seed <- 13

q1_1 <- sum(Questios_plot[,13])
q1_2 <- sum(Questios_plot[,14])
q1_3 <- sum(Questios_plot[,15])
q1_4 <- sum(Questios_plot[,16])
q1_5 <- sum(Questios_plot[,17])
q1_6 <- sum(Questios_plot[,18])
q1_7 <- sum(Questios_plot[,19])
q1_8 <- sum(Questios_plot[,20])
q1_9 <- sum(Questios_plot[,21])
q1_10 <- sum(Questios_plot[,22])
q1_11 <- sum(Questios_plot[,23])
q1_12 <- sum(Questios_plot[,24])
q1_13 <- sum(Questios_plot[,25])
q1_14 <- sum(Questios_plot[,26])
q1_15 <- sum(Questios_plot[,27])
q1_16 <- sum(Questios_plot[,28])
q1_17 <- sum(Questios_plot[,29])
q1_18 <- sum(Questios_plot[,30])
q1_19 <- sum(Questios_plot[,31])
q1_20 <- sum(Questios_plot[,32])
q1_total_response <- sum(q1_1,q1_2,q1_3,q1_4,q1_5,q1_6,q1_7,q1_8,q1_9,q1_10,q1_11,q1_12,
                         q1_13,q1_14,q1_15,q1_16,q1_17,q1_18,q1_19,q1_20)
q1destable <- data.frame(c(q1_1,q1_2,q1_3,q1_4,q1_5,q1_6,q1_7,q1_8,q1_9,q1_10,q1_11,q1_12,
                         q1_13,q1_14,q1_15,q1_16,q1_17,q1_18,q1_19,q1_20))
row.names(q1destable) <- c("Altyapı ve Üstyapı Çalışmaları","Daha İyi Hizmet","Kültür,Sanat ve Spor","Teknolojik Altyapı","Çevre Düzenleme",
  "Esnafa Destek","İmar,İskan ve Tapu","Veterinerlik Hiz","Çevre Temizliği ve İlaçlama","Eğitim ve Kreş İmkanları",
  "Huzur ve Güven Ortamı","Yeşil Alan","İş İmkanları","Sosyal Sorumluluk","Yaşlı ve Engelli","Ulaşım",
  "Hastane","Otopark","Kentsel Dönüşüm","Doğal Afetler")
colnames(q1destable)<-"freq"
q1destable$Percent <- NA
q1destable$Percent <- c(q1destable[1,1]/q1destable[21,1],q1destable[2,1]/q1destable[21,1],
                        q1destable[3,1]/q1destable[21,1],q1destable[4,1]/q1destable[21,1],
                        q1destable[5,1]/q1destable[21,1],q1destable[6,1]/q1destable[21,1],
                        q1destable[7,1]/q1destable[21,1],q1destable[8,1]/q1destable[21,1],
                        q1destable[9,1]/q1destable[21,1],q1destable[10,1]/q1destable[21,1],
                        q1destable[11,1]/q1destable[21,1],q1destable[12,1]/q1destable[21,1],
                        q1destable[13,1]/q1destable[21,1],q1destable[14,1]/q1destable[21,1],
                        q1destable[15,1]/q1destable[21,1],q1destable[16,1]/q1destable[21,1],
                        q1destable[17,1]/q1destable[21,1],q1destable[18,1]/q1destable[21,1],
                        q1destable[19,1]/q1destable[21,1],q1destable[20,1]/q1destable[21,1])

q1destable$RealPercent <- NA 
for (i in 1:20) {
  q1destable[i,2] <- round(q1destable[i,2]*100,digits = 3)
}
q1destable$Percentcases <- NA 
for (i in 1:20) {
  q1destable[i,3] <- round(q1destable[i,1]/nrow(Questios_plot)*100,digits = 3)
}
q1destable$Percent <- NULL
q1destable <- data.frame(freq = colSums(Questios_plot[13:32]),
                         RealPercentq1 = round((colSums(Questios_plot[13:32])/sum(colSums(Questios_plot[13:32])))*100,digits = 3),
                         Percentcasesq1 = round((colSums(Questios_plot[13:32])/nrow(Questios_plot[13:32]))*100,digits = 3))
q1destable$Names <- rownames(q1destable)
q1destable$Names <- NULL

#adding male and female percentages into df
q1destable[20,]
q1destable$MalePercent <- NA
q1destable$FemalePercent <- NA

seed<-13
for (i in 1:20) {
  q1destable[i,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot[[seed]]),1)*100,2))[3,3]
  seed <- seed+1
}
rm(seed)

seed<-13
for (i in 1:20) {
  q1destable[i,5] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot[[seed]]),1)*100,2))[4,3]
  seed <- seed+1
}
rm(seed)
data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[13]]),1)*100,2))

#adding Neigborhood(District) difference to df
addition <- data.frame(A = NA,B = NA,C = NA,D = NA,E = NA,"F" = NA,G = NA,H = NA,I = NA,J = NA,K = NA,L = NA,
                       M = NA,N = NA,O = NA,P = NA,R = NA,S = NA,T = NA,U = NA)

q1destable <- cbind(q1destable,addition)

data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[13]]),1)*100,2))[22,3]



seed <- 13
for (i in 1:20) {
  q1destable[i,6]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[21,3]
  q1destable[i,7]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[22,3]
  q1destable[i,8]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[23,3]
  q1destable[i,9]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[24,3]
  q1destable[i,10]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[25,3]
  q1destable[i,11]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[26,3]
  q1destable[i,12]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[27,3]
  q1destable[i,13]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[28,3]
  q1destable[i,14]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[29,3]
  q1destable[i,15]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[30,3]
  q1destable[i,16]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[31,3]
  q1destable[i,17]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[32,3]
  q1destable[i,18]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[33,3]
  q1destable[i,19]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[34,3]
  q1destable[i,20]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[35,3]
  q1destable[i,21]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[36,3]
  q1destable[i,22]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[37,3]
  q1destable[i,23]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[38,3]
  q1destable[i,24]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[39,3]
  q1destable[i,25]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[40,3]
  seed <- seed+1
}

#same job but better script 
#I tried to put it into function to do it to another dataframes
  seed <- 13
  for (i in 1:20) {
    c <- 21
    for (b in 6:25) {
      q2destable[i,b]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[c,3]
      c <- c+1
    }
    seed <- seed+1
  }
  rm(seed)
  rm(b)
  rm(c)



#I want to plot some graphs about percentages of different district to the questions and also gender difference
q1destable 
str(q1destable)#all table is continious
q1destable$Response <- rownames(q1destable)
q1destable$Response <-factor(q1destable$Response)

District <- q1destable %>% gather(key = District, value = Value, A:U)
District
str(District)
District$District <- factor(District$District)
ggplot(District, aes(Response, Value,fill=District)) + geom_bar(stat = "identity",color="black") +
  scale_x_discrete(limits=District$Response[c(1:8)])
 

District$id <- 1:400

ggplot(District, aes(x = id,y = Value ))+geom_line(aes(color=Response),size=0.5)
ggplot(District, aes(x = RealPercentq1,y = Value ))+geom_jitter(aes(color=Response),size=0.5) 

ggplot(q1destable, aes(x = RealPercentq1,y = MalePercent ))+geom_jitter(aes(color=Response),size=0.5) 

ggplot(data = q1destable, aes(x = RealPercentq1,y=Response,fill=A))+geom_bar(stat = "identity")

ggplot(data = District, aes(x = Value,y=Response))+
  geom_bar(stat = "identity")#Yeşil alan,çevre düzenleme, kültür sanat spor,huzur ve güven ortamı


ggplot(data = District, aes(x = Value,y=Response,fill=District))+geom_bar(stat = "identity")+
  facet_wrap(District~.)


ggplot(data = q1destable, aes(x = A,y=B))+
  geom_point(aes(color=Response))


Response_dis = q1destable %>% group_by(Response) %>% summarise(A = sum(A),
                                                   B = sum(B),
                                                   C = sum(C),
                                                   D = sum(D))


Response_dis <- melt(Response_dis)
names(Response_dis) = c("Response","District","Percentage")

ggplot(data=Response_dis,aes(x = Response,y = Percentage,fill=District)) + 
  geom_bar(colour='black' , stat='identity', position='dodge') +  
  theme_bw()+
  theme(axis.text.x = element_text(hjust=1,angle=45),
        plot.title = element_text(hjust=0.5)) + ## center 
  ggtitle('Gener Sale') + 
  scale_fill_brewer(palette = 'YlGnBu')+
  ylab('Percentage')


rm(Response_dis2)
rm(p1)
Response_dis2 = Big_Data %>% group_by(`Mahalle(District)`) %>% summarise(Count = n())
p1 = ggplot(aes(x = `Mahalle(District)` , y = Count , fill=Count) , data=Response_dis2) +
  geom_bar(colour='black',stat='identity') +
  theme_bw()+
  theme(axis.text.x = element_text(angle=45,hjust=1) , 
        plot.title = element_text(hjust=0.5))+  ## title center
  ggtitle('Platform Count')+
  scale_fill_distiller(palette = 'RdYlBu') +
  ylab('Count')
grid.arrange(p1, ncol = 1)

str(q1destable)




#better version of the above code for q2----



q2destable <- data.frame(freq = colSums(Questios_plot[34:54]),
                         RealPercentq2 = round((colSums(Questios_plot[34:54])/sum(colSums(Questios_plot[34:54])))*100,digits = 3),
                         Percentcasesq2 = round((colSums(Questios_plot[34:54])/nrow(Questios_plot[34:54]))*100,digits = 3))



q2destable[21,]
q2destable = q2destable[-c(22),]


q2destable$MalePercent <- NA
q2destable$FemalePercent <- NA

seed<-34
for (i in 1:21) {
  q2destable[i,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot[[seed]]),1)*100,2))[3,3]
  seed <- seed+1
}
rm(seed)

seed<-34
for (i in 1:21) {
  q2destable[i,5] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot[[seed]]),1)*100,2))[4,3]
  seed <- seed+1
}
rm(seed)
addition <- data.frame(A = NA,B = NA,C = NA,D = NA,E = NA,"F" = NA,G = NA,H = NA,I = NA,J = NA,K = NA,L = NA,
                       M = NA,N = NA,O = NA,P = NA,R = NA,S = NA,T = NA,U = NA)
q2destable <- cbind(q2destable,addition)

seed <- 34
for (i in length(q2destable)) {
  c <- 21
  for (b in 6:25) {
    q2destable[i,b]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[c,3]
    c <- c+1
  }
  seed <- seed+1
}


District2 <- q2destable %>% gather(key = District, value = Value, A:U)
District2
str(District2)
District2$District <- factor(District2$District)
ggplot(q2destable,aes(x = A))+geom_bar()
ggplot(District2, aes(x = District,y = Value))


#Q3destable-----
colnames(Questios_plot[56:74])


q3destable <- data.frame(freq = colSums(Questios_plot[56:74]),
                         RealPercentq3 = round((colSums(Questios_plot[56:74])/sum(colSums(Questios_plot[56:74])))*100,digits = 3),
                         Percentcasesq3 = round((colSums(Questios_plot[56:74])/nrow(Questios_plot[56:74]))*100,digits = 3))
q3destable[19,]
q3destable = q3destable[-c(20),]


q3destable$MalePercent <- NA
q3destable$FemalePercent <- NA

seed<-56
for (i in 1:19) {
  q3destable[i,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot[[seed]]),1)*100,2))[3,3]
  seed <- seed+1
}
rm(seed)

seed<-56
for (i in 1:19) {
  q3destable[i,5] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot[[seed]]),1)*100,2))[4,3]
  seed <- seed+1
}
rm(seed)


q3destable<-cbind(q3destable,addition)
seed <- 56
for (i in 1:19) {
  c <- 21
  for (b in 6:25) {
    q3destable[i,b]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[c,3]
    c <- c+1
  }
  seed <- seed+1
}
rm(seed)
rm(c)
rm(b)



#q4destable----
colnames(Questios_plot[76:86])
q4destable <- data.frame(freq = colSums(Questios_plot[76:86]),
                         RealPercentq4 = round((colSums(Questios_plot[76:86])/sum(colSums(Questios_plot[76:86])))*100,digits = 3),
                         Percentcasesq4 = round((colSums(Questios_plot[76:86])/nrow(Questios_plot[76:86]))*100,digits = 3))

q4destable = q4destable[-c(12),]


q4destable$MalePercent <- NA
q4destable$FemalePercent <- NA

seed<-76
for (i in 1:11) {
  q4destable[i,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot[[seed]]),1)*100,2))[3,3]
  seed <- seed+1
}
rm(seed)

seed<-76
for (i in 1:11) {
  q4destable[i,5] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot[[seed]]),1)*100,2))[4,3]
  seed <- seed+1
}
rm(seed)

q4destable<-cbind(q4destable,addition)
seed <- 76
for (i in 1:11) {
  c <- 21
  for (b in 6:25) {
    q4destable[i,b]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[c,3]
    c <- c+1
  }
  seed <- seed+1
}
rm(seed)
rm(c)
rm(b)


#q7destable----
colnames(Questios_plot[88:110])
q7destable <- data.frame(freq = colSums(Questios_plot[88:110]),
                         RealPercentq7 = round((colSums(Questios_plot[88:110])/sum(colSums(Questios_plot[88:110])))*100,digits = 3),
                         Percentcasesq7 = round((colSums(Questios_plot[88:110])/nrow(Questios_plot[88:110]))*100,digits = 3))
q7destable = q7destable[-c(24),]


q7destable$MalePercent <- NA
q7destable$FemalePercent <- NA

q7destable[1,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Altyapı ve Üstyapı.y`),1)*100,2))[3,3]
q7destable[2,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Bölgenin Gelişimi`),1)*100,2))[3,3]
q7destable[3,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Eğitim ve Kreşler.y`),1)*100,2))[3,3]
q7destable[4,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Gençlere ve Çocuklara Yatırım`),1)*100,2))[3,3]
q7destable[5,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Çevre Düzenlemesi`),1)*100,2))[3,3]
q7destable[6,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Çevre Temizliği`),1)*100,2))[3,3]
q7destable[7,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Daha İyi Hizmet ve İletişim`),1)*100,2))[3,3]
q7destable[8,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Eğlence ve Oyun Alanları`),1)*100,2))[3,3]
q7destable[9,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Hastane ve Rehabilitasyon`),1)*100,2))[3,3]
q7destable[10,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$Otopark.y),1)*100,2))[3,3]
q7destable[11,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Huzur ve Güven Ortamı.y`),1)*100,2))[3,3]
q7destable[12,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Kültür, Sanat ve Spor`),1)*100,2))[3,3]
q7destable[13,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`İmar , İskan ve Tapu`),1)*100,2))[3,3]
q7destable[14,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$İstihdam),1)*100,2))[3,3]
q7destable[15,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Plaj Yapımı`),1)*100,2))[3,3]
q7destable[16,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Yeşil Alan ve Park`),1)*100,2))[3,3]
q7destable[17,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Sosyal Tesis Fiyat Düzenlemesi`),1)*100,2))[3,3]
q7destable[18,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Yaşlı ve Engelliler`),1)*100,2))[3,3]
q7destable[19,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Esnafa Destek`),1)*100,2))[3,3]
q7destable[20,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Sokak Hayvanları Kontrolü`),1)*100,2))[3,3]
q7destable[21,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Ulaşım ve Trafik`),1)*100,2))[3,3]
q7destable[22,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Uyuşturucu ile Mücadele`),1)*100,2))[3,3]
q7destable[23,4] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Nostaljik Yapılanma`),1)*100,2))[3,3]

#better version of the code on the up side 
seed<-88
for (i in 1:23) {
  q7destable[i,5] <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot[[seed]]),1)*100,2))[4,3]
  seed <- seed+1
}
rm(seed)


q7destable<-cbind(q7destable,addition)
seed <- 88
for (i in 1:23) {
  c <- 21
  for (b in 6:25) {
    q7destable[i,b]  <- data.frame(round(prop.table(table(Questios_plot$Mahalle,Questios_plot[[seed]]),1)*100,2))[c,3]
    c <- c+1
  }
  seed <- seed+1
}
rm(seed)
rm(c)
rm(b)

multipleresponsedescriptive <- list(q1destable,q2destable,q3destable,q4destable,q7destable)#saving as list


#round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Yeşil Alan.x`),1),2)# 14% of both gender says that green area is the most imporant thing for future of municipality
#aaaaa <- data.frame(round(prop.table(table(Questios_plot$Cinsiyet,Questios_plot$`Çevre Düzenleme.x`),1)*100,2))
#aaaaa[3,3]
#aaaaa[4,3]
#rm(aaaaa)
# 14% of both gender says that green area is the most imporant thing for future of municipality
#colnames(Questios_plot)


#just ex-----
#table(Questios_plot$Cinsiyet,Questios_plot$`Altyapı ve Üstyapı Çalışmaları`)
#sn.table
#sn.table2 <- table(Questios_plot$Cinsiyet,Questios_plot$`Daha İyi Hizmet.x`)
#sn.table2
#margin.table(sn.table, 2)
#round(prop.table(sn.table2), 2) # toplam olarak yüzdelik 
#round(prop.table(sn.table2,1), 2)#erkeklerin yüzde kaçı ne demiş veya kadınların yüzde kaçı ne demiş.
#round(prop.table(sn.table,2), 2)#Trueların yüzde kaçı true veya yüzde kaçı false
#chisq.test(sn.table)
#sn.table3 <- table(Questios_plot$Mahalle,Questios_plot$`Altyapı ve Üstyapı Çalışmaları`)
#rm(sn.table2)










