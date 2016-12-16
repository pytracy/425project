library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(ggthemes) # For providng themed colour scales when plotting graphs.
library(animation)
uberApr14Data<-read.csv("C:/Users/Yan Peng/Desktop/uber-raw-data-apr14.csv")
uberAug14Data<-read.csv("C:/Users/Yan Peng/Desktop/uber-raw-data-aug14.csv")
uberSep14Data<-read.csv("C:/Users/Yan Peng/Desktop/uber-raw-data-sep14.csv")
uberMay14Data<-read.csv("C:/Users/Yan Peng/Desktop/uber-raw-data-may14.csv")
uberJun14Data<-read.csv("C:/Users/Yan Peng/Desktop/uber-raw-data-jun14.csv")
uberJul14Data<-read.csv("C:/Users/Yan Peng/Desktop/uber-raw-data-jul14.csv")
# Formatting the Date/Time to â€˜m/d/Y  H:M:Sâ€? 
uberApr14Data$Date.Time <- strptime(uberApr14Data$Date.Time, format = "%m/%d/%Y %H:%M:%S")
uberMay14Data$Date.Time <- strptime(uberMay14Data$Date.Time, format = "%m/%d/%Y %H:%M:%S")
uberJun14Data$Date.Time <- strptime(uberJun14Data$Date.Time, format = "%m/%d/%Y %H:%M:%S")
uberJul14Data$Date.Time <- strptime(uberJul14Data$Date.Time, format = "%m/%d/%Y %H:%M:%S")
uberAug14Data$Date.Time <- strptime(uberAug14Data$Date.Time, format = "%m/%d/%Y %H:%M:%S")
uberSep14Data$Date.Time <- strptime(uberSep14Data$Date.Time, format = "%m/%d/%Y %H:%M:%S")

uberApr14Data$Time <- format(uberApr14Data$Date.Time, "%H:%M:%S")
uberAug14Data$Time <- format(uberAug14Data$Date.Time, "%H:%M:%S")
uberSep14Data$Time <- format(uberSep14Data$Date.Time,  "%H:%M:%S")
uberMay14Data$Time <- format(uberMay14Data$Date.Time,  "%H:%M:%S")
uberJun14Data$Time <- format(uberJun14Data$Date.Time,  "%H:%M:%S")
uberJul14Data$Time <- format(uberJul14Data$Date.Time,  "%H:%M:%S")

uberApr14Data$Date<- format(uberApr14Data$Date.Time, "%m/%d/%Y")
uberAug14Data$Date<- format(uberAug14Data$Date.Time, "%m/%d/%Y")
uberSep14Data$Date<- format(uberSep14Data$Date.Time, "%m/%d/%Y")
uberMay14Data$Date<- format(uberMay14Data$Date.Time, "%m/%d/%Y")
uberJun14Data$Date<- format(uberJun14Data$Date.Time, "%m/%d/%Y")
uberJul14Data$Date<- format(uberJul14Data$Date.Time,"%m/%d/%Y")



# Adding Month Column 
uberApr14Data$Month <- as.integer(format(uberApr14Data$Date.Time, "%m"))
uberAug14Data$Month <- as.integer(format(uberAug14Data$Date.Time, "%m"))
uberSep14Data$Month <- as.integer(format(uberSep14Data$Date.Time, "%m"))
uberMay14Data$Month <- as.integer(format(uberMay14Data$Date.Time, "%m"))
uberJun14Data$Month <- as.integer(format(uberJun14Data$Date.Time, "%m"))
uberJul14Data$Month <- as.integer(format(uberJul14Data$Date.Time, "%m"))


# Adding Hour Column
uberApr14Data$Hour <- as.integer(format(uberApr14Data$Date.Time, "%H"))
uberAug14Data$Hour <- as.integer(format(uberAug14Data$Date.Time, "%H"))
uberSep14Data$Hour <- as.integer(format(uberSep14Data$Date.Time, "%H"))
uberMay14Data$Hour <- as.integer(format(uberMay14Data$Date.Time, "%H"))
uberJun14Data$Hour <- as.integer(format(uberJun14Data$Date.Time, "%H"))
uberJul14Data$Hour <- as.integer(format(uberJul14Data$Date.Time, "%H"))

# Adding Day Column 

uberApr14Data$Day <- as.integer(format(uberApr14Data$Date.Time, "%d"))
uberAug14Data$Day <- as.integer(format(uberAug14Data$Date.Time, "%d"))
uberSep14Data$Day <- as.integer(format(uberSep14Data$Date.Time, "%d"))
uberMay14Data$Day <- as.integer(format(uberMay14Data$Date.Time, "%d"))
uberJun14Data$Day <- as.integer(format(uberJun14Data$Date.Time, "%d"))
uberJul14Data$Day <- as.integer(format(uberJul14Data$Date.Time, "%d"))

# Adding Weekday Column 

uberApr14Data$WeekDay <- as.integer(format(uberApr14Data$Date.Time, "%w"))
uberAug14Data$WeekDay <- as.integer(format(uberAug14Data$Date.Time, "%w"))
uberSep14Data$WeekDay <- as.integer(format(uberSep14Data$Date.Time, "%w"))
uberMay14Data$WeekDay <- as.integer(format(uberMay14Data$Date.Time, "%w"))
uberJun14Data$WeekDay <- as.integer(format(uberJun14Data$Date.Time, "%w"))
uberJul14Data$WeekDay <- as.integer(format(uberJul14Data$Date.Time, "%w"))

total<-rbind(uberApr14Data, uberMay14Data, uberJun14Data, uberJul14Data,uberAug14Data, uberSep14Data)
head(total)
attach(total)
Month
nrow(total[total$Month==4,][total$Day==1,])
#Calculate Day-Month number
Monthday<-matrix(nrow=31,ncol=6)
colnames(Monthday)=c("Apr","May","Jun","Jul","Aug","Sep")
for(i in c(5,7,8)){
  month<-subset(total,Month==i)
  for(j in 1:31){
    monthday<-subset(month,Day==j)
    Monthday[j,i-3]=nrow(monthday)
  }
}
for (i in c(4,6,9)){
  month<-subset(total,Month==i)
  for(j in 1:30){
    monthday<-subset(month,Day==j)
    Monthday[j,i-3]=nrow(monthday)
  }
}
Monthday



#Calculate Hour-Day number
Dayhour<-matrix(nrow=186,ncol=24)
for(i in c(5,7,8)){
  month<-subset(total,Month==i)
  for(j in 1:31){
    monthday<-subset(month,Day==j)
    for(h in 0:23){
      dayhour<-subset(monthday,Hour==h)
      Dayhour[31*(i-4)+j,h+1]=nrow(dayhour)
    }
  }
}

for (i in c(4,6,9)){
  month<-subset(total,Month==i)
  for(j in 1:30){
    monthday<-subset(month,Day==j)
    for(h in 0:23){
      dayhour<-subset(monthday,Hour==h)
      Dayhour[31*(i-4)+j,h+1]=nrow(dayhour)
    }
  }
}
Dayhour=Dayhour[-c(31,93,186),]
Dayhour

qlat=numeric(4)
for(n in 1:4){
  qlat[n]=40.5+0.1*n
}
qlat
qlon=numeric(5)
for(n in 1:5){
  qlon[n]=-74.25+0.1*n
}
qlon

summary(Lat)
summary(Lon)

qlat[5]
dayhourzone=array(dim=c(186,24,12))
for(i in c(5,7,8)){
  month<-subset(total,Month==i)
  for(j in 1:31){
    monthday<-subset(month,Day==j)
    for(h in 0:23){
      dayhour<-subset(monthday,Hour==h)
      for(m in 1:12){
        k=ceiling(m/4)
        l=m-4*floor((m-1)/4)
        latzone<-subset(dayhour,(qlat[k]<=Lat)*(Lat<qlat[k+1])==1)
        zone<-subset(latzone,(qlon[l]<=Lon)*(Lon<qlon[l+1])==1)
        dayhourzone[31*(i-4)+j,h+1,m]=nrow(zone)
      }
    }
  }
}

for (i in c(4,6,9)){
  month<-subset(total,Month==i)
  for(j in 1:30){
    monthday<-subset(month,Day==j)
    for(h in 0:23){
      dayhour<-subset(monthday,Hour==h)
      for(m in 1:12){
        k=ceiling(m/4)
        l=m-4*floor((m-1)/4)
        latzone<-subset(dayhour,(qlat[k]<=Lat)*(Lat<qlat[k+1])==1)
        zone<-subset(latzone,(qlon[l]<=Lon)*(Lon<qlon[l+1])==1)
        dayhourzone[31*(i-4)+j,h+1,m]=nrow(zone)
      }
    }
  }
}
dayhourzone
dayhourzonenew=dayhourzone[-c(31,93,186),,]


Monthday
Mdc<-c(Monthday[,1],Monthday[,2],Monthday[,3],Monthday[,4],Monthday[,5],Monthday[,6])
Mdc[-c(31,93,186)]
plot(Mdc)

lat<-subset(total,(qlat[1]<=Lat)*(Lat<qlat[5])==1)
zone<-subset(lat,(qlon[1]<=Lon)*(Lon<qlon[5])==1)
ggplot(zone, aes(x=Lon, y=Lat, color = Hour)) +
  geom_point(size = 1) +
  scale_x_continuous(limits = c(qlon[1],qlon[5])) +
                       scale_y_continuous(limits = c(qlat[1],qlat[5])) +
                                            ggtitle("NYC MAP BASED ON UBER RIDES -AUG 14 EACH HOUR (0 to 23)")


#weekday and weekends
c<-seq(1:183)
wkd<-c(rep(c(2,3,4,5,6,7,1),26),2)
daywkd<-t(rbind(c,wkd))
dim(daywkd)
weekday<-subset(daywkd,daywkd[,2]<=5)[,1]
wod1<-subset(daywkd,daywkd[,2]==1)[,1]
wod2<-subset(daywkd,daywkd[,2]==2)[,1]
wod3<-subset(daywkd,daywkd[,2]==3)[,1]
wod4<-subset(daywkd,daywkd[,2]==4)[,1]
wod5<-subset(daywkd,daywkd[,2]==5)[,1]
wod6<-subset(daywkd,daywkd[,2]==6)[,1]
wod7<-subset(daywkd,daywkd[,2]==7)[,1]
wod<-t(rbind(wod1,wod2,wod3,wod4,wod5,wod6,wod7)[,1:26])
wod[,1]
m2=matrix(nrow=24,ncol=7)
  for(j in 1:7){
m2[,j]=apply(dayhourzonenew[wod[,j],,2],2,mean)
}


zonenum=matrix(nrow=24,ncol=12)
for(i in 1:12){
zonenum[,i]=apply(dayhourzonenew[weekday,,i],2,sum)
}
wkdayavezone<-zonenum/length(weekday)
wkdayhour<-Dayhour[weekday,]
wkdayhourave<-apply(wkdayhour,2,mean)
wkendshour<-Dayhour[wkends,]
wkendshourave<-apply(wkendshour,2,mean)

wkends<-daywkd[-weekday,][,1]
wkendsavezone=matrix(nrow=24,ncol=12)
for(i in 1:12){
  wkendsavezone[,i]=apply(dayhourzonenew[wkends,,i],2,sum)/length(wkends)
}
wkendsavezone

colnames(wkdayhour)=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14",
                         "15","16","17","18","19","20","21","22","23","24")
wka<-data.frame(t(rbind(c(wkdayhourave),c(wkendshourave),v)))
wkendshourave
v<-c(0:23)
head(total)
wkda<-data.frame(t(rbind(c(wkdayhourave),v)))
colnames(wkda)=c("WkdayHourave","Hour")
colnames(wka)=c("WkdayHourave","WkendsHourave","Hour")
par(mar=c(5,5,5,5))
ggplot(data=wka,aes(x=Hour,y=Number.Pickups))+
  labs(x="Hour",y="Number of Pickups")+
  geom_point(aes(x=Hour,y=wka$WkdayHourave,color="blue"))+
  geom_point(aes(x=Hour,y=wka$WkendsHourave,color="red"))+
  theme(legend.position="bottom")+
  scale_color_manual(labels=c("Weekdays","Weekends"),values=c("blue","red"))+
   ggtitle("NY Weekdays&Weekends Average pickups by Hour")
?scale_color_manual
barplot(MDvec)
ggplot(MDvec)+geom(MDvec)
library(lubridate)
library("dplyr")
library("lubridate")
library("fields")
library("magrittr")

head(total)
test$time <- ymd_hms(total$Time)
test.data <- mutate(test.data,otf = difftime(res,ord,units="min"))
test.data <- mutate(test.data,dow = wday(ord))
test.data <- mutate(test.data,hod = as.numeric(format(test.data$ord, "%H")))
dow<-wday(as.Date(total$Date))
total$Dow<-wday(as.Date(total$Date))
heat.data <- matrix(rep(NA,7*24),nrow = 7, ncol = 24)
#loop over the days and hours and calculate the median TAT
for(i in 1:7){
  for(j in 0:23){
    heat.data[i,j+1] <- subset(total, total$Dow==i&total$Hour==j)
  }
}

#Heatmap
m2=matrix(nrow=7,ncol=24)
for(j in 1:7){
  m2[j,]=apply(dayhourzonenew[wod[,j],,2],2,mean)
}
test<-dayhourzonenew[1:30,,7]
par(mar=c(4,4,2,2))
image.plot(1:7,seq(from=0.5, to=23.5, by = 1),m2,axes=FALSE, 
           xlab = "Day of week", ylab = "Hour ", ylim=c(0,24)
           ,main="Heat map of Average pickups in Zone2 by hour in a week ")
points(0,0)
axis(side=1, at=1:7, labels=c("M","T","W","R","F","SA","SU"), las=2, cex.axis = 0.8)
axis(side=2, at= 0:24, labels=0:24, las=1, cex.axis=0.8)

weather <- read.csv("C:/Users/Yan Peng/Desktop/Grad 1/425/project/uber/weather.csv")
WeatherApr14<-read.csv("C:/Users/Yan Peng/Desktop/weather.csv")
Md4<-Monthday[,1][1:30]
attach(WeatherApr14)
holiday<-numeric(30)
holiday[1]=1
holiday[20]=1
holiday[18]=1
holiday[22]=1
w<-as.factor(wkd[1:30])
cc<-c[1:30]
csq1<-cc^2
out1=lm(Md4~cc+csq1+w+WeatherApr14$Temp+WeatherApr14$Rain+holiday)
summary(out1)
Rreg[1:30]
MDvec<-as.vector(Monthday)[-c(31,93,186)]


ggplot(total, aes(x=Lon, y=Lat, color = Hour)) +
  geom_point(size = 1) +
  scale_x_continuous(limits = c(min_long, max_long)) +
  scale_y_continuous(limits = c(min_lat, max_lat)) +
  geom_abline(y=qlon[2])
  ggtitle("NYC MAP BASED ON UBER RIDES -AUG 14 EACH HOUR (0 to 23)")

cubic<-array(dim=c(3,2,2))  
cubic[1,,]=matrix(c(4,3,2,1),2,2)  
cubic[2,,]=matrix(c(8,7,6,5),2,2)
cubic[3,,]=matrix(c(12,11,10,9),2,2)
as.vector(cubic)
data.frame(cubic)
sevenzone<-dayhourzonenew[,,c(2,3,6,7,8,10,11)]
length(as.vector(sevenzone))
dhz<-as.vector(sevenzone)
data.frame(sevenzone)
dd<-c(rep(MDvec,168))
hh<-rep(c(rep(1:24,times=183)),7)
zz<-rep(1:7,times=4392)
ww<-c(rep(wkd,168))
ss<-data.frame(t(rbind(dhz,dd,hh,zz,ww)))
attach(ss)
head(ss)

apply(dayhourzonenew,-1,sum)
cubic
apply(cubic,-1,sum)
weather <- read.csv("C:/Users/Yan Peng/Desktop/Grad 1/425/project/uber/weather.csv", header=FALSE)
colnames(weather)=c("HT","LT","RF")
weather[1,1]=60
head(weather)
head(Temp)
Temp$H5
Temp[,7]
H<-Temp[,c(7,9,11,13,15,17)]
as.vector(H)
Temp$H6
MDvec
Temperature[1,1]=19.7
attach(Temperature)
TH<-as.matrix(Temperature[,c(7,9,11,13,15,17)])
TH4to9<-as.vector(TH)[-c(31,63,64,93,186)]
TL<-as.matrix(Temperature[,c(8,10,12,14,16,18)])
TL4to9<-as.vector(TL)[-c(31,63,64,93,186)]
MD<-MDvec[-c(62,63)]
R<-as.matrix(Rain[4:9])
R4to9<-as.vector(R)[-c(31,63,64,93,186)]
Rreg<-as.numeric(R4to9)
THreg<-as.numeric(TH4to9)
TLreg<-as.numeric(TL4to9)
TAreg<-(THreg+TLreg)/2
wkdreg<-as.factor(wkd[-c(62,63)])
Ho<-rep(0,183)
Ho[c(1,18,20,21,22,35,41,47,56,69,70,75,76,95,118,154,160,164,169,170,179)]=1
length(T4to9)
Horeg<-Ho[-c(62,63)]
day<-c[-c(62,63)]
daysq<-c181^2
daycub<-c181^3
HT2<-as.numeric(weather$HT[-c(62,63)])
LT2<-as.numeric(weather$LT[-c(62,63)])
ATN<-(HTN+LTN)/2
R2<-weather$RF[-c(62,63)]
M30<-c(rep(1,10),rep(2,10),rep(3,10))
M31<-c(rep(1,10),rep(2,10),rep(3,11))
M<-c(rep(1,30),rep(2,31),rep(3,30),rep(4,31),rep(5,31),rep(6,30))
Mreg<-as.factor(M[-c(62,63)])
out=lm(MD~day+daysq+HT2+LT2+R2+wkdreg+Horeg)

summary(out)
out2=lm(MD~day+HT2+LT2+R2+wkdreg+Horeg+Mreg+wkdreg:R2+wkdreg:HT2)
summary(out2)
MDvec
barplot(MDvec)
c181<-c[-c(62,63)]
