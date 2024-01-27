DATE<-c(1961,1962,1963,1964,1965,1966,1967,1968,1969,1970,1971,1972,1973,1974,1975,1976
         ,1977,1978,1979,1980,1981,1982,1983,1984,1985,1986,1987,
         1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,1998,
         1999,2000,2001,2002,2003,2004,2005,2006,2007,2008,2009,
         2010,2011,2012,2013,2014,2015,2016,2017,2018)
DATE2<-c(1961,1971,1981,1991,2001,2011,2018)



Myanmar<-data[159,c(6:63)]
head(Myanmar)
print(Myanmar)
plot(DATE2,data[176,c(6:63)],type='b',col='#442288',pch=8,xlab="Year",ylab="Area",
     ylim=c(0,200000),main="Agricultural Land (Sq.km)")
lines(DATE2,Myanmar,type='b',col='Green',pch=5)
lines(DATE2,data[90,c(6:63)],type='b',col='#85D33D',pch=1)
lines(DATE2,data[80,c(6:63)],type='b',col='#FED23F',pch=2)
lines(DATE2,data[200,c(6:63)],type='b',col='#EB7D5B',pch=3)
lines(DATE2,data[189,c(6:63)],type='b',col='#6CA2EA',pch=4)
legend("topleft",legend=c('Norway','Myanmar','Greenland','United Kingdom','Romania','Poland'),
       col = c( "#442288", "Green","#85D33D","#FED23F",'#EB7D5B',"#6CA2EA"),bty='n',
       pch=c(8,5,1,2,3,4),pt.cex=0.999,cex=0.6)



copydata<-data[,50:63]
A<-copydata%>%replace(is.na(.),0)%>%
  summarise_all(sum)
World<-as.double(A[1,])
Myanmar<-as.numeric(data[159,c(6,15,25,35,45,55,63)])
Indonesia<-as.numeric(data[105,c(6,15,25,35,45,55,63)])
India<-as.numeric(data[108,c(6,15,25,35,45,55,63)])
Cambodia<-as.numeric(data[122,c(6,15,25,35,45,55,63)])
Thailand<-as.numeric(data[232,c(6,15,25,35,45,55,63)])
Bangladesh<-as.numeric(data[19,c(6,15,25,35,45,55,63)])
datatest<-data[55:63,6:7]
MyanmarWorld<-data.frame(Myanmar,Indonesia,India,Cambodia,Thailand,Bangladesh)
barplot(t(as.matrix(MyanmarWorld)),col=c('#667C26','yellow','#3090C7','#F88017','pink','#990012'),
        main='Agricultural Land (Sq.km)',xlab="Area",ylab="2018",beside=TRUE)
legend("topleft",legend=c("Myanmar","Indonesia","India","Cambodia","Thailand","Bangladesh"),
       col=c('#667C26','yellow','#3090C7','#F88017','pink','#990012'),
       pch=c(16,16,16,16,16,16),pt.cex=0.999,cex=0.6,horiz=FALSE,bty='n')
m<-lm(data[,63]~c(0:263))
plot(data[,63],main="Agricultural Land (Sq.km)",xlab="Country",ylab="2018",frame=FALSE,col='#006a4e')
abline(m,col='blue')
