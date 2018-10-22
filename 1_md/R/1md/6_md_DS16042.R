# Uzdevums
# Atrast programmā R testus, kas pārbauda eksponencialitāti.
# Pārbaudīt, vai datu masīva quakes mainīgais stations ir sadalīts pēc
# eksponenciālā sadalījuma. Pārliecināties, vai tests strādā pareizi uz
# eksponenciāli ģenerētiem datiem.

attach(quakes)
stations<-quakes$stations


hist(stations,prob=T, main="Histogramma",
     col=45,xlab="Izlases vertības", ylab="Relatīvais biežums",
     ylim=c(0,0.045))
x<-seq(0,140,by=0.01)
n<-length(stations)

#Eksponencialais sadalijums
teta_m<-n/sum(stations) #pēc momentu metodes
lines(x,dexp(x,teta_m),col="red")

teta_t<-n/sum(stations) #pēc ticamības funkcijas metodes
lines(x,dexp(x,teta_t), col="blue")

#LogNormalais sadalijums
#Momentu metode
mu_m<-2*log(mean(stations))-log(mean(stations^2))/2
sigma2_m<-log(mean(stations^2))-2*log(mean(stations))
lines(x,dlnorm(x,mu_m,sigma2_m), col="green", lwd=2)

#Ticamības f-jas metode
mu_t<-sum(log(stations))/n 
sigma2_t<-(sum((log(stations)-mu_t)^2))/n 
lines(x,dlnorm(x,mu_t,sigma2_t), col="yellow", lwd=2)

