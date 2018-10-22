# Uzdevums
# Ielādēt programmā R datu masīvu quakes
# (iebūvēti dati), kas satur 1000 novērojumus (rindiņas) un 5
# lielumus (kolonas), kas raksturo seismisko aktivitāti pie Fidži
# salām.

# 1.Aprakstīt visus piecus mainīgos, kas attēloti kolonu viedā. Ko
#   tie raksturo?

# 2.Konstruēt atbilstošos kastu grafikus (izmantojot komandu boxplot). 
#   Aprakstīt, ko attēlo kastu grafiki. Kuri sadalījumi ir
#   asimetriski? Vai tas ir redzams no kastu grafikiem? Kuriem
#   mainīgajiem ir izlecēji? Ko nozīmē, ka datos ir daudz izlecēju?


attach(quakes)

#lat: Notikuma vietas ģeogrāfiskie platuma grādi
par(mfrow=c(1,2))
lat<-quakes$lat
boxplot(lat, main="Box-Plot, Latitude")
hist(lat, main="Histogramma, Latitude")
 
#long: Notikuma vietas ģeogrāfiskie garuma grādi
par(mfrow=c(1,2))
long<-quakes$long 
boxplot(long, main="Box-Plot, Long")
hist(long, main="Histogramma, Long")

#depth: Dziļums(km)
par(mfrow=c(1,2))
depth<-quakes$depth 
boxplot(depth, main="Box-Plot, Depth")
hist(depth, main="Histogramma,Depth")


#mag: Magnitūdas (pēc Rihtera skalas)
par(mfrow=c(1,2))
mag<-quakes$mag
boxplot(stations, main="Box-Plot, Magnitude")
hist(mag, main="Histogramma,Magnitude")

#stations: Staciju skaits, kas ziņoja par notikumu
par(mfrow=c(1,2))
stations<-quakes$stations 
boxplot(stations, main="Box-Plot, Stations")
hist(stations, main="Histogramma,Stations")

#Secinājumi 
# Kastu grfiki attēlo mediānu,  1. un 3. kvartili, un izlēcējus ārpus tām. 
# Daudz izlēcēju parasti raksturo neprecīzus datus, vai nepietiekamu novērojumu skaitu. 
# No apskatītajiem piemēriem izlēcēji nav novērojami tikai 'Depth' datiem. 
# Savukārt asimetriju uz labo pusi var novērot 'Sations' un 'Magnitue' grafikiem,
# kuri ir asimetriski uz labo pusi.



