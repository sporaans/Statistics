# 1. uzdevums
# Amerikas Automobiļu Asociācija publicēja datus (Defensive Driving:
# Managing Time and Space, 1991), kur analizē atkarību starp vidējo apstāšanos
# laiku (y = distance, in feet) mašīnas ātrumu (x = speed, in miles per hour).
# Dati carstopping.txt satur 63 novērojumus. Vai determinācijas koeficients R2 ir
# nozīmīgs ar lielu vērtību? Vai regresijas lineārā taisne labi pieguļ datu punktiem?

dati<-read.table(file="carstopping.txt",header=T)

attach(dati)
names(dati)
plot(Speed,StopDist)

# regresijas taisne
fit<-lm(StopDist~Speed)

summary(fit)
#R-squared:  0.8752
#Regresijas koeficienti nozīmīgi un arī f-tests norāda uz nozīmību

abline(fit)
# Lineārā regresijas taisne labi pieguļ datu punktiem. Aptuveni 87% no kopējās dispersijas izskaidrots ar
# vienkāršās lineārās regresijas palīdzību!

# 2. uzdevums
# Aplūkosim sekojošus datus practical.txt. Veikt lineāro regresijas
# analīzi: novērtēt koeficientus, pārbaudīt vai tie ir statististki nozīmīgi, aprēķināt
# determinācijas koeficientu R2, aprēķināt ticamības intervālus regresijas koeficientiem.
# Vai 0 ietilpst ticamības intervālā slīpuma koeficientam b? Vai b ir
# statistiski nozīmīgs? Komentēt: kāpēc šāda pretruna: lai gan slīpuma koeficients
# b ir tuvu nullei, tomēr tas ir statistiski nozīmīgs.

dati2<-read.table(file="practical.txt",header=T)

attach(dati2)
names(dati2)

plot(x,y)

fit2<-lm(y~x)
summary(fit2)

abline(fit2)
# koeficienti statistiski nozīmīgi
# R-squared:  0.243
# Slīpuma koeficients: 0.099
# Ja vien koeficients nav 0, tad tā statistikso nozīmību neietekmē tā vērtība.
