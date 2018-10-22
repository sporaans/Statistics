# Uzdevums 1. Ielasīt programmā R datus CMB.
# Līdzīgi kā lekcijā veikt lineārās un polinomiālās
# regresijas analīzi. Pārbaudīt ar ANOVA palīdzību, kura kārta polinomiālajai
# regresijai būtu piemērota. Veikt atlikumu analīzi līdzīgi kā lekcijā.
# Izdarīt attiecīgus secinājumus.
library(car)
dati<-read.table("cmb.txt", header=T)
attach(dati)
plot(Cl~ell)

fit1<-lm(Cl~ell)
summary(fit1)
abline(fit1,col="red",lwd=2)
plot(fit1$residuals) #slīpuma koeficients tuvu 0, grafiks līdzīgs oriģinālajam

acf(fit1$residuals) # atlikumi uzrāda kolerāciju

# polinomiālā regresija
fit1<-lm(Cl~poly(ell,degree=2,raw=T))
fit2<-lm(Cl~poly(ell,degree=3,raw=T))

summary(fit2)
anova(fit1,fit2)
plot(Cl~ell)
lines(sort(ell), fitted(fit2)[order(ell)], col='red')

# uzlabojums nozimigs 3 kārtas polinomam, tomēr R^2 ir relatīvi mazs (0.09),
# jo liela punktu izkliede, kad ell>600

# Uzdevums 2. Ielasīt programmā R datus LifeCycleSavings, kas raksturo ietaupījumu
# attiecību pret ienākumiem atkarībā no pieciem faktoriem. Līdzīgi kā
# otrajā uzdevumā veikt daudzfaktoru lienāro regresijas analīzi. Novērtēt arī atbilstošās
# korelācijas. Pārbaudīt modeļa nosacījumus, veikt diagnostiku, apskatīt
# mainīgo selekcijas problēmu. Veikt atbisltošus secinājumus.

attach(LifeCycleSavings)
head(LifeCycleSavings)

fit<-lm(sr~.,data=LifeCycleSavings)
summary(fit)
# dpi vismazāk nozīmīgais, izņem no modeļa
fit1<-lm(sr ~ pop15 + pop75 + ddpi, data = LifeCycleSavings)
summary(fit1)

anova(fit,fit1) 
# p-vertiba>0.05 nav vērā ņemama statiskā uzlabojuma, 
# arī R^2 nav vērā ņemam uzlabojuma, keficienti statisti nozīmīgi, izņemot pop75,
# kura nozīmīgumu var apšaubīt (p-vērt=0.07)

