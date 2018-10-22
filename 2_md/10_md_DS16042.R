# 1. uzdevums. Aplūkot asinspiediena datus bloodpress.txt. Veikt daudzfaktoru
# regresiju, atlasot mainīgos (ievērojot multikolinearitāti) un veicot attiecīgo regresijas
# diagnostiku. Vai šeit ir nozīmīga mijiedarbība starp faktoriem?

dati<-read.table("bloodpress.txt",header=T)
dati1<-dati[,-1]
dati1
attach(dati1)

fit<-lm(BP~.,data=dati1)
summary(fit) #R^2 = 99%, arī t-tests uzrāda statisku nozīmību, tomēr ne visi koeficienti ir statistiksi nozīmīgi.

library(car)
sort(vif(fit)) #weight pārsniedz 4 (vif=8), aizdomas par multikolirinitāti
cor(dati1)[1,]

fit1<-lm(BP~.,data=dati1[,-3]) #modelis bez weight
summary(fit1)
sort(vif(fit1)) # visi vif<4, dispersija nav ietekmēta

step(fit1) 
fit2<-lm(formula = BP ~ Age + BSA + Pulse + Stress, data = dati1) #modelis pēc AIC, izmet dur
summary(fit2) #Stress nav statistiski nozīmīgs hvz ko darīt tālāk
sort(vif(fit2)) #vif ok
cor(dati1[,c(-3,-5)])[1,]

fitm<-lm(BP ~ Age * BSA * Pulse * Stress, data = dati1)
summary(fitm) #Nav nozīmīgas mijiedarbības

# 2. uzdevums Veikt ANOVA analīzi datiem, kurus izmantojām kontroldarbā par
# bērnu lasītprasmi. Pamatot ar Tukey post-hoc testa analīzi, no kura vecuma bērnu
# lasītprasme vairs nemainās.
src<-read.csv("dati_lasitprasme.csv", header=FALSE)

dati<-src[(4:nrow(src)),]
dati[dati==""]<-NA 
dati[dati=="?"]<-NA
names<-c("Dzimums","VardiMin","TMTA","TMTB","Vecums")
names(dati)<-names
head(dati)
attach(dati)



