# 1. uzdevums. Optometru datiem par bērnu lasītprasmi (kas izmantoti eksāmenā
# un ielikti serverī) veikt ANOVA analīzi, aizstājot novērojumus ar viņu rangiem.
# Pārbaudīt, no kura vecuma (un klases) lasītprasme vairs statistiski nozīmīgi neatšķiras. 
# Parādīt tabulas formā iegūtās Tukey post-hoc testa p-vērtības un salīdzināt,
# kur ir iegūtas lielākās atšķirības ar klasisko ANOVA analīzi.

src<-read.csv("dati_lasitprasme.csv", header=FALSE, dec=".", stringsAsFactors=FALSE)

dati<-src[(4:nrow(src)),]
dati[dati==""]<-NA 
dati[dati=="?"]<-NA
names<-c("Dzimums","VardiMin","TMTA","TMTB","Vecums")
names(dati)<-names
dati$VardiMin<-as.numeric(dati$VardiMin)
dati$Vecums<-factor(dati$Vecums)
attach(dati)
fit<-lm(VardiMin~Vecums, data = dati)
kruskal.test(VardiMin~Vecums, data = dati)
