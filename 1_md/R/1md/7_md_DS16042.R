# Hipotēžu pārbaude divām izlasēm. No grāmatas A modern
# introduction to probability and statistics izmantosim datus par sausas un
# slapjas urbšanas vidējiem laikiem (angliski mean drill time), kas aprakstīti,
# piemēram, 221 - 223.lpp.

#1. Aprakstīt izvēlētos datus. Ielasīt urbšanas datus programmā R.
#   Pamatot, kāpēc svarīgi salīdzināt slapjas un sausas urbšanas vidējos
#   laikus.
dati<-read.table(file="drill.txt",header=TRUE)
depth<-c(dati$Depth,dati$Depth2)
dry<-c(dati$Dry,dati$Dry2)
wet<-c(dati$Wet,dati$Wet2)

#Var noskaidrot vai dziļums un urbšanas veids ietekmē urbšanas laiku.

#2. Programmā R konstruēt izkliedes grafikus vidējam urbšanas laikam
#   attiecībā pret dziļumu. Vai urbšanas laiki atkarīgi no urbšanas
#   dziļuma? Līdz kuram dziļumam? Pārdomāt kādi varētu būt iemesli.
par(mfrow=c(1,2)) 
plot(depth,dry,xlab="dziļums",ylab="Vidējais urbšanas laiks",main="Sausā urbšana")
plot(depth,wet,xlab="dziļums",ylab="Vidējais urbšanas laiks",main="Slapjā urbšana")

# Analizējot grafikus, var noevērot, ka līdz aptuveni 250m dziļums neietekmē urbšanas laiku.
# Pēc tam laiks pieaug proporcionāli urbšanas dziļumam

#3. Konstruēt kastu grafikus slapjai un sausai urbšanai (vienā grafikā),
#   iedot aprakstošas statistikas. Salīdzināt abas izlases. Konstruēt
#   kvantiļu-kvantiļu grafiku. Vai dispersijas varētu abām izlasēm būt vienādas?
par(mfrow=c(1,1))
boxplot(dry,wet,main="Box-plot", names=c("Sausā urbšana","Slapjā urbšana"))

summary(dry)
summary(wet)

qqplot(dry,wet,main="Q-Q plot", xlab="Sausā urbšana", ylab="Slapjā urbšana")
# Grafikā redzams, ka rezultāti kārtojas vienā līnijā, tad dispersijas varētu būt vienādas.

#4. Izmantojot divu izlašu t-testu (R komanda t.test) salīdzināt abas
#   izlases. Salīdzināt p-vērtības t-testam, pieņemot, ka dispersijas ir
#   vienādas un ka nav vienādas. Vai dispersiju vienādība varētu būt
#   pamatota? Veikt attiecīgo hipotēžu pārbaudi par dispersiju
#   vienādību (komanda var.test). Veikt abu izlašu salīdzinājumu

t.test(dry,wet,var.equal=TRUE)#h0 hipotēze, ka dispersijas ir vienādas
t.test(dry,wet,var.equal=FALSE)#h1 hipotēze, ka dispersijas nav vienādas
var.test(dry,wet)# p-vērtība = 0.05288 nevar noraidīt hipotēzi, ka dispersijas ir vienādas

#Abu izlašu salīdzinašana, ja urbšanas laiku neietekmē dziļums līdz aptuveni 250

dry250<-dry[1:50]
wet250<-wet[1:50]

boxplot(dry250,wet250,main="Box-plot", names=c("Sausā urbšana","Slapjā urbšana"))

summary(dry250)
summary(wet250)

qqplot(dry250,wet250,main="Q-Q plot", xlab="Sausā urbšana", ylab="Slapjā urbŠana")

t.test(dry250,wet250,var.equal=TRUE)#Pieņem, ka dispersijas ir vienādas
t.test(dry250,wet250,var.equal=FALSE)#Pieņem, ka dispersijas nav vienādas

#5. Atrast ticamības intervālus vidējo vērtību starpībai abām izlasēm.
#   Vai 0 atrodas 95% ticamibas intervālā? (tad nenoraidām hipotēzi par vidējo vērtību vienādību).
n1<-length(wet);n1
n2<-length(dry);n2

t<-1.96
SP<-sqrt(((n1-1)*sd(wet)^2+(n2-1)*sd(dry)^2)/(n1+n2-2))
right<-(mean(wet)-mean(dry))-t*SP*sqrt(1/n1+1/n2)
left<-(mean(wet)-mean(dry))+t*SP*sqrt(1/n1+1/n2)
interval<-c(right,left)
interval 
#0 neatrodas ticamābas intervālā, noraidām hipotēzi par vid.vērt.vienādību