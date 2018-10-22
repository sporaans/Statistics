# Uzdevums
# Aprakstīt programmā R iebūvēto datu piemēru
# InsectSprays. Konstruēt kastu grafiku, kur salīdzināti visi kukaiņu skati
# pēc to apsmidzināšanas veidiem vienā grafikā. Kuras izlases varētu būt
# līdzīgākas? Aprakstīt izlases ar aprakstošo statistiku palīdzību. Pārbaudīt
# ar histogrammu, kvantiļu - kvantiļu un varbūtību - varbūtību grafiku
# palīdzību, vai 5 dažādās izlases varētu būt normāli sadalītas. Tās izlases,
# kuras līdzīgas pa divi savā starpā, salīdzināt ar kvantiļu - kvantiļu un
# varbūtību - varbūtību grafiku palīdzību

attach(InsectSprays)

names(InsectSprays)
boxplot(count~spray, xlab="Tips", ylab="Skaits", main="Box-Plot, InsectSpray")

# Līdzīgas savā starpā varētu būt A, B ,F izlases, kā arī C,D, E.  
# Aprakstošā statstikas
summary(count[spray=="A"])
summary(count[spray=="B"])
summary(count[spray=="C"])
summary(count[spray=="D"])
summary(count[spray=="E"])
summary(count[spray=="F"])

par(mfrow=c(1,3))
qqnorm(count[spray=="A"])
qqline(count[spray=="A"]) 

f<-ecdf(count[spray=="A"])
p<-pnorm(sort(count[spray=="A"]),mean(count[spray=="A"]),sd(count[spray=="A"]))
plot(f(sort(count[spray=="A"])),p,xlab="Teoretiskas varbutibas",
	ylab="Paraugu varbutibas",
	main="Normal P-P plot",ylim=c(0,1))
abline(0,1)
hist(count[spray=="A"], xlab="Count, Spray A", )



par(mfrow=c(1,3))
qqnorm(count[spray=="B"])
qqline(count[spray=="B"]) 


f<-ecdf(count[spray=="B"])
p<-pnorm(sort(count[spray=="B"]),mean(count[spray=="B"]),sd(count[spray=="B"]))
plot(f(sort(count[spray=="B"])),p,xlab="Teoretiskas varbutibas",
	ylab="Paraugu varbutibas",
	main="Normal P-P plot",ylim=c(0,1))
abline(0,1)
hist(count[spray=="B"], xlab="Count, Spray B", )



par(mfrow=c(1,3))
qqnorm(count[spray=="C"])
qqline(count[spray=="C"]) 

f<-ecdf(count[spray=="C"])
p<-pnorm(sort(count[spray=="C"]),mean(count[spray=="C"]),sd(count[spray=="C"]))
plot(f(sort(count[spray=="C"])),p,xlab="Teoretiskas varbutibas",
	ylab="Paraugu varbutibas",
	main="Normal P-P plot",ylim=c(0,1))
abline(0,1)
hist(count[spray=="C"], xlab="Count, Spray C", )



par(mfrow=c(1,3))
qqnorm(count[spray=="D"])
qqline(count[spray=="D"]) 

f<-ecdf(count[spray=="D"])
p<-pnorm(sort(count[spray=="D"]),mean(count[spray=="D"]),sd(count[spray=="D"]))
plot(f(sort(count[spray=="D"])),p,xlab="Teoretiskas varbutibas",
	ylab="Paraugu varbutibas",
	main="Normal P-P plot", ylim=c(0,1))

hist(count[spray=="D"], xlab="Count, Spray D", )

par(mfrow=c(1,3))
qqnorm(count[spray=="E"])
qqline(count[spray=="E"]) 

f<-ecdf(count[spray=="E"])
p<-pnorm(sort(count[spray=="E"]),mean(count[spray=="E"]),sd(count[spray=="E"]))
plot(f(sort(count[spray=="E"])),p,xlab="Teoretiskas varbutibas",
	ylab="Paraugu varbutibas",
	main="Normal P-P plot",ylim=c(0,1))
abline(0,1)
hist(count[spray=="E"], xlab="Count, Spray E", )

par(mfrow=c(1,3))
qqnorm(count[spray=="F"])
qqline(count[spray=="F"])
 
f<-ecdf(count[spray=="F"])
p<-pnorm(sort(count[spray=="F"]),mean(count[spray=="F"]),sd(count[spray=="F"]))
plot(f(sort(count[spray=="F"])),p,xlab="Teoretiskas varbutibas",
	ylab="Paraugu varbutibas",
	main="Normal P-P plot",ylim=c(0,1))
abline(0,1)
hist(count[spray=="F"], xlab="Count, Spray F", )


##Salīdzina A,B,F un C,D,E
par(mfrow=c(1,3))
qqplot(count[spray=="A"],count[spray=="B"],xlab="Count, A", ylab="Count, B", main="Q-Q plot")
qqplot(count[spray=="A"],count[spray=="F"],xlab="Count, A", ylab="Count, F", main="Q-Q plot")
qqplot(count[spray=="B"],count[spray=="F"],xlab="Count, B", ylab="Count, F", main="Q-Q plot")
fA<-ecdf(count[spray=="A"])
fB<-ecdf(count[spray=="B"])
fF<-ecdf(count[spray=="B"])
min<-min(count[spray=="A"],count[spray=="B"],count[spray=="F"])
max<-max(count[spray=="A"],count[spray=="B"],count[spray=="F"])
x<-seq(min,max,by=0.01)

plot(fA(x),fB(x), xlab="Count, A", ylab="Count, B", main="P-P plot")
lines(c(0,1),c(0,1))
plot(fA(x),fF(x), xlab="Count, A", ylab="Count, F", main="P-P plot")
lines(c(0,1),c(0,1))
plot(fB(x),fF(x), xlab="Count, B", ylab="Count, F", main="P-P plot")
lines(c(0,1),c(0,1))


par(mfrow=c(1,3))
qqplot(count[spray=="C"],count[spray=="D"],xlab="Count, C", ylab="Count, D", main="Q-Q plot")
qqplot(count[spray=="C"],count[spray=="E"],xlab="Count, C", ylab="Count, E", main="Q-Q plot")
qqplot(count[spray=="D"],count[spray=="E"],xlab="Count, D", ylab="Count, E", main="Q-Q plot")

fC<-ecdf(count[spray=="C"])
fD<-ecdf(count[spray=="D"])
fE<-ecdf(count[spray=="E"])
min<-min(count[spray=="C"],count[spray=="D"],count[spray=="E"])
max<-max(count[spray=="C"],count[spray=="D"],count[spray=="E"])
x<-seq(min,max,by=0.01)

plot(fC(x),fD(x), xlab="Count, C", ylab="Count, D", main="P-P plot")
lines(c(0,1),c(0,1))
plot(fC(x),fE(x), xlab="Count, C", ylab="Count, E", main="P-P plot")
lines(c(0,1),c(0,1))
plot(fD(x),fE(x), xlab="Count, D", ylab="Count, E", main="P-P plot")
lines(c(0,1),c(0,1))

