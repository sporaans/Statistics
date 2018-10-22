
# 1 uzdevums
# Izmantojot paketi jeb bibliotēku prob un materiālu https:
#  //cran.r-project.org/web/packages/IPSUR/vignettes/IPSUR.pdf
# definēt varbūtību telpu eksperimentam, kur met divus spēļu kauliņus.
# Aprēķināt pirmās nodarbības 12.slaida uzdotos mājasdarbus ar
# programmu R.

#1.1
library(prob)

S<-rolldie(2,makespace = T)

#1.1.1
  #P(A)
  pa<-sum(subset(S,X1==X2)$probs);pa
  #P(B)
  pb<-sum(subset(S,X1+X2>=7 & X1+X2<=10)$probs);pb
  #P(C)
  pc<-sum(subset(S,X1+X2==2| X1+X2==7| X1+X2==8)$probs);pc

##1.1.2
  skelums<-sum(subset(S,X1==X2 &X1+X2>=7 & X1+X2<=10 &(X1+X2==2| X1+X2==7| X1+X2==8))$probs)
  isTRUE(skelums==pa*pb*pc)#TRUE

##1.1.3
  pab<-sum(subset(S,X1==X2 & X1+X2>=7 & X1+X2<=10)$probs)
  isTRUE(pab==pa*pb)#FALSE
  
  pbc<-sum(subset(S,X1+X2>=7 & X1+X2<=10 &(X1+X2==2| X1+X2==7| X1+X2==8))$probs)
  isTRUE(pbc==pb*pc)#FALSE
  
#1.2
K<-rolldie(3,makespace = T)
#labveligo skaits
pl<-nrow(subset(K,X1!=X2 & X1!=X3 &X2!=X3 &(X1==1|X2==1|X3==1)))

#visu skaits
pv<-nrow(subset(K,X1!=X2 & X1!=X3 &X2!=X3))

#rezultats
p2<-pl/pv;p2 #0.5

#2 uzdevums
# Izveidot funkciju, kas diskrētam gadījuma lielumam (kuram iepriekš uzdots sadalījuma likums)
# izrēķina matemātisko cerību, dispersiju,
# standartnovirzi; kādā uzdotā punktā aprēķina sadalījuma funkciju un to
# uzzīmē visā definīcijas apgabalā. Šai funkcijai arī jāizrēķina varbūtību
# piederēt kādā fiksētā intervālā, tas ir, P(a < X < b) un jāuzzīmē
# f(x) = P(X = x) visā definīcijas apgabalā. Abus grafikus novietot blakus.
# Pielietot funkciju, ja X ~ Bernulli(p) un ja X ~ Binomial(n, p).


## jauzdod sadalîjums likums!
p<-0.5
#s.likums<-data.frame(kol1=c(0,1-p),kol2=c(1,p));s.likums
s.likums<-matrix(c(0,1,1-p,p),2,2,byrow=T)
s.likums

ff<-function(s.likums,A,B)
{
  # matemâtiskâ cerîba
  mat.cer<-sum(s.likums[1,]*s.likums[2,])
  # P(A<X<B)
  n<-length(s.likums[1,])
  sk1<-sum(s.likums[1,]>A)
  sk2<-sum(s.likums[1,]<B)
  varb<-sum(s.likums[2,(n-sk1+1):sk2])
  
  # varbûtîbu diskrçtâ blîvuma funkcija
  par(mfrow=c(1,2))
  plot(as.vector(as.matrix(s.likums[1,])),
       as.vector(as.matrix(s.likums[2,])),
       xlab="",ylab="")
  # sadalîjuma funkcija
  cumsum(as.vector(as.matrix(s.likums[2,]))) 
  
  list(mat.cer=mat.cer,varb=varb)
}
ff(s.likums,-3,3)




  