
# Uzdevums
# Centrālā robežteorēma. 
# 1)Ģenerēt skaitā n = 2, n = 10 un
# n = 50 datus no X^2, exp(1), U[−1, 1] un LogN(1, 1) sadalījumiem, 
# 2)aprēķināt X(vid). 
# Atkārtot 1) un 2) soli N = 1000 reizes un konstruēt
# atbilstošās histogrammas. Pievienot atbilstošo teorētisko (asimptotisko)
# robežsadalījumu, kuru var iegūt no centrālās robežteorēmas. 
# Kuriem sadalījumiem aproksimācija ir labāka? Vai
# datu apjoms n = 100 ir pietiekošs, lai redzētu konverģenci? Programmas
# kodu ar secinājumiem nodot, visus grafikus noformēt, saglabāt un nodot
# kopā ar mājasdarbu.


#Vienmērīgais sadalījums U[−1, 1]
fun.crt<-function(n)
{
  dati<-runif(n,-1,1)
  stat<-sqrt(n)*(mean(dati)-mu)/sigma
}

par(mfrow=c(2,2))

N<-1000
mu<-0      # (a+b)/2, kur a=-1, b=1
sigma2<-1/3 # (b-a)^2/12, kur a=-1, b=1
sigma<-sqrt(sigma2)

n<-2
rez<-replicate(N,fun.crt(n)) 
hist(rez,prob=T,main = ("Vienmērīgais sadalījums n=2"))
x<-seq(-4,4,by=0.01)
lines(x,dnorm(x),lwd=2,col="red") # pievienojam N(0,1)
lines(x,dnorm(x,mu,sigma2/n),lwd=2,col="blue")

n<-10
rez<-replicate(N,fun.crt(n)) 
hist(rez,prob=T,main = ("Vienmērīgais sadalījums n=10"))
x<-seq(-4,4,by=0.01)
lines(x,dnorm(x),lwd=2,col="red")
lines(x,dnorm(x,mu,sigma2/n),lwd=2,col="blue")

n<-50
rez<-replicate(N,fun.crt(n)) 
hist(rez,prob=T,main = ("Vienmērīgais sadalījums n=50"))
x<-seq(-4,4,by=0.01)
lines(x,dnorm(x),lwd=2,col="red")
lines(x,dnorm(x,mu,sigma2/n),lwd=2,col="blue")

n<-100
rez<-replicate(N,fun.crt(n)) 
hist(rez,prob=T,main = ("Vienmērīgais sadalījums n=100"))
x<-seq(-4,4,by=0.01)
lines(x,dnorm(x),lwd=2,col="red")
lines(x,dnorm(x,mu,sigma2/n),lwd=2,col="blue")

#HI-kvadrāta sadalījums 
fun.crt<-function(n)
{
  dati<-rchisq(n,1)
  stat<-sqrt(n)*(mean(dati)-mu)/sigma
}

par(mfrow=c(2,2))
N<-1000
mu<-1      # mu = k , k=1
sigma2<-2*mu
sigma<-sqrt(sigma2)


n<-2
rez<-replicate(N,fun.crt(n)) 
hist(rez,prob=T,main = ("HI-kvadrāta sadalījums n=2"))
x<-seq(-4,4,by=0.01)
lines(x,dnorm(x),lwd=2,col="red") # pievienojam N(0,1)
lines(x,dnorm(x,mu,sigma2/n),lwd=2,col="blue")

n<-10
rez<-replicate(N,fun.crt(n)) 
hist(rez,prob=T,main = ("HI-kvadrāta sadalījums n=10"))
x<-seq(-4,4,by=0.01)
lines(x,dnorm(x),lwd=2,col="red")
lines(x,dnorm(x,mu,sigma2/n),lwd=2,col="blue")

n<-50
rez<-replicate(N,fun.crt(n)) 
hist(rez,prob=T,main = ("HI-kvadrāta sadalījums n=50"))
x<-seq(-4,4,by=0.01)
lines(x,dnorm(x),lwd=2,col="red")
lines(x,dnorm(x,mu,sigma2/n),lwd=2,col="blue")

n<-100
rez<-replicate(N,fun.crt(n)) 
hist(rez,prob=T,main = ("HI-kvadrāta sadalījums n=100"))
x<-seq(-4,4,by=0.01)
lines(x,dnorm(x),lwd=2,col="red")
lines(x,dnorm(x,mu,sigma2/n),lwd=2,col="blue")

#EXP(1) sadalījums 
fun.crt<-function(n)
{
  dati<-rexp(n,1)
  stat<-sqrt(n)*(mean(dati)-mu)/sigma
}

par(mfrow=c(2,2))
N<-1000
mu<-1      # 1/lambda,   lambda=1
sigma2<-1  # 1/lambda^2, lambda=1
sigma<-sqrt(sigma2)

n<-2
rez<-replicate(N,fun.crt(n)) 
hist(rez,prob=T,main = ("EXP(1) sadalījums n=2"))
x<-seq(-4,4,by=0.01)
lines(x,dnorm(x),lwd=2,col="red") # pievienojam N(0,1)
lines(x,dnorm(x,mu,sigma2/n),lwd=2,col="blue")

n<-10
rez<-replicate(N,fun.crt(n)) 
hist(rez,prob=T,main = ("EXP(1) sadalījums n=10"))
x<-seq(-4,4,by=0.01)
lines(x,dnorm(x),lwd=2,col="red")
lines(x,dnorm(x,mu,sigma2/n),lwd=2,col="blue")

n<-50
rez<-replicate(N,fun.crt(n)) 
hist(rez,prob=T,main = ("EXP(1) sadalījums n=50"))
x<-seq(-4,4,by=0.01)
lines(x,dnorm(x),lwd=2,col="red")
lines(x,dnorm(x,mu,sigma2/n),lwd=2,col="blue")

n<-100
rez<-replicate(N,fun.crt(n)) 
hist(rez,prob=T,main = ("EXP(1) sadalījums n=100"))
x<-seq(-4,4,by=0.01)
lines(x,dnorm(x),lwd=2,col="red")
lines(x,dnorm(x,mu,sigma2/n),lwd=2,col="blue")

#Lognormālais  sadalījums LogN(1, 1)
fun.crt<-function(n)
{
  dati<-rlnorm(n,1,1) 
  stat<-sqrt(n)*(mean(dati)-mu)/sigma
}

par(mfrow=c(2,2))
N<-1000
mu<-exp(1.5)     # mu=exp(mu+sigma^2/2), kur mu=1, sigma^2=1
sigma2<-(exp(1)-1)*exp(3)  # 1/lambda^2, lambda=1
sigma<-sqrt(sigma2) # (exp(sigma^2)-1)*exp(2*mu+sigma^2)

n<-2
rez<-replicate(N,fun.crt(n)) 
hist(rez,prob=T,main = ("Lognormālais sadalījums n=2"))
x<-seq(-4,4,by=0.01)
lines(x,dnorm(x),lwd=2,col="red") # pievienojam N(0,1)
lines(x,dnorm(x,mu,sigma2/n),lwd=2,col="blue")

n<-10
rez<-replicate(N,fun.crt(n)) 
hist(rez,prob=T,main = ("Lognormālais sadalījums n=10"))
x<-seq(-4,4,by=0.01)
lines(x,dnorm(x),lwd=2,col="red")
lines(x,dnorm(x,mu,sigma2/n),lwd=2,col="blue")

n<-50
rez<-replicate(N,fun.crt(n)) 
hist(rez,prob=T,main = ("Lognormālais sadalījums n=50"))
x<-seq(-4,4,by=0.01)
lines(x,dnorm(x),lwd=2,col="red")
lines(x,dnorm(x,mu,sigma2/n),lwd=2,col="blue")

n<-100
rez<-replicate(N,fun.crt(n)) 
hist(rez,prob=T,main = ("Lognormālais sadalījums n=100"))
x<-seq(-4,4,by=0.01)
lines(x,dnorm(x),lwd=2,col="red")
lines(x,dnorm(x,mu,sigma2/n),lwd=2,col="blue")

#Secinajumi
# Labako aproksimaciju var noverot exponenciālajam un vienmērīgajam sadalījumam pie n=100. 
# Pārējiem sadalījumiem būtu nepieciešams lielāks datu apjoms
