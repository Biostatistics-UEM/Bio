n <- 100
desvio <- 2.34
ep <- desvio/sqrt(n)
media <- 74
IC <- media+qnorm(c(0.025,0.975))*ep
IC



tempo <- c(2.9, 3.4, 3.5, 4.1, 4.6, 4.7, 4.5, 3.8, 5.3, 4.9, 4.8, 5.7, 5.8, 5, 3.4, 5.9,
           6.3, 4.6, 5.5, 6.2)
tempo
media <- mean(tempo)
n <- length(tempo)
ep <- sd(tempo)/sqrt(n)
IC <- media+qt(c(0.025, 0.975),n-1)*ep
IC
t.test(tempo)

tree<-c(19.4, 21.4, 22.3, 22.1, 20.1, 23.8, 24.6, 19.9, 21.5, 19.1, 18.9, 22.2,
21.6, 22.7, 20.2)
tree
n<-length(tree)
n

media<-mean(tree)
media
ep <- sd(tree)/sqrt(n)
IC <- media+qt(c(0.025, 0.975), n-1)*ep
IC
t.test(tree,conf.level=.99)


n <- 100
pa <- 0.45
pb <- 0.55
epa <- sqrt(pa*(1-pa)/n)
IC <- pa+qnorm(c(0.025,.975))*epa
IC
epb <- sqrt(pb*(1-pb)/n)
ICb <- pb+qnorm(c(0.025,.975))*epb
ICb

prop.test(32,n=43)
prop.test(0.6*50, n=50)

peso<-c(98,97,102,100,98,101,102,105,95,102,100)
peso
n <- 11
variancia <- var(peso)
IC <- (n-1)*variancia/qchisq(c(.975, 0.025),n-1)
IC

dados <-
    read.table('http://www.ics.uci.edu/~babaks/BWR/Home_files/BodyTemperature.txt',
               h=T)
head(dados)
t.test(dados$Temperature)
n1 <- sample(dados$Temperature, 20)
n2 <- sample(dados$Temperature, 20)
n3 <- sample(dados$Temperature, 20)
n4 <- sample(dados$Temperature, 20)
n5 <- sample(dados$Temperature, 20)

t.test(n1)$conf.int
t.test(n2)$conf.int
t.test(n3)$conf.int
t.test(n4)$conf.int
t.test(n5)$conf.int
