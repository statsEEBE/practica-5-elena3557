
# Q2: Distribución poblacional

# a) Identificar modelo:

#VARIABLES DISCRETAS_ Binomial (encuesta) / Binomial negativa (ensayo de resistencia) / Poisson / Hipergeomètrica

#VARIABLES CONTINUAS_ Uniforme / Exponencial / Normal(timepo para el primer evento)

# b) Identificar parámetros




#Distribución poblacional es una normal

media <- 95.3
sigma <- 5.7

# N-> N(media,sigma^2)

curve(dnorm(x, mean=media, sd= sigma),xlim=c(80,120), col="red")
      
Y <- function(i){sum(rnorm(4, mean=media, sd=sigma))}
Y(1)

Y100000 <- sapply(1:100000, Y)
mean(Y100000)

hist(Y100000, freq=FALSE)
curve(dnorm(x,4*media, 2*sigma), col="red", add =TRUE)

#resultado:
4*media

#b)
Y <- function(i){sum(rnorm(4, mean=media, sd=sigma))}

Y100000 <- sapply(1:100000, Y)
var(Y100000)

100*sigma^2
#Resultado=3249

#c)
curve(dnorm(x, mean=media, sd= sigma),xlim=c(80,120), col="red")
1-pnorm(103, media, sigma)

#d)
xbar <- function(i){mean(rnorm(4, mean=media, sd=sigma))}

xbar100000 <- sapply(1:100000, xbar)
hist(xbar100000, freq=FALSE)

mean(xbar100000<98)

curve(dnorm (x, media, sigma/sqrt(4)), add=TRUE, col="red")

#e)
#Y=suma muestral = sum
#xbar= media muestral = mean
#s^2 = varianza muesta = var

Ssq <- function(i){var(rnorm(100,mean=media, sd=sigma))}
Ssq100000 <- sapply(1:100000, Ssq)
hist(Ssq100000, freq=FALSE)
mean(Ssq100000>32)

hist(Ssq100000*(100-1)/sigma^2, freq=FALSE)
curve(dchisq(x, 100-1), add=TRUE, col="red")

w <- 32*(100-1)/sigma^2
w
1-pchisq(w,100-1)
mean(Ssq100000>32)
