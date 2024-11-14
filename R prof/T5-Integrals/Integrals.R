#Numerical integration

#STANDARD NORMAL DENSITY

SND1 <- function(x){
  res1 <- 1/sqrt(2*pi)*exp(-0.5*x^2)
  return(res1)
}

x1 <- seq(-3,3, by = 0.01)
plot (x1, SND1(x1), type = "l")

#BOUNDS OF INTEGRATION -2 TO -1

a <- -2
b <- -1

#Develop a partition
deltaX <- 0.1
part1 <- seq(a,b,by = deltaX)

val1 <- 0
for (i in 2: length(part1)){
  val1 <- val1 + SND1(part1[ i ])*deltaX
}

val1

#Real answer   0.1359051

pnorm(-1,0,1) - pnorm(-2,0,1)

sum(SND1(part1[2:length(part1)])*deltaX)

