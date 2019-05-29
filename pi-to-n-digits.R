# Calculation of pi to n-digits using the Chudnovsky algorithm
library(Rmpfr)

piDigits <- function(n){

  # Approximately 3 bits per decimal place, and 14 digits per iteration;
  # uses conservative buffer of 3.5 bits per decimial
  bits = 3.5*n
  s=0
  iterations = ceiling(n/14)
  for (k in 0:iterations){
    x=factorialMpfr(6*k,precBits = bits)*mpfr((545140134*k + 13591409),bits)
    y=factorialMpfr(3*k,precBits = bits)*factorialMpfr(k,precBits = bits)^3 * mpfr(-262537412640768000,bits)^k
    s = s + x/y
  }
  a=(426880 * sqrt(mpfr(10005,bits)))/s
  
  print(a,digits=n)
  
  # to check accuracy against actual value
  # print(a-Const('pi',prec=bits),digits=20)
}