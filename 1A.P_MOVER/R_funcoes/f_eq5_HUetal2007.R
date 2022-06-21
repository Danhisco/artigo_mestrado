f_eq5_HUetal2007 <- function(x, speciation, migration_rate, JL, JM){
  small_theta <- 2*JL*speciation
  capital_theta <- 2*JM*speciation
  migrants <- JL*migration_rate
  f1 <- function(y) (1-y)^(capital_theta-1)/y
  .C <- 1/integrate(f1,1/JM, 1)$value
  c1 <- function(Q){
    lc1 <- lgamma(2*migrants+small_theta+1) -
      (lgamma(2*migrants*(1-Q)+small_theta) + lgamma(2*migrants*Q+1))
    exp(lc1)
  }
  f2 <- function(Q){
    c1(Q)*(1-x)^(2*migrants*(1-Q)+small_theta-1)*x^(2*migrants*Q-1)*(1-Q)^(capital_theta-1)/Q
    }
  l_ <- try(integrate(f2, 1/JM, 1))
  return(ifelse(class(l_)=="try-error",NA,.C*l_$value))
}
