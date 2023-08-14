#Verificando RUT


dgv <- function(T) {
  M <- 0
  S <- 1
  while (T > 0) {
    S <- (S + (T %% 10) * (9 - M %% 6)) %% 11
    T <- floor(T / 10)
    M <- M + 1
  }
  
  if (S == 0) {
    return(10)
  } else {
    return(S - 1)
  }
}
