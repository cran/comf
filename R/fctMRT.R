# Calculation of MRT

# Function takes input parameters from room geometry, temperature, and position and returns MRT

calcMRT <- function(a, b, c, Fmax, A, B, C, D, E, Temperatures){
  # Zwischenspeicher
  tau <- matrix(0, 24 , 3)
  j <- 1
  for(i in 1:nrow(tau)){
    if((i>=17)&&(identical((i+3)%%4,0)))
    {j <- 2}
    else{ j <- 1}
    tau[i,1] <- a[i]/c[i]*B[j]+A[j]
    tau[i,2] <- C[j]+D[j]*b[i]/c[i]+E[j]*a[i]/c[i]
    tau[i,3] <- Fmax[j]*(1-exp(-a[i]/(c[i]*tau[i,1])))*(1-exp(-b[i]/(c[i]*tau[i,2])))
  }
  m <- 1
  #Zwischenspeicher
  midstep <- matrix(0,nrow = 6, ncol = 1)
  for(i in 1:nrow(tau)){
    if(i > m*4){m<-m+1}
    midstep[m,1] <- midstep[m,1] + tau[i,3]
  }

  # Correcting temperatures
  Sumtemp=0
  {summid=0
  for(i in 1:nrow(midstep)){summid <- summid + midstep[i,1]}
  for(i in 1:length(Temperatures)){Sumtemp <- Sumtemp + midstep[i,1]*(Temperatures[i]+273.15)^4}}
  # Calculating MRT
  MRT = (Sumtemp/summid)^(1/4)-273.15
  MRT
}