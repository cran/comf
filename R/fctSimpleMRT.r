# Simple calculation of MRT
# Assumes a room with only right angles

calcSimpleMRT<-function(length,width,height,position,Temperatures)
{
  a <- c(abs(position[2]),abs(position[2]-length),
         abs(position[2]),abs(position[2]-length),
         abs(position[2]),abs(position[2]-length),
         abs(position[2]),abs(position[2]-length),
         abs(position[3]),abs(position[3]-width),
         abs(position[3]),abs(position[3]-width),
         abs(width-position[3]),abs(position[3]),
         abs(width-position[3]),abs(position[3]),
         abs(position[2]),abs(position[2]-length),
         abs(position[2]),abs(position[2]-length),
         abs(position[2]),abs(position[2]-length),
         abs(position[2]),abs(position[2]-length))
  b <- c(abs(position[1]-height),abs(position[1]-height),
         abs(position[1]),abs(position[1]),
         abs(position[1]),abs(position[1]),
         abs(position[1]-height),abs(position[1]-height),
         abs(height-position[1]),abs(position[1]),
         abs(height-position[1]),abs(position[1]),
         abs(position[1]),abs(height-position[1]),
         abs(position[1]),abs(height-position[1]),
         abs(position[2]),abs(length-position[2]),
         abs(position[2]),abs(length-position[2]),
         abs(position[2]),abs(length-position[2]),
         abs(position[2]),abs(length-position[2])
         )
  c <- c(abs(width-position[3]),abs(width-position[3]),
         abs(width-position[3]),abs(width-position[3]),
         abs(position[3]),abs(position[3]),
         abs(position[3]),abs(position[3]),
         abs(position[2]),abs(position[2]),
         abs(position[2]),abs(position[2]),
         abs(position[2]-length),abs(position[2]-length),
         abs(position[2]-length),abs(position[2]-length),
         abs(position[1]),abs(position[1]),
         abs(position[1]),abs(position[1]),
         abs(height-position[1]),abs(height-position[1]),
         abs(height-position[1]),abs(height-position[1]))
  Fmax <- c(0.118,0.116)
  A <- c(1.216,1.396)
  B <- c(0.169,0.130)
  C <- c(0.717,0.951)
  D <- c(0.087,0.080)
  E <- c(0.052,0.055)
  MRT <- calcMRT(a,b,c,Fmax,A,B,C,D,E,Temperatures)
  MRT
}