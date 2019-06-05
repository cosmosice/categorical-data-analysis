rm(list = ls())
M <- matrix(c(306,457,524,383,621,835),nr=2,byrow = T)
M6 <- matrix(c(383,621,835,89,99,123),nr=2,byrow = T)
G <- 0
H <- 0
tmp2 <- 0
tmp3 <- 0
for (j in 1:2) {
  for (t in (j+1):3) {
    tmp <- M6[2,t]
    tmp2 <- sum(tmp2,tmp)
  }
  tmp3 <- M6[1,j]
  G <- sum(G,tmp3*tmp2)
}
tmp2 <- 0
tmp3 <- 0
for (j in 2:3) {
  for (t in 1:(j-1)) {
    tmp <- M6[2,t]
    tmp2 <- sum(tmp2,tmp)
  }
  tmp3 <- M6[1,j]
  H <- sum(H,tmp3*tmp2)
}
G
H
G-H
