###################################PCA######################################$###

install.packages("devtools")
library(ISLR)
library(devtools)
install_github("vqv/ggbiplot")
library(ggbiplot)

A = data.frame(iris)
sf = sample(2,nrow(A),replace = TRUE,prob = c(0.7,0.3))
trd = A[sf==1,]
tsd = A[sf==2,]

str(A)
nums = unlist(lapply(trd, is.numeric))  
nums
B = trd[ , nums==TRUE]
B
B1 = na.omit(B)

model2 = prcomp(B1)
model2
psych::pairs.panels(model2$x)
str(model2)
plot(model2$x[,1],model2$x[,2])
ggbiplot(model2)
plot(model2)