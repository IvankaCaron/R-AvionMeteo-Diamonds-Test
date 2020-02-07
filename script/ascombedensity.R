library(ggplot2)
library(dplyr)
anscombe <- datasets::anscombe
anscombe
summary((anscombe))

model1 <-  lm(y1 ~ x1, data = anscombe)
apply(anscombe[,1:4],2,mean)
apply(anscombe[,1:4],2,var)
apply(anscombe[,5:8],2,mean)
apply(anscombe[,5:8],2,var)
cor(anscombe)[1:4,5:8]
diag(cor(anscombe)[1:4,5:8])
summary(model1)
predict(model1)

model2 <-  lm(y2 ~ x2, data = anscombe)
summary(model2)
model3 <-  lm(y3 ~ x3, data = anscombe)
summary(model3)
model4 <-  lm(y4 ~ x4[!is.na(x4)], data = anscombe)
summary(model4)
anscombe%>%ggplot(aes(x1, y1)) +
  geom_point() +
  geom_smooth(method = 'lm', fullrange = T)

anscombe%>%ggplot(aes(x2, y2)) +
  geom_point() +
  geom_smooth(method = 'lm', fullrange = T)

anscombe%>%ggplot(aes(x3, y3)) +
  geom_point() +
  geom_smooth(method = 'lm', fullrange = T)

anscombe%>%ggplot(aes(x4, y4)) +
  geom_point() +
  geom_smooth(method = 'lm')



plot(anscombe$x1, anscombe$y1,
     xlim = c(4, 20),
     ylim = c(4, 14), pch = 20)
abline(lm(anscombe$y1 ~ anscombe$x1), lty = 2)

par(mfrow = c(2, 2), mar = c(2, 2, 0, 0)+.1)
f <- function(df) {
  names(df) = c("x", "y")
  m = lm(y ~ x, data = df)
  plot(y ~ x, data = df, pch = 19, bty = "n", 
       xlim = range(anscombe[,1:4]),
       ylim = range(anscombe[,5:8]))
  abline(m, col = "red")
}
for (i in 1:4) {
  f(anscombe[,paste(c("x", "y"), i, sep = "")])
}

residu1 <-  rstudent(model1)   
residu1
plot(density(residu1))
residu2 <-  rstudent(model2)   
plot(density(residu2))
residu3 <-  residuals(model3)   
plot(density(residu3))
residu4 <-  residuals(model4)   
residu4
plot(density(residu4))

residuals(model1)

densite <- density(residuals(model1))
densite
hist(residuals(model1),col="yellow",freq=F)
lines(densite, col = "red",lwd=3)

densite <- density(residuals(model2))
densite
hist(residuals(model2),col="yellow",freq=F)
lines(densite, col = "red",lwd=3)

densite <- density(residuals(model3))
densite
hist(residuals(model3),col="yellow",freq=F)
lines(densite, col = "red",lwd=3)

densite <- density(residuals(model4))
densite
hist(residuals(model4),col="yellow",freq=F)
lines(densite, col = "red",lwd=3)

