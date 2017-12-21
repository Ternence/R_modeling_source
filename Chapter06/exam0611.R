Anscombe<-data.frame(
  X =c(10.0, 8.0, 13.0, 9.0, 11.0, 14.0, 6.0, 4.0, 12.0, 7.0, 5.0),
  Y1=c(8.04, 6.95, 7.58, 8.81, 8.33, 9.96, 7.24, 4.26, 10.84, 4.82, 5.68),
  Y2=c(9.14, 8.14, 8.74, 8.77, 9.26, 8.10, 6.13, 3.10, 9.13, 7.26, 4.74),
  Y3=c(7.46, 6.77, 12.74, 7.11, 7.81, 8.84, 6.08, 5.39, 8.15, 6.44, 5.73),
  X4=c(rep(8,7), 19, rep(8,3)),
  Y4=c(6.58, 5.76, 7.71, 8.84, 8.47, 7.04, 5.25, 12.50, 5.56, 7.91, 6.89)
)
summary(lm(Y1~X, data=Anscombe))
summary(lm(Y2~X, data=Anscombe))
summary(lm(Y3~X, data=Anscombe))
summary(lm(Y4~X4,data=Anscombe))

attach(Anscombe)
X =c(10.0, 8.0, 13.0, 9.0, 11.0, 14.0, 6.0, 4.0, 12.0, 7.0, 5.0)
plot(c(3,20), c(3,13), type="n", xlab = "X", ylab = "Y")
points(X,Y1)
abline(lm(Y1~X))

plot(c(3,20), c(3,13), type="n", xlab = "X", ylab = "Y")
points(X,Y2)
abline(lm(Y2~X))

plot(c(3,20), c(3,13), type="n", xlab = "X", ylab = "Y")
points(X,Y3)
abline(lm(Y3~X))

plot(c(3,20), c(3,13), type="n", xlab = "X", ylab = "Y")
points(X4,Y4)
abline(lm(Y4~X4))

X2<-X^2
lm2.sol<-lm(Y2~X+X2)
summary(lm2.sol)
x<-seq(min(X), max(X), by=0.1)
b<-coef(lm2.sol)
y<-b[1]+b[2]*x+b[3]*x^2
plot(c(3,20), c(3,13), type="n", xlab = "X", ylab = "Y")
points(X,Y2)
lines(x,y)

i<-1:11; Y31<-Y3[i!=3]; X3<-X[i!=3]
lm3.sol<-lm(Y31~X3)
summary(lm3.sol)
plot(c(3,20), c(3,13), type="n", xlab = "X", ylab = "Y")
points(X,Y3)
abline(lm3.sol)

##误差的独立性
library(car)
durbinWatsonTest(lm2.sol)
durbinWatsonTest(lm3.sol)

##线性模型是否适合
##成分残差图(component plus residual)
crPlots(lm2.sol, one.page=T, ask=F)
crPlots(lm3.sol, one.page=T, ask=F)

##等方差性
###记分检验 p>0.05 等方差性 否则就是异方差性
ncvTest(lm2.sol)
ncvTest(lm3.sol)
###散点图
###car包spreadLevelPlot()函数创建一个添加了最佳拟合曲线的散点图，展示标准化残差绝对值与拟合值的关系。
### 同方差性的条件下，点应在线的周围水平随机分布。非水平的曲线暗示着异方差性。
spreadLevelPlot(lm2.sol)
spreadLevelPlot(lm3.sol)
