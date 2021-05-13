#### Masterarbeit##############################
setwd("/Users/leonjacob/Documents/Universität/Masterarbeit")
install.packages("ggplot2")
install.packages("MultipleBubbles")
install.packages("tseries")
install.packages("cowplot")
install.packages("exuber")
install.packages("xtable")
library(exuber)
library(ggplot2)
library(MultipleBubbles)
library(tseries)
library(cowplot)
library(xtable)

####### Data ####
transdata<-read.csv("transdata.csv")
temp<-transdata[,1:2]
temperature<-temp$temp*(-1)
index<-temp$kyr


####### Empirical application of Bubble testing #####
#### 1. V isual appearance of the time series itself #####
#positive values of temperature records
temp.vis <- ggplot(data=temp) + geom_line(aes(x=kyr, y=temp)) + scale_x_reverse() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill="transparent"),
        panel.border = element_rect(colour="black", fill=NA),
        text = element_text(family="serif")) +
  ylab("Temperature ") + xlab("Kyr")

ggsave("temp-vis.pdf", plot=temp.vis, width=10, height=6) 

#Negative Values of temperature records
temp.vis.2 <- ggplot(data=temp) + geom_line(aes(x=kyr, y=-temp)) + scale_x_reverse() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA),
        text = element_text(family="serif", size=10)) +
  ylab("Temperature") + xlab("Kyr")

ggsave("temp-vis-2.pdf", plot=temp.vis.2, width=15, height=10, units="cm") 

#### 2. RTADF##########################
##window size = 50 because of rule of thumb from PSY (2015)
  
#test statistic

min.window<-floor((0.01+1.8/sqrt(length(temperature)))*length(temperature))
temp.radf.0<-radf(data=temperature, minw=min.window, lag=0)
temp.radf.1<-radf(data=temperature, minw=min.window, lag=1)
temp.radf.2<-radf(data=temperature, minw=min.window, lag=2)
temp.radf.3<-radf(data=temperature, minw=min.window, lag=3)
temp.radf.6<-radf(data=temperature, minw=min.window, lag=6)
temp.radf.aic<-radf(data=temperature, minw=min.window, lag=13)

temp.radf.58<-list(c(temp.radf.0$gsadf,temp.radf.1$gsadf,temp.radf.2$gsadf,temp.radf.3$gsadf,temp.radf.6$gsadf, temp.radf.aic$gsadf), c(temp.radf.0$sadf,temp.radf.1$sadf,temp.radf.2$sadf,temp.radf.3$sadf,temp.radf.6$sadf, temp.radf.aic$sadf) )

  #critical values
set.seed(123)
crit.radf.58<-radf_mc_cv(n=798, minw=58, nrep=2000, seed=123)
crit.radf.20<-radf_mc_cv(n=798, minw=20, nrep=2000, seed=123)

  #table for the generalized adf statistic

gsadf.temp.58<-rbind(cbind(temp.radf.0$gsadf, t(crit.radf.58$gsadf_cv)), cbind(temp.radf.0$sadf, t(crit.radf.58$sadf_cv)))

  #plotting the critical values against the test statistic
radf.matrix.58<-data.frame(temp.radf.0$bsadf, crit.radf.58$bsadf_cv[,1], crit.radf.58$bsadf_cv[,2], index[59:length(index)])
colnames(radf.matrix.58)=c("test", "ten", "five", "kyr")

radf.graph.58<-ggplot(data=radf.matrix.58)+geom_line(aes(x=kyr, y=test, color="Test Statistic"))+geom_line(aes(x=kyr, y=ten, color="90 %"))+
  geom_line(aes(x=kyr, y=five, color="95 %"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA),
        text = element_text(family="serif"),
        legend.position=c(0.8,0.7),
        legend.key=element_rect(fill=NA, colour=NA),
        legend.title=element_blank())+
  scale_x_reverse()+
  ylab("BSADF")+xlab("Kyr")+ scale_color_manual(values=c("Test Statistic"="black", "90 %"="blue", "95 %"="red"))

ggsave("bsadf-graph-58.pdf", plot=radf.graph.58, width=15, height=10, units="cm")



  #minw=20
#GSADF SADF and BSADF values
temp.radf.20.0<-radf(data=temperature, minw=20, lag=0)
temp.radf.20.1<-radf(data=temperature, minw=20, lag=1)
temp.radf.20.2<-radf(data=temperature, minw=20, lag=2)
temp.radf.20.3<-radf(data=temperature, minw=20, lag=3)
temp.radf.20.6<-radf(data=temperature, minw=20, lag=6)
temp.radf.20.aic<-radf(data=temperature, minw=20, lag=13)


gsadf.temp.20<-rbind(cbind(temp.radf.20.0$gsadf, t(crit.radf.20$gsadf_cv)), cbind(temp.radf.20.0$sadf, t(crit.radf.20$sadf_cv))) #Testing values
d<-798-as.data.frame(datestamp(temp.radf.20.0, crit.radf.20)) #datestamping the bubbles

  #graphic for r0=20

radf.matrix.20<-data.frame(temp.radf.20.0$bsadf, crit.radf.20$bsadf_cv[1:778,1], crit.radf.20$bsadf_cv[1:778,2], index[21:length(index)])
colnames(radf.matrix.20)=c("test", "ten", "five", "kyr")
t<-as.data.frame(cbind(temp$temp, temp$kyr))

radf.graph.20<-ggplot(data=radf.matrix.20) + 
  geom_rect(data=d, mapping=aes(xmin=series1.Start, xmax=series1.End, ymin=-Inf, ymax=Inf), color='grey', alpha=0.2) + 
  geom_line(aes(x=kyr, y=test, color="Test Statistic"))+
  geom_line(aes(x=kyr, y=five, color="95 %"))+
  scale_x_reverse()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA),
        text = element_text(family="serif"),
        legend.position=c(0.85,0.85),
        legend.key=element_rect(fill=NA, colour=NA))+
  ylab("BSADF")+xlab("Kyr")+scale_color_manual(name="", values=c("Test Statistic"="black", "95 %"="red"))

ggsave("bsadf-graph-2.pdf", plot=radf.graph.20, width=15, height=10, units="cm")

temp.stamp<-ggplot(data=as.data.frame(transdata))+
  geom_rect(data=798-as.data.frame(datestamp(temp.radf.20.0, crit.radf.20)), mapping=aes(xmin=series1.Start, xmax=series1.End, ymin=-Inf, ymax=Inf), color='grey', alpha=0.2)+
  geom_line(aes(x=kyr, y=temp))+scale_x_reverse()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA),
        text = element_text(family="serif"))+
  ylab("Temperature")+xlab("Kyr")
ggsave("tempstamp.pdf", plot=temp.stamp, width=15, height=10, units="cm")



#### 3. CUSUM Testing####

#Cusum test statistic series
#n=size of training sample
#y=time series

## 3.1 function test statistic

cusum.test.statistics<-function(n, x){
  a<-x[(n+1):(length(x)-1)]
  S<-vector(length=length(a), mode="numeric")
  b<-diff(x)
  for(i in 1:length(a)){
    S[i]<-(a[i]-x[n])/sqrt(var(b[1:(n+i)]))
  }
  return(S)
}

# test statistic for temperature sample for different sizes of training sample n
cusum.test.20<-cusum.test.statistics(n=20, x=temperature)
cusum.test.58<-cusum.test.statistics(n=58, x=temperature)
cusum.test.100<-cusum.test.statistics(n=100, x=temperature)

#monte carlo simulations for critical values of H0

cusum.monte.carlo<-function(m, o, t, alpha){
  rep.rw<-replicate(m, arima.sim(model=list(order=c(0,1,0)), n=o))
  a<-matrix(NA, nrow=m, ncol=(o-t))
  for(i in 1:m){
    b<-cusum.test.statistics(rep.rw[,i], n=t)
    a[i,]<-b
  }
  crit<-vector(length=ncol(a))
  for (i in 1:length(crit)){
    crit[i]<-quantile(a[,i], probs=(1-alpha), na.rm=TRUE)
  }
  return(crit)
}
# critical value sequences for different values of alpha and different sizes of training sample 

cusum.mc.20.5<-cusum.monte.carlo(m=2000, o=798, t=20, alpha=0.05)
cusum.mc.20.10<-cusum.monte.carlo(m=2000, o=798, t=20, alpha=0.1)
cusum.mc.58.5<-cusum.monte.carlo(m=2000, o=798, t=58, alpha=0.05)
cusum.mc.58.10<-cusum.monte.carlo(m=2000, o=798, t=58, alpha=0.1)
cusum.mc.100.5<-cusum.monte.carlo(m=2000, o= 798, t=100, alpha=0.05)
cusum.mc.100.10<-cusum.monte.carlo(m=2000, o= 798, t=100, alpha=0.10)

#graphic cusum training sample = 20

cusum.decision.20<-data.frame(cusum.mc.20.5[1:777], cusum.test.20, index[22:length(index)], cusum.test.20-cusum.mc.20.5[1:777])
colnames(cusum.decision.20)<-c("five", "test", "kyr","x")
datestamp.cusum.20<-as.data.frame(cusum.decision.20[cusum.decision.20$x>0,3])
colnames(datestamp.cusum.20)<-"x"

cusum.graph.20<-ggplot(data=cusum.decision.20) + 
  geom_rect(data=datestamp.cusum.20, mapping=aes(xmin=x, xmax=x, ymin=-Inf, ymax=Inf), color='grey', alpha=0.2)+
  geom_line(aes(x=kyr, y=five, color="95 %")) +
  geom_line(aes(x=kyr, y=test, color="Test Statistic")) +
  scale_x_reverse()+
  ggtitle("n=20")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA),
        text = element_text(family="serif", size=10),
        legend.position="none",
        axis.title.x=element_blank(),
        plot.title=element_text(hjust=0.5)) +
  ylab("CUSUM") + scale_color_manual(name="", values=c("Test Statistic"="black", "95 %"="red"))

ggsave("cusum-testing-20.pdf", plot=cusum.graph.20, width=10, height=6)


# graphic  cusum training sample = 50

cusum.decision.58<-data.frame(cusum.mc.58.5[1:length(cusum.test.58)], cusum.test.58,index[60:length(index)])
colnames(cusum.decision.58)<-c("five", "test", "kyr")

cusum.graph.58<-ggplot(data=cusum.decision.58) + geom_line(aes(x=kyr, y=five, color="95 %"))  + 
  geom_line(aes(x=kyr, y=test, color="Test Statistic")) +
  scale_x_reverse()+
  ggtitle("n=58")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA),
        text = element_text(family="serif",size=10),
        legend.position="none",
        axis.title.x=element_blank(),
        plot.title=element_text(hjust=0.5)) +
  ylab("CUSUM") + scale_color_manual(name="", values=c("Test Statistic"="black", "95 %"="red"))

ggsave("cusum-testing-58.pdf", plot=cusum.graph.50, width=10, height=6)

# graphic cusum training sample = 100

cusum.decision.100<-data.frame(cusum.mc.100.5[1:length(cusum.test.100)], cusum.test.100, index[102:length(index)], cusum.test.100-cusum.mc.100.5[1:length(cusum.test.100)])
colnames(cusum.decision.100)<-c("five", "test", "kyr","x")
datestamp.cusum.100<-as.data.frame(cusum.decision.100[cusum.decision.100$x>0,3])
colnames(datestamp.cusum.100)<-"x"

cusum.graph.100<-ggplot(data=cusum.decision.100) +
  geom_rect(data=datestamp.cusum.100, mapping=aes(xmin=x, xmax=x, ymin=-Inf, ymax=Inf), color='grey', alpha=0.2)+
  geom_line(aes(x=kyr, y=five, color="95 %")) +
  geom_line(aes(x=kyr, y=test, color="Test Statistic")) +
  scale_x_reverse()+
  ggtitle("n=100")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA),
        text = element_text(family="serif", size=10),
        legend.position="bottom",
        legend.key=element_rect(fill=NA, colour=NA),
        plot.title=element_text(hjust=0.5)) +
  ylab("CUSUM") + xlab("Kyr") + scale_color_manual(name="", values=c("Test Statistic"="black", "95 %"="red"))

ggsave("cusum-testing-100.pdf", plot=cusum.graph.100, width=10, height=6)

all<-plot_grid(cusum.graph.20, cusum.graph.58, cusum.graph.100, nrow=3, rel_heights = c(1,1,1.35))

ggsave("alle-cusum.pdf", plot=all, height=18, width=15, units="cm")






#### 4. FLUC testing ####
# repeatedly estimating the AR coefficient and calculating ADF F-Statistics
# t=size of training sample
# y=time series under evaluation
# the test statistic is standardized by the mean and std deviation of ADF statistic under the null hypothesis

fluc.test.statistics<-function(y, t){
  n<-length(y)
  Z<-vector(length=(length(y)-t), mode="numeric")
  for(i in 1:length(Z)){
    b<-lm(y[2:(i+t)]~y[1:(i+t-1)])
    Z[i]<-(((b$coefficients[2]-1)/coef(summary(b))[2,2])+2.1814)/0.7499
  }
  return(Z)
}


fluc.monte.carlo<-function(m, o, t, alpha){
  rep.rw<-replicate(m, arima.sim(model=list(order=c(0,1,0)), n=(o-1)))
  a<-matrix(NA, nrow=m, ncol=(o-t))
  for(i in 1:m){
    b<-fluc.test.statistics(rep.rw[,i], t=t)
    a[i,]<-b
    print(i)
  }
  crit<-vector(length=ncol(a))
  for (i in 1:length(crit)){
    crit[i]<-quantile(a[,i], probs=(1-alpha), na.rm=TRUE)
    print(i)
  }
  return(crit)
}

# section of test statistic for training samples of 20,50,100

fluc.test.20<-fluc.test.statistics(y=temperature, t=20)
fluc.test.50<-fluc.test.statistics(y=temperature, t=50)
fluc.test.100<-fluc.test.statistics(y=temperature, t=100)

# fluc monte carlo simulation for critical values
# m = number of monte carlo replications
# t = size of the training sample
# alpha = significance level
# o = size of the total sample

fluc.monte.carlo<-function(m, o, t, alpha){
  rep.rw<-replicate(m, arima.sim(model=list(order=c(0,1,0)), n=(o-1)))
  a<-matrix(NA, nrow=m, ncol=(o-t))
  for(i in 1:m){
    b<-fluc.test.statistics(rep.rw[,i], t=t)
    a[i,]<-b
    print(i)
  }
  crit<-vector(length=ncol(a))
  for (i in 1:length(crit)){
    crit[i]<-quantile(a[,i], probs=(1-alpha), na.rm=TRUE)
    print(i)
  }
  return(crit)
}

#fluc monte carlo 
fluc.mc.20.5<-fluc.monte.carlo(m=2000, o=798, t=20, alpha=0.05)
fluc.mc.20.10<-fluc.monte.carlo(m=2000, o=798, t=20, alpha=0.1)


#Plot FLUC against MC 
fluc.test.matrix.20<-data.frame(fluc.test.20, fluc.mc.20.5, fluc.mc.20.10, index[21:length(index)])
colnames(fluc.test.matrix.20)<-c("Test", "Five", "Ten", "kyr")

fluc.test.graph.20<-ggplot(data=fluc.test.matrix.20)+geom_line(aes(x=kyr, y=Test, color="Test statistic"))+
  geom_line(aes(x=kyr, y=Five, color="95%")) + geom_line(aes(x=kyr, y=Ten, color="90%"))+
  scale_x_reverse()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA),
        text = element_text(family="serif", size=10),
        legend.key=element_rect(fill=NA, colour=NA),
        legend.position="bottom",
        axis.title.x = element_blank(),
        legend.title=element_blank()) +
  ylab("FLUC") + scale_color_manual(values=c("Test statistic"="black", "95%"="red", "90%"="blue"))

ggsave("fluc.pdf",plot=fluc.test.graph.20, width=15, height=8, units="cm")
sqrt(fluc.mc.20.5+log(seq(from=21, to=798)/20))
#### 5. Busetti Taylor ####


#x = vector under evaluation
#break point tau

set.seed(600)

BT<-function(x, tau0){
  a<-vector(length=(length(x)-tau0), mode="numeric")
  N<-length(x)
  sigma<-sum(diff(x)^2)/N
  for(i in 1:(N-tau0)){
    a[i]<-sum((x[N]-x[i:N])^2)/(sigma*((N-i)^2))
  }
  return(max(a))
}

BT.monte.carlo<-function(tau0, m, o, alpha){
  rep.rw<-replicate(m, arima.sim(model=list(order=c(0,1,0)), n=(o-1)))
  e<-vector(length(m), mode="numeric")
  for(i in 1:m){
    e[i]<-BT(rep.rw[,i], tau0=tau0)
    print(i)
  }
  result<-quantile(e, probs=(1-alpha), na.rm=TRUE)
  return(result)
}

BT.MC<-function(a, b){
  x<-matrix(NA, nrow=1, ncol=3)
  for(i in 1:length(a)){
    for(j in 1:length(b)){
      x[i,j]<-BT.monte.carlo(tau0=1, m=2000, o=a[i],alpha=b[j])
    }
  }
  return(round(x, digits=4))
}

Crit.BT<-BT.MC(a=length(temperature), b=c(0.1,0.05,0.01))
BT.total<-BT(temperature, tau0=1)
#### 6. Chow type DF statistic ####


DFC<-function(y, tau0){
  a<-vector(length=(length(y)-tau0), mode="numeric")
  N<-length(y)
  z<-y
  b<-diff(y)
  for(i in 1:length(a)){
  	z[1:(i-1)]<-0
    delta<-lm(b~z[1:(N-1)])
    sigma<-1/(N-2)*sum((b-delta$coefficients[2]*z[1:(N-1)])^2)
    a[i]<-sum(b[i:(N-1)]*y[i:(N-1)])/(sqrt(sigma)*sqrt(sum(y[i:(N-1)]^2)))
  }
  return(max(a))	
}

# Monte Carlo
#m=number of monte carlo replications
#tau0=point of break
DFC.monte.carlo<-function(tau0, m, o, alpha){
  rep.rw<-replicate(m, arima.sim(model=list(order=c(0,1,0)), n=(o-1)))
  e<-vector(length(m), mode="numeric")
  for(i in 1:m){
    e[i]<-DFC(rep.rw[,i], tau0=tau0)
    print(i)
  }
  result<-quantile(e, probs=(1-alpha), na.rm=TRUE)
  return(result)
}

DFC.MC<-function(a, b){
  x<-matrix(NA, nrow=1, ncol=3)
  for(i in 1:length(a)){
    for(j in 1:length(b)){
      x[i,j]<-DFC.monte.carlo(tau0=1, m=2000, o=a[i],alpha=b[j])
    }
  }
  return(round(x, digits=4))
}

Crit.DFC<-DFC.MC(a=c(length(temperature)), b=c(0.1,0.05,0.01))
DFC.total<-DFC(y = temperature, tau0=1)
colnames(DFC.total)<-c("DFC", "kyr")

##graph of the test statistic

DFC.graph<-ggplot(data=DFC.total)+geom_line(aes(x=kyr, y=DFC))

#### 7. One shot test for the identification of the first bubble ####

##what is the first bubble

first.bubble<-as.data.frame(cbind(temperature[1:100], temp$kyr[1:100]))
termination<-which.min(first.bubble$V1)
ending<-which.max(first.bubble$V1)
transformation<-first.bubble$V1[termination:ending]-first.bubble$V1[termination]
BT.first<-BT(first.bubble$V1[termination:ending], tau0=1)
DFC.first<-DFC(transformation, tau0=1)

DFC.MC.fb<-DFC.MC(a=43, b=c(0.1,0.05,0.01))
BT.MC.fb<-BT.MC(a=43, b=c(0.1,0.05,0.01))

first.bubble.graph<-ggplot(data=first.bubble) + geom_line(aes(x= V2  ,  y=  V1 )) + scale_x_reverse() +
  geom_vline(xintercept=c(787, 744), linetype="dotdash", color="blue") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA),
        text = element_text(family="serif")) + ylab("Temperature") + xlab("Kyr")
  
#ggsave("first-bubble.pdf", plot=first.bubble.graph, width=8, height=8, units="cm")

#### 8. graphic for random walk ####

random.walk<-function(n, delta){
  wn<-c(0, rnorm(n=(n-1)))
  for(i in 2:n){
    wn[i]<-delta*wn[i-1]+wn[i]
  }
  return(wn)
}

set.seed(126)

rw1<-ggplot()+geom_line(aes(x=seq(1:100), y=random.walk(n=100, delta=1))) +
  theme(panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.background = element_blank(),
         panel.border = element_rect(colour="black", fill=NA),
         text = element_text(family="serif", size=10),
        plot.title= element_text(hjust=0.5),
        axis.title.y = element_blank()) + xlab("Time") + ylab("") +
         labs(title=expression("Simulated AR(1) process for"~rho == 1))

#ggsave("rw1.pdf", plot(rw1), width=10, height=2)

set.seed(129)
rw0.85<-ggplot()+geom_line(aes(x=seq(1:100), y=random.walk(n=100, delta=0.85))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA),
        text = element_text(family="serif", size=10),
        plot.title=element_text(hjust=0.5),
        axis.title.y = element_blank()) + xlab("Time") + ylab("") +
        labs(title=expression("Simulated AR(1) process for"~rho == 0.85))
#ggsave("rw085.pdf", plot(rw0.85), width=10, height=2)

set.seed(130)
rw1.2<-ggplot()+geom_line(aes(x=seq(1:100), y=random.walk(n=100, delta=1.03))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA),
        text = element_text(family="serif", size=10),
        plot.title=element_text(hjust=0.5),
        axis.title.y = element_blank()) + xlab("Time") + ylab("") +
  labs(title=expression("Simulated AR(1) process for"~rho == 1.03))

rw.graph<-plot_grid(rw0.85, rw1, rw1.2, nrow=3, rel_widths=c(1,1,1), rel_heights = c(1,1,1))
#ggsave("rw_graph.pdf",plot=rw.graph, height=12, width=12, units="cm")


#### 9. bubble graphic of S&P500 Data ####

sup.data<-read.csv("GSPC2.csv", header=TRUE)
sup.data$Date<-as.Date(sup.data$Date)
sup<-ggplot(data=sup.data)+geom_line(aes(x=Date, y=Adj.Close))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA),
        text = element_text(family="serif", size=10)) + xlab("Time") + ylab("Price in $") 
#ggsave("sup.pdf", plot=sup, width=15, height=6, units="cm")

#### 10. Evans bubble as an example (figure) ####

evans<-as.data.frame(sim_evans(n=100, alpha=2, delta=0.5, tau=0.01,pi=0.85, r=0.02))
colnames(evans)<-("x")
evans.plot<-ggplot(data=evans)+geom_line(aes(x=seq(1:100), y=x))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA),
        text = element_text(family="serif", size=10),
        axis.title.y=element_blank()) + xlab("Time") 
#ggsave("evans.pdf",plot=evans.plot, width=7, height=7, units="cm")
  



## Monte Carlo Analysis for the Tests ##


################## Monte Carlo Analysis ######
library(snowfall)
library(exuber)
library(ggplot2)
#### Random Walk for varying AR parameters######

random.walk.break<-function(n, delta, tau){
  wn<-c(0, rnorm(n=(n-1)))
  z<-floor(tau*n)
  for(i in 2:z){
    wn[i]<-wn[i-1]+wn[i]
  }
  for(j in (z+1):n){
    wn[j]<-wn[j-1]*delta+wn[j]
  }
  return(wn)
}

random.walk.break.constant<-function(n, delta, tau){
  wn<-c(0, rnorm(n=(n-1)))
  z<-floor(tau*n)
  for(i in 2:z){
    wn[i]<-wn[i-1]+wn[i]+1/n
  }
  for(j in (z+1):n){
    wn[j]<-wn[j-1]*delta+wn[j]+1/n
  }
  return(wn)
}


#### 1. Critival values ####

crit_BT_size<-function(M, a, n){
  result<-matrix(NA, nrow=length(n), ncol=3)
  for(i in 1:length(n)){
    x_matrix<-replicate(n=M, random.walk.break(n=n[i], delta=a, tau=0.7))
    statistics<-sfApply(x_matrix, 2, BT, tau0=0.1*n[i])
    result[i,]<-quantile(x=statistics, probs=c(0.9,0.95,0.99))
  }
  return(result)
}

critical_values_BT<-crit_BT_size(M=2000, a=1, n=c(43,798))


###### critical values for DFC test

crit_DFC_size<-function(M, a, n){
  result<-matrix(NA, nrow=length(n), ncol=3)
  for(i in 1:length(n)){
    x_matrix<-replicate(n=M, random.walk.break(n=n[i], delta=a, tau=0.7))
    statistics<-sfApply(x_matrix, 2, DFC, tau0=0.1*n[i])
    result[i,]<-quantile(x=statistics, probs=c(0.9,0.95,0.99))
  }
  return(result)
}

u<-crit_DFC_size(M=2000, a=1, n=c(45,798))


##### critical values for GSADF

crit_values_GSADF<-function(M, n, mw_grid){
  result<-matrix(NA, nrow=length(mw_grid), ncol=3)
  for(i in 1:length(mw_grid)){
    x_matrix<-replicate(n=M, random.walk.break(n=n, delta=1, tau=0.7))
    mw<-floor((0.01+1.8/sqrt(mw_grid[i]))*n)
    statistics<-sfApply(x_matrix, 2, function(x) radf(x,mw,0)$gsadf)
    result[i,]<-t(quantile(statistics, probs=c(0.9,0.95,0.99)))
    print(n[i])
  }
  return(result)
}

#### critical values for SADF

crit_values_SADF<-function(M, n, mw_grid){
  result<-matrix(NA, nrow=length(mw_grid), ncol=3)
  for(i in 1:length(mw_grid)){
    x_matrix<-replicate(n=M, random.walk.break(n=n, delta=1, tau=0.7))
    mw<-floor((0.01+1.8/sqrt(mw_grid[i]))*n)
    statistics<-sfApply(x_matrix, 2, function(x) radf(x,mw,0)$sadf)
    result[i,]<-t(quantile(statistics, probs=c(0.9,0.95,0.99)))
    print(n[i])
  }
  return(result)
}

#### critical values for CUSUM

cusum<-function(y, n){
  S<-vector(length=length(y)-n)
  a<-diff(y)
  for(i in 1:length(S)){
    my<-mean(a[1:(n+i-1)])
    sigma<-(n+i)^(-1)*sum((a[1:(n+i-1)]-my)^2)
    S[i]<-(y[n+i]-y[n])/sqrt(sigma)
  }  
  return(S)
}

crit_values_cusum<-function(M, n){
  t<-floor((0.01+1.8/sqrt(n))*n)
  x_matrix<-replicate(n=M, random.walk.break(n=n, delta=1, tau=0.4))
  statistics<-apply(x_matrix, 2, function(x) cusum(y=x, n=t))
  crit<-apply(statistics, 1, quantile, probs=(1-0.05))
  crit<-(crit^2-log(seq(t:(n-1)/t)))/sqrt(seq(t:(n-1)))
  return(crit)
}

#critical values fluc detector
crit_values_fluc<-function(M, n=n){
  t<-floor((0.01+1.8/sqrt(n))*n)
  x_matrix<-replicate(n=M, random.walk.break(n=n, delta=1, tau=0.4))
  statistics<-apply(x_matrix, 2, function(x) fluc.test.statistics(y=x, t=t))
  crit<-apply(statistics, 1, quantile, probs=(1-(0.05/(n-t))))
  return(crit)
}


## asymptotic critcal values
grid_size<-c(100,200,500,1000)

asy_BT<-crit_BT_size(M=2000, a=1, n=2000)

asy_DFC<-crit_DFC_size(M=2000, a=1, n=2000)
sfInit(parallel=TRUE, cpus=8)

sfLibrary(exuber)
asy_GSADF<-crit_values_GSADF(M=2000, n=2000, mw_grid=grid_size)
sfStop()

sfLibrary(exuber)
asy_SADF<-crit_values_SADF(M=2000, n=2000, mw_grid=grid_size)

asy_critical_values<-round(data.frame(rbind(asy_GSADF, asy_SADF, asy_BT, asy_DFC)), digits=4)
colnames(asy_critical_values)<-c("ten", "five", "one")



#### 2. Rejection frequencies functions ####

# M = number of Monte Carlo Siulations
# a = autoregressive Parameter for an AR(1) process
# n = sample size

# Busetti Taylor 

power_fun_BT_size<-function(M, a, n){
  result<-vector(length=length(n))
  for(i in 1:length(n)){
    x_matrix<-replicate(n=M, random.walk.break(n=n[i], delta=a, tau=0.7))
    statistics<-sfApply(x_matrix, 2, BT, tau0=0.1*n[i])
    result[i]<-mean(statistics>asy_BT[2])
    print(n[i])
  }
  return(result)
}

# DFC statistic 

power_fun_DFC_size<-function(M, a, n){
  result<-vector(length=length(n))
  for(i in 1:length(n)){
    x_matrix<-replicate(n=M, random.walk.break(n=n[i], delta=a, tau=0.7))
    statistics<-apply(x_matrix, 2, DFC, tau0=n[i]*0.1)
    result[i]<-mean(statistics>asy_DFC[2])
    print(n[i])
  }
  return(result)
}

# GSADF 
# Random Walk does have a constant as in the null hypothesis of the paper, 
# this is negligable in comparison to the stochastic trend of the process

power_fun_GSADF_size<-function(M, a, n){
  result<-vector(length=length(n))
  for(i in 1:length(n)){
    x_matrix<-replicate(n=M, random.walk.break(n=n[i], delta=1, tau=0.7))
    mw<-floor((0.01+1.8/sqrt(n[i]))*n[i])
    statistics<-apply(x_matrix, 2, function(x) radf(x,mw,0)$gsadf)
    result[i]<-mean(statistics>asy_GSADF[i,2])
    print(n[i])
  }
  return(result)
}

rej_freq_GSADF<-function(M, a, n){
  result<-vector(length=length(n))
  for(i in 1:length(n)){
    crit<-crit_values_GSADF(M=M, n=n[i], mw_grid=n[i])
    x_matrix<-replicate(n=M, random.walk.break(n=n[i],tau=0.5, delta=a))
    mw<-floor((0.01+1.8/sqrt(n[i]))*n[i])
    statistics<-apply(x_matrix, 2, function(x) radf(x,mw,0)$gsadf)
    result[i]<-mean(statistics>crit[2])
    print(n[i])
  }
  return(result)
}
# SADF 

power_fun_SADF_size<-function(M, a, n){
  result<-vector(length=length(n))
  for(i in 1:length(n)){
    x_matrix<-replicate(n=M, random.walk.break(n=n[i], delta=a, tau=0.7))
    mw<-floor(0.01+1.8/sqrt(n[i])*n[i])
    statistics<-apply(x_matrix, 2, function(x) radf(x,mw,0)$sadf)
    result[i]<-mean(statistics>asy_SADF[i,2])
    print(n[i])
  }
  return(result)
}

rej_freq_SADF<-function(M, a, n){
  result<-vector(length=length(n))
  for(i in 1:length(n)){
    crit<-crit_values_SADF(M=M, n=n[i], mw_grid=n[i])
    x_matrix<-replicate(n=M, random.walk.break(n=n[i],tau=0.5, delta=a))
    mw<-floor((0.01+1.8/sqrt(n[i]))*n[i])
    statistics<-apply(x_matrix, 2, function(x) radf(x,mw,0)$sadf)
    result[i]<-mean(statistics>crit[2])
    print(n[i])
  }
  return(result)
}

power_fun_fluc_size<-function(M, n){
  result<-vector(length=length(n))
  t<-floor((0.01+1.8/sqrt(n))*n)
  for(i in 1: length(n)){
    crit<-crit_values_fluc(M=M, n=n[i])
    x_matrix <- replicate(n=M, random.walk(n=n[i], delta=1))
    statistics <- apply(x_matrix, 2, function(x) fluc.test.statistics(t=t[i], y=x))
    breaks <- statistics-crit
    rej<-vector(length=length(M))
    for(j in 1:ncol(breaks)){
      if(any(breaks[,j]>0)){rej[j]<-1}else{rej[j]<-0}}
    result[i]<-mean(rej)
    print(n[i])
  }
  return(result)
}

power_cusum_size<-function(M, n, a){
  result<-vector(length=length(n))
  t<-floor((0.01+1.8/sqrt(n))*n)
  for(i in 1: length(n)){
    crit<-crit_values_cusum(M=M, n=n[i])
    x_matrix <- replicate(n=M, random.walk.break(n=n[i], delta=a, tau=0.1))
    statistics <- apply(x_matrix, 2, function(x) cusum(n=t[i],y=x))
    breaks <- statistics-crit
    rej<-vector(length=length(M))
    for(j in 1:ncol(breaks)){
      if(any(breaks[,j]>0)){rej[j]<-1}else{rej[j]<-0}}
    result[i]<-mean(rej)
    print(n[i])
  }
  return(result)
}
#### 3. Size comparisons ####
# Application of the test decisions, for data under the H0 for the test statistic

size_BT<-power_fun_BT_size(M=2000, a =1, n=grid_size)
size_DFC<-power_fun_DFC_size(M=2000, a=1, n=grid_size)
sfLibrary(exuber)
size_GSADF<-power_fun_GSADF_size(M=2000, a =1, n=grid_size)
size_SADF<-power_fun_SADF_size(M=2000, a=1, n=grid_size)
size_CUSUM<-power_cusum_size(M=2000, a=1, n=grid_size)
size_FLUC<-power_fun_fluc_size(M=2000, n=grid_size)
size_of_tests<-data.frame(size_GSADF, size_SADF, size_BT, size_DFC, size_CUSUM)
colnames(size_of_tests)<-c("GSADF", "SADF", "supBT", "supDFC", "CUSUM")
rownames(size_of_tests)<-c("T=100", "T=200", "T=500", "T=1000")
xtable(size_of_tests, digits=3)

#### 4. Monte Carlo simulations for the rejection frequencies #####
# a = 1.02
# M = 2000

n_grid<-seq(from=20, to=1000, by=20)

sfInit(parallel=TRUE, cpus=4)
decision_BT_size<-power_fun_BT_size(M=2000,a=1.02, n=n_grid)
decision_DFC_size<-power_fun_DFC_size(M=2000, a =1.02, n=n_grid)
sfLibrary(exuber)
decision_GSADF_size<-rej_freq_GSADF(M=2000, a =1.02, n=n_grid)
decision_SADF_size<-rej_freq_SADF(M=2000, a=1.02, n=n_grid)

# Matrix of rejection frequencies

rejection_frequencies_sample_size<-data.frame(decision_BT_size, decision_GSADF_size, decision_SADF_size, decision_DFC_size, n_grid)
colnames(rejection_frequencies_sample_size)<-c("BT","GSADF", "SADF","DFC","grid")


###### Graph for the rejection frequencies in dependence to the sample size

size_power_graph<-ggplot(data=rejection_frequencies_sample_size)+ geom_line(aes(x=n_grid, y=BT, color="supBT"))+
  geom_line(aes(x=n_grid, y=GSADF, color="GSADF"))+
  geom_line(aes(x=n_grid, y=SADF, color="SADF"))+
  geom_line(aes(x=n_grid, y=DFC, color="supDFC"))+
  scale_color_manual(values=c("supBT"="yellow", "GSADF"="black", "SADF"="blue", "supDFC"="red"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA),
        text = element_text(family="serif", size=10),
        legend.key=element_rect(fill=NA, colour=NA),
        legend.title=element_blank(),
        legend.position=c(0.75,0.25))+
  xlab("T")+ylab("Rejection frequency")

ggsave("size-power1.pdf", plot=size_power_graph, width=15, height=10, units="cm")




#### 5. Monte Carlo simulations fot periodically collapsing bubbles #####

# Random Walk with drift

random.walk.drift<- function(n, delta, my, Dnull, sigma){
  wn<-c(Dnull, rnorm(n=(n-1), sd=sqrt(sigma)))
  for(i in 2:n){
    wn[i]<-wn[i-1]*delta+wn[i]+my
  }
  return(wn)
}

# Fundamental component of Price

fundamentals<-function(n, delta, my, R, Dnull, sigma){
  a<-random.walk.drift(n, delta, my, Dnull, sigma)
  result<-vector(length=length(n))
  for(i in 1:n){
    result[i]<-(1+R)/R^2*my+1/R*a[i]
  }
  return(result)
}

# Price build by Values of Evans (1991) 

P<-function(n, pi, delta, my, R, Dnull, sigma){
  x<-fundamentals(n, delta, my, R, Dnull, sigma)+20*sim_evans(n=n, pi=pi)
  return(x)
}

## Rejection frequencies for GSADF with periodically collapsing Evans (1991) bubbles

power_GSADF_random<-function(M, n){
  result<-vector(length=length(n))
  for(i in 1: length(n)){
    crit<-crit_values_GSADF(M=M, n=n[i], mw_grid=n[i])
    x_matrix <- replicate(n=M, P(n=n[i], pi=0.85, delta=1, my=0.0373, R=0.05, Dnull=1.3, sigma=0.1574))
    mw <- floor(0.01+1.8/sqrt(n[i])*n[i])
    statistics <- sfApply(x_matrix, 2, function(x) radf(x, mw, 0)$gsadf)
    result[i]<-mean(statistics>crit[2])
    print(i)
  }
  return(result)
}

# Rejeciton frequencies for CUSUM with periodically collapsing Evans (1991) bubbles

power_cusum_random<-function(M, n){
  result<-vector(length=length(n))
  t<-floor((0.01+1.8/sqrt(n))*n)
  for(i in 1: length(n)){
    crit<-crit_values_cusum(M=M, n=n[i])
    x_matrix <- replicate(n=M, P(n=n[i], pi=0.85, delta=1, my=0.0373, R=0.05, Dnull=1.3, sigma=0.1574))
    statistics <- apply(x_matrix, 2, function(x) cusum(n=t[i],y=x))
    breaks <- statistics-crit
    rej<-vector(length=length(M))
    for(j in 1:ncol(breaks)){
      if(any(breaks[,j]>0)){rej[j]<-1}else{rej[j]<-0}}
    result[i]<-mean(rej)
    print(n[i])
  }
  return(result)
}

power_cusum_size<-function(M, n){
  result<-vector(length=length(n))
  t<-floor((0.01+1.8/sqrt(n))*n)
  for(i in 1: length(n)){
    crit<-crit_values_cusum(M=M, n=n[i])
    x_matrix <- replicate(n=M, random.walk.break(n=n, delta=1, tau=0.1))
    statistics <- apply(x_matrix, 2, function(x) cusum(n=t[i],y=x))
    breaks <- statistics-crit
    rej<-vector(length=length(M))
    for(j in 1:ncol(breaks)){
      if(any(breaks[,j]>0)){rej[j]<-1}else{rej[j]<-0}}
    result[i]<-mean(rej)
    print(n[i])
  }
  return(result)
}
# rejeciton frequencies for SADF periodically collapsing Evans (1991) bubbles

power_SADF_random<-function(M, n){
  result<-vector(length=length(n))
  for(i in 1: length(n)){
    crit<-crit_values_SADF(M=M, n=n[i], mw_grid=n[i])
    x_matrix <- replicate(n=M, P(n=n[i], pi=0.85, delta=1, my=0.0373, R=0.05, Dnull=1.3, sigma=0.1574))
    mw <- floor(0.01+1.8/sqrt(n[i])*n[i])
    statistics <- sfApply(x_matrix, 2, function(x) radf(x, mw, 0)$sadf)
    result[i]<-mean(statistics>crit[i])
    print(i)
  }
  return(result)
}

power_fluc_random<-function(M, n){
  result<-vector(length=length(n))
  t<-floor((0.01+1.8/sqrt(n))*n)
  for(i in 1: length(n)){
    crit<-crit_values_fluc(M=M, n=n[i])
    x_matrix <- replicate(n=M, P(n=n[i], pi=0.85, delta=1, my=0.0373, R=0.05, Dnull=1.3, sigma=0.1574))
    statistics <- apply(x_matrix, 2, function(x) fluc.test.statistics(t=t[i], y=x))
    breaks <- statistics-crit
    rej<-vector(length=length(M))
    for(j in 1:ncol(breaks)){
      if(any(breaks[,j]>0)){rej[j]<-1}else{rej[j]<-0}}
    result[i]<-mean(rej)
    print(n[i])
  }
  return(result)
}

# Results of Monte Carlo simulations with 2000 replications
sfLibrary(exuber)
decision_GSADF_random<-power_GSADF_random(M=2000, n_grid)
decision_cusum_random<-power_cusum_random(M=2000, n_grid)
decision_SADF_random<-power_SADF_random(M=2000, n_grid)
decision_FLUC_random<-power_fluc_random(M=2000, n=n_grid)

decision_random<-data.frame(decision_GSADF_random, decision_SADF_random, decision_cusum_random, decision_FLUC_random, n_grid)
colnames(decision_random)<-c("GSADF","SADF","CUSUM","FLUC", "grid")

decision_random_graph<-ggplot(data=decision_random)+geom_line(aes(x=n_grid, y=GSADF, colour="GSADF"))+
  geom_line(aes(x=n_grid, y=CUSUM, colour="CUSUM"))+
  geom_line(aes(x=n_grid, y=SADF, colour="SADF"))+
  geom_line(aes(x=n_grid, y=FLUC, colour="FLUC"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(colour="black", fill=NA),
        text = element_text(family="serif", size=10),
        legend.key=element_rect(fill=NA, colour=NA),
        legend.title=element_blank(),
        legend.position=c(0.75,0.25))+
  scale_color_manual(values=c("SADF"="yellow", "GSADF"="black", "CUSUM"="red", "FLUC"="blue"))+
  xlab("T")+ylab("Rejection frequency")

ggsave(plot=decision_random_graph, filename="Random_graph.pdf", height=10, width=15, units="cm")

