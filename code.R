#因子分析作业
#目标：预测下一年进球
library(ggplot2)
library(glmnet)
library(boot)
library(corrplot)
library(car)
library(mvstats)
library(showtext)
library(MASS)
showtext.auto(enable=T)
par(family="STSong")

dat=read.table("英超球员数据.txt",header=T,sep="\t")
str(dat)
X=model.matrix(下一年进球~.,dat[,-c(1,3,4)])
Y=dat$下一年进球
X=scale(X) #标准化
X=X[,-c(1,31)] #去除掉全部为NA的列，剩余30个变量
mx=cor(X) #协方差矩阵
corrplot(mx,tl.col="black",type="full") #协方差矩阵图
mydata=data.frame(cbind(Y,X))

#glm方法预测
mod1=glm(Y~.,data=mydata,family="poisson")
summary(mod1)
set.seed(1000)
cv1=cv.glm(mydata,mod1,K=10)$delta[1]
coef(mod1)
cv1

#step方法
stepAIC(mod1,direction="backward")
mod1.1=glm(formula = Y ~ 年龄 + 位置前锋 + 位置中场 + 出场 + 
             出场时间 + 传球 + 过人 + 抢断 + 越位 + 红牌 + 
             黄牌 + 射门 + 左脚进球 + 右脚进球 + 直接任意球进球 + 
             拦截 + 乌龙球, family = "poisson", data = mydata)
summary(mod1.1)
set.seed(1000)
cv1.1=cv.glm(mydata,mod1.1,K=10)$delta[1]
coef(mod1.1)
cv1.1

#ridge方法预测
set.seed(1000)
mod2=cv.glmnet(X,Y,family="poisson",alpha=0,nfolds=10,lambda=10^seq(-2,1,length=100))
plot(mod2)
cv2=min(mod2$cvm)
coef(mod2)
cv2

#lasso方法预测
set.seed(1000)
mod3=cv.glmnet(X,Y,family="poisson",alpha=1,nfolds=10,lambda=10^seq(-3,0,length=100))
plot(mod3)
cv3=min(mod3$cvm)
coef(mod3)
cv3

#主成分分析
pca.fit=princomp(X)
summary(pca.fit) #可以看到前6个主成分的累计贡献率达到70%以上，可以使用前6个主成分来做因子分析。
plot(pca.fit,type="lines") 
cbind(round(pca.fit$loadings[,1:6],2))
pca.score=pca.fit$scores
head(pca.score)

biplot(pca.fit) #第一主成分和第二主成分的plot
scatterplot(pca.score[,1],pca.score[,2],xlab="Comp.1",ylab="Comp.2",smoother=FALSE) 
scatterplot(pca.score[,1],pca.score[,2],xlab="Comp.1",ylab="Comp.2",smoother=FALSE,id.method="identify")
#找出了一些的得分比较高的人。

#因子分析
fac.out=factpc(X,6,rotation="varimax") #因子分析，取出6个因子，做方差最大旋转
fac.out$Vars
data.frame(cbind(round(fac.out$loadings,2),round(fac.out$common,2)),row.names=colnames(X)) #因子载荷矩阵和共性方差
fac.score=fac.out$scores
head(fac.score)
scatterplot(fac.score[,1],fac.score[,2],xlab="Fac.1",ylab="Fac.2",smoother=FALSE)
scatterplot(fac.score[,1],fac.score[,2],xlab="Comp.1",ylab="Comp.2",smoother=FALSE,id.method="identify") 

#因子回归
mydata3=data.frame(cbind(Y,fac.score))
names(mydata3)=c("下一年进球","进攻","辅助","防守","犯规","准确率","任意球")
mod5=glm(下一年进球~.,data=mydata3,family="poisson")
summary(mod5)
set.seed(1000)
cv5=cv.glm(mydata3,mod5,K=10)$delta[1]
coef(mod5)
cv5

result=c("回归"=cv1,"逐步回归"=cv1.1,"ridge"=cv2,"lasso"=cv3,"因子回归"=cv5)
result
coef(mod3)
coef(mod5)
result2=data.frame(x0=names(dat)[-c(1,3,4,34)],x1=data.frame(cbind(round(fac.out$loadings,2),round(fac.out$common,2)),row.names=colnames(X))$Factor1,x2=coef(mod3)[-1])
write.table(result2,sep="&",file="参数比较.txt",row.names=F)

#lasso
pre=exp(predict(mod3,newx=X))
plot(Y,pre,xlab="实际值",ylab="预测值")

#可视化
myfun=function(i){
  phdata=data.frame(x=names(mydata3),y=unlist(c(mydata3[i,])))
  phdata$y2=1/(1+exp(-phdata$y))
  phdata$x2=c(1:7)
  ph=ggplot(data=phdata[-1,],aes(x=x2,y=y2))+
    geom_bar(stat="identity",
             alpha=0.8,
             fill="red",
             color="white")+
    coord_polar(theta="x",direction=-1)+
    labs(x="",y="",title=dat$球员[i])+
    scale_x_continuous(labels=as.character(phdata$x)[2:7])+
    annotate("text",x=6.5,y=1,label=paste("下一年进球","=",phdata$y[1]),color="black",size=5)+
    scale_y_continuous(limits=c(0,1))+
    theme_bw()+
    theme(axis.text.x=element_text(size=15),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          panel.border=element_blank(),
          plot.title=element_text(size=18))
  print(ph)
  ggsave(paste(dat$位置[i],dat$球员[i],".pdf",sep=""))
}

for(i in c(order(mydata3$进攻,decreasing=T)[1:4],order(mydata3$辅助,decreasing=T)[1:4],order(mydata3$防守,decreasing=T)[1:4],order(mydata3$犯规,decreasing=T)[1:4])){
  myfun(i)
}

#曼城和利物浦的比较
mc_qf=which(dat$球队=="曼城" & dat$位置=="前锋")
mc_zc=which(dat$球队=="曼城" & dat$位置=="中场")
mc_hw=which(dat$球队=="曼城" & dat$位置=="后卫")
lwp_qf=which(dat$球队=="利物浦" & dat$位置=="前锋")
lwp_zc=which(dat$球队=="利物浦" & dat$位置=="中场")
lwp_hw=which(dat$球队=="利物浦" & dat$位置=="后卫")

myfun1=function(i,j){
  phdata=data.frame(t(as.matrix(mydata3[c(i,j),])))
  phdata=data.frame(x=names(mydata3),y=c(phdata[,1],phdata[,2]),add=rep(c(paste(dat$球队[i],dat$球员[i],sep="."),paste(dat$球队[j],dat$球员[j],sep=".")),each=7))
  phdata$y2=1/(1+exp(-phdata$y))
  phdata$x2=c(1:7)
  ph=ggplot(data=phdata[-c(1,8),],
            aes(x=x2,y=y2,fill=add))+
    geom_bar(stat="identity",
             alpha=0.8,
             color="white",
             position="dodge")+
    coord_polar(theta="x",direction=-1)+
    labs(x="",y="",title=paste("两队",dat$位置[i],"队员比较",sep=""),fill="")+
    scale_x_continuous(labels=as.character(phdata$x)[2:7])+
    scale_y_continuous(limits=c(0,1))+
    theme_bw()+
    theme(axis.text.x=element_text(size=15),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          panel.border=element_blank(),
          legend.position="bottom",
          legend.key.width=unit(1,"cm"),
          legend.key=element_rect(colour='white',
                                  fill='white',
                                  size=1),
          legend.text=element_text(size=10),
          legend.key.size=unit(0.7,'cm'),
          plot.title=element_text(size=18))
  print(ph)
  ggsave(paste(dat$位置[i],i,j,".pdf",sep=""))
}

for(i in 1:length(mc_qf)){
  for(j in 1:length(lwp_qf)){
    myfun1(mc_qf[i],lwp_qf[j])
  }
}

for(i in 1:length(mc_zc)){
  for(j in 1:length(lwp_zc)){
    myfun1(mc_zc[i],lwp_zc[j])
  }
}

for(i in 1:length(mc_hw)){
  for(j in 1:length(lwp_hw)){
    myfun1(mc_hw[i],lwp_hw[j])
  }
}
