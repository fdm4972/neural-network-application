rm(list=ls())
library("neuralnet")

data<-read.table("C:/Research/SantaBarbara/Mission Creek/MC01/bestpara.txt",header=TRUE)

erfit <- neuralnet(error~k_all+ksat_all+RCm+RCbeta+soilD_all,data, hidden=0, threshold=0.01)
print(erfit)

#Plot the neural network
plot(erfit)

#Test the neural network on some training data (training)
kh=data[(41:50),1]
k=data[(41:50),2]
ksat=data[(41:50),3]
rcm=data[(41:50),4]
rcbeta=data[(41:50),5]
error=data[(41:50),6]
testdata <- cbind(kh,k,ksat,rcm,rcbeta)
net.results <- compute(erfit, testdata) #Run them through the neural network

#Lets see what properties net.sqrt has
ls(net.results)

#Lets see the results
#print(net.results$net.result)

#Lets display a better version of the results
cleanoutput <- cbind(testdata,error,
                     as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("k","ksat","rcm","rcbeta","soilD","expected error","neural net error")
print(cleanoutput)
x=array(0,dim=c(10,1));
i=1
for (i in 1:10)
{
  x[i,1]=i
}
plot(x,error,type="p",col='blue',
     main='Expected & Neural error', xlab='Runs',ylab='Errors',ylim=c(15,30))
points(x,net.results$net.result,type="p",col='red')
legend(7,30,c('Expected error','Neural fitted error'),bty='n',lty=c(1,1),col=c('blue','red'))

#Generate arrays of radomly selected parameters
i=1
calnum=1000
k1=array(0,dim=c(calnum,1));
soilD=array(0,dim=c(calnum,1));
ksat1=array(0,dim=c(calnum,1));
rcm1=array(0,dim=c(calnum,1));
rcbeta1=array(0,dim=c(calnum,1));
bestfit=array(0,dim=c(1,5))
for (i in 1:calnum)
{
  soilD[i,1]=0.5+0.1*runif(1)
  k1[i,1]=5+10*runif(1)
  ksat1[i,1]=0.003+0.01*runif(1)
  rcm1[i,1]=0.1+runif(1)
  rcbeta1[i,1]=10*runif(1)
}

#calculate error based on neural network fitted relationship (validation)
  testdata1 <- cbind(k1,ksat1,rcm1,rcbeta1,soilD)
  net.results1 <- compute(erfit, testdata1)

#select the parameter set based on objective function
er_thresh=999
for (i in 1:calnum)
{
  
  yy = net.results1$net.result[i,1]
  if(yy < er_thresh) 
    {er_thresh=yy
     bestfit=testdata1[i,1:5]
  }
}

result=rbind(er_thresh,bestfit)
colnames(result) <- c("k","ksat","rcm","rcbeta","soilD")
print(result)
