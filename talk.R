

###

#### First let's  take a look at the central limit theorem
P <- 100000 # population size
n <- 10 ### the sample size
reps <- 1000
min <- 10
max <- 20
 s <- runif(n = P,min= min,max=max)

 hist(s)
 
 out <- replicate(n=reps,mean(sample(x=s,size=n,replace = T)))
hist(out)


##### let's make up some data

 Gender <- c("M","F")
 Eye <- c("Blue","Brown")
 Age <-   3:10
 
 
d <- expand.grid(Gender=Gender,Age=Age,Eye=Eye) 



d$Height <- rep(0,nrow(d))


d$Height[d$Gender == "M"] <- 4 + 0.05*d$Age[d$Gender == "M"] + rnorm(1,0,.5)
d$Height[d$Gender == "F"] <- 4 + 0.1*d$Age[d$Gender == "F"] + rnorm(1,0,.8)



library(ggplot2)
library(data.table)

p1 <- ggplot(data=d,aes(y=Height,x=Age,color=Gender))+facet_wrap(~Eye)

p1 + geom_point()


p1 + geom_point() + geom_smooth(method="lm",formula = y~x)
p1 + geom_point() + geom_smooth(method="lm",formula = y~x)


p2 <- ggplot(data=d,aes(y=Height,x=Gender))+facet_wrap(~Eye)

p2 + geom_boxplot() + facet_wrap(~Eye)

p2 + geom_boxplot() + facet_wrap(~Eye) + geom_jitter(width = 0.8)

######### maybe let's look at the standard error


class(d)


d <- as.data.table(d)


s <- d[,list(Height = mean(Height),SE=sd(Height)/sqrt(length(Height)),Cout=length(Height)),
       
       by = list(Gender,Eye)]


p3 <- ggplot(s,aes(x=Gender,y=Height)) 

p3 + geom_point()+ facet_wrap(~Eye) + geom_errorbar(aes(ymin=Height-SE,ymax= Height+SE))


names(d)
######### now let's fit regressions for each of these cases 
###### Let's try height vs age


fit <- lm(data=d,formula = Height ~ Age)
summary(fit)


fit2 <- lm(data=d,formula = Height ~ Age*Gender)
summary(fit2)
coefficients(fit2)
coefficients(fit2)


library(plyr)


out2 <- ddply(.data=d,.variables = c("Gender","Eye"),.fun =
        
        
        function(df){
       
          s1 <- summary(lm(data=df,formula = Height ~ Age))
          
                s1$r.squared
            out <-  as.data.frame(s1$coefficients)
          
            out$r2 <- rep(s1$r.squared,2)
            
        return(out)     
             
       
        }
        
        
        
        )

############################################################################## This is the end of the 
######################################## Presentation







out2

library(data.table)

out2 <- melt(data=out2,id.vars = c("Age","Color","Number")) 
head(out2)

library(ggplot2)


out2$Number <- as.numeric(out2$Number)

p1 <- ggplot(data=out2,aes(x=Number,y=value,color=Color))+geom_point() +facet_wrap(~Age+Color)

p1 + geom_smooth(method="lm",formula = y~x) +geom_smooth(method="lm",formula = y~ poly(x,2))

out2$L <- log(out2$value)



p2 <- ggplot(data=out2,aes(x=Number,y=L,color=Color))+geom_point() +facet_wrap(~Age+Color)

p2 + geom_smooth(method="lm",formula = y~x) +geom_smooth(method="lm",formula = y~ poly(x,2))



#################### so what if we wanted to run linear models on each of these


out3 <- ddply(.data=out2,.variables = c("Color","Age"),.fun =
                
                
                function(df){
                  s <-summary(lm(L~Number ,data=df))
                  out_c <- as.data.frame(s$coefficients)
                  out_c$R2 <-  rep(s$r.squared,2) 
                  
                  out_c$Type <-  rep("D1",2)
                  
                  s2 <-summary(lm(L~Number+ I(Number^2) ,data=df))
                  out_c2 <- as.data.frame(s2$coefficients)
                  out_c2$R2 <-  rep(s2$r.squared,3) 
                  out_c2$Type <-  rep("D2",3)
                  
                  out <- rbindlist(list(out_c,out_c2))
                  
                  
                  return(out)
                }
              
              
              
)



out3




####### okay but what if we want to do an exponential fit
##### we note that maybe these don't fit that wwell




###### so what if we want to do an exponential fit???

library(broom)
library(dplyr)
data(Orange)

dim(Orange)
head(Orange)




























