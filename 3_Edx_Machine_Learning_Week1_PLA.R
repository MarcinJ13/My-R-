#Initial seed (this will create other seeds)
startseed <- 29
set.seed(startseed)

# how many data points to generate at each attempt
n=100

# how many data points to generate for montecarlo check
nc=10000


# how many attempts 
a=1000

#if you want to do manually and see all the steps set this value to FALSE
skipmanual = TRUE

#Prepare table for storing results
my_results = data.frame (attempt=numeric(0), seed=numeric(0), 
                         iterations=numeric(0), n = numeric(0),
                         checkfalse = numeric(0), checktrue=numeric(0),
                         nrofmcmismatches=numeric(0), nrofmc = numeric(0))


for (attemptnr in 1:a) {

#define seed for this run (this is done automatically based on startseed)

thisrunseed = round(runif(1, min=1, max=10000))

# create points and f line 
set.seed(thisrunseed)
x = runif(2,min=-1,max=1)
y = runif(2,min=-1,max=1)  

points1 = data.frame(x,y)
slope1 = (points1[1,2]-points1[2,2])/(points1[1,1]-points1[2,1])
intercept1 =points1[1,2]-slope1*points1[1,1]
pla_f = matrix(c(intercept1,slope1,-1),3,1)

if(skipmanual==FALSE) {
  #you can see the original f function here:
  p <- ggplot(points1, aes(x=x, y=y))
  p+geom_point()+
  geom_abline(intercept = -pla_f[1]/pla_f[3], slope = -pla_f[2]/pla_f[3])+
  coord_cartesian(ylim=c(-1, 1),xlim=c(-1, 1)) +
  ggtitle(paste("F function and two points used to generate it - seed=",thisrunseed))
}

#create n points
pla_data = data.frame(x0=1,x1=runif(n,min=-1,max=1), x2=runif(n,min=-1,max=1))
# assign result to the points using f function
pla_data$fy = -t(sign(t(pla_f) %*% t(as.matrix(pla_data))))
pla_data$origclass= factor(pla_data$fy)
p <- ggplot(pla_data, aes(x=x1, y=x2))


if(skipmanual==FALSE) {
  #display initial assignment
  p+
  geom_point(data=pla_data[pla_data$fy==1 &  pla_data$fy==1,],size=5,aes(x=x1, y=x2,color="fpos"),shape=18)+
  geom_point(data=pla_data[pla_data$fy==-1 &  pla_data$fy==-1,],size=5,aes(x=x1, y=x2,color="fneg"),shape=18)+
  geom_abline(intercept = -pla_f[1]/pla_f[3], slope = -pla_f[2]/pla_f[3], color="purple")+
  ggtitle(paste("Initial assignment based on f function"))
}

#initial w (all 0)
pla_w = matrix(c(0,0,0),3,1)
#pla_w = matrix(c(0,0,0),3,1)
i = 0
#initially all points are misclassified (0)



# you can now run it step by step (step 1,  step 2, repeat) for visualization.
#Or run the whole process at once (below). 


if(skipmanual==FALSE) { #start of skipping manual steps.

#step 1##################
i=i+1
pla_data$wy = t(sign(t(pla_w) %*% t(as.matrix(pla_data[,1:3]))))
pla_data$match = (pla_data$wy==pla_data$fy)
if (nrow(pla_data[pla_data$match==FALSE,])>0) {
false_point_index = sample(1:nrow(pla_data[pla_data$match==FALSE,]),1)
false_point =  pla_data[pla_data$match==FALSE,][false_point_index,] 
pla_data[pla_data$match==FALSE,][false_point_index,"match"]="PICKED"
p+
  geom_point(data=pla_data[pla_data$fy==1 &  pla_data$fy==1,],size=5,aes(x=x1, y=x2,color="fpos_gpos"),shape=18)+
  geom_point(data=pla_data[pla_data$fy==-1 &  pla_data$fy==-1,],size=5,aes(x=x1, y=x2,color="fneg_gneg"),shape=18)+
  geom_point(data=pla_data[pla_data$match==FALSE | pla_data$match=="PICKED",],size=5,aes(x=x1, y=x2,color="fg_mismatch"),shape=18)+
  geom_point(data=false_point,size=7,aes(x=x1, y=x2,color="picked"),shape=18)+
  geom_abline(intercept = -pla_f[1]/pla_f[3], slope = -pla_f[2]/pla_f[3], color="purple")+
  geom_abline(intercept = -pla_w[1]/pla_w[3], slope = -pla_w[2]/pla_w[3], color="black")+
  ggtitle(paste("Iteration", i, "- picking point"))
} else {
  p+
    geom_point(data=pla_data[pla_data$fy==1 &  pla_data$fy==1,],size=5,aes(x=x1, y=x2,color="fpos_gpos"),shape=18)+
    geom_point(data=pla_data[pla_data$fy==-1 &  pla_data$fy==-1,],size=5,aes(x=x1, y=x2,color="fneg_gneg"),shape=18)+
    geom_abline(intercept = -pla_f[1]/pla_f[3], slope = -pla_f[2]/pla_f[3], aes(color="f_function"))+
    geom_abline(intercept = -pla_w[1]/pla_w[3], slope = -pla_w[2]/pla_w[3], aes(color="g_function"))+
    ggtitle(paste("Iteration", i, "- final_situation"))
  
}
#End of step 1##################



#step 2##################
pla_w = unname(t(as.matrix(pla_w + false_point$fy*false_point[,1:3])))
p <- ggplot(pla_data, aes(x=x1, y=x2))
p+
  geom_point(data=pla_data[pla_data$fy==1 &  pla_data$fy==1,],size=5,aes(x=x1, y=x2,color="fpos_gpos"),shape=18)+
  geom_point(data=pla_data[pla_data$fy==-1 &  pla_data$fy==-1,],size=5,aes(x=x1, y=x2,color="fneg_gneg"),shape=18)+
  geom_point(data=pla_data[pla_data$match==FALSE| pla_data$match=="PICKED",],size=5,aes(x=x1, y=x2,color="fg_mismatch"),shape=18)+
  geom_point(data=false_point,size=7,aes(x=x1, y=x2,color="picked"),shape=18)+
  geom_abline(intercept = -pla_f[1]/pla_f[3], slope = -pla_f[2]/pla_f[3], color="purple")+
  geom_abline(intercept = -pla_w[1]/pla_w[3], slope = -pla_w[2]/pla_w[3],  color="black")+
  ggtitle(paste("Iteration", i, "- adjusting w"))
#End of step 2##################

} #end of skipping manual steps.

 
#this is automatic (normal approach with skipmanual set to TRUE)
if(skipmanual==TRUE)  {
  
  

endloop = FALSE
while (endloop==FALSE) {
i=i+1
pla_data$wy = t(sign(t(pla_w) %*% t(as.matrix(pla_data[,1:3]))))
pla_data$match = (pla_data$wy==pla_data$fy)
if (nrow(pla_data[pla_data$match==FALSE,])>0) {
  false_point_index = sample(1:nrow(pla_data[pla_data$match==FALSE,]),1)
  false_point =  pla_data[pla_data$match==FALSE,][false_point_index,] 
  pla_data[pla_data$match==FALSE,][false_point_index,"match"]="PICKED"
  pla_w = unname(t(as.matrix(pla_w + false_point$fy*false_point[,1:3])))
} else {
  endloop = TRUE
}
}

} #end of normal approach



if(skipmanual==FALSE)  {
p+
  geom_point(data=pla_data[pla_data$fy==1 &  pla_data$fy==1,],size=5,aes(x=x1, y=x2,color="fpos_gpos"),shape=18)+
  geom_point(data=pla_data[pla_data$fy==-1 &  pla_data$fy==-1,],size=5,aes(x=x1, y=x2,color="fneg_gneg"),shape=18)+
  geom_abline(intercept = -pla_f[1]/pla_f[3], slope = -pla_f[2]/pla_f[3],  color="purple")+
  geom_abline(intercept = -pla_w[1]/pla_w[3], slope = -pla_w[2]/pla_w[3],  color="black")+
  ggtitle(paste("Iteration", i, "- final_situation"))
  
}

checkfalse1 = nrow(pla_data[pla_data$match==FALSE,])
checktrue1 = nrow(pla_data[pla_data$match==TRUE,])



#monte carlo
pla_datamc = data.frame(x0=1,x1=runif(nc,min=-1,max=1), x2=runif(nc,min=-1,max=1))
# assign result to the points using f function
pla_datamc$fy = -t(sign(t(pla_f) %*% t(as.matrix(pla_datamc))))
pla_datamc$wy = t(sign(t(pla_w) %*% t(as.matrix(pla_datamc[,1:3]))))
pla_datamc$match=factor(pla_datamc$fy ==pla_datamc$wy )
mismatches = nrow(pla_datamc[pla_datamc$match==FALSE,])


if(skipmanual==FALSE)  {
#display montecarlo results
p <- ggplot(pla_datamc, aes(x=x1, y=x2,color=match))
p+geom_point(size=1)+
  geom_abline(intercept = -pla_f[1]/pla_f[3], slope = -pla_f[2]/pla_f[3])+
  geom_abline(intercept = -pla_w[1]/pla_w[3], slope = -pla_w[2]/pla_w[3])+
  coord_cartesian(ylim=c(-1, 1),xlim=c(-1, 1)) +
  ggtitle(paste("Iteration", i, "- error fraction",mismatches/nc,"(",mismatches,"/",nc,")"))
}


my_results = rbind(my_results, 
                   c(attemptnr, thisrunseed, i, n, checkfalse1, checktrue1,mismatches, nc))



} #end of attempt loop


names(my_results) <- c("attempt_nr","seed_for_this_run", "iterations", "nr_of_data_points",
                       "check_how_many_false","check_how_many_true","monte_carlo_mismatches", 
                       "monte_carlo_data_points")
                         

paste("average iterations for n=",n, "is",round(mean(my_results$iterations),4))
paste("average fraction of monte carlo mismatches for n=",n, "is",round(mean(my_results$monte_carlo_mismatches/my_results$monte_carlo_data_points),4))



if(skipmanual==FALSE) {
                 
# you can see last attempt results here
ggplot(pla_data, aes(x=x1, y=x2))+
geom_point(data=pla_data[pla_data$fy==1 &  pla_data$fy==1,],size=5,aes(x=x1, y=x2,color="fpos_gpos"),shape=18)+
geom_point(data=pla_data[pla_data$fy==-1 &  pla_data$fy==-1,],size=5,aes(x=x1, y=x2,color="fneg_gneg"),shape=18)+
geom_abline(intercept = -pla_f[1]/pla_f[3], slope = -pla_f[2]/pla_f[3],  color="purple")+
geom_abline(intercept = -pla_w[1]/pla_w[3], slope = -pla_w[2]/pla_w[3],  color="black")+
ggtitle(paste("Iteration", i, "- final_situation"))

  
# you can see last attempt monte carlo results here
ggplot(pla_datamc, aes(x=x1, y=x2,color=match))+geom_point(size=1)+
  geom_abline(intercept = -pla_f[1]/pla_f[3], slope = -pla_f[2]/pla_f[3])+
  geom_abline(intercept = -pla_w[1]/pla_w[3], slope = -pla_w[2]/pla_w[3])+
  coord_cartesian(ylim=c(-1, 1),xlim=c(-1, 1)) +
  ggtitle(paste("Iteration", i, "- error fraction",mismatches/nc,"(",mismatches,"/",nc,")"))

}



