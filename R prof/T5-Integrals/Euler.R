
###EULER'S METHOD FIRST DERIVATIVE

fty <- function(t, y){
  (2*y)-(3*t) 
}


t0 <- 0
y0 <- 1
N <- 0.4  ##value we want to find  
h = 0.1  ##delta to use
L <-(N+h)/h


tsol <- matrix(0,L,3)

tsol[1,1]<-0
tsol[1,2]<-1
tsol[1,3]<-fty(t0,y0)

for(i in 2:L){
  tsol[i,1]<-tsol[i-1,1]+h
  tsol[i,2]<-tsol[i-1,2]+(h*(tsol[i-1,3]))
  tsol[i,3]<-fty(tsol[i,1],tsol[i,2])
}

tsol[L,]

##SECOND DERIVATIVE  

fty2 <- function(t, y){
  (4*y)-(6*t)-3 
}


tsol2 <- matrix(0,L,4)

tsol2[1,1]<-0
tsol2[1,2]<-1
tsol2[1,3]<-fty(t0,y0)
tsol2[1,4]<-fty2(t0,y0)

for(i in 2:L){
  tsol2[i,1]<-tsol2[i-1,1]+h
  tsol2[i,2]<-tsol2[i-1,2]+(h*(tsol2[i-1,3] + (h/2*tsol2[i-1,4])))
  tsol2[i,3]<-fty(tsol2[i,1],tsol2[i,2])
  tsol2[i,4]<-fty2(tsol2[i,1],tsol2[i,2])
  
}

tsol2[L,]


##HEUNS METHOD 


tsol3 <- matrix(0,L,4)

tsol3[1,1]<-0
tsol3[1,2]<-1
tsol3[1,3]<-fty(t0,y0)  ###K1
tsol3[1,4]<-fty(t0+h,y0+(h*tsol3[1,3]))  ###K2

for(i in 2:L){
  tsol3[i,1]<-tsol3[i-1,1]+h
  tsol3[i,2]<-tsol3[i-1,2]+h*((0.5*tsol3[i-1,3])+(0.5*tsol3[i-1,4]))
  tsol3[i,3]<-fty(tsol3[i,1],tsol3[i,2])
  tsol3[i,4]<-fty(tsol3[i,1]+h, tsol3[i,2]+(h*tsol3[i,3]))
  
}

tsol3[L,]
tsol3



##MID POINT


tsol4 <- matrix(0,L,4)

tsol4[1,1]<-0
tsol4[1,2]<-1
tsol4[1,3]<-fty(t0,y0)  ###K1
tsol4[1,4]<-fty(t0+0.5*h,y0+(0.5*h*tsol4[1,3]))  ###K2

for(i in 2:L){
  tsol4[i,1]<-tsol4[i-1,1]+h
  tsol4[i,2]<-tsol4[i-1,2]+h*tsol4[i-1,4]
  tsol4[i,3]<-fty(tsol4[i,1],tsol4[i,2])
  tsol4[i,4]<-fty(tsol4[i,1]+0.5*h, tsol4[i,2]+(0.5*h*tsol4[i,3]))
  
}

tsol4[L,]
tsol4

