# Set initial variables
mat.rows<-60; mat.cols=60; mat.size<-mat.rows*mat.cols ## Size of matrix
yrs<-200 ## Number of years in simulation
## starting proportions of each stage (must sum to 1)
s.prop<-c(sp3=0.20, sp4=0.15, sp5=0.15, sp6=0.50) 
## probability of burning in a given year
p.burn<-c(pb1=0.05, pb2=0.005, pb3=0.0008, pb4=0.0008)
max.burn.size<-20 ## maximum dimension of a burn
## coefficient to reduce maximum fire size
c.burn<-c(cb1=0.1, cb2=0.1, cb3=.8, cb4=1)

orig.matrix<-matrix(rep(3:6,s.prop*mat.size), mat.rows, mat.cols)
all.colors<-c("red", "yellow", "yellow", "yellowgreen", 
              "darkolivegreen4", "darkolivegreen")

BURN.fun<-function(next.yr, row, col, size) {
    # Size of burn area
    ## size is the maximum dimension of a square burn area
    burn.r<-runif(1, 1,size); burn.c<-runif(1, 1,size)
    for (bc in col:max((col-burn.c), 1)) {
        for (br in row:max((row-burn.r), 1)) {
            next.yr[br,bc]<-1
        }
    }
    return(next.yr)
}
YEAR.fun<-function(matrix.yr) {
    rows<-nrow(matrix.yr); cols<-ncol(matrix.yr)
    current.yr<-matrix.yr
    next.yr<-current.yr
    for (c in 1:cols) {
        for (r in 1:rows) {
            # for cells with fire
            if(current.yr[r,c]==1) {next.yr[r,c]<-2; next}
            # for cells that were burned
            if(current.yr[r,c]==2) {next.yr[r,c]<-3; next}
            # random chance of vegetation cells burning
            if(current.yr[r,c]==3 & rbinom(1,1,p.burn[1])==1) {
                next.yr<-BURN.fun(next.yr, r,c, max.burn.size*c.burn[1]); next}
            if(current.yr[r,c]==4 & rbinom(1,1,p.burn[2])==1) {
                next.yr<-BURN.fun(next.yr, r,c, max.burn.size*c.burn[2]); next}
            if(current.yr[r,c]==5 & rbinom(1,1,p.burn[3])==1) {
                next.yr<-BURN.fun(next.yr, r,c, max.burn.size*c.burn[3]); next}
            if(current.yr[r,c]==6 & rbinom(1,1,p.burn[4])==1) {
                next.yr<-BURN.fun(next.yr, r,c, max.burn.size*c.burn[4]); next}
            # for cells going through succession
            if(current.yr[r,c]==3 & rbinom(1,1,1/10)==1) {next.yr[r,c]<-4; next}
            if(current.yr[r,c]==4 & rbinom(1,1,1/10)==1) {next.yr[r,c]<-5; next}
            if(current.yr[r,c]==5 & rbinom(1,1,1/10)==1) {next.yr[r,c]<-6; next}
            if(current.yr[r,c]==6) {next.yr[r,c]<-6}
        }
    }
    return(next.yr)
}

image(orig.matrix, col=all.colors[3:6], xaxt="n", yaxt="n") 
t1<-vector(); t3<-vector(); t4<-vector(); t5<-vector(); t6<-vector()
for(y in 1:yrs) {
    if(y==1) yrN<-orig.matrix
    VyrN<-as.vector(yrN)
    p1<-length(VyrN[VyrN==1]); t1<-c(t1,p1)
    p3<-length(VyrN[VyrN==2 | VyrN==3]); t3<-c(t3,p3)
    p4<-length(VyrN[VyrN==4]); t4<-c(t4,p4)
    p5<-length(VyrN[VyrN==5]); t5<-c(t5,p5)
    p6<-length(VyrN[VyrN==6]); t6<-c(t6,p6)
    yrN1<-YEAR.fun(yrN)
    current.cols<-all.colors[sort(unique(as.vector(yrN1)))]
    time<-Sys.time(); while (Sys.time()<time+0.2){T<-0}
    # dev.off()
    image(yrN1, col=current.cols, xaxt="n", yaxt="n")
    yrN<-yrN1
}

ylim<-max(max(t1/mat.size*100),max(t3/mat.size*100),max(t4/mat.size*100),
          max(t5/mat.size*100),max(t6/mat.size*100))
## ylim<-10
plot(1:yrs,t3/mat.size*100,ylim=c(0,ylim*1.1), type="l", lwd=3, col="yellow",
     xlab="years", ylab="percent cover")
points(1:yrs, t1/mat.size*100, type="l", lwd=3, col="red")
points(1:yrs, t4/mat.size*100, type="l", lwd=3, col="yellowgreen")
points(1:yrs, t5/mat.size*100, type="l", lwd=3, col="darkolivegreen4")
points(1:yrs, t6/mat.size*100, type="l", lwd=3, col="darkolivegreen")
legend("top", horiz=TRUE, bty="n", cex=0.9,
       legend=c("Stage1", "Stage2", "Stage3", "Stage4", "fire"), 
       fill=c("yellow", "yellowgreen", "darkolivegreen4", "darkolivegreen", "red"))