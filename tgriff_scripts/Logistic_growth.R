require(manipulate)
# calculating n iteratively
max.time<-500; max.Rmax<-1; max.k<-1000
manipulate ({
    max.time<-500
    t<-seq(0,time,1)
    max.Rmax<-1
    max.k<-1000
    n0<-1
    ne<-vector(); nl<-vector()
    re<-vector(); rl<-vector(); Re<-vector(); Rl<-vector()
    # exponential growth
    for (i in 1:length(t)) {
        if(i==1) {ne[i]<-n0; re[i]<-Rmax; Re[i]<-0; next}
        ne[i]<-ne[i-1]+ne[i-1]*Rmax
        re[i]<-Rmax
        Re[i]<-ne[i]-ne[i-1]
    }
    # logistic growth
    for (i in 1:length(t)) {
        if(i==1) {nl[i]<-n0; rl[i]<-Rmax; Rl[i]<-0; next}
        nl[i]<-nl[i-1]+nl[i-1]*Rmax*((k-nl[i-1])/k)
        rl[i]<-Rmax*((k-nl[i])/k)
        Rl[i]<-nl[i]-nl[i-1]
    }
    par(mfrow=c(2,1))
    par(mar=c(4.1, 4.1, 2.6, 2.1))
    plot(t,ne, ylim=c(0,max.k*1.1), type="l", lwd=2, col="red",
         main="Population Size over time", cex.main=0.8,
         xlab="time", ylab="pop. size (n)", cex.lab=0.8)
    points(t,nl, type="l", lwd=2, col="black")
    abline(h=k, lty=3, lwd=3, col="black")

    if(growth.rate==1) {
        plot(ne,Re, type="l", xlim=c(0,k*1.05), ylim=c(0,max(Rl)*2), 
             lwd=2, col="red",
             main="Population Growth Rate (R) v. Population Size (n)", 
             xlab="number of individuals (n)", ylab="new individuals/ time (R)", 
             cex.main=0.8, cex.lab=0.8, cex.axis=0.8)
        points(nl,Rl, type="l", lwd=2, col="black")
    }
    
    if(growth.rate==2) {
    plot(ne,re, type="l", xlim=c(0,max.k*1.05), ylim=c(0,max.Rmax*1.05) , 
         lwd=2, col="red",
         main="Per Capita Growth Rate (r) v. Population Size (n)", 
         xlab="number of individuals (n)", ylab="new individuals/ time/ n (r)", 
         cex.main=0.8, cex.axis=0.8, cex.lab=0.8)
    points(nl,rl, type="l", lwd=2, col="black")
    }
},
time=slider(0,max.time, max.time*0.2),
Rmax=slider(0,max.Rmax, max.Rmax*0.1),
k=slider(10,max.k, max.k*0.8),
growth.rate=picker("Population Growth Rate" =1, "Per Capita Growth Rate" =2)
)

## logistic growth calculating n with logit equation
# manipulate ({
    #t1<-seq(0,10,0.1)
    # k1<-100
    # b1<-99
    # n1<-k1/(1+b1*exp(-r1*9.8*t1))
    # plot(t1,n1,type="l",lwd=2, col="red")
#},
# r1=slider(0,1,0.1)
# )