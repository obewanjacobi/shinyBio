library(shiny)

n0<-1
max.time<-500
max.Rmax<-1
max.k<-1000

function(input, output) {
  
  rv <- reactiveValues(
    t = NULL,
    ne = vector(),
    nl = vector(),
    Re = vector(),
    re = vector(),
    rl = vector(),
    Rl = vector()
  )
  
  observe({
    rv$t <- seq(0,input$time,1)
    # exponential growth
    for (i in 1:length(rv$t)) {
      if(i==1) {rv$ne[i]<-n0; rv$re[i]<-input$Rmax; rv$Re[i]<-0; next}
      rv$ne[i]<-rv$ne[i-1]+rv$ne[i-1]*input$Rmax
      rv$re[i]<-input$Rmax
      rv$Re[i]<-rv$ne[i]-rv$ne[i-1]
    }
    # logistic growth
    for (i in 1:length(rv$t)) {
      if(i==1) {rv$nl[i]<-n0; rv$rl[i]<-input$Rmax; rv$Rl[i]<-0; next}
      rv$nl[i]<-rv$nl[i-1]+rv$nl[i-1]*input$Rmax*((input$k-rv$nl[i-1])/input$k)
      rv$rl[i]<-input$Rmax*((input$k-rv$nl[i])/input$k)
      rv$Rl[i]<-rv$nl[i]-rv$nl[i-1]
    }
  })
  
  output$pop <- renderPlot({
    plot(rv$t,rv$ne, ylim=c(0,max.k*1.1), type="l", lwd=2, col="red",
         main="Population Size over time", cex.main=0.8,
         xlab="time", ylab="pop. size (n)", cex.lab=0.8)
    points(rv$t,rv$nl, type="l", lwd=2, col="black")
    abline(h=input$k, lty=3, lwd=3, col="black")
  })
  
  output$growth <- renderPlot({
    if(input$growthRate==1) {
      plot(rv$ne,rv$Re, type="l", xlim=c(0,input$k*1.05), ylim=c(0,max(rv$Rl)*2), 
           lwd=2, col="red",
           main="Population Growth Rate (R) v. Population Size (n)", 
           xlab="number of individuals (n)", ylab="new individuals/ time (R)", 
           cex.main=0.8, cex.lab=0.8, cex.axis=0.8)
      points(rv$nl,rv$Rl, type="l", lwd=2, col="black")
    }
    
    if(input$growthRate==2) {
      plot(rv$ne,rv$re, type="l", xlim=c(0,max.k*1.05), ylim=c(0,max.Rmax*1.05) , 
           lwd=2, col="red",
           main="Per Capita Growth Rate (r) v. Population Size (n)", 
           xlab="number of individuals (n)", ylab="new individuals/ time/ n (r)", 
           cex.main=0.8, cex.axis=0.8, cex.lab=0.8)
      points(rv$nl,rv$rl, type="l", lwd=2, col="black")
    }
  })
  
}