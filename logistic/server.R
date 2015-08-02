library(shiny)


function(input, output) {
  generationsTillMaturity <- 1 # how many generations it takes for a generation
  # to mature

  # REACTIVE VALUES
  rv <- reactiveValues(sim = 0
                       , n_t = input$n_0
                       , Lt = 0
                       , lt = 0
                       , littersizes = list(rep(-1, input$totalTime))
                       , dead = list(rep(-1, input$totalTime))
                       , b = input$b
                       , d = input$d
                       , daframe = data.frame(time = rep(0, input$totalTime)
                                    , numLitters = rep(0, input$totalTime)
                                    , population = rep(n_0, input$totalTime)
                                    , birthrate = rep(0, input$totalTime)
                                    , born = rep(0, input$totalTime)
                                    , deathrate = rep(0, input$totalTime)
                                    , dead = rep(0, input$totalTime)
                                    , theoretical = rep(n_0, input$totalTime)))
  # END REACTIVE VALUES
  st <- 0
  living <- NULL
  initial <- numeric(length = 8)
  t <- 0
  n_0 <- 0
  n_t <- n_0 # initial value, garbage value to start

  # Initializing variables to be super assigned to by multiple functions
  observe({
    input$totalTime
    rv$daframe <<- data.frame(time = rep(0, input$totalTime)
                          , numLitters = rep(0, input$totalTime)
                          , population = rep(n_0, input$totalTime)
                          , birthrate = rep(0, input$totalTime)
                          , born = rep(0, input$totalTime)
                          , deathrate = rep(0, input$totalTime)
                          , dead = rep(0, input$totalTime)
                          , theoretical = rep(n_0, input$totalTime))


    rv$littersizes <- list(rep(-1, input$totalTime)) # Initial values to make
    # these objects persistant
    rv$dead <- list(rep(-1, input$totalTime)) # Initial values to make these
    # objects persistant
    rv$living <- list(rep(-1, input$totalTime)) # Initial values to make these
    # objects
    rv$t <- 0
    n_t <<- n_0 # initial value, garbage value to start
    b <<- input$b
    d <<- input$d
    m <<- input$m
    initial[1] <<- 0
    initial[2] <<- 0
    initial[3] <<- n_t
    initial[4] <<- max(0, (b + (d - b)/2*(n_t/m)))
    initial[5] <<- 0
    initial[6] <<- d + (b - d)/2*(n_t/m) + max(0, -(b + (d - b)/2*(n_t/m)))
    initial[7] <<- 0
  })

  update <- function() {
    dfrow <- numeric(length = 8)

    rv$t <- rv$t + 1
    n_0 = rv$n_0
    b = rv$b
    bt = rv$bt
    d = rv$d
    dt = rv$dt
    m = input$m
    lt = rv$lt
    Lt = rv$Lt
    if(d <= b){
      bstar <-  b + (d - b)/2*(n_t/m) # Important factor in birth rate at time t
      dstar <-  d + (b - d)/2*(n_t/m) # Important factor in death rate at time t
      bt <<- max(0, bstar)            # birth rate at time t
      dt <<- dstar + max(0, -bstar)   # death rate at time t
    }
    else{
      rv$bt <- b
      rv$dt <- d
    }
    st <<- 8*sqrt(bt)/sqrt(b) # averate size of litters
    lt <<- sqrt(bt)*sqrt(b)/8 # rate of litters
    Lt <<- rpois(n = 1, lambda = lt*n_t) # number of litters based on litter
    # rate
    if (Lt > 0) {
      rv$littersizes[[t]] <<- rpois(Lt, lambda = st) # produce sizes of each litter
    }
    else {rv$littersizes[[t]] <<- NULL}

    if (is.null(dead)) {} # Do nothing
    else {
      dead[[t]] <<- rpois(1, lambda = dt*n_t) # produce dead rabbits
    }
    if (dead[[t]] > n_t){
      dead[[t]] <<- n_t
    }
    # keep track of the living rabbits and when they were born
    birthsThisGeneration <- sum(rv$littersizes[[t]])
    if (birthsThisGeneration >= 1) {
      #living[[t]] <<- 1:birthsThisGeneration
    }
    else {
      #living[[t]] <<- NULL
    }
    for (i in 1:rv$dead[[t]]) {
    }


    # population sizes = previous population + sum of all litters - dead rabbits
    n_t <- n_t + sum(rv$littersizes[[t]]) - sum(rv$dead[[t]])

    dfrow[1] <- t
    dfrow[2] <- Lt
    dfrow[3] <- n_t
    dfrow[4] <- bt
    dfrow[5] <- sum(littersizes[[t]])
    dfrow[6] <- dt
    dfrow[7] <- sum(dead[[t]])
    if(b >= d){
      if(n_0 < m){
          dfrow[8] <- m / (1 + ((m-n_0)/n_0)*exp(-1*(b-d)*t))
      }
      if(m <= n_0){
          dfrow[8] <- m / (1 - ((n_0 - m)/n_0)*exp((d-b)*t))
      }
    }
    if(d > b){
        dfrow[8] <- n_0*exp((b-d)*t)
    }
    daframe[t,] <<- dfrow
    #####
  }

  popGraph <- function(m, b, d, n_0, totalTime, display) {

    plot(0, type='n', ylab = "Population (n)"
         , xlab = "Time (Insert Units)", xlim = c(0,(totalTime*1.05)),
         ylim = c(0,(max(rv$daframe$population)*1.05)))
    if(display == 1 | display == 2){
      lines(x=rv$daframe$time, rv$daframe$population, type = 'l')
    }
    if((display == 1) | (display == 3)){
      lines(x=rv$daframe$time, rv$daframe$theoretical, col = "red")
    }
    if(b > d)
      lines(x=rv$daframe$time, rep(m, length(rv$daframe$time)), col = "blue")

  }

  makeCenters <- function(numblitters){
    car <- ceiling(sqrt(numblitters))
    x <- (1:car)/(car+1)
    y <- (1:car)/(car+1)
    df <- expand.grid(x,y)
    names(df) <- c("x", "y")
    distance <- 1/(car+1)
    bools <- c(rep(TRUE, numblitters),
               rep(FALSE, car^2-numblitters))
    selected <- sample(bools, size = car^2, replace = FALSE)
    results <- list(centers = df[selected,],distance = distance)
    return(results)
  }

  observe({
    input$goButton
    if(input$seed){
      set.seed(input$setter)
    }
    else{
      set.seed(as.numeric(Sys.time()))
    }})

#  rv <- reactiveValues(
#    t = NULL,
#    ne = vector(),
#    nl = vector(),
#    Re = vector(),
#    re = vector(),
#    rl = vector(),
#    Rl = vector()
#  )

  observeEvent(input$goButton, {
    t <<- 0
    # reset dataframe
    rv$daframe <<- rv$data.frame(time = rep(0, input$totalTime + 1)
                          , numLitters = rep(0, input$totalTime + 1)
                          , population = rep(n_0, input$totalTime + 1)
                          , birthrate = rep(0, input$totalTime + 1)
                          , born = rep(0, input$totalTime + 1)
                          , deathrate = rep(0, input$totalTime + 1)
                          , dead = rep(0, input$totalTime + 1)
                          , theoretical = rep(n_0, input$totalTime + 1))
    rv$n_0 <- input$n_0
    rv$n_t <- n_0
    rv$m <- input$m
    rv$b <- input$b
    rv$d <- input$d
    for(i in 1:input$totalTime + 1) {
      update()
    }
    rv$sim <- rv$sim + 1
  })

  output$pop <- renderPlot({
    rv$sim
    disp <- as.numeric(input$display)
    popGraph(  rv$m
             , rv$b
             , rv$d
             , rv$n_0
             , input$totalTime
             , disp)
  })

  output$discuss <- renderText(HTML("<h2>Explanation</h2> <div><p>There can be up to
                          three lines on the graph. </p>
                          <ol><li>The <font color='red'>red</font> line represents the theoretical population. </li>
                          <li>The <strong>black</strong> line represents a simulated population under the given parameters. </li>
                          <li>The <font color='blue'>blue</font> line will only appear if the carrying capacity is
                         relevant. It is not useful when the minimum death rate is
                         greater than the maximum birth rate.</li></ol></div>"))

#  output$growth <- renderPlot({
#    if(input$growthRate==1) {
#      plot(rv$ne,rv$Re, type="l", xlim=c(0,input$k*1.05), ylim=c(0,max(rv$Rl)*2),
#           lwd=2, col="red",
#           main="Population Growth Rate (R) v. Population Size (n)",
#           xlab="number of individuals (n)", ylab="new individuals/ time (R)",
#           cex.main=0.8, cex.lab=0.8, cex.axis=0.8)
#      points(rv$nl,rv$Rl, type="l", lwd=2, col="black")
#    }

  output$momentF <- renderUI({
    sliderInput (  label = "Time To View"
                 , inputId = "mom"
                 , step = 1
                 , min = 0
                 , max = input$totalTime
                 , value = 0
                 , animate = animationOptions(loop = TRUE))
  })
  output$field <- renderPlot({
    input$goButton
    time <- as.numeric(input$mom)
    if (time == 0) {
      na <- initial[3]
      xa <- runif(na)
      ya <- runif(na)
    }
    else if (!is.null(littersizes) && !is.null(littersizes[[time]])
        && sum(littersizes[[time]] > 0)) {
      na <- daframe$population[time] - sum(littersizes[[time]]) # number of
                                                                # adults
      xa <- runif(na)
      ya <- runif(na)
      nl <- daframe$numLitters[time]
      myCens <- makeCenters(nl)
      cents <- myCens$centers
      d <- myCens$distance
      xb <- NULL
      yb <- NULL
      for(i in 1:nl){
        cent <- cents[i,]
        xbbit <- rnorm(littersizes[[time]][i],mean = cent$x, sd = d/6)
        ybbit <- rnorm(littersizes[[time]][i],mean = cent$y, sd = d/6)
        xb <- c(xb, xbbit)
        yb <- c(yb, ybbit)
      }
    }
    else {
      na <- daframe$population[time]
      xa <- runif(na)
      ya <- runif(na)
    }
    par(mfrow = c(1,2))
    plot(xa,ya, axes = FALSE, cex = 0.5
         , pch = 19
         , main = "Adults"
         , xlab = ""
         , ylab = "")
    if (time == 0) {
      plot(0, 0, col = "transparent"
           , cex = 0.5
           , pch = 19
           , axes = FALSE
           , main = "Warren"
           , xlab = ""
           , ylab = "")
    }
    else if (!is.null(littersizes) && !is.null(littersizes[[time]])
        && sum(littersizes[[time]]) > 0) {
      plot(xb,yb, axes = FALSE, cex = 0.5
           , pch = 19
           , main = "Warren"
           , xlab = ""
           , ylab = "")
    }
    else {}
    par(mfrow = c(1,1))
  })
  output$population <- renderText({
    "Total Population: "
    if (input$mom == 0) {
      initial[3]
    }
    else {
      daframe[input$mom, "population"]
    }
  })
  output$babies <- renderText({
    "Litter sizes: "
    if (input$mom == 0) {
      0
    }
    else {
      littersizes[[input$mom]]
    }
  })
  outputOptions(output, "momentF", suspendWhenHidden = FALSE)
#    if(input$growthRate==2) {
#      plot(rv$ne,rv$re, type="l", xlim=c(0,max.k*1.05), ylim=c(0,max.Rmax*1.05) ,
#           lwd=2, col="red",
#           main="Per Capita Growth Rate (r) v. Population Size (n)",
#           xlab="number of individuals (n)", ylab="new individuals/ time/ n (r)",
#           cex.main=0.8, cex.axis=0.8, cex.lab=0.8)
#      points(rv$nl,rv$rl, type="l", lwd=2, col="black")
#    }
#  })
}