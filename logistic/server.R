library(shiny)


function(input, output) {
  generationsTillMaturity <- 1 # how many generations it takes for a generation
  # to mature

  # REACTIVE VALUES
  rv <- reactiveValues(sim = 0
                       , littersizes = list(integer(0))
                       , dead = list(integer(0))
                       , daframe = data.frame(time = NULL
                                    , numLitters = NULL
                                    , population = NULL
                                    , birthrate = NULL
                                    , born = NULL
                                    , deathrate = NULL
                                    , dead = NULL)
                      , beginning = TRUE)
  # END REACTIVE VALUES

  daframe <- data.frame(time = NULL
             , numLitters = NULL
             , population = NULL
             , birthrate = NULL
             , born = NULL
             , deathrate = NULL
             , dead = NULL)
  t <- 0
  n_0 <- 0
  n_t <- 0
  b <- 0
  d <- 0
  m <- 0
  littersizes <- list(integer(0))
  dead <- list(integer(0))
  st <- 0
  initial <- numeric(length = 7)

  observeEvent(input$reset,{
    rv$beginning <- TRUE
  })

  observeEvent(input$goButton,{
    if (input$goButton > 0) {
      rv$beginning <- FALSE
    }
  })

  output$beginning <- reactive({
    rv$beginning
  })

  outputOptions(output, "beginning", suspendWhenHidden = FALSE)


  advanceTime <- function() {
    dfrow = numeric(length = 7)

    t <<- t + 1

    if(d <= b){
      bstar =  b + (d - b)/2*(n_t/m) # Important factor in birth rate at time t
      dstar =  d + (b - d)/2*(n_t/m) # Important factor in death rate at time t
      bt <- max(0, bstar)            # birth rate at time t
      dt <- dstar + max(0, -bstar)   # death rate at time t
    }
    else{
      bt <- b
      dt <- d
    }

    st <<- 8*sqrt(bt)/sqrt(b) # averate size of litters
    lt <<- sqrt(bt)*sqrt(b)/8 # rate of litters
    Lt <<- rpois(n = 1, lambda = lt*n_t) # number of litters based on litter
    # rate
    littersizes[[t]] <<- rpois(Lt, lambda = st) # produce sizes of each litter

    if (is.null(dead)) {
    }
    else {
      dead[[t]] <<- rpois(1, lambda = dt*n_t) # produce dead rabbits
    }
    if (dead[[t]] > n_t){
      dead[[t]] <<- n_t
    }

    # keep track of the living rabbits and when they were born
    birthsThisGeneration <- sum(littersizes[[t]])

    # population sizes = previous population + sum of all litters - dead rabbits
    n_t <<- n_t + sum(littersizes[[t]]) - sum(dead[[t]])

    dfrow[1] = t
    dfrow[2] = Lt
    dfrow[3] = n_t
    dfrow[4] = bt
    dfrow[5] = sum(littersizes[[t]])
    dfrow[6] = dt
    dfrow[7] = sum(dead[[t]])
    daframe[t,] <<- dfrow
  }

  popGraph <- function(m, b, d, n_0, totalTime, display) {

    plot(0, type='n', ylab = "Population (n)"
         , xlab = "Time (Insert Units)", xlim = c(0,(totalTime*1.05)),
         ylim = c(0,(max(rv$daframe$population)*1.05)))
    if(display == 1 | display == 2){
      lines(x=rv$daframe$time, rv$daframe$population, type = 'l')
    }
    if((display == 1) | (display == 3)){
      lines(x=rv$daframe$time, theoretical(0:input$totalTime), col = "red")
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


  observeEvent(input$goButton, {
    if(input$seed){
      set.seed(input$setter)
    }
    else{
      set.seed(as.numeric(Sys.time()))
    }

    n_0 <<- input$n_0
    m <<- input$m
    t <<- 1
    n_t <<- input$n_0
    littersizes <<- list(integer(0))
    dead <<- list(integer(0))
    b <<- input$b
    d <<- input$d
    # reset dataframe
    daframe <<- data.frame(time = rep(0, input$totalTime)
                          , numLitters = rep(0, input$totalTime)
                          , population = rep(n_0, input$totalTime)
                          , birthrate = rep(0, input$totalTime)
                          , born = rep(0, input$totalTime)
                          , deathrate = rep(0, input$totalTime)
                          , dead = rep(0, input$totalTime))

    # initialize initial state of population
    init_bt <- max(0, b + (d - b)/2*(n_t/m))
    init_dt <- d + (b - d)/2*(n_t/m) + max(0, -(b + (d - b)/2*(n_t/m)))
    initial <<- c(0, 0, n_t, init_bt, 0, init_dt, 0)
    daframe <<- rbind(initial, daframe)
    for(i in 1:input$totalTime + 1) {
      advanceTime()
    }
    rv$littersizes <- littersizes
    rv$dead <- dead
    rv$daframe <- daframe
    rv$sim <- rv$sim + 1
    output$pop <- renderPlot({
      rv$sim
      disp <- as.numeric(input$display)
      popGraph(m
              , b
              , d
              , input$n_0
              , input$totalTime
              , disp)
    })
  })

  theoretical <- function(times) {
    b = input$b
    d = input$d
    n_0 = input$n_0
    m = input$m
    if(b >= d){
      if(n_0 < m){
        pop = m / (1 + ((m-n_0)/n_0)*exp(-1*(b-d)*times))
      }
      if(m <= n_0){
        pop = m / (1 - ((n_0 - m)/n_0)*exp((d-b)*times))
      }
    }
    if(d > b){
      pop = n_0*exp((b-d)*times)
    }
    return(pop)
  }
  observe({
    output$initialGraph <- renderPlot({
      times <- 1:input$totalTime
      plot(times, theoretical(times), type = "l"
           , col = "red")
    })
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
  observeEvent(rv$sim, {
    output$field <- renderPlot({
      time <- as.numeric(input$mom)
      if (time == 0) {
        na <- initial[3]
        xa <- runif(na)
        ya <- runif(na)
      }
      else if (!is.null(rv$littersizes) && !is.null(rv$littersizes[[time]])
          && sum(rv$littersizes[[time]] > 0)) {

        # number of adults
        na <- rv$daframe$population[time] - sum(rv$littersizes[[time]])
        xa <- runif(na)
        ya <- runif(na)
        nl <- rv$daframe$numLitters[time]
        myCens <- makeCenters(nl)
        cents <- myCens$centers
        d <- myCens$distance
        xb <- NULL
        yb <- NULL
        for(i in 1:nl){
          cent <- cents[i,]
          xbbit <- rnorm(rv$littersizes[[time]][i],mean = cent$x, sd = d/6)
          ybbit <- rnorm(rv$littersizes[[time]][i],mean = cent$y, sd = d/6)
          xb <- c(xb, xbbit)
          yb <- c(yb, ybbit)
        }
      }
      else {
        na <- rv$daframe$population[time]
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
      else if (!is.null(rv$littersizes) && !is.null(rv$littersizes[[time]])
          && sum(rv$littersizes[[time]]) > 0) {
        plot(xb,yb, axes = FALSE, cex = 0.5
             , pch = 19
             , main = "Warren"
             , xlab = ""
             , ylab = "")
      }
      else {}
      par(mfrow = c(1,1))
    })
  })
  output$population <- renderText({
    if (input$mom == 0) {
      num <- initial[3]
    }
    else {
      num <- rv$daframe[input$mom, "population"]
    }
    paste("Population: ", num)
  })
  output$babies <- renderText({
    if (input$mom == 0) {
      num <- integer(0)
    }
    else {
      num <- rv$littersizes[[input$mom]]
    }
    paste("Littersizes: ", num)
  })
  outputOptions(output, "momentF", suspendWhenHidden = FALSE)
}