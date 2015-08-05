library(shiny)


function(input, output) {
  generationsTillMaturity <- 1 # how many generations it takes for a generation
  # to mature

  # compute probability for different death causes
  deathProbs <- function(n, b, d, m) {
    # n = current population size
    # b = max birth rate
    # d = min death rate
    # m = carrying capacity (may not be relevant)
    if (b > d) {
      lm <- 0.01^(n/(1.2*m))
      fox <- 0.5*(1- lm)
      mal <- 0.5*(1 - lm)
    } else {
      mal <- 0
      lm <- 0.5 + 0.5 * 0.02^(n/m)
      fox <- 1 - lawn
    }
    c(lm, fox, mal)
  }
  
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
  deathTypes <- matrix()
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
    
    # now simulate causes of death:  lawnmover, fox, malnutrition
    deadNow <- dead[[t]]
    probs <- deathProbs(n_t, b, d, m)
    deathCauses[t-1,] <<- rmultinom(1, size = deadNow, prob = probs)

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
    deathCauses <<- matrix(0, (input$totalTime +1)*3, ncol = 3)
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
      endTime <- input$totalTime
      b <- input$b
      d <- input$d
      m <- input$m
      times <- 1:endTime
      plot(times, theoretical(times), type = "l"
           , col = "red")
      if (b > d) {
        lines(x = c(0,endTime), y = c(m,m), col = "blue")
      }
    })
  })

  output$initialDiscuss <- renderText(HTML(
      "<h2>Explanation</h2> <div><p>There can be up to two lines on the graph. </p>
            <ol><li>The <font color='red'>red</font> line represents the theoretical population 
                  determined by the selected parameters.</li>
                <li>The <font color='blue'>blue</font> line will only appear if the carrying
                  capacity is
                  relevant. It is not relevant when the minimum death rate is
                  at least as big as the maximum birth rate.</li>
            </ol></div>"))

  output$discuss <- renderText(HTML("<h2>Explanation</h2> <div><p>There can be up to
                          three lines on the graph. </p>
                          <ol><li>The <font color='red'>red</font> line represents the theoretical population determined by the selected parameters. </li>
                          <li>The <strong>black</strong> line represents a simulated population, using the given parameters. </li>
                          <li>The <font color='blue'>blue</font> line will only appear if the carrying capacity is
                         relevant. It is not relevant when the minimum death rate is
                         at least as big as the maximum birth rate.</li></ol></div>"))

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
  
  output$momentG <- renderUI({
    sliderInput (  label = "Time To View"
                   , inputId = "momG"
                   , step = 1
                   , min = 0
                   , max = input$totalTime
                   , value = 0
                   , animate = animationOptions(loop = TRUE))
  })
  
  observeEvent(rv$sim, {
    output$gy <- renderPlot({
      time <- input$momG
      if (is.null(time) || time == 0 ) {
        deaths <- c(0L,0L,0L)
      } else {
        deaths <- deathCauses[time, ]
      }
      lm <- deaths[1]
      fox <- deaths[2]
      mal <- deaths[3]
      n <- sum(deaths)
      
      if ( n == 0) {
        plot(0, 0, col = "transparent"
             , axes = FALSE
             , main = "Graveyard"
             , sub = "Only recently-deceased unburied rabbits are shown."
             , xlab = ""
             , ylab = ""
             , xlim = c(0,1)
             , ylim = c(0,1))
      } else {
        xd <- runif(n)
        yd <- runif(n)
        cause <- c(rep("red", lm), rep("black", fox), rep("blue", mal))
        plot(xd, yd, col = cause
             , pch = 19
             , cex = 1.5
             , axes = FALSE
             , main = "Graveyard"
             , sub = "Only recently-deceased unburied rabbits are shown."
             , xlab = ""
             , ylab = ""
             , xlim = c(0,1)
             , ylim = c(0,1))
      }
    })
  })
  
  
  observeEvent(rv$sim, {
    output$field <- renderPlot({
      time <- input$mom
      if (is.null(time) || time == 0) {
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
          sd <- d/6
          xbbit <- rnorm(rv$littersizes[[time]][i],mean = cent$x, sd = sd)
          ybbit <- rnorm(rv$littersizes[[time]][i],mean = cent$y, sd = sd)
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
           , ylab = ""
           , xlim = c(0,1)
           , ylim = c(0,1))
      if (time == 0) {
        plot(0, 0, col = "transparent"
             , cex = 0.5
             , pch = 19
             , axes = FALSE
             , main = "Warren"
             , xlab = ""
             , ylab = ""
             , xlim = c(0,1)
             , ylim = c(0,1))
      }
      else if (!is.null(rv$littersizes) && !is.null(rv$littersizes[[time]])
               && sum(rv$littersizes[[time]]) > 0) {
        plot(xb,yb, axes = FALSE, cex = 0.5
             , pch = 19
             , main = "Warren"
             , xlab = ""
             , ylab = ""
             , xlim = c(0,1)
             , ylim = c(0,1))
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
  
  output$babies <- renderTable({
    if (input$mom == 0) {
      out <- NULL
    }
    else {
      num <- rv$littersizes[[input$mom]]
      if (length(num) > 0) {
        mat <- matrix(num, ncol = length(num))
        colnames(mat) <- paste0("Litter ",1:length(num))
        rownames(mat) <- "Litter Sizes"
        out <- mat
      } else {
        out <- NULL
        }
    }
    out
  })
  
  
  output$deathTallies <- renderTable({
    time <- input$momG
    if (is.null(time) || time == 0) {
      deaths <- c(0L,0L,0L)
    } else {
      deaths <- as.integer(deathCauses[input$momG, ])
    }
    mat <- matrix(deaths, ncol = 3)
    colnames(mat) <- c("Lawnmower (red)", "Fox (black)", "Malnutrition (blue)")
    rownames(mat) <- "Death Cause"
    mat
  })
  
  outputOptions(output, "momentF", suspendWhenHidden = FALSE)
}