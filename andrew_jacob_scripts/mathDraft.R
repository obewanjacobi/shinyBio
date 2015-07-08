
m <- 1000   # carrying capacity
b  <- 0.05  # birth rate
d  <- 0.025 # death rate
n_0 <- 2    # Initial Poplulation
n_t <- n_0   # initial value, garbage value to start
n_t_1 <- n_t # value from last time step
totalTime <- 550
t <- 0
nt <- n_0 # initial value, garbage value to start


bt <- 0 # Initial values to make these objects persistan_t
dt <- 0 # Initial values to make these objects persistan_t
st <- 0 # Initial values to make these objects persistan_t
lt <- 0 # Initial values to make these objects persistan_t
Lt <- 0 # Initial values to make these objects persistan_t
littersizes <- list(rep(-1, totalTime + 1)) # Initial values to make these objects persistan_t
dead <- list(rep(-1, totalTime + 1)) # Initial values to make these objects persistan_t
living <- list(rep(-1, totalTime + 1)) # Initial values to make these objects


daframe <- data.frame(time = rep(0, totalTime + 1), numLitters = rep(0, totalTime + 1)
                      , population = rep(n_0, totalTime + 1)
                      , birthrate = rep(n_0, totalTime + 1), born = rep(0, totalTime + 1)
                      , deathrate = rep(0, totalTime + 1), dead = rep(0, totalTime + 1)
                      , theoretical = rep(n_0, totalTime + 1))
dfrow <- numeric(length = 8)
dfrow[1] <- t
dfrow[2] <- Lt
dfrow[3] <- nt
dfrow[4] <- bt
dfrow[5] <- 0
dfrow[6] <- 0
dfrow[7] <- 0
dfrow[8] <- 0

update <- function() {
  t <<- t + 1
  bstar <-  b + (d - b)/2*(nt/m) # Important factor in birth rate at time t
  dstar <-  d + (b - d)/2*(nt/m) # Important factor in death rate at time t
  bt <<- max(0, bstar)          # birth rate at time t
  dt <<- dstar + max(0, -bstar) # death rate at time t
  st <<- 8*sqrt(bt)/sqrt(b) # averate size of litters
  lt <<- sqrt(bt)*sqrt(b)/8 # rate of litters
  Lt <<- rpois(n = 1, lambda = lt*nt) # number of litters based on litter rate
  if (is.null(littersizes)) {
  }
  else {
    littersizes[[t]] <<- rpois(Lt, lambda = st) # produce sizes of each litter
  }
  if (is.null(dead)) {
  }
  else {
    dead[[t]] <<- rpois(1, lambda = dt*nt) # produce dead rabbits
  }

  # keep track of the living rabbits and when they were born
  birthsThisGeneration <- sum(littersizes[[t]])
  if (birthsThisGeneration >= 1) {
    living[[t]] <<- 1:birthsThisGeneration
  }
  else {
    living[[t]] <<- NULL
  }
  for (i in 1:dead[[t]]) {
  }

  if (length(littersizes[[t]])) {
    # population sizes = previous population + sum of all litters - dead rabbits
    nt <<- nt + sum(littersizes[[t]]) - sum(dead[[t]])
  }

  dfrow[1] <- t
  dfrow[2] <- Lt
  dfrow[3] <- nt
  dfrow[4] <- bt
  dfrow[5] <- sum(littersizes[[t]])
  dfrow[6] <- dt
  dfrow[7] <- sum(dead[[t]])
  # below line is jacob's broken equation
  # dfrow[8] <- ((m*n_0)/(m-n_0)*exp((b-d)/(m^2)*t))/(1+(n_0)/(m-n_0)*exp((b-d)/(m^2)*t))
  dfrow[8] <- m / (1 + ((m-n_0)/n_0)*exp(-1*(b-d)*t))
  daframe[t+1,] <<- dfrow
  #####
}


for (i in 1:totalTime) {
  update()
}

png(paste(as.character(as.numeric(Sys.time())),"modelplot.png", sep=""), width = 480, height = 480)
plot(x=daframe$time, daframe$population, ylab = "Population (n)"
     , xlab = "Time (Insert Units)")

lines(x=daframe$time, daframe$theoretical, col = "red")
dev.off()
