
m <- 1000   # carrying capacity
b  <- 0.05  # birth rate
d  <- 0.025 # death rate
n0 <- 2
nt <- n0   # initial value, garbage value to start
nt_1 <- nt # value from last time step
totalTime <- 550
t <- 0
simulated_nt <- n0 # initial value, garbage value to start


bt <- 0 # Initial values to make these objects persistant
dt <- 0 # Initial values to make these objects persistant
st <- 0 # Initial values to make these objects persistant
lt <- 0 # Initial values to make these objects persistant
Lt <- 0 # Initial values to make these objects persistant
littersizes <- list(rep(-1, totalTime + 1)) # Initial values to make these objects persistant


daframe <- data.frame(time = rep(0, totalTime + 1), numLitters = rep(0, totalTime + 1)
                      , population = rep(n0, totalTime + 1)
                      , birthrate = rep(n0, totalTime + 1), born = rep(0, totalTime + 1)
                      , deathrate = rep(0, totalTime + 1), dead = rep(0, totalTime + 1)
                      , theoretical = rep(n0, totalTime + 1))
dfrow <- numeric(length = 8)
dfrow[1] <- t
dfrow[2] <- Lt
dfrow[3] <- simulated_nt
dfrow[4] <- bt
dfrow[5] <- 0
dfrow[6] <- 0
dfrow[7] <- 0
dfrow[8] <- 0

update <- function() {
  t <<- t + 1
  bstar <-  b + (d - b)/2*(simulated_nt/m) # birth rate at time t
  dstar <-  d + (b - d)/2*(simulated_nt/m) # death rate at time t
  bt <<- max(0, bstar)
  dt <<- dstar + max(0, -bstar)
  st <<- sqrt(bt) # averate size of litters
  lt <<- sqrt(bt) # rate of litters
  Lt <<- rpois(n = 1, lambda = lt*simulated_nt) # number of litters based on litter rate
  if (is.null(littersizes)) {
    littersizes <<- list(rpois(Lt, lambda = st)) # produce sizes of each litter
  }
  else {
    littersizes[[t]] <<- rpois(Lt, lambda = st) # produce sizes of each litter
  }
  dead <- rpois(1, lambda = dt*simulated_nt) # produce dead rabbits
  if (length(littersizes[[t]])) {
    # population sizes = previous population + sum of all litters - dead rabbits
    simulated_nt <<- simulated_nt + sum(littersizes[[t]]) - dead
  }

  dfrow[1] <- t
  dfrow[2] <- Lt
  dfrow[3] <- simulated_nt
  dfrow[4] <- bt
  dfrow[5] <- sum(littersizes[[t]])
  dfrow[6] <- dt
  dfrow[7] <- dead
  # below line is jacob's broken equation
  # dfrow[8] <- ((m*n0)/(m-n0)*exp((b-d)/(m^2)*t))/(1+(n0)/(m-n0)*exp((b-d)/(m^2)*t))
  dfrow[8] <- m / (1 + ((m-n0)/n0)*exp(-1*(b-d)*t))
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