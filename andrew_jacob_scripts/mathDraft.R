
m <- 1000   # carrying capacity
b  <- 0.2  # birth rate
d  <- 0.1 # death rate
n0 <- 2
nt <- n0   # initial value, garbage value to start
nt_1 <- nt # value from last time step
totalTime <- 10000
t <- 0
simulated_nt <- n0 # initial value, garbage value to start


bt <- 0 # Initial values to make these objects persistant
dt <- 0 # Initial values to make these objects persistant
st <- 0 # Initial values to make these objects persistant
lt <- 0 # Initial values to make these objects persistant
Lt <- 0 # Initial values to make these objects persistant
littersizes <- list(rep(-1, totalTime + 1)) # Initial values to make these objects persistant


daframe <- data.frame(time = 0, numLitters = 0, population = n0, birthrate = 0
                      , born = 0, deathrate = 0, dead = 0, theoretical = n0)
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
  bt <<- max(0, b*(1-simulated_nt/m))
  dt <<- d + max(0, b*(simulated_nt/m - 1))
  st <<- sqrt(bt) # size of litters
  lt <<- sqrt(bt) # number of litters
  Lt <<- rpois(n = 1, lambda = lt*simulated_nt)
  if (is.null(littersizes)) {
    littersizes <<- list(rpois(Lt, lambda = st))
  }
  else {
    littersizes[[t]] <<- rpois(Lt, lambda = st)
  }
  dead <- rpois(1, lambda = dt*simulated_nt)
  if (length(littersizes[[t]])) {
    simulated_nt <<- simulated_nt + sum(littersizes[[t]]) - dead
  }

  dfrow[1] <- t
  dfrow[2] <- Lt
  dfrow[3] <- simulated_nt
  dfrow[4] <- bt
  dfrow[5] <- sum(littersizes[[t]])
  dfrow[6] <- dt
  dfrow[7] <- dead
  dfrow[8] <- (m/b)*(-b + b*n0/m + d)*exp(-b*t/m) + m - d*m/b
  daframe <<- rbind(daframe, dfrow)
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