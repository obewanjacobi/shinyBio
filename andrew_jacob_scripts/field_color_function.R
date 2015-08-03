#---------------------------------------------------------------------------
# Jacob Townson
# Field Color Function
# This function makes a graph that plots the population (n) in a field with
# a color to represent how close the population is to the carrying capacity
# (m). The closer it is, the more brown the graph becomes. Once it passes 
# the carrying capacity, the graph becomes a dark brown.
#---------------------------------------------------------------------------


# Full green for pop = 0

# 1.2 times carrying capacity brown
# red =165, green = 125

# If d > b, there is no m. So there must be an n_0 in order for the 
# function to work. n_0 is the initial population. 

field.color <- function(m = (2*n_0), n){
  prop <- n/m
  if(prop > 1.2){
    prop <- 1.2
  }
  loss <- prop * 130
  grn <- 255 - loss
  gain <- prop * 165
  rd <- gain
  color <- rgb(red = rd, blue = 0, green = grn, maxColorValue = 255)
  plot(0,0, axes = FALSE, xlab = '', ylab = '', main = 'Adults in Field')
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], 
       col = color)
  points(runif(n, -1, 1), runif(n, -1, 1), pch = 19, cex = .5)
}