# R Script with Data for Pre and Post Tests from classroom data on app

kstudents <- c(1:14)
gstudents <- c(1:12)

gprescore <- c(6/11, 4/11, 6/11, 7/11, 7/11, 4/11, 6/11, 4/11, 6/11, 3/11, 5/11, 5/11)*100
kprescore <- c(6/11, 4/11, 6/11, 7/11, 3/11, 6/11, 3/11, 8/11, 5/11, 4/11, 6/11, 6/11, 6/11, 6/11)*100

gpostscore <- c(8/11, 5/11, 5/11, 5/11, 9/11, 8/11, 8/11, 3/11, 8/11, 6/11, 8/11, 7/11)*100
kpostscore <- c(5/11, NA, 6/11, 6/11, 4/11, 7/11, 1/11, 9/11, 4/11, 5/11, 9/11, 8/11, 10/11, 5/11)*100

tgriff <- data.frame(gstudents, gprescore, gpostscore)

kopp <- data.frame(kstudents, kprescore, kpostscore)

gp <- gprescore/100*11
kp <- kprescore/100*11
ga <- gpostscore/100*11
ka <- kpostscore/100*11
inst <- c(rep("griffith", 12), rep("kopp",14))
prepost <- data.frame(instructor = inst, pre = c(gp,kp), post = c(ga, ka))
prepost$diff <- prepost$post - prepost$pre

t.test(diff ~ instructor, data = prepost)

library(lattice)
lattice::histogram( ~ diff | instructor,
                    data = prepost,
                    layout = c(1,2), 
                    breaks = seq(from = -2.5, to = 4.5, by = 1),
                    type = "count",
                    xlab = "score difference (post - pre)")