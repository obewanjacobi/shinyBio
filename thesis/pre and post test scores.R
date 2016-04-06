# R Script with Data for Pre and Post Tests from classroom data on app

kstudents <- c(1:14)
gstudents <- c(1:12)

gprescore <- c(6/11, 5/11, 6/11, 7/11, 7/11, 4/11, 6/11, 5/11, 6/11, 3/11, 5/11, 5/11)
kprescore <- c(6/11, 4/11, 6/11, 7/11, 3/11, 6/11, 3/11, 8/11, 5/11, 4/11, 6/11, 6/11, 6/11, 6/11)

gpostscore <- c(rep(NA, 12))
kpostscore <- c(rep(NA, 14))

tgriff <- data.frame(gstudents, gprescore, gpostscore)

kopp <- data.frame(kstudents, kprescore, kpostscore)