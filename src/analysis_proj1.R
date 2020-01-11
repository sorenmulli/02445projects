######
#Initial work on project 1
#####
#setwd("/home/sorenwh/Nextcloud/semester3/StatProj/afl/02445projects/src")

rm(list = ls())

load(file = "data/armdata.RData")
our_experiment_no <- 4

data_collection <- armdata[[our_experiment_no]] 

#Person 1, repetition 1
#par(mfrow = c(2,1))
example <- data_collection[[1]][[1]]
summary(example)

plot(example[,1], ylim = c(-10, 60), col = "red", main = "Experiment 4, person 1, repetition 1: Arm movement data", xlab = "Coordinate no.", ylab = "Coordinate value")
points(example[,2], col = "blue")
points(example[,3], col = "green")
legend(1, 60, legend=c("x-coord.", "y-coord.", "z-coord."),
       col=c("red", "blue", "green"), lty=1:2, cex=0.8)

rbPal <- colorRampPalette(c('red','blue'))
colors <- rbPal(10)[as.numeric(cut(example[,3],breaks = 10))]
plot(example[,1], example[,2], col = colors, main = "Experiment 4, person 1, repetition 1: Arm movement data" , ylab = "Y-værdi", xlab ="X-værdi")
legend(10, -0.5,legend = "Farve: z-koordinat",)

