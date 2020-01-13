######
#Initial work on project 1
######
#setwd("/home/sorenwh/Nextcloud/semester3/StatProj/afl/02445projects/src")

rm(list = ls());

load(file = "data/armdata.RData");
our_experiment_no <- 4;

data_collection <- armdata[[our_experiment_no]];

#Person 1, repetition 1
#par(mfrow = c(2,1))
# data_collection[[person]][[repetition]]
example <- data_collection[[1]][[1]];
summary(example);

plot(example[,1], ylim = c(-10, 60), col = "red",
	 main = "Experiment 4, person 1, repetition 1: Arm movement data",
	 xlab = "Coordinate no.", ylab = "Coordinate value");
points(example[,2], col = "blue");
points(example[,3], col = "green");
legend(1, 60, legend=c("x-coord.", "y-coord.", "z-coord."),
       col=c("red", "blue", "green"), lty=1:2, cex=0.8);

rbPal <- colorRampPalette(c('red','blue'));
colors <- rbPal(10)[as.numeric(cut(example[,3],breaks = 10))];
plot(example[,1], example[,2], col = colors,
	 main = "Experiment 4, person 1, repetition 1: Arm movement data" , ylab = "Y-vaerdi", xlab ="X-vaerdi");
legend(10, -0.5,legend = "Farve: z-koordinat",);

# Creates dataframe with columns person, rep, 100x, 100y, 100z
df <- data.frame(matrix(ncol = 302, nrow = 100));
names. <- rep(NA, 302);
names.[1] <- "person"; names.[2] <- "repetition";
for (i in 1:300) {
	if (i <= 100) {
		names.[i+2] <- paste(c("x", as.character(i)), collapse = "");
	} else if (i <= 200) {
		names.[i+2] <- paste(c("y", as.character(i-100)), collapse = "");
	} else {
		names.[i+2] <- paste(c("z", as.character(i-200)), collapse = "");
	}
}
names(df) <- names.;
for (i in 1:10) {
	for (j in 1:10) {
		idx <- (i-1) * 10 + j;
		print(idx);
		df$person[idx] <- i;
		df$repetition[idx] <- j;
		df[idx, 3:302] <- as.vector(data_collection[[i]][[j]]);
	}
}
df$person <- as.factor(df$person);
df$repetition <- as.factor(df$repetition);

logfit <- lm(person ~ . - repetition, data = df);









