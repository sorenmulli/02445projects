######
#Initial work on project 1
######
setwd(dirname(rstudioapi::getActiveDocumentContext()$path));
require(tree);
library(class);

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
	 main = "Experiment 4, person 1, repetition 1: Arm movement data" , ylab = "Y-coord.", xlab ="X-coord.");
#legend(10, -0.5,legend = "Colour: z-coord.",);

legend(20, -0.5,title="Colour: z-coord",legend=round( quantile(example[,3], (1:10)/10))
,col =rbPal(10),pch=20)


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
names(df) <- names.;names. <- rep(NA, 302);
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

for (i in 1:10) {
	for (j in 1:10) {
		idx <- (i-1) * 10 + j;
		df$person[idx] <- i;
		df$repetition[idx] <- j;
		df[idx, 3:302] <- as.vector(data_collection[[i]][[j]]);
	}
}
df$person <- as.factor(df$person);
df$repetition <- as.factor(df$repetition);

# Splits into train and test
# Performs leave-one-out cross-validation
# Classification trees and KNN is used
tree.preds <- rep(NA, 100);
knn.preds <- rep(NA, 100);
for (i in 1:100) {
	tree.model <- tree(person ~ . - repetition, data = df, subset = setdiff(1:100, i));
	tree.preds[i] <- predict(tree.model, df[i, ], type = "class");
	knn.preds[i] <- knn(df[setdiff(1:100, i), 3:302], df[i, 3:302], cl = df[setdiff(1:100, i), ]$person, k = 3);
}
tree.acc <- mean(tree.preds == df$person);
knn.acc <- mean(knn.preds == df$person);
tree.acc
knn.acc

#NEW DATA FRAME for ANOVA
rm(list = ls());

load(file = "data/armdata.RData");

raw_movement <- unlist(armdata, recursive = T)
coordinate <- rep( c(
    rep("x", 100),
    rep("y", 100),
    rep("z", 100)
    ),
    1600
) 
repetition <- c()
person <- c()
experiment <- c()
for (i in 1:16){ #experiments
  print(i)
  for (j in 1:10){ #persons
    for (k in 1:10){ #repetitions
      experiment <- c(experiment, rep(i, 300))
      person <- c(person, rep(j, 300))
      repetition <- c(repetition, rep(k, 300))
    }
  }
}

arm_dataframe <- data.frame(
  "pos" <- raw_movement,
  "coordinate" <- as.factor(coordinate),
  "repetition" <- as.factor(repetition),
  "person" <- as.factor(person),
  "experiment" <- as.factor(experiment)
)
model <- lm(pos ~ coordinate + repetition + person+ experiment)
anova(model)
summary(model)
