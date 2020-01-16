######
#Initial work on project 1
######

setwd(dirname(rstudioapi::getActiveDocumentContext()$path));
require(tree);
library(class);
library(plotly);

#install.packages("plotly")
#library(jsonlite)
#exportJSON <- toJSON(armdata)
#write(exportJSON, "armdata.json")

rm(list = ls());
load(file = "data/armdata.RData");

#mean(is.na(unlist(armdata, recursive = T) ))

our_experiment_no <- 4;
data_collection <- armdata[[our_experiment_no]];


#plot_ly(x=example2[,1], y=example2[,2], z=example2[,3], type = "scatter3d", mode = "lines")

#Person 1, repetition 1
#par(mfrow = c(2,1))
# data_collection[[person]][[repetition]]

#plot(example[,1], ylim = c(-10, 60), col = "red",
#	 main = "Experiment 4, person 1, repetition 1: Arm movement data",
#	 xlab = "Coordinate no.", ylab = "Coordinate value");
#points(example[,2], col = "blue");
#points(example[,3], col = "green");
#legend(1, 60, legend=c("x-coord.", "y-coord.", "z-coord."),
#       col=c("red", "blue", "green"), lty=1:2, cex=0.8);

#example <- data_collection[[1]][[1]];
#xample
#summary(example);


#rbPal <- colorRampPalette(c('red','blue'));



#colors <- rbPal(10)[as.numeric(cut(example[,3],breaks = 10))];
#plot(example[,1], example[,2], col = colors,
#	 main = "Experiment 4, person 1, repetition 1: Arm movement data",
#	 ylab = "Y-coord.", xlab ="X-coord.");
#lines(example[,1], example[,2])

#legend(20, -0.5,title="Colour: z-coord",legend=round( quantile(example[,3], (1:10)/10))
#,col =rbPal(10),pch=20)

#example2 <- data_collection[[2]][[1]];
#rbPal <- colorRampPalette(c('red','blue'));
#colors <- rbPal(10)[as.numeric(cut(example2[,3],breaks = 10))];
#plot(example2[,1], example2[,2], col = colors,
#     main = "Experiment 4, person 2, repetition 1: Arm movement data" , ylab = "Y-coord.", xlab ="X-coord.");
#legend(10, -0.5,legend = "Colour: z-coord.",);

#legend(10, 1,title="Colour: z-coord",legend=round( quantile(example2[,3], (1:10)/10))
 #      ,col =rbPal(10),pch=20)




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
set.seed(69)
tree.preds <- rep(NA, 100);
knn.preds <- rep(NA, 100);
for (i in 1:100) {
	tree.model <- tree(person ~ . - repetition, data = df, subset = setdiff(1:100, i));
	tree.preds[i] <- predict(tree.model, df[i, ], type = "class");
	knn.preds[i] <- knn(df[setdiff(1:100, i), 3:302], df[i, 3:302], cl = df[setdiff(1:100, i), ]$person, k = 3);
}

knn.acc <- mean(knn.preds == df$person);
tree.acc <- mean(tree.preds == df$person);

knn.acc
tree.acc

#3NN: Classifier 1
#Tree: Classifier 2
mcnemar <- function(preds1, preds2,  true){
  n <- length(true)
  n11 <- sum(preds1 == true & preds2== true)
  
  n12 <- sum(preds1 == true & preds2 != true)
  n21 <- sum(preds1 != true & preds2 == true)
  
  n22 <- sum(preds1 != true & preds2 != true)
  
  n11; n12; n21; n22
  
  
  r <- n12 / (n12 + n21); r
  #Null hypothesis r = 0.5
  #Test in binomial distribution
  
  m <- min(c(n12,n21))
  prob <- 1/2
  N <- n12 + n21
  p <- 2*pbinom(m, N, prob)
  print(sprintf("p value: %s", p))
  
  
  
  alpha <- 0.05
  theta.hat <- (n12-n21)/n; theta.hat
  Q <- (n^2 * (n+1)*(theta.hat +1)*(1-theta.hat))/(n*(n12+n21)-(n12-n21)^2)
  Beta.p <- (theta.hat+1)/2 * (Q-1)
  Beta.q <- (1-theta.hat)/2 * (Q-1)
  
  theta.low <- 2*qbeta(alpha, Beta.p, Beta.q)-1
  theta.upper <- 2*qbeta(1-alpha, Beta.p, Beta.q)-1
  print(sprintf("theta_hat: %s", theta.hat))
  
  print(sprintf("theta confidence interval: [%s, %s]", theta.low, theta.upper))
  
  

}
baseline <- sample(1:10, 100, replace = T)
mean(baseline == df$person)
mcnemar(knn.preds, tree.preds, df$person)

mcnemar(knn.preds,baseline, df$person)

mcnemar(tree.preds,baseline, df$person)



#NEW DATA FRAME for ANOVA
rm(list = ls());



load(file = "data/armdata.RData");

coordinates <- rep(NA, 300);
for (i in 1:300) {
	if (i <= 100) {
		coordinates[i] <- paste(c("x", as.character(i)), collapse = "");
	} else if (i <= 200) {
		coordinates[i] <- paste(c("y", as.character(i-100)), collapse = "");
	} else {
		coordinates[i] <- paste(c("z", as.character(i-200)), collapse = "");
	}
}

raw_movement <- unlist(armdata, recursive = T);
coordinate <- rep(coordinates, 1600);
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
  pos <- raw_movement,
  coordinate <- as.factor(coordinate),
  repetition <- as.factor(repetition),
  person <- as.factor(person),
  experiment <- as.factor(experiment)
)
#Use last (control) experiment as reference
levels(arm_dataframe$experiment) <-c(2:16, 1)
  
model <- lm(pos ~ coordinate + repetition + person + experiment)
anova(model)
summary(model)


plot1_dataframe <- subset(arm_dataframe, experiment == 4 & (person == 1))
plot1_dataframe$x_coord <- plot1_dataframe$pos[plot1_dataframe$coordinate == "x"]
plot1_dataframe$y_coord <- plot1_dataframe$pos[plot1_dataframe$coordinate == "y"]
plot1_dataframe$z_coord <- plot1_dataframe$pos[plot1_dataframe$coordinate == "z"]
plot1_dataframe$repetitions <- plot1_dataframe$repetition[plot1_dataframe$coordinate == "z"]

plot_ly(plot1_dataframe,
        x=~x_coord, y=~y_coord, z=~z_coord,
        type = "scatter3d", mode = "lines",
        line = list(width = 4), 
        split = ~repetitions
)%>%
layout(
  title = "Arm movement data: Experiment 4, person 1, ten repetitions",
  scene = list(
    xaxis = list(title = "X"),
    yaxis = list(title = "Y"),
    zaxis = list(title = "Z")
  ))



plot2_dataframe <- subset(arm_dataframe, experiment == 4 & (person == 2))
plot2_dataframe$x_coord <- plot2_dataframe$pos[plot1_dataframe$coordinate == "x"]
plot2_dataframe$y_coord <- plot2_dataframe$pos[plot1_dataframe$coordinate == "y"]
plot2_dataframe$z_coord <- plot2_dataframe$pos[plot1_dataframe$coordinate == "z"]
plot2_dataframe$repetitions <- plot2_dataframe$repetition[plot1_dataframe$coordinate == "z"]

plot_ly(plot2_dataframe,
        x=~x_coord, y=~y_coord, z=~z_coord,
        type = "scatter3d", mode = "lines", line = list(width = 4), split = ~repetitions)%>%
layout(
  title = "Arm movement data: Experiment 4, person 2, ten repetitions",
  scene = list(
    xaxis = list(title = "X"),
    yaxis = list(title = "Y"),
    zaxis = list(title = "Z")
  ))
x
