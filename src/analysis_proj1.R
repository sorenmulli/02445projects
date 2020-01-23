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

plot(tree.model)
text(tree.model)

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
  print(n11)
  print(n12)
  print(n21)
  print(n22)
  m <- min(c(n12,n21))
  prob <- 1/2
  N <- n12 + n21
  p <- 2*pbinom(m, N, prob)
  print(sprintf("p value: %s", p))
  
  
  
  alpha <- 0.01
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
sum(is.na(raw_movement))

coordinate <- rep(coordinates, 1600);
coordinate <- rep(c(
   rep("x", 100),
   rep("y", 100),
   rep("z", 100)),
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
  pos <- raw_movement,
  coordinate <- as.factor(coordinate),
  repetition <- as.factor(repetition),
  person <- as.factor(person),
  experiment <- as.factor(experiment)
)
#Use last (control) experiment as reference
# levels(arm_dataframe$experiment) <-c(2:16, 1)

options("max.print" = 200);
ps <- c();
msqs <- c();

inter_ps <- c();
inter_msqs <- c();

person_ps <- c();
person_msqs <- c();

resi_msqs <- c();
par(mfrow = c(3, 4));

for (i in 1:length(coordinates)) {
	coor = coordinates[i];
	s = subset(arm_dataframe, arm_dataframe$coordinate == coor);
	model <- lm(s$pos ~ s$experiment * s$person);
	an <- anova(model);
	
	msqs <- c(msqs, an$`Mean Sq`[1]);
	ps <- c(ps, an$`Pr(>F)`[1]);
	
	person_ps <- c(person_ps, an$`Pr(>F)`[2]);
	person_msqs <- c(person_msqs, an$`Mean Sq`[2]);
	
	
	inter_ps <- c(inter_ps, an$`Pr(>F)`[3]);
	inter_msqs <- c(inter_msqs, an$`Mean Sq`[3]);
	
	resi_msqs <- c(resi_msqs, an$`Mean Sq`[4]);
	
	if ((i + 15) %% 25 == 0) {
	  print(coor);
	  print(an);
	  plot(s$experiment[!is.na(s$pos)], model$residuals, 
	       main = paste("Model residuals for", `coor`),
	       xlab = "Experiment no.",
	       ylab = "Residuals")
	  
	  #qqnorm(model$residuals, main = paste("Model residuals for", `coor`));
	  
	  #qqline(model$residuals);
	  #hist(model$residuals, main = paste("Model residuals for", `coor`), xlab = "Residuals", breaks = 50);
	}
	summary(model)
}

mean(msqs);
mean(person_msqs)
mean(inter_msqs);
mean(resi_msqs)

par(mfrow = c(1, 3));
unordered <- p.adjust(ps, method = "BH");
plot( unordered[1:100], main = "X-coordinate: Adjusted p values", ylab = "p", xlab ="x-coordinate");
plot( unordered[100:200], main = "Y-coordinate: Adjusted p values", ylab = "p", xlab ="y-coordinate");
plot( unordered[200:300], main = "Z-coordinate:  Adjusted p values", ylab = "p", xlab ="z-coordinate");


par(mfrow = c(1, 2));
sorted <- sort(ps);
plot(sorted, main = "Unadjusted, sorted p values", ylab = "p");
adjusted <- p.adjust(sorted, method = "BH");
plot(adjusted, main = "Adjusted, sorted p values", ylab = "p");


alpha <- .01;
print(mean(sorted < alpha));
print(mean(adjusted < alpha));
print(sum(sorted < alpha));
print(sum(adjusted < alpha));

mean(person_ps < alpha);
mean(p.adjust(person_ps, method = "BH") < alpha);
sum(person_ps < alpha);
sum(p.adjust(person_ps, method = "BH") < alpha);

print(mean(ps == 0))

print(mean(person_ps == 0))
mean(person_ps)

mean(inter_ps < alpha);
mean(p.adjust(inter_ps, method = "BH") < alpha);
sum(inter_ps < alpha);
sum(p.adjust(inter_ps, method = "BH") < alpha);



#model <- lm(pos ~ experiment + coordinate + repetition + person);
#anova(model);
#summary(model);

#model <- lm(pos ~ experiment);
#anova(model);
#summary(model);


#par(mfrow = c(1, 2));
#hist(model$residuals, breaks = 100, main = "ANOVA residuals", xlab = "Residuals");
#qqnorm(model$residuals);
#qqline(model$residuals);



#par(mfrow = c(1, 1))
plot(experiment[!is.na(pos)], model$residuals, 
     xlab = "Experiment no.",
     ylab = "Residuals")

#plot(repetition[!is.na(pos)], model$residuals, 
 #    xlab = "Repetition no.",
  #   ylab = "Residuals")

plot(person[!is.na(pos)], model$residuals, 
     xlab = "Person no.",
     ylab = "Residuals")




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


