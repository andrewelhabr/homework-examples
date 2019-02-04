
##### Problem 3 #####

rm(list = ls())
library(ggplot2)

nodes <- 9
trials <- c(10^3,10^4,10^5)
arcs_norm <- c(13,5.5,5.2,3.2,3.2)
arcs_expo <- c(7,16.5,14.7,6,10.3,4,20,16.5)

quantiles <- c(0.05,0.25,0.5,0.75,0.95)
mean_completion_times <- rep(0,length(trials))
mean_completion_times_sd <- rep(0,length(trials))
mean_completion_times_95CI_HL <- rep(0,length(trials))
percentile_estimates <- array(0,c(length(trials),length(quantiles)))
percentile_estimates_95CI_LB <- array(0,c(length(trials),length(quantiles)))
percentile_estimates_95CI_UB <- array(0,c(length(trials),length(quantiles)))
durations_per_trial <- list()

set.seed(42)
for (k in 1:length(trials)){
  arc_lengths <- array(0,c(trials[k],length(arcs_norm)+length(arcs_expo)))
  durations <- array(0,c(trials[k],nodes))
  for (i in 1:trials[k]){
    arcs_norm_sim <- pmax(rnorm(length(arcs_norm),arcs_norm,arcs_norm/4),0)
    arcs_expo_sim <- rexp(length(arcs_expo),1./arcs_expo)
    arc_lengths[i,] <- c(arcs_norm_sim,arcs_expo_sim)
    durations[i,2] <- arc_lengths[i,1]
    durations[i,3] <- max(arc_lengths[i,2],durations[i,2]+arc_lengths[i,6])
    durations[i,4] <- durations[i,2]+arc_lengths[i,3]
    durations[i,5] <- durations[i,2]+arc_lengths[i,9]
    durations[i,6] <- max(durations[i,2]+arc_lengths[i,7],durations[i,3]+arc_lengths[i,8],durations[i,5]+arc_lengths[i,12])
    durations[i,7] <- durations[i,4]+arc_lengths[i,10]
    durations[i,8] <- max(durations[i,5]+arc_lengths[i,11],durations[i,7]+arc_lengths[i,5])
    durations[i,9] <- max(durations[i,6]+arc_lengths[i,4],durations[i,8]+arc_lengths[i,13])
  }
  
  mean_completion_times[k] <- mean(durations[,9])
  mean_completion_times_sd[k] <- sd(durations[,9])
  mean_completion_times_95CI_HL[k] <- qt(0.975,trials[k]-1)*mean_completion_times_sd[k]/sqrt(trials[k])
  percentile_estimates[k,] <- unname(quantile(durations[,9],quantiles))
  percentile_estimates_95CI_LB[k,] <- unname(quantile(durations[,9],quantiles-qnorm(0.975)*sqrt(quantiles*(1-quantiles)/trials[k])))
  percentile_estimates_95CI_UB[k,] <- unname(quantile(durations[,9],quantiles+qnorm(0.975)*sqrt(quantiles*(1-quantiles)/trials[k])))
  
  durations_df <- data.frame(trial = k, durations = durations[,9])
  durations_per_trial[[k]] <- durations_df
  }

mean_completion_times
mean_completion_times_95CI_HL
percentile_estimates
percentile_estimates_95CI_UB
percentile_estimates_95CI_LB

durations_to_plot <- do.call(rbind,durations_per_trial)
trial_names <- list("1"="k=10^3","2"="k=10^4","3"="k=10^5")
trial_labeller <- function(variable,value){
  return(trial_names[value])
}
png("ISyE_6832_Homework_1_Problem_3.png", width = 450, height = 350)
suppressWarnings(ggplot(durations_to_plot, aes(x=durations)) + 
  geom_histogram(binwidth=1,color="black") +
  facet_grid(trial~.,scales="free",labeller=trial_labeller) +
  labs(x="Project Completion Time (min)",y = "Count"))
dev.off()



##### Problem 4c #####

rm(list = ls())

replications <- 100
entities <- c(10^3,10^4,10^5)

Y_bar <- rep(0,length(entities))
Y_bar_95CI_UB <- rep(0,length(entities))
Y_bar_95CI_LB <- rep(0,length(entities))
Y_bar_95CI_HL <- rep(0,length(entities))

set.seed(42)
for(k in 1:length(entities)){
  arrivals <- array(0,c(entities[k],replications))
  services <- array(0,c(entities[k],replications))
  delays <- array(0,c(entities[k],replications))
  for(j in 1:replications){
    arrivals[-1,j] <- rexp(entities[k]-1,1)
    services[,j] <- rgamma(entities[k],0.8,1)
    for(i in 2:entities[k]){
      delays[i,j] <- max(delays[i-1,j]+services[i-1,j]-arrivals[i,j],0)
    }
  }
  delay_means <- colMeans(delays)
  Y_bar[k] <- mean(delay_means)
  Y_bar_95CI_UB[k] <- Y_bar[k] + qt(0.975,replications-1)*sd(delay_means)/sqrt(replications)
  Y_bar_95CI_LB[k] <- Y_bar[k] - qt(0.975,replications-1)*sd(delay_means)/sqrt(replications)
  Y_bar_95CI_HL[k] <- qt(0.975,replications-1)*sd(delay_means)/sqrt(replications)
}

Y_bar
Y_bar_95CI_HL

Y_df <- data.frame(log10_entities = log10(entities),
                   mean = Y_bar,
                   LB = Y_bar_95CI_LB,
                   UB = Y_bar_95CI_UB)
steady_state_mean <- 3.6

png("ISyE_6832_Homework_1_Problem_4c.png", width = 350, height = 350)
ggplot(Y_df, aes(x = log10_entities, y = mean)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = UB, ymin = LB)) +
  geom_hline(yintercept= steady_state_mean, color="red") + 
  geom_text(aes(log10(entities)[1]+0.5, steady_state_mean, label = "Steady-State Mean", color="red", vjust = -1)) +
  theme(legend.position="none") +
  labs(x = "Log_10(Entity Delays)", y = "Mean Entity Delay (min)")
dev.off()



##### Problem 4d #####

rm(list = ls())
library(matrixStats)

replications <- 100
entities <- 10^5
extra <- 1000

set.seed(42)
for(k in 1:length(entities)){
  arrivals <- array(0,c(entities[k]+extra,replications))
  services <- array(0,c(entities[k]+extra,replications))
  delays <- array(0,c(entities[k]+extra,replications))
  for(j in 1:replications){
    arrivals[-1,j] <- rexp(entities[k]+extra-1,1)
    services[,j] <- rgamma(entities[k]+extra,0.8,1)
    for(i in 2:entities[k]+extra){
      delays[i,j] <- max(delays[i-1,j]+services[i-1,j]-arrivals[i,j],0)
    }
  }
  X_bar <- colMeans(delays[(1+extra):(entities[k]+extra),])
  X_bar_sd <- colSds(delays[(1+extra):(entities[k]+extra),])
  X_bar_95CI_UB <- X_bar + qnorm(0.975,0,1)*X_bar_sd/sqrt(entities[k])
  X_bar_95CI_LB <- X_bar - qnorm(0.975,0,1)*X_bar_sd/sqrt(entities[k])
  X_bar_95CI_HL <- qnorm(0.975,0,1)*X_bar_sd/sqrt(entities[k])
}

X_bar
X_bar_95CI_HL
steady_state_mean <- 3.6
contain_true_mean <- sum(X_bar_95CI_LB <= steady_state_mean & X_bar_95CI_UB >= steady_state_mean)/replications
contain_true_mean

X_df <- data.frame(replication = c(1:replications),
                   mean = X_bar,
                   LB = X_bar_95CI_LB,
                   UB = X_bar_95CI_UB)

png("ISyE_6832_Homework_1_Problem_4d.png", width = 350, height = 350)
ggplot(X_df, aes(x = replication, y = mean)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymax = UB, ymin = LB)) +
  geom_hline(yintercept= steady_state_mean, color="red") +
  geom_text(aes(38.5, steady_state_mean, label = "Steady-State Mean", color="red", vjust = -1)) +
  theme(legend.position="none") +
  labs(x = "Replication", y = "Mean Entity Delay (min)")
dev.off()
