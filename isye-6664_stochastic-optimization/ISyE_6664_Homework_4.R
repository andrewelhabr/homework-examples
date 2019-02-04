
# Andrew ElHabr
# ISyE 6664
# 9/29/17

# Homework 4 -------------------------------------------------------------------

# Problem 1 --------------------------------------------------------------------

rm(list = ls())

################################################################################

# System Inputs

S <- c(1, 2, 3)
# A is equal to max_s |A_s|
# Note that this should be made to be more robust to make the script better.
A <- 2
R <- matrix(c(0, 0, 3, 4, 4, -50),
            nrow = 3,
            ncol = 2,
            byrow = TRUE)
P <- array(0, c(2, 3, 3))
P[, , 1] <- matrix(c(1/2, 1/2, 0, 2/3, 0, 1/3),
                   nrow = 2,
                   ncol = 3,
                   byrow = TRUE)
P[, , 2] <- matrix(c(1, 0, 0, 0, 0, 1),
                   nrow = 2,
                   ncol = 3,
                   byrow = TRUE)
P[, , 3] <- matrix(c(1, 0, 0, 0, 0, 0),
                   nrow = 2,
                   ncol = 3,
                   byrow = TRUE)
alpha <- 0.8

################################################################################

# Arbitrary Inputs

epsilon <- 0.001
max_iter <- 100
V_current <- c(0, 0, 0)

################################################################################

# Initialization

V <- array(0, c(3, max_iter))
d <- array(0, c(3, max_iter))
d_epsilon <- array(0, c(3, 1))
B <- array(0, c(length(S), A))
threshold <- TRUE
i <- 1

################################################################################

# Functions

# Bellman equation for each s in S and a in A_s (not calculating an optimal
# value).
Bellman <- function(s, a, R, P, V_current, alpha) {
  B[s, a] <- R[s, a] + alpha * sum(P[a, , s] * V_current)
  B[s, a]
}

# Value Iteration algorithm.
Value_Iteration <-
  function(S,
           A,
           R,
           P,
           alpha,
           epsilon = 0.001,
           max_iter = 100,
           V_current = c(0, 0, 0),
           print_messages = TRUE) {
    while (i <= max_iter & threshold) {
      # Finding optimal value and action for each state for iteration i.
      for (s in 1:length(S)) {
        for (a in 1:A) {
          B[s, a] <- Bellman(s, a, R, P, V_current, alpha)
        }
        
        V[s, i] <- max(B[s, ])
        d[s, i] <- which.max(B[s, ])
      }
      
      # Checking if supnorm threshold has been violated yet.
      if (max(abs(V[, i] - V_current)) > epsilon * (1 - alpha) / (2 * alpha)) {
        threshold <- TRUE
      } else {
        threshold <- FALSE
      }
      
      # Printing last value even if print_messages is set to FALSE
      if (print_messages == TRUE |
          (i >= max_iter) | (threshold == FALSE)) {
        cat("Iteration = ", i)
        cat("\n")
        cat("Value = ", round(V[, i], 2))
        cat("\n")
        cat("d = ", d[, i])
        cat("\n")
      }
      
      V_current <- V[, i]
      i <- i + 1
    }
    
    # Storing the epsilon-optimal decision rule.
    for (s in 1:length(S)) {
      d_epsilon[s] <- which.max(B[s, ])
    }
    
    V <- t(V[,1:(i-1)])
    d <- t(d[, 1:(i-1)])
    
    invisible(list(V = V, d = d, d_epsilon = d_epsilon))
  }

################################################################################

# Run Value Iteration with given and selected system parameters.

output <- Value_Iteration(S, A, R, P, alpha, epsilon, max_iter, V_current)
output

## Iteration =  45
## V_epsilon =  8.37 12.56 10.70
## d_epsilon =  1 2 1
