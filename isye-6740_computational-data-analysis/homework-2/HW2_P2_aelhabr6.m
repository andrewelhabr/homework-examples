%% Problem 2 - Gradient Descent for OLS

clear all
close all

% Loading in csv files necessary for this problem.
load('MLR.csv')
load('True_Beta.csv')

% Setting our design matrix and response vector.
X = MLR(:,1:30);
y = MLR(:,31);

% Preprocessing and initialization for the algorithm.
[n, p] = size(X);
L = max(eig(X'*X/n));
step_size = 1/L;
iterations = 1000;
gradient = zeros(p,iterations);
beta = zeros(p,iterations+1);
k = 1;

% Run the Gradient Descent Algorithm.
while k <= iterations
    gradient(:,k) = (1/n)*X'*(X*beta(:,k) - y);
    beta(:,k+1) = beta(:,k) - step_size*gradient(:,k);
    k = k + 1;
end

% Cuting off the extra set of beta coefficients that was computed.
%beta = beta(:,2:end);
beta = beta(:,1:end-1);

% Part (a)
% Finding the objective function value for the beta for each iteration.
f_beta = (1/(2*n))*sum((repmat(y,1,iterations) - X*beta).^2);

% Calculating the OLS estimator.
beta_hat = inv(X'*X)*X'*y;

% Finding the objective function value for the OLS estimator.
f_beta_hat = (1/(2*n))*sum((repmat(y,1,iterations) - X*repmat(beta_hat,1,iterations)).^2);

% Finding the logarithmic difference between the objective function values
% for the beta for each iteration and for the OLS estimator.
log_f_diff = log(f_beta' - f_beta_hat');

% Plotting the logarithmic difference between the objective function values
% for the beta for each iteration and for the OLS estimator.
figure(1);
plot(1:iterations, log_f_diff)
xlabel('Iteration')
ylabel('$$log(f(\beta^k) - f(\hat{\beta}))$$', 'Interpreter', 'Latex')

pause

% Part (b)
% Finding the squared L2 norm for the difference of the beta for each
% iteration and the true beta.
l2_beta_diff_true = sum((beta - repmat(True_Beta',1,iterations)).^2);

% Plotting the squared L2 norm for the difference of the beta for each
% iteration and the true beta.
figure(2);
plot(1:iterations, l2_beta_diff_true)
xlabel('Iteration')
ylabel('$$||\beta^{(k)} - \beta^*||_2^2$$', 'Interpreter', 'Latex')

pause

% Part (c)
% Finding the squared L2 norm for the difference of the beta for each
% iteration and the OLS estimator.
l2_beta_diff_hat = sum((beta - repmat(beta_hat,1,iterations)).^2);

% Plotting the squared L2 norm for the difference of the beta for each
% iteration and the OLS estimator.
figure(3);
plot(1:iterations, l2_beta_diff_hat)
xlabel('Iteration')
ylabel('$$||\beta^{(k)} - \hat{\beta}||_2^2$$', 'Interpreter', 'Latex')

% beta^(k) converges to beta_hat faster than beta_true.
