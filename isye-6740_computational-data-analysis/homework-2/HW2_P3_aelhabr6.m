%% Problem 3 - Stochastic Gradient Descent for OLS

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
step_size = [0.1/L; 1.7/L; 1/L; 0.01/L];
iterations = 1000;
passes = 20;

% Run the Stochastic Gradient Descent Algorithm for each step size.
for i = 1:4
    
    % Initialization for each step size.
    gradient = zeros(p,iterations*passes);
    beta = zeros(p,iterations*passes+1);
    XX = repmat(X,passes,1);
    yy = repmat(y,passes,1);
    k = 1;
    
    while k <= iterations*passes
        gradient(:,k) = XX(k,:)'*(XX(k,:)*beta(:,k) - yy(k));
        beta(:,k+1) = beta(:,k) - step_size(i)*gradient(:,k);
        k = k + 1;
    end

    % Cuting off the extra set of beta coefficients that was computed.
    %beta = beta(:,1:end-1);
    beta = beta(:,2:end);
    
    % Storing the betas for each pass.
    beta_pass = beta(:,iterations:iterations:end);
    %beta_pass = beta;
    
    % Setting the length of the x-axis for all plots.
    x_axis_length = passes;
    %x_axis_length = passes*iterations;

    % Part (a)
    % Finding the objective function value for the beta for each pass.
    f_beta_pass = (1/(2*n))*sum((repmat(y,1,x_axis_length) - X*beta_pass).^2);

    % Calculating the OLS estimator.
    beta_hat = inv(X'*X)*X'*y;
    
    % Finding the objective function value for the OLS estimator.
    f_beta_hat = (1/(2*n))*sum((repmat(y,1,x_axis_length) - X*repmat(beta_hat,1,x_axis_length)).^2);
    
    % Finding the logarithmic difference between the objective function
    % values for the beta for each pass and for the OLS estimator.
    log_f_diff_pass = log(f_beta_pass' - f_beta_hat');

    % Plotting the logarithmic difference between the objective function
    % values for the beta for each pass and for the OLS estimator.
    figure(3*i-2);
    plot(1:x_axis_length, log_f_diff_pass)
    xlabel('Pass')
    ylabel('$$log(f(\beta^k) - f(\hat{\beta}))$$', 'Interpreter', 'Latex')

    pause

    % Part (b)
    % Finding the squared L2 norm for the difference of the beta for each
    % pass and the true beta.
    l2_beta_diff_true = sum((repmat(True_Beta',1,x_axis_length) - beta_pass).^2);

    % Plotting the squared L2 norm for the difference of the beta for each
    % pass and the true beta.
    figure(3*i-1);
    plot(1:x_axis_length, l2_beta_diff_true)
    xlabel('Pass')
    ylabel('$$||\beta^* - \beta^{(k)}||_2^2$$', 'Interpreter', 'Latex')

    pause

    % Part (c)
    % Finding the squared L2 norm for the difference of the beta for each
    % pass and the OLS estimator.
    l2_beta_diff_hat = sum((repmat(beta_hat,1,x_axis_length) - beta_pass).^2);

    % Plotting the squared L2 norm for the difference of the beta for each
    % iteration and the OLS estimator.
    figure(3*i);
    plot(1:x_axis_length, l2_beta_diff_hat)
    xlabel('Pass')
    ylabel('$$||\hat{\beta} - \beta^{(k)}||_2^2$$', 'Interpreter', 'Latex')

    pause
       
end
