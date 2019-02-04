%In the train, valid and test data, remember the last column is label. Plot
%your figures in the defination of functions.
load('train-greedy.mat')
load('valid-greedy.mat')
load('test-greedy.mat')
load('true-beta.mat')

forward_greedy(train,validation,test,beta);
pause;
ridge_reg(train,validation,test,beta);
pause;
lasso_wrapper(train,validation,test,beta);
pause;
refined_est(train,validation,test,beta);

%Part a, implement the forward greedy algorithm
%Input: train data, validation data and test data
%Output: number of optimized features, optimal beta, estimation error and 
%        prediction error.
%        Plot your errors as iteration changes
function forward_greedy(train,validation,test,beta)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%         Your Code Starts Here         %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Preprocessing the inputs.
X = train(:,1:end-1);
y = train(:,end);
X_tilde = validation(:,1:end-1);
y_tilde = validation(:,end);
X_bar = test(:,1:end-1);
y_bar = test(:,end);
[m,n] = size(X);

% Preprocessing the greedy algorithm.
K = 15;
M = 1000000;
A = [];
beta_k = zeros(n,K+1);

% Implementing the greedy algorithm.
for k = 1:K
    i_s = zeros(m,1);
    i_k = [];
    sparse_matrix = zeros(m,n);
    for j = 1:n
        i_s(j) = abs(train(:,j)'*(X*beta_k(:,k) - y));
    end
    % Storing previously selected i values as very small numbers so that
    % they do not get picked again.
    i_s(A) = -M;
    [i_s_max, i_k] = max(i_s);
    A = [A; i_k];
    for r = 1:length(A)
        sparse_matrix(:,A(r)) = X(:,A(r));
    end
    % Perturbing what we are inversing to avoid errors with MATLAB.
    beta_k(:,k+1) = inv(sparse_matrix'*sparse_matrix + 0.01*eye(n))*sparse_matrix'*y;
end
beta_k = beta_k(:,2:end);

% Computing all of the errors.
validation_error = zeros(K,1);
estimation_error = zeros(K,1);
prediction_error = zeros(K,1);
for k = 1:K
    validation_error(k) = norm(y_tilde - X_tilde*beta_k(:,k),2)^2;
    estimation_error(k) = norm(beta_k(:,k) - beta,2);
    prediction_error(k) = (1/length(y_bar))*norm(y_bar - X_bar*beta_k(:,k),2)^2;
end

% Finding the optimal beta using the validation error.
[val,ind] = min(validation_error);
opt_beta = beta_k(:,ind);
num_of_opt_features = sum(opt_beta ~= 0);
opt_beta
num_of_opt_features

% Plotting all of the errors as a function of the number of iterations.
figure(1)
plot(1:K,validation_error)
title('Validation Error vs. Number of Iterations')
%saveas(gcf,'HW4_P3_aelhabr6_Figure_1.png')
figure(2)
plot(1:K,estimation_error)
title('Estimation Error vs. Number of Iterations')
%saveas(gcf,'HW4_P3_aelhabr6_Figure_2.png')
figure(3)
plot(1:K,prediction_error)
title('Prediction Error vs. Number of Iterations')
%saveas(gcf,'HW4_P3_aelhabr6_Figure_3.png')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%         Your Code Ends Here         %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end


%Part b, implement the ridge regreesion estimator
%Input: train data, validation data and test data
%Output: optimal beta, optimal lambda, estimation error and prediction error.
%        Plot your errors as iteration changes
function ridge_reg(train,validation,test,beta)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%        Your Code Starts Here         %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Preprocessing the inputs.
X = train(:,1:end-1);
y = train(:,end);
X_tilde = validation(:,1:end-1);
y_tilde = validation(:,end);
X_bar = test(:,1:end-1);
y_bar = test(:,end);
[m,n] = size(X);

% Preprocessing for the finding the ridge regression estimator.
lambda = [0.0125; 0.025; 0.05; 0.1; 0.2];
beta_ridge = zeros(n,length(lambda));
validation_ridge_error = zeros(length(lambda),1);
estimation_ridge_error = zeros(length(lambda),1);
prediction_ridge_error = zeros(length(lambda),1);

% Finding the ridge regression estimator and computing all of the errors.
for i = 1:length(lambda)
    beta_ridge(:,i) = inv(X'*X + 2*n*lambda(i)*eye(n))*X'*y;
    validation_ridge_error(i) = norm(y_tilde - X_tilde*beta_ridge(:,i),2)^2;
    estimation_ridge_error(i) = norm(beta_ridge(:,i) - beta,2);
    prediction_ridge_error(i) = (1/length(y_bar))*norm(y_bar - X_bar*beta_ridge(:,i),2)^2;
end

% Finding the optimal ridge regression beta using the validation error.
[val,ind] = min(validation_ridge_error);
opt_beta_ridge = beta_ridge(:,ind);
num_of_opt_ridge_features = sum(opt_beta_ridge ~= 0);
%opt_beta_ridge
%num_of_opt_ridge_features

% Plotting all of the errors as a function of the regulation parameter.
figure(4)
plot(lambda,validation_ridge_error)
title('Validation Error vs. Lambda')
%saveas(gcf,'HW4_P3_aelhabr6_Figure_4.png')
figure(5)
plot(lambda,estimation_ridge_error)
title('Estimation Error vs. Lambda')
%saveas(gcf,'HW4_P3_aelhabr6_Figure_5.png')
figure(6)
plot(lambda,prediction_ridge_error)
title('Prediction Error vs. Lambda')
%saveas(gcf,'HW4_P3_aelhabr6_Figure_6.png')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%         Your Code Ends Here         %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end

%Part c, use lasso package to get optimized parameter.
%Input: train data, validation data and test data.
%Output: optimal beta, optimal lambda, estimation error and prediction
%        error. 
function lasso_wrapper(train,validation,test,beta)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%         Your Code Starts Here         %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Preprocessing the inputs.
X = train(:,1:end-1);
y = train(:,end);
X_tilde = validation(:,1:end-1);
y_tilde = validation(:,end);
X_bar = test(:,1:end-1);
y_bar = test(:,end);
[m,n] = size(X);

% Running MATLAB's lasso function.
[B,S] = lasso(X,y);
B_intercept = [S.Intercept; B];

% Preprocessing for the errors.
beta_w_intercept = [0; beta];
X_tilde_w_intercept = [ones(1,length(y_tilde)); X_tilde']';
X_bar_w_intercept = [ones(1,length(y_bar)); X_bar']';
validation_lasso_error = zeros(length(S.Lambda),1);
estimation_lasso_error = zeros(length(S.Lambda),1);
prediction_lasso_error = zeros(length(S.Lambda),1);

% Computing all of the errors.
for k = 1:length(S.Lambda)
    validation_lasso_error(k) = norm(y_tilde - X_tilde_w_intercept*B_intercept(:,k),2)^2;
    estimation_lasso_error(k) = norm(B_intercept(:,k) - beta_w_intercept,2);
    prediction_lasso_error(k) = (1/length(y_bar))*norm(y_bar - X_bar_w_intercept*B_intercept(:,k),2)^2;
end

% Finding the optimal lasso beta and lambda using the validation error.
[val,ind] = min(validation_lasso_error);
opt_beta_lasso = B_intercept(:,ind);
opt_lambda = S.Lambda(ind);
num_of_opt_lasso_features = sum(opt_beta_lasso ~= 0);
%opt_beta_lasso
%opt_lambda
%num_of_opt_lasso_features

% Plotting all of the errors as a function of the regulation parameter.
figure(7)
plot(S.Lambda,validation_lasso_error)
title('Validation Error vs. Lambda')
%saveas(gcf,'HW4_P3_aelhabr6_Figure_7.png')
figure(8)
plot(S.Lambda,estimation_lasso_error)
title('Estimation Error vs. Lambda')
%saveas(gcf,'HW4_P3_aelhabr6_Figure_8.png')
figure(9)
plot(S.Lambda,prediction_lasso_error)
title('Prediction Error vs. Lambda')
%saveas(gcf,'HW4_P3_aelhabr6_Figure_9.png')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%         Your Code Ends Here         %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end




%Part d, get your refined optimizer.
%Output: refined beta and estimation error.
function refined_est(train,validation,test,beta)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%         Your Code Starts Here         %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Preprocssing the inputs.
X = train(:,1:end-1);
y = train(:,end);
X_tilde = validation(:,1:end-1);
y_tilde = validation(:,end);
X_bar = test(:,1:end-1);
y_bar = test(:,end);
[m,n] = size(X);

% Finding the optimal beta and lambda according to lasso.
[B,S] = lasso(X,y);
B_intercept = [S.Intercept; B];
beta_w_intercept = [0; beta];
X_tilde_w_intercept = [ones(1,length(y_tilde)); X_tilde']';
validation_lasso_error = zeros(length(S.Lambda),1);
for k = 1:length(S.Lambda)
    validation_lasso_error(k) = norm(y_tilde - X_tilde_w_intercept*B_intercept(:,k),2)^2;
end
[val,ind] = min(validation_lasso_error);

% Finding the refit beta based off the optimal beta according to lasso.
sparse_matrix_2 = zeros(m,n);
B_opt = B_intercept(:,ind);
for i = 2:n
    if B_opt(i)~= 0
        sparse_matrix_2(:,i-1) = X(:,i-1);
    else
        ;
    end
end

% Computing the refit beta and refit and lasso estimation errors.
beta_refit = inv(sparse_matrix_2'*sparse_matrix_2 + 0.01*eye(n))*sparse_matrix_2'*y;
%sum(beta_refit ~= 0)
%sum(B_opt ~= 0)
refit_estimation_error = norm(beta_refit - beta,2);
lasso_estimation_error = norm(B_opt(2:end) - beta,2);
refit_estimation_error
lasso_estimation_error

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%         Your Code Ends Here         %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
end

