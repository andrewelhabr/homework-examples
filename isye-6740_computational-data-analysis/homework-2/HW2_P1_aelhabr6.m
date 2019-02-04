%% Problem 1 - Ordinary Least Squares Regression

clear all
close all

% Loading in csv files necessary for this problem.
load('MLR.csv')
load('True_Beta.csv')

% Setting our design matrix and response vector.
X = MLR(:,1:30);
y = MLR(:,31);

% Calculating the OLS estimator.
beta_hat = inv(X'*X)*X'*y;

% Computing and printing the squared error.
squared_error = norm(beta_hat - True_Beta')^2;
%disp(squared_error);
% 0.1991

% Displaying the output.
sentence = ['The squared error is ', num2str(squared_error), '.'];
disp(sentence);

