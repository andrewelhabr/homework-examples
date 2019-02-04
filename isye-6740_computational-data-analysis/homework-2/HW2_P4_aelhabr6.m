%% Problem 4 - Email Spam Filter Via Discriminant Analysis

clear all
close all

% Loading in mat file necessary for this problem.
load('spamdata.mat')

% Setting our design matrix and response vector.
X_train = training_set;
y_train = training_set_label;
X_test = testing_set;
y_test = testing_set_label;

% Preprocessing and initialization for the algorithm.
[n_train, ~] = size(X_train);
[n_test, d] = size(X_test);



% Part (a) Gaussian Discriminant Analysis %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Estimating the means for the conditional distributions and the parameter
% for the Y distribution.
mu_hat_0 = X_train'*(ones(n_train,1) - y_train)/(n_train - y_train'*ones(n_train,1));
mu_hat_1 = X_train'*y_train/(y_train'*ones(n_train,1));
p_hat = y_train'*ones(n_train,1)/n_train;

% Estimating the covariance matrix.
dummy_matrix = zeros(d,d);
for i = 1:n_train
    dummy_matrix = dummy_matrix + (X_train(i,:)' - mu_hat_0 - y_train(i)*(mu_hat_1 - mu_hat_0))*(X_train(i,:)' - mu_hat_0 - y_train(i)*(mu_hat_1 - mu_hat_0))';
end
Sigma_hat = dummy_matrix/n_train;
%Sigma_hat = (1/n_train)*(X_train' - repmat(mu_hat_0,1,n_train))*(X_train' - repmat(mu_hat_1,1,n_train))';
Sigma_hat = Sigma_hat + 0.01*eye(d);

% Computing the predictions and error for the training set.
P_xy_train = zeros(n_train,2);
P_xy_train_v2 = zeros(n_train,1);
for i = 1:n_train
    P_xy_train(i,1) = (1/((2*pi)^(d/2)*det(Sigma_hat)^(1/2)))*exp(-(1/2)*(X_train(i,:)' - mu_hat_0)'*inv(Sigma_hat)*(X_train(i,:)' - mu_hat_0))*(1 - p_hat); 
    P_xy_train(i,2) = (1/((2*pi)^(d/2)*det(Sigma_hat)^(1/2)))*exp(-(1/2)*(X_train(i,:)' - mu_hat_1)'*inv(Sigma_hat)*(X_train(i,:)' - mu_hat_1))*p_hat; 
    P_xy_train_v2(i) = P_xy_train(i,2)/(P_xy_train(i,1) + P_xy_train(i,2));
end

[~, I_train] = max(P_xy_train');

predictions_train = I_train' - ones(n_train,1);
error_train = 1 - sum(predictions_train == y_train)/n_train;
%disp(error_train);
predictions_train_v2 = round(P_xy_train_v2);
error_train_v2 = 1 - sum(predictions_train_v2 == y_train)/n_train;
%disp(error_train_v2);

% Computing the predictions and error for the testing set.
P_xy_test = zeros(n_test,2);
P_xy_test_v2 = zeros(n_test,1);
for i = 1:n_test
    P_xy_test(i,1) = (1/((2*pi)^(d/2)*det(Sigma_hat)^(1/2)))*exp(-(1/2)*(X_test(i,:)' - mu_hat_0)'*inv(Sigma_hat)*(X_test(i,:)' - mu_hat_0))*(1 - p_hat); 
    P_xy_test(i,2) = (1/((2*pi)^(d/2)*det(Sigma_hat)^(1/2)))*exp(-(1/2)*(X_test(i,:)' - mu_hat_1)'*inv(Sigma_hat)*(X_test(i,:)' - mu_hat_1))*p_hat; 
    P_xy_test_v2(i) = P_xy_test(i,2)/(P_xy_test(i,1) + P_xy_test(i,2));
end
[~,I_test] = max(P_xy_test');

predictions_test = I_test' - ones(n_test,1);
error_test = 1 - sum(predictions_test == y_test)/n_test;
%disp(error_test);
predictions_test_v2 = round(P_xy_test_v2);
error_test_v2 = 1 - sum(predictions_test_v2 == y_test)/n_test;
%disp(error_test_v2);

% Displaying the output.
sentence_GDA = ['For GDA, the training error is ', num2str(error_train),', and the testing error is ', num2str(error_test), '.'];
disp(sentence_GDA);



% Part (b) Naive Bayes - Gaussian Discriminant Analysis %%%%%%%%%%%%%%%%%%%

% Estimating the covariance matrix.
sigma_hat_NBGDA = zeros(d,1);
for j = 1:d
    dummmy_sum = 0;
    for i = 1:n_train
        dummmy_sum = dummmy_sum + (X_train(i,j) - mu_hat_0(j) - y_train(i)*(mu_hat_1(j) - mu_hat_0(j)))^2;
    end
    sigma_hat_NBGDA(j) = (1/n_train)*dummmy_sum;
end
Sigma_hat_NBGDA = diag(sigma_hat_NBGDA);

% Computing the predictions and error for the training set.
P_xy_NBGDA_train = zeros(n_train,1);
for i = 1:n_train
    dummy_product_y_0 = 1;
    dummy_product_y_1 = 1;
    for j = 1:d
        dummy_product_y_0 = dummy_product_y_0*(1/(2*pi*sigma_hat_NBGDA(j))^(1/2))*exp(-(X_train(i,j) - mu_hat_0(j))^2/(2*sigma_hat_NBGDA(j)));
        dummy_product_y_1 = dummy_product_y_1*(1/(2*pi*sigma_hat_NBGDA(j))^(1/2))*exp(-(X_train(i,j) - mu_hat_1(j))^2/(2*sigma_hat_NBGDA(j)));
    end
    P_xy_NBGDA_train(i) = dummy_product_y_1*p_hat/(dummy_product_y_0*(1 - p_hat) + dummy_product_y_1*p_hat);
end

predictions_NGBDA_train = round(P_xy_NBGDA_train);
error_NGBDA_train = 1 - sum(predictions_NGBDA_train == y_train)/n_train;
%disp(error_NGBDA_train);

% Computing the predictions and error for the testing set.
P_xy_NBGDA_test = zeros(n_test,1);
for i = 1:n_test
    dummy_product_y_0 = 1;
    dummy_product_y_1 = 1;
    for j = 1:d
        dummy_product_y_0 = dummy_product_y_0*(1/(2*pi*sigma_hat_NBGDA(j))^(1/2))*exp(-(X_test(i,j) - mu_hat_0(j))^2/(2*sigma_hat_NBGDA(j)));
        dummy_product_y_1 = dummy_product_y_1*(1/(2*pi*sigma_hat_NBGDA(j))^(1/2))*exp(-(X_test(i,j) - mu_hat_1(j))^2/(2*sigma_hat_NBGDA(j)));
    end
    P_xy_NBGDA_test(i) = dummy_product_y_1*p_hat/(dummy_product_y_0*(1 - p_hat) + dummy_product_y_1*p_hat);
end

predictions_NGBDA_test = round(P_xy_NBGDA_test);
error_NGBDA_test = 1 - sum(predictions_NGBDA_test == y_test)/n_test;
%disp(error_NGBDA_test);

% Displaying the output.
sentence_NGBDA = ['For NB-GDA, the training error is ', num2str(error_NGBDA_train),', and the testing error is ', num2str(error_NGBDA_test), '.'];
disp(sentence_NGBDA);



% Part (c) Naive Bayes - Bernoulli Discriminant Analysis %%%%%%%%%%%%%%%%%%

% Estimating the paramters for the conditional distributions.
X_indicator_train = zeros(n_train,d);
for i = 1:n_train
    for j = 1:d
        if X_train(i,j) > 0
            X_indicator_train(i,j) = 1;
        else
            X_indicator_train(i,j) = 0;
        end
    end
end
gamma_hat_0 = X_indicator_train'*(ones(n_train,1) - y_train)/(n_train - y_train'*ones(n_train,1));
gamma_hat_1 = X_indicator_train'*y_train/(y_train'*ones(n_train,1));

% Replacing any 0 value of gamma with a small value.
replacement = 0.01;
for j = 1:d
    if gamma_hat_0(j) == 0
        gamma_hat_0(j) = replacement;
    else
        gamma_hat_0(j) = gamma_hat_0(j);
    end
        if gamma_hat_1(j) == 0
        gamma_hat_1(j) = 0.01;
    else
        gamma_hat_1(j) = gamma_hat_1(j);
    end
end

% Computing the predictions and error for the training set.
P_xy_NBBDA_train = zeros(n_train,1);
for i = 1:n_train
    dummy_product_y_0 = 1;
    dummy_product_y_1 = 1;
    for j = 1:d
        dummy_product_y_0 = dummy_product_y_0*gamma_hat_0(j)^(X_indicator_train(i,j))*(1 - gamma_hat_0(j))^(1 - X_indicator_train(i,j));
        dummy_product_y_1 = dummy_product_y_1*gamma_hat_1(j)^(X_indicator_train(i,j))*(1 - gamma_hat_1(j))^(1 - X_indicator_train(i,j));
    end
    P_xy_NBBDA_train(i) = dummy_product_y_1*p_hat/(dummy_product_y_0*(1 - p_hat) + dummy_product_y_1*p_hat);
end

predictions_NBBDA_train = round(P_xy_NBBDA_train);
error_NBBDA_train = 1 - sum(predictions_NBBDA_train == y_train)/n_train;
%disp(error_NBBDA_train);

% Initialiazing for the predictions that we will make for the testing set.
X_indicator_test = zeros(n_test,d);
for i = 1:n_test
    for j = 1:d
        if X_test(i,j) > 0
            X_indicator_test(i,j) = 1;
        else
            X_indicator_test(i,j) = 0;
        end
    end
end

% Computing the predictions and error for the testing set.
P_xy_NBBDA_test = zeros(n_test,1);
for i = 1:n_test
    dummy_product_y_0 = 1;
    dummy_product_y_1 = 1;
    for j = 1:d
        dummy_product_y_0 = dummy_product_y_0*gamma_hat_0(j)^(X_indicator_test(i,j))*(1 - gamma_hat_0(j))^(1 - X_indicator_test(i,j));
        dummy_product_y_1 = dummy_product_y_1*gamma_hat_1(j)^(X_indicator_test(i,j))*(1 - gamma_hat_1(j))^(1 - X_indicator_test(i,j));
    end
    P_xy_NBBDA_test(i) = dummy_product_y_1*p_hat/(dummy_product_y_0*(1 - p_hat) + dummy_product_y_1*p_hat);
end

predictions_NBBDA_test = round(P_xy_NBBDA_test);
error_NBBDA_test = 1 - sum(predictions_NBBDA_test == y_test)/n_test;
%disp(error_NBBDA_test);

% Displaying the output.
sentence_NBBDA = ['For NB-BDA, the training error is ', num2str(error_NBBDA_train),', and the testing error is ', num2str(error_NBBDA_test), '.'];
disp(sentence_NBBDA);



% Part (d) Quadratic Discriminant Analysis %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
% Estimating the covaraince matrix for the conditional distributions.
dummy_matrix_0 = zeros(d,d);
dummy_matrix_1 = zeros(d,d);
for i = 1:n_train
    if y_train(i) == 0
        dummy_matrix_0 = dummy_matrix_0 + (X_train(i,:)' - mu_hat_0)*(X_train(i,:)' - mu_hat_0)';
    else
        dummy_matrix_1 = dummy_matrix_1 + (X_train(i,:)' - mu_hat_1)*(X_train(i,:)' - mu_hat_1)';
    end
end
Sigma_hat_QDA_0 = dummy_matrix_0/(n_train - y_train'*ones(n_train,1));
Sigma_hat_QDA_0 = Sigma_hat_QDA_0 + 0.01*eye(d);
Sigma_hat_QDA_1 = dummy_matrix_1/(y_train'*ones(n_train,1));
Sigma_hat_QDA_1 = Sigma_hat_QDA_1 + 0.01*eye(d);

% Computing the predictions and error for the training set.
P_xy_QDA_train = zeros(n_train,2);
P_xy_QDA_train_v2 = zeros(n_train,1);
for i = 1:n_train
    P_xy_QDA_train(i,1) = (1/((2*pi)^(d/2)*det(Sigma_hat_QDA_0)^(1/2)))*exp(-(1/2)*(X_train(i,:)' - mu_hat_0)'*inv(Sigma_hat_QDA_0)*(X_train(i,:)' - mu_hat_0))*(1 - p_hat); 
    P_xy_QDA_train(i,2) = (1/((2*pi)^(d/2)*det(Sigma_hat_QDA_1)^(1/2)))*exp(-(1/2)*(X_train(i,:)' - mu_hat_1)'*inv(Sigma_hat_QDA_1)*(X_train(i,:)' - mu_hat_1))*p_hat; 
    P_xy_QDA_train_v2(i) = P_xy_QDA_train(i,2)/(P_xy_QDA_train(i,1) + P_xy_QDA_train(i,2));
end
[~, I_QDA_train] = max(P_xy_QDA_train');

predictions_QDA_train = I_QDA_train' - ones(n_train,1);
error_QDA_train = 1 - sum(predictions_QDA_train == y_train)/n_train;
%disp(error_QDA_train);
predictions_QDA_train_v2 = round(P_xy_QDA_train_v2);
error_QDA_train_v2 = 1 - sum(predictions_QDA_train_v2 == y_train)/n_train;
%disp(error_QDA_train_v2);

% Computing the predictions and error for the testing set.
P_xy_QDA_test = zeros(n_test,2);
P_xy_QDA_test_v2 = zeros(n_test,1);
for i = 1:n_test
    P_xy_QDA_test(i,1) = (1/((2*pi)^(d/2)*det(Sigma_hat_QDA_0)^(1/2)))*exp(-(1/2)*(X_test(i,:)' - mu_hat_0)'*inv(Sigma_hat_QDA_0)*(X_test(i,:)' - mu_hat_0))*(1 - p_hat); 
    P_xy_QDA_test(i,2) = (1/((2*pi)^(d/2)*det(Sigma_hat_QDA_1)^(1/2)))*exp(-(1/2)*(X_test(i,:)' - mu_hat_1)'*inv(Sigma_hat_QDA_1)*(X_test(i,:)' - mu_hat_1))*p_hat; 
    P_xy_QDA_test_v2(i) = P_xy_QDA_test(i,2)/(P_xy_QDA_test(i,1) + P_xy_QDA_test(i,2));
end
[~,I_QDA_test] = max(P_xy_QDA_test');

predictions_QDA_test = I_QDA_test' - ones(n_test,1);
error_QDA_test = 1 - sum(predictions_QDA_test == y_test)/n_test;
%disp(error_QDA_test);
predictions_QDA_test_v2 = round(P_xy_QDA_test_v2);
error_QDA_test_v2 = 1 - sum(predictions_QDA_test_v2 == y_test)/n_test;
%disp(error_QDA_test_v2);

% Displaying the output.
sentence_QDA = ['For QDA, the training error is ', num2str(error_QDA_train),', and the testing error is ', num2str(error_QDA_test), '.'];
disp(sentence_QDA);
