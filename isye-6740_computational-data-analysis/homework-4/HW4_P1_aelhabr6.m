%In this problem, you need to implement KNN algorithm.
%The inputs: data from train-KNN and test-KNN
%            In the datasets, the first column is your horizontal coordinate(x axis), 
%            the second column is your vertical coordinate(y axis), 
%            the third column represents the data label.       
%
%
%The outputs: plot of original data,
%             plots of your classification results after implementing KNN
%             algorithm with K= 1, 2, 5, 20,
%             error rate of classification results with K= 1, 2, 5, 20.
%              
%Load the training data and plot it.
%To better recognize your results during grading process, here you are
%required to use "o" to represent label 1, use "+" to represent label 2, 
%and use "*" to represent label 3.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%         Your Code Starts Here         %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load('train-KNN.mat')

% Plotting the training data.
figure(1);
scatter(a(1:50,1),a(1:50,2),'r+')
hold on
scatter(a(51:100,1),a(51:100,2),'go')
hold on
scatter(a(101:150,1),a(101:150,2),'b*')
hold off
title('Training Set')
legend('label 1','label 2','label 3')
%saveas(gcf,'HW4_P1_aelhabr6_Figure_1.png')

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%         Your Code Ends Here         %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%
%Implement KNN algorithm with your testing data(remember you need to consider
%both l1 distance and l2 distance) and save the classification results in
%test_label_l1 and test_label_l2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%         Your Code Starts Here         %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
load('test-KNN.mat')

% Preprocessing.
test_label_l1 = [];
test_label_l2 = [];
K = [1 2 5 20];
A_matrix = a(:,1:2);
B_matrix = zeros(length(a),2,length(B));
dist_l1 = zeros(length(B),length(a));
dist_l2 = zeros(length(B),length(a));
for i = 1:length(B)
    B_matrix(:,:,i) = repmat(B(i,:),length(a),1);
end

% Finding the L1 and L2 distances between each testing and training point.
for i = 1:length(B)
    for j = 1:length(a)
        dist_l1(i,j) = norm(A_matrix(j,:) - B_matrix(j,:,i),1);
        dist_l2(i,j) = norm(A_matrix(j,:) - B_matrix(j,:,i),2);
    end
end

% Classifying each testing point by its k nearest neighbors using both L1
% and L2 distances for each k.
class_l1 = zeros(length(K),length(B));
class_l2 = zeros(length(K),length(B));
for j = 1:length(K)
    labels_l1 = zeros(length(B), K(j));
    labels_l2 = zeros(length(B), K(j));
    
    % Utilizing user-made min_k function, which is located at the bottom of
    % this script.
    for r = 1:length(B)
        indices_l1 = min_k(dist_l1(r,:),K(j));
        labels_l1(r,:) = ceil(indices_l1/50)';
        indices_l2 = min_k(dist_l2(r,:),K(j));
        labels_l2(r,:) = ceil(indices_l2/50)';
    end
    if K(j) == 1
        class_l1(j,:) = labels_l1';
        class_l2(j,:) = labels_l2';
    else
        class_l1(j,:) = mode(labels_l1');
        class_l2(j,:) = mode(labels_l2');
    end
end
test_label_l1 = class_l1;
test_label_l2 = class_l2;

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%         Your Code Ends Here         %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%Plot your classification results (i.e. test_label_l1 & test_label_l2), 
%See Lecture 2 page 43/47 as a reference.
%Remember to indicate your distance type and K values
%To better recognize your results during grading process, here you are
%required to use "o" to represent label 1, use "+" to represent label 2, 
%and use "*" to represent label 3.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%         Your Code Starts Here         %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Plotting the labeled training data for each k using both the L1 and L2
% distances.
for j = 1:length(K)
    ind_l1_label1 = find(test_label_l1(j,:) == 1);
    ind_l1_label2 = find(test_label_l1(j,:) == 2);
    ind_l1_label3 = find(test_label_l1(j,:) == 3);
    ind_l2_label1 = find(test_label_l2(j,:) == 1);
    ind_l2_label2 = find(test_label_l2(j,:) == 2);
    ind_l2_label3 = find(test_label_l2(j,:) == 3);

    scatter_data_l1_label1 = B(ind_l1_label1,:);
    scatter_data_l1_label2 = B(ind_l1_label2,:);
    scatter_data_l1_label3 = B(ind_l1_label3,:);
    scatter_data_l2_label1 = B(ind_l2_label1,:);
    scatter_data_l2_label2 = B(ind_l2_label2,:);
    scatter_data_l2_label3 = B(ind_l2_label3,:);

    figure(2*j);
    scatter(scatter_data_l1_label1(:,1),scatter_data_l1_label1(:,2),'r+')
    hold on
    scatter(scatter_data_l1_label2(:,1),scatter_data_l1_label2(:,2),'go')
    hold on
    scatter(scatter_data_l1_label3(:,1),scatter_data_l1_label3(:,2),'b*')
    hold off
    title(['Testing Set with L1 distance for k = ' num2str(K(j))])
    legend('label 1','label 2','label 3')
    %saveas(gcf,['HW4_P1_aelhabr6_Figure_' num2str(2*j) '.png'])

    figure(2*j+1);
    scatter(scatter_data_l2_label1(:,1),scatter_data_l2_label1(:,2),'r+')
    hold on
    scatter(scatter_data_l2_label2(:,1),scatter_data_l2_label2(:,2),'go')
    hold on
    scatter(scatter_data_l2_label3(:,1),scatter_data_l2_label3(:,2),'b*')
    hold off
    title(['Testing Set with L2 distance for k = ' num2str(K(j))])
    legend('label 1','label 2','label 3')
    %saveas(gcf,['HW4_P1_aelhabr6_Figure_' num2str(2*j+1) '.png'])
end

function [min_k_indices] = min_k(v,k)
upper_bound = max(v)+1;
min_k_indices = zeros(k,1);
for j = 1:k
    [m,ind] = min(v);
    min_k_indices(j) = ind;
    v(ind) = upper_bound;
end
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%         Your Code Ends Here         %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
