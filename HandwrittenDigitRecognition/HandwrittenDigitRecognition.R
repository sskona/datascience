####################################################################################################################################################################
#                                                 Hand Written Digit Recognition Classification Problem
####################################################################################################################################################################
# 1. Business Understanding
# 2. Data Understanding
# 3. Data Preparation & EDA
# 4. SVM Models Building 
#       Use default Kernel parameters using Kernels & Accuracy verification 
#         - Linear
#         - Polynomial 
#         - RBF
# 
# 5. Cross validation & Checking overfitting
#         - Tunning linear SVM model 
#         - Tuning Polynomial SVM Model
#         - Tuning RBF SVM model
# 
# 6. Selection of Best Model
#
####################################################################################################################################################################
#                                                         Business and Data Understanding
####################################################################################################################################################################
# 1. Business Understanding
# 
#   - Classify the handwritten digits for pattern recognition as 0-9
#   - Use MNIST datasets for solving the problem
#
# 2. Data Understanding
#   - Use MNIST datasets - Ref Link(s) : https://en.wikipedia.org/wiki/MNIST_database
# 
# This train and test data from MNIST (Modified National Institute of Standards and Technology database) are actually mixed data-sets.
# The MNIST database contains 60,000 training images and 10,000 testing images.
# Half of the training set and half of the test set were taken from NIST's training dataset, 
# while the other half of the training set and the other half of the test set were taken from NIST's testing dataset
#   - The datasets do not have any headers
#   - 1st Column is Label to classify which digit (0-9) the row is
#   - All other 784 columns are 29x28 matrix to capture pixel color value in the range of 0-255
# 
# Loading Libraries
# -----------------
#install.packages("caret")
#install.packages("kernlab")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("ggplot2")
#install.packages("gridExtra")
#install.packages("doParallel")

library(kernlab)
library(readr)
library(caret)
library(caTools)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
# library(doParallel)
# registerDoParallel()

####################################################################################################################################################################
#                                                 3.  Data Preparation and EDA
####################################################################################################################################################################

      # Loading Data - These Data files without header endsup with auto header with names as V1, V2, .. etc.
          # Load Train Data
          mnist_trainData <- read.csv("mnist_train.csv", sep =',', stringsAsFactors = FALSE, header = FALSE)
          # Load Test Data
          mnist_testData <- read.csv("mnist_test.csv", sep =',', stringsAsFactors = FALSE, header = FALSE)

      # View Dimensions
          dim(mnist_trainData)
          dim(mnist_testData)

      # Verify Structure of the dataset
          str(mnist_trainData)
          str(mnist_testData)

      # View first few rows
          head(mnist_trainData)
          head(mnist_testData)

      # Check the data
          summary(mnist_trainData)
          summary(mnist_testData)

      # Verifying the distribution of Digits (i.e. Labels) in both Train and Test Data
          # Both datasets have approximately similar distribution of digits 0-9 
          # All digits 0-9 data is available, also train and test data not skewed in numbers
          plot(factor(mnist_trainData$V1))
          plot(factor(mnist_testData$V1))


      # Data Quality check
          # Check if pixel values in each column is 0-255 only
          # Number of cells in 28x28 should be equal to Number of TRUE values matching range of 0-255
          (nrow(mnist_trainData)*784 == sum(sapply(mnist_trainData [, -1], function(x) (x > 0 | x <=255))))
          (nrow(mnist_testData)*784 == sum(sapply(mnist_testData [, -1], function(x) (x > 0 | x <=255))))

          # Check if label values in each column is 0-9 only
          # Number of cells in 28x28 should be equal to Number of TRUE values matching range of 0-255
          (nrow(mnist_trainData) == sum(sapply(mnist_trainData [, 1], function(x) (x > 0 | x <=9))))
          (nrow(mnist_testData) == sum(sapply(mnist_testData [, 1], function(x) (x > 0 | x <=9))))
          
      # Check train and test data set column count and names
          ncol (mnist_trainData) == ncol (mnist_testData)
          sum(colnames (mnist_trainData) == colnames (mnist_testData)) == ncol(mnist_trainData)
          
      # Duplicate Data check & comments
          # Checking duplicate rows 
          mnist_trainData <- unique(mnist_trainData)
          mnist_testData <- unique(mnist_testData)
          # checking duplicate columns - Not Applicable (28x28 matrix - 784 columns)

      # NA value verification 
          # Check if any values are NA in columns
          # checking missing value - value 0 indicates no NA values
          sum(sapply(mnist_trainData, function(x) sum(is.na(x))))
          sum(sapply(mnist_testData, function(x) sum(is.na(x))))
          
      # Missing value verification & imputation
          # Check if any values are NULL in columns
          # checking missing value - value 0 indicates no NULL values
          sum(sapply(mnist_trainData, function(x) sum(is.null(x))))
          sum(sapply(mnist_testData, function(x) sum(is.null(x))))

      # Bringing all pixel values same scale from 0-255 range across all columns 
          # Without scaling data RBF Kernel results are very poor while Linear & Polynomial results are consistent 
          # Experimented alternate scaling methods as default scale () leads to NaN due to zero-varience
          # Method 1 - Verified with cale(x, center = TRUE, scale = FALSE)
          # Method 2 - Using (y - mean(y)) / sd(y) ^ as.logical(sd(y))
          #          - Ref Link : https://stackoverflow.com/questions/15363610/why-does-scale-return-nan-for-zero-variance-columns
          # Method 3 - Divide all pixel values with 255
          #          - This is the best scaling method for these data-sets
        
          mnist_trainData[, -1] <- sapply(mnist_trainData [, -1], function(x) x/255)
          mnist_testData[, -1] <- sapply(mnist_testData [, -1], function(x) x/255)

      # Making our target class i.e. digit column V1 to factor
          mnist_trainData$V1 <- factor(mnist_trainData$V1)
          mnist_testData$V1 <- factor(mnist_testData$V1)
    
      # Preparing training data with 10% from Train dataset i.e. ~6000 rows
      # Using complete test data i.e. - 10000 rows
          
          set.seed(100)
          train_indices = sample.split(mnist_trainData$V1, SplitRatio = 0.1)
          train = mnist_trainData [train_indices,]

####################################################################################################################################################################
#                                                             4.  SVM Models Building
####################################################################################################################################################################
# Optimal hyper parameters should be chosen using after initial validation of default Karnel parameters
          
          # Using Linear Kernel  - Setting default kernel parameters
          #---------------------------------------------------------
          
          Model_linear <- ksvm(V1~ ., data = train, scale = FALSE, kernel = "vanilladot")
          # Verying against complete test data
          Eval_linear<- predict(Model_linear, mnist_testData)
          #confusion matrix - Linear Kernel
          confusionMatrix(Eval_linear,mnist_testData$V1)
          
          # Accuracy : 0.9112
          # Statistics by Class:
          #                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
          # Sensitivity            0.9704   0.9850   0.9109   0.8604   0.9420   0.8599   0.9395   0.9173   0.8347   0.8781
          # Specificity            0.9931   0.9927   0.9894   0.9867   0.9880   0.9854   0.9951   0.9919   0.9895   0.9897
          
          
          # Using Polynomial Kernel - Setting default kernel parameters
          #------------------------------------------------------------
        
          Model_Poly <- ksvm(V1 ~ ., data = train, scale = FALSE, kernel = "polydot")
          # 92% with Model_Poly <- ksvm(V1 ~ ., data = train, scale = 0.1, degree = 2, C=0.1, kernel = "polydot")
          
          # Verifying against Full test data
          Eval_Poly <- predict(Model_Poly, mnist_testData)
          #confusion matrix - RBF Kernel
          confusionMatrix(Eval_Poly, mnist_testData$V1)

          # Accuracy : 0.9112
          # Statistics by Class:
          #                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
          # Sensitivity            0.9704   0.9850   0.9109   0.8604   0.9420   0.8599   0.9395   0.9173   0.8347   0.8781
          # Specificity            0.9931   0.9927   0.9894   0.9867   0.9880   0.9854   0.9951   0.9919   0.9895   0.9897
          
          # Polynomial is consitent with 91% accuracy using degree = 2 in the experiments
                    
          # Using RBF Kernel - Setting default kernel parameters
          #-----------------------------------------------------
          
          Model_RBF <- ksvm(V1 ~ ., data = train, scale = FALSE, kernel = "rbfdot")
          # Verifying against Full test data
          Eval_RBF2<- predict(Model_RBF, mnist_testData)
          #confusion matrix - RBF Kernel
          confusionMatrix(Eval_RBF2,mnist_testData$V1)
          
          # Accuracy : 0.9515
          #                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
          # Sensitivity            0.9837   0.9885   0.9399   0.9406   0.9542   0.9406   0.9718   0.9368   0.9374   0.9177
          # Specificity            0.9960   0.9972   0.9945   0.9931   0.9935   0.9944   0.9947   0.9952   0.9939   0.9937
          
          # Accuracy is very close to 95% by Using sigma = 0.01, 0.02, 0.03 and C = 2,4,5 
          # Model_RBF <- ksvm(V1 ~ ., data = train, scale = FALSE, kernel = "rbfdot", kpar = list(sigma = 0.01), C = 2, cross=3 )
          # Model_RBF <- ksvm(V1 ~ ., data = train, scale = FALSE, kernel = "rbfdot", kpar = list(sigma = 0.02), C = 4, cross=3 )
          # Model_RBF <- ksvm(V1 ~ ., data = train, scale = FALSE, kernel = "rbfdot", kpar = list(sigma = 0.03), C = 5, cross=3 )

          CV_startTime <- Sys.time()
####################################################################################################################################################################
#                                         Hyperparameter tuning, Cross Validation and Checking overfitting
####################################################################################################################################################################
#          
# SVM - Linear Kernel
          
          # Using the train function from caret package to perform crossvalidation
          trainControl <- trainControl(method="cv", number=5)
          
          # Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
          metric <- "Accuracy"
          
          # Making grid of "sigma" and C values.
          grid <- expand.grid(C=seq(1, 10, by=1))
          
          # Performing 5-fold cross validation
          fit.svm_linear <- train(V1~., data=train, method="svmLinear", metric=metric, 
                                  tuneGrid=grid, trControl=trainControl)
          
          # Printing cross validation result
          print(fit.svm_linear)
          
          # Support Vector Machines with Linear Kernel 
          
          # 5999 samples
          # 784 predictor
          # 10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 
          
          # No pre-processing
          # Resampling: Cross-Validated (5 fold) 
          # Summary of sample sizes: 4801, 4799, 4798, 4798, 4800 
          # Resampling results across tuning parameters:
            
          #  C   Accuracy   Kappa    
          #  1  0.9076517  0.8973481
          #  2  0.9068173  0.8964212
          #  3  0.9068173  0.8964212
          #  4  0.9068173  0.8964212
          #  5  0.9068173  0.8964212
          #  6  0.9068173  0.8964212
          #  7  0.9068173  0.8964212
          #  8  0.9068173  0.8964212
          #  9  0.9068173  0.8964212
          # 10  0.9068173  0.8964212
          
          # Accuracy was used to select the optimal model using the largest value.
          # The final value used for the model was C = 1.
          
          # Plotting model results
          plot(fit.svm_linear)
          
  # Checking overfitting - Linear - SVM

          # Validating the model results on Full test data
          evaluate_linear<- predict(fit.svm_linear, mnist_testData)
          confusionMatrix(evaluate_linear, mnist_testData$V1)
          
          # Confusion Matrix and Statistics
          
          #         Reference
          #Prediction    0    1    2    3    4    5    6    7    8    9
          #         0  951    0   14    4    2    7   13    3   10    9
          #         1    0 1118    6    8    0    6    5   14   19    7
          #         2    4    4  940   34    3    4   10   23   12    1
          #         3    1    1   18  869    0   48    2    5   35   10
          #         4    3    0   11    2  925   13    7   14   15   43
          #         5    8    2    6   49    3  767   19    1   36    9
          #         6   11    1   10    1    6    8  900    0    6    1
          #         7    1    0   11    7    1    3    0  943   14   36
          #         8    1    9   14   26    4   30    2    2  813    7
          #         9    0    0    2   10   38    6    0   23   14  886
          
          # Overall Statistics
          
          # Accuracy : 0.9112          
          # 95% CI : (0.9055, 0.9167)
          # No Information Rate : 0.1135          
          # P-Value [Acc > NIR] : < 2.2e-16       
          
          # Kappa : 0.9013          
          # Mcnemar's Test P-Value : NA  
          
          # Statistics by Class:
            
          #                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
          # Sensitivity            0.9704   0.9850   0.9109   0.8604   0.9420   0.8599   0.9395   0.9173   0.8347   0.8781
          # Specificity            0.9931   0.9927   0.9894   0.9867   0.9880   0.9854   0.9951   0.9919   0.9895   0.9897
          # Pos Pred Value         0.9388   0.9451   0.9082   0.8787   0.8955   0.8522   0.9534   0.9281   0.8954   0.9050
          # Neg Pred Value         0.9968   0.9981   0.9897   0.9844   0.9936   0.9863   0.9936   0.9905   0.9823   0.9864
          # Prevalence             0.0980   0.1135   0.1032   0.1010   0.0982   0.0892   0.0958   0.1028   0.0974   0.1009
          # Detection Rate         0.0951   0.1118   0.0940   0.0869   0.0925   0.0767   0.0900   0.0943   0.0813   0.0886
          # Detection Prevalence   0.1013   0.1183   0.1035   0.0989   0.1033   0.0900   0.0944   0.1016   0.0908   0.0979
          # Balanced Accuracy      0.9818   0.9888   0.9501   0.9235   0.9650   0.9226   0.9673   0.9546   0.9121   0.9339

          CV_timeLinear <- Sys.time()
          
#########################################################################################################################################################################
# SVM - Polynomial Kernel
          # Using the train function from caret package to perform crossvalidation
          trainControl <- trainControl(method="cv", number=5)
          
          # Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
          metric <- "Accuracy"
          
          # Making grid of "sigma" and C values. 
          #grid <- expand.grid(.scale=seq(0.1, 0.2, by=0.1), .degree=seq(1,2,3), .C=seq(1, 2, by=1))
          #  scale  C  Accuracy   Kappa    
          # 0.1    1  0.9208214  0.9119903
          # The final values used for the model were degree = 1, scale = 0.1 and C = 1.
          # grid <- expand.grid(.scale=seq(0.1, 0.2, by=0.1), .degree=3, .C=seq(2, 3, by=1))
          #  scale  C  Accuracy   Kappa    
          # 0.1    2  0.9509927  0.9455274
          # grid <- expand.grid(.scale=seq(0.1, 0.2, by=0.1), .degree=4, .C=seq(2, 3, by=1))
          # Accuracy : 0.9472          
          
          # Results are best for degree = 2
          grid <- expand.grid(.scale=seq(0.1, 0.2, by=0.1), .degree=2, .C=seq(1, 2, by=1))
          # Support Vector Machines with Polynomial Kernel 
          
          # Performing 5-fold cross validation
          fit.svm_poly <- train(V1~., data=train, method="svmPoly", metric=metric, 
                                tuneGrid=grid, trControl=trainControl)
          
          # Printing cross validation result
          print(fit.svm_poly)
          
          # Support Vector Machines with Polynomial Kernel 
          
          # 5999 samples
          # 784 predictor
          # 10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 
          
          # No pre-processing
          # Resampling: Cross-Validated (5 fold) 
          # Summary of sample sizes: 4799, 4800, 4798, 4797, 4802 
          # Resampling results across tuning parameters:
            
          # scale  C  Accuracy   Kappa    
          # 0.1    1  0.9549927  0.9499738
          # 0.1    2  0.9549927  0.9499738
          # 0.2    1  0.9544924  0.9494177
          # 0.2    2  0.9544924  0.9494177
          
          # Tuning parameter 'degree' was held constant at a value of 2
          # Accuracy was used to select the optimal model using the largest value.
          # The final values used for the model were degree = 2, scale = 0.1 and C = 1.
          
          # Plotting model results
          plot(fit.svm_poly)

  # Checking overfitting - SVM - Polynomial

          # Validating the model results on Full test data
          evaluate_poly <- predict(fit.svm_poly, mnist_testData)
          confusionMatrix(evaluate_poly, mnist_testData$V1)
          
          # Confusion Matrix and Statistics
          #             Reference
          # Prediction   0    1    2    3    4    5    6    7    8    9
          #         0  962    0    8    0    2    1    9    3    5    9
          #         1    0 1122    2    0    0    2    3   11    2    6
          #         2    1    2  983   18    5    3    3   16    2    0
          #         3    1    1    9  955    0   18    1    3   14    9
          #         4    2    0    6    0  939    3    4   11    7   27
          #         5    6    0    1    9    0  841    9    0    8    5
          #         6    7    4    8    0    6    6  929    0    2    1
          #         7    1    0    9   10    1    1    0  971   12    8
          #         8    0    6    5   14    2   11    0    2  913    7
          #         9    0    0    1    4   27    6    0   11    9  937
          
          # Overall Statistics
          
          # Accuracy : 0.9552         
          # 95% CI : (0.951, 0.9592)
          # No Information Rate : 0.1135         
          # P-Value [Acc > NIR] : < 2.2e-16      
          
          # Kappa : 0.9502         
          # Mcnemar's Test P-Value : NA             
          
          # Statistics by Class:
          
          #                      Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
          # Sensitivity            0.9816   0.9885   0.9525   0.9455   0.9562   0.9428   0.9697   0.9446   0.9374   0.9286
          # Specificity            0.9959   0.9971   0.9944   0.9938   0.9933   0.9958   0.9962   0.9953   0.9948   0.9935
          # Pos Pred Value         0.9630   0.9774   0.9516   0.9446   0.9399   0.9568   0.9647   0.9585   0.9510   0.9417
          # Neg Pred Value         0.9980   0.9985   0.9945   0.9939   0.9952   0.9944   0.9968   0.9937   0.9933   0.9920
          # Prevalence             0.0980   0.1135   0.1032   0.1010   0.0982   0.0892   0.0958   0.1028   0.0974   0.1009
          # Detection Rate         0.0962   0.1122   0.0983   0.0955   0.0939   0.0841   0.0929   0.0971   0.0913   0.0937
          # Detection Prevalence   0.0999   0.1148   0.1033   0.1011   0.0999   0.0879   0.0963   0.1013   0.0960   0.0995
          # Balanced Accuracy      0.9888   0.9928   0.9735   0.9697   0.9748   0.9693   0.9830   0.9699   0.9661   0.9611
          

          CV_timePoly <- Sys.time()
#########################################################################################################################################################################
# SVM - RDF Kernel
          
          # Using the train function from caret package to perform crossvalidation
          trainControl <- trainControl(method="cv", number=5)
          
          # Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
          metric <- "Accuracy"
          
          # Making grid of "sigma" and C values. 
          # Tested with various combinations of sigma = 0.1 to 0.5 etc along with C = 1 to 10 - Over Accuracy is 11% (approx)
          # 
          grid <- expand.grid(.sigma=seq(0.01, 0.05, by=0.01), .C=seq(1, 5, by=1))
          
          # Performing 5-fold cross validation

          fit.svm_radial <- train(V1~., data=train, method="svmRadial", metric=metric, 
                                  tuneGrid=grid, trControl=trainControl)
          
          
          
          # Printing cross validation result
          print(fit.svm_radial)
          # Support Vector Machines with Radial Basis Function Kernel 
          
          # 5999 samples
          # 784 predictor
          # 10 classes: '0', '1', '2', '3', '4', '5', '6', '7', '8', '9' 
          
          # No pre-processing
          # Resampling: Cross-Validated (5 fold) 
          # Summary of sample sizes: 4800, 4801, 4799, 4798, 4798 
          # Resampling results across tuning parameters:
            
          # sigma  C  Accuracy   Kappa    
          # 0.01   1  0.9458259  0.9397846
          # 0.01   2  0.9531600  0.9479357
          # 0.01   3  0.9566604  0.9518263
          # 0.01   4  0.9581602  0.9534938
          # 0.01   5  0.9584936  0.9538640
          # 0.02   1  0.9576587  0.9529371
          # 0.02   2  0.9609937  0.9566438
          # 0.02   3  0.9618276  0.9575709
          # 0.02   4  0.9624945  0.9583123
          # 0.02   5  0.9626611  0.9584973
          # 0.03   1  0.9613257  0.9570139
          # 0.03   2  0.9629930  0.9588668
          # 0.03   3  0.9631601  0.9590527
          # 0.03   4  0.9633269  0.9592382
          # 0.03   5  0.9633269  0.9592382
          # 0.04   1  0.9603261  0.9559039
          # 0.04   2  0.9618269  0.9575722
          # 0.04   3  0.9618272  0.9575725
          # 0.04   4  0.9619940  0.9577580
          # 0.04   5  0.9619940  0.9577580
          # 0.05   1  0.9561570  0.9512714
          # 0.05   2  0.9573247  0.9525694
          # 0.05   3  0.9573247  0.9525694
          # 0.05   4  0.9573247  0.9525694
          # 0.05   5  0.9573247  0.9525694
          
          # Plotting model results
          plot(fit.svm_radial)
          # Accuracy was used to select the optimal model using the largest value.
          # The final values used for the model were sigma = 0.03 and C = 4.
          
          
  # Checking overfitting - Non-Linear - SVM - RDF Kernel
          

          # Validating the model results on Full test data
          evaluate_radial <- predict(fit.svm_radial, mnist_testData)
          
          confusionMatrix(evaluate_radial, mnist_testData$V1)

          # Confusion Matrix and Statistics
          
          #               Reference
          # Prediction    0    1    2    3    4    5    6    7    8    9
          #           0  966    0    7    0    1    3    8    2    5    5
          #           1    0 1124    0    0    0    1    3    9    1    5
          #           2    0    3  999   12    2    3    0   16    3    2
          #           3    0    1    5  963    0   10    0    2   10   13
          #           4    0    0    1    0  950    1    3    7    5   22
          #           5    6    1    0   10    0  861    4    0    8    3
          #           6    5    3    2    1    7    6  939    0    1    1
          #           7    1    0    9   10    1    1    0  973    6    8
          #           8    2    3    9   12    1    4    1    3  932   10
          #           9    0    0    0    2   20    2    0   16    3  940
          
          # Overall Statistics
          
          # Accuracy : 0.9647          
          # 95% CI : (0.9609, 0.9682)
          # No Information Rate : 0.1135          
          # P-Value [Acc > NIR] : < 2.2e-16       
          
          # Kappa : 0.9608          
          # Mcnemar's Test P-Value : NA              
          
          # Statistics by Class:
          
          # Class: 0 Class: 1 Class: 2 Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
          # Sensitivity            0.9857   0.9903   0.9680   0.9535   0.9674   0.9652   0.9802   0.9465   0.9569   0.9316
          # Specificity            0.9966   0.9979   0.9954   0.9954   0.9957   0.9965   0.9971   0.9960   0.9950   0.9952
          # Pos Pred Value         0.9689   0.9834   0.9606   0.9592   0.9606   0.9642   0.9731   0.9643   0.9539   0.9563
          # Neg Pred Value         0.9984   0.9988   0.9963   0.9948   0.9964   0.9966   0.9979   0.9939   0.9953   0.9923
          # Prevalence             0.0980   0.1135   0.1032   0.1010   0.0982   0.0892   0.0958   0.1028   0.0974   0.1009
          # Detection Rate         0.0966   0.1124   0.0999   0.0963   0.0950   0.0861   0.0939   0.0973   0.0932   0.0940
          # Detection Prevalence   0.0997   0.1143   0.1040   0.1004   0.0989   0.0893   0.0965   0.1009   0.0977   0.0983
          # Balanced Accuracy      0.9911   0.9941   0.9817   0.9745   0.9815   0.9809   0.9886   0.9712   0.9759   0.9634          

          CV_timeRBF <- Sys.time()
          
####################################################################################################################################################################
#                                                         6. Selection of Best Model
####################################################################################################################################################################
          
          # Best Model to use   : SVM Radial - For Best Accuracy (96.4%) with very highly-end computational environment
          #                       SVM Polynomial - For slightly less Accuracy (95.5%) and not a very high-end computational environment
          # Polynomial
          # Hyper parameters    : degree = 2, scale = 0.1 and C = 1
          # Train Accuracy      : 0.9519874
          # Test Accuracy       : 0.9555
          fit.svm_poly
          
          # RBF
          # Hyper parameters    : sigma = 0.03 and C = 4
          # Train Accuracy      : 0.9633269
          # Test Accuracy       : 0.9647
          
          fit.svm_radial
          
          print ('SVM Linear - Cross Validation and Overfit Check')
          CV_timeLinear - CV_startTime
          print ('SVM Polynomial - Cross Validation and Overfit Check')
          CV_timePoly - CV_timeLinear
          print ('SVM RBF - Cross Validation and Overfit Check')
          CV_timeRBF - CV_timePoly
          
          