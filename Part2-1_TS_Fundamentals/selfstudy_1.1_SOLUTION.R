##############################################
# Time Series Analysis with R (TSAR)
# PART 2: Time Series Analysis
# Self-Study 1.2: Basic Steps of Forecasting
# gwendolin.wilke@fhnw.ch
##############################################

# For the case of the car manufactorer, describe the five steps of forecasting in the context of this project.

#### 1. Business Understanding and Problem Definition  

# + The main stakeholders should be defined.
# + Everyone has been questioned about which way he or she can benefit from the new system. 
# # In case of the fleet company probably the group of specialists was not recognized as stakeholders which led to complications in gathering relevant information and later in finding an appropriate statistical approach and deployment of the new forecasting method.

#### 2. Data Understanding: Information Gathering and Exploratory Analysis. 

# + Data set of past sales should be obtained, including surrounding information such as the way data were gathered, possible outliers and incorrect records, special values in the data.
# + Expertise knowledge should be obtained from people responsible for the sales such as seasonal price fluctuations, if there is dependency of the price on the situation in economy, also finding other possible factors which can influence the price.
# + Graphs which show dependency of the sale price on different predictor variables should be considered.
# + Dependency of the sale price on month of the year should be plot.

#### 3. Data Preparation

# + Possible outliers and inconsistent information should be found (for example very small, zero or even negative prices).

#### 4. Choosing and fitting models

# + A model to start from (for example a linear model) and predictor variables which most likely affect the forecasts should be chosen. 

#### 5. Evaluating a forecasting model

# + Predicting performance of the models must be evaluated.
# + The model should be changed (for example by transforming parameters, adding or removing predictor variables) and it's performance evaluated. 
#   This should be done iteratively wit step 4 a few times until a satisfactory model is found.

#### 6. Deploying a Forecasting Model

# + The appropriate software should be deployed to the company and relevant people should be educated how to use this software.
# + Forecasting accuracy should be checked against new sales. If necessary the model should be updated and then the deployed software.
