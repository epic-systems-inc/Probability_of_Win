# Goal
The aim of this project is to produce reliable probability estimates for the binary problem of whether or not EPIC will win a given sales lead.
Winning a sales lead is defined as being awarded a purchase order (PO).

## Why
There is currently a probability estimator in place but it is more accurately described as a set of basic rules
and back-of-the-envelope calculations than a model. It has recently drawn suspicion for its inaccuracies among executive leadership and there is a desire for
a new solution. Enter a probabilistic model.

## Implementation Details
R is used as the computational engine for this project. Heavy use is made of the caret and dplyr packages; caret for training and validating the models and dplyr for wrangling the data.
Several models are tested and a select few are transmitted to SQL Server for deploying to the test environment (see Operationalizing for more details).

### Operationalizing
First, the selected models are serialized and transmitted to SQL Server to be stored in binary form inside
a table. A stored procedure then makes use of the model(s) for prediction. 
The process flow is as follows:
1. Users fill out information through our sales lead tracking interface
2. An Ajax call sends this data as parameters stored in user-defined table types to a stored procedure
3. The stored procedure concatenates the data into one temp table and adds additional data not passed in through Ajax
4. An R script collects the data in the temp table and makes a probability prediction using the model object
5. sp_execute_external_script is used to run the R script within the stored procedure and return the probability
6. The probability is presented to the user as they interact with the sales lead tracking interface.



