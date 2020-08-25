# Healthcare-Fraud-Detection-using-Clustering-Technique
A machine learning model is built using clustering technique to zero in on the top 1.75% anomalies that are potentially fraudulent.

The two major steps here are:  
1. Engineer the following features from the "Inpatient charges data" followed by a detailed exploratory analysis of selective variables:   
a. Ratio of discharges between DRG-Provider combination and total discharges at a State, Zip code and HRR level   
b. Deviations of charges and payments related to every DRG at a State, Zip and HRR level relative to the mean values of the respective groups  

2. Peer comparisons on payments and charges among providers of the same state are made in addition to comparing individual provider’s covered charge to total charge through existing features. Combine correlated variables into a single component based on the results of principal components analysis. Perform K-means clustering to identify anomalous groups.

# Getting Started
Use the "Healthcare Fraud Detection - Feature Engineering.R" Script to inspect   
- the data cleaning and feature engineering process   

Use the "Healthcare Fraud Detection - Feature Engineering.R" Script to inspect   
- additional feature engineering  
- principal component analysis  
- K-means clustering to find anomalies  

# Source of Data:  
inpatientCharges.csv



