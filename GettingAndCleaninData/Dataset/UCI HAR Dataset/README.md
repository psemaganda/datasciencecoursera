---
title: "run_analysis.R"
author: "psemaganda"
date: "November 24, 2014"
output: pdf_document
---
This R script is run on data files for the Human Activity Recognition Using Smartphones Dataset
Version 1.0.
You have to run the script from the folder containing the Dataset on your computer.
Your working directory must contain both the test and train subfolder.
You also should have activity_labels.txt, and features.txt files in your working directory.

The script will read in 'X_test.txt', 'subject_test.txt', 'y_test.txt' files from 'test' subfolder, and columnbind them to a dataframe 'testData'.
Files 'X_train.txt', 'subject_train.txt', and 'y_train.txt' will be read from 'train' subfolder
to create 'trainData' dataframe.
The variable names will be read from the "features.txt" files into a vector 'varNames'.
The variable names 'Subject', and 'Activity' will be appended to the 'varNames' vector.
The variable names for both the 'testData', and 'trainData' will be asigned the same names from vector 'varNames' before we merge the two dataframe into one.
The two dataframe 'testData', and 'trainData' will be merged to one dataframe 'mergedData'

After the data has been merged, it will be filtered to retain only the 'means', and 'std' activities.
Once we have the 'means', and 'std' data, we create a new dataframe 'castData', independent tidy data set with the average of each variable for each activity and each subject.
This dataframe will writen to your working directory.





