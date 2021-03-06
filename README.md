Peer-graded Assignment: Getting and Cleaning Data Course Project
==================================================================
Narayana Reddy Vudumula
==================================================================

This project tries to collect and clean the data set that was given. 

Main program and goals
======================================

Main program is called run_analysis.R and it accomplishes the following four tasks to achieve the final results

    1. Merges the training and the test sets to create one data set.
    2. Extracts only the measurements on the mean and standard deviation for each measurement.
    3. Uses descriptive activity names to name the activities in the data set
    4. Appropriately labels the data set with descriptive variable names.
    5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

how to run
============
Emphasis

Set the working directory to your current code directory
copy run_analysis.R to your working directory
copy "data" directory to your current working dorectory
Run the run_analysis.R file from the R command prompt.
Output file will be produced in the data directory 

The project includes the following files:
=========================================

- 'README.txt'

- run_analysis.R - the main prgram that reads input data from the given files and produces the final output

- './data/VariableDesc.xlsx': File that acts as a dictionary where each original variable is provided with new descriptive name.

- 'codebook.md': Is the code Book that describes all the variables in the output files.

- './data/TotalData.xlsx': Contains mean and stadard deviation measurements data.

- './data/TotalDataAvg.txt': Contains the averages or mean values of the data in the above file.

