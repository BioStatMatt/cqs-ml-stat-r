# Machine Learning and Statistics in R

**2018 CQS Summer Short Course: Machine Learning and Statistics in R**

This CQS Summer Institute course provides an introduction and overview of the most powerful and common machine learning methods and statistical inference techniques using R (The R Project for Statistical Computing: https://www.r-project.org). Upon completion of this course, participants will be able to select and apply off-the-shelf machine learning and statistical software tools to many different types of analytical tasks. **This course requires some familiarity with R/RStudio and basic math.**

## Syllabus

### Day 1: Data Management

* R and RStudio basics
  * RStudio IDE
* R data structures
  * Vectors
  * Lists
  * Data frames
* Importing and exporting data
  * Simulating data
  * Flat files (e.g., CSV)
  * RData files
  * External databases
* Data manipulation
  * `reshape`
  * `melt` and `cast`
  * `subset`

### Day 2: Supervised Learning Part 1

* Supervised learning principles 
  * Linear regression vs. K-nearest-neighbors
  * Bias/variance trade-off
  * Objective (loss) functions
  * Optimism
  * Cross-validation
* Regression methods
  * Modeling associations
  * Model selection and shrinkage
* Classification and regression trees 
* Random forests (bagging)

### Day 3: Supervised Learning Part 2

* Gradient boosted trees (boosting)
* Neural networks
  * Fully connected
  * Convolutional
  * Deep learning

### Day 4: Unsupervised Learning

* Unsupervised learning principles
  * Model selection and parsimony
* Dimension reduction
  * Principal components analysis
* Clustering 
  * K-means
  * Hierarchical

### Day 5: Statistical Inference

* Principles of statistical inference 
  * Populations, samples, and sampling biases
  * Statistics and sampling distributions
  * Confidence intervals and P-values
  * Study design and confounding
  * Model selection and prespecification
* Regression methods
  * Linear and generalized linear regression
  * Model assumptions and diagnostics
  * Multiple degree-of-freedom tests
* Bootstrap

## Background and Software Installation

This course requires the use of RStudio. This software is open source and freely available for download and installation on all operating systems. See https://www.rstudio.org/

We will additionally make use of several freely available add-on packages for R, which are listed below and can be installed using the following R commands:

```rstats
install.packages("reshape2")
```

This course is very ``hands on.'' I recommend that participants who are not familiar with R or RStudio work through some of the excellent and free online beginner tutorials for R/RStudio. The following link is a great place to start: 
https://support.rstudio.com/hc/en-us/articles/201141096-Getting-Started-with-R

## Other Resources

* Introduction to Statistical Learning: http://www-bcf.usc.edu/~gareth/ISL/
