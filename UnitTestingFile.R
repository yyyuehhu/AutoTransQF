source('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/R Version/AutoTransQF.R')
source('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/R Version/autotransfuncQF.R')
source('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/R Version/ADStatQF.R')

itest = 18
if (itest == 1) {    
    #check default setting 
    idat = read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest.csv', header = FALSE)
    cat('Running 20 x 50 example') 
    start_time = Sys.time()
    print(AutoTransQF(idat)[1])
    end_time = Sys.time()
    Time_used = end_time - start_time
    cat(paste('Took', Time_used, 'secs.'))

} else if(itest == 2) {
    #check screenwrite
    idat = read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest2.csv', header = FALSE)
    cat('Running 2 x 50 example, with iscreenwrite = 1') 
    start_time = Sys.time()
    paramstruct = list(iscreenwrite = 1)
    print(AutoTransQF(idat, paramstruct)[1])
    end_time = Sys.time()
    Time_used = end_time - start_time
    cat(paste('Took', Time_used, 'secs.'))
  
} else if(itest == 3) {
    #check transformation via minimizing skewness
    idat = read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest3.csv', header = FALSE)
    cat(paste("Running 2 x 100 example, min'", 'ing skewness, with iscreenwrite = 1'),sep = '') 
    start_time = Sys.time()
    paramstruct = list(istat = 2, iscreenwrite = 1)
    print(AutoTransQF(idat, paramstruct)[1])
    end_time = Sys.time()
    Time_used = end_time - start_time
    cat(paste('Took', Time_used, 'secs.'))
    
} else if(itest == 4) {
    #transformation of variables with very large sample size 
    idat = read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest4.csv', header = FALSE)
    cat('Running 5 x 20000 example') 
    start_time = Sys.time()
    paramstruct = list(istat = 2, iscreenwrite = 1)
    print(AutoTransQF(idat, paramstruct)[1])
    end_time = Sys.time()
    Time_used = end_time - start_time
    cat(paste('Took', Time_used, 'secs.'))
  
} else if(itest == 5) {
    #check usage of feature names
    idat = read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest5.csv', header = FALSE)
    cat('Running 2 x 200 example, to test feature names')
    testname = c('Testname1', 'Testname2')
    start_time = Sys.time()
    paramstruct = list(FeatureNames = testname, iscreenwrite = 1)
    print(AutoTransQF(idat, paramstruct)[1])
    end_time = Sys.time()
    Time_used = end_time - start_time
    cat(paste('Took', Time_used, 'secs.'))
    
} else if(itest == 6) {
    # check unmatched feature names 
    idat = read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest6.csv', header = FALSE)
    cat('Testing unmatched feature names', '\n')
    testname = c('Testname1', 'Testname2')
    start_time = Sys.time()
    paramstruct = list(FeatureNames = testname, iscreenwrite = 1)
    print(AutoTransQF(idat, paramstruct)[1])
    end_time = Sys.time()
    Time_used = end_time - start_time
    cat(paste('Took', Time_used, 'secs.'))
  
} else if(itest == 7) {
    #normal distributed data vector 
    paramstruct = list(iscreenwrite = 1)
    idat = read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest7.csv', header = FALSE)
    cat('Testing normal data')
    start_time = Sys.time()
    print(AutoTransQF(idat, paramstruct)[1])
    end_time = Sys.time()
    Time_used = end_time - start_time
    cat(paste('Took', Time_used, 'secs.'))
  
} else if(itest == 8) {
    #data vector with binary values (test error message)
    idat = read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest8.csv', header = FALSE)
    cat('Testing binary data', '\n')
    start_time = Sys.time()
    AutoTransQF(idat)
    end_time = Sys.time()
    Time_used = end_time - start_time
    cat('\n', paste('Took', Time_used, 'secs.'))
    
} else if(itest == 9) {
    #Standard deviation is equal to 0
    idat = read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest9.csv', header = FALSE)
    cat('Testing 0sd data', '\n')
    start_time = Sys.time()
    AutoTransQF(idat)
    end_time = Sys.time()
    Time_used = end_time - start_time
    cat(paste('Took', Time_used, 'secs.'))
  
} else if(itest == 10) {
    #computational time of high dimension data
    idat = read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest10.csv', header = FALSE)
    cat('Running 1000 x 500 example')
    start_time = Sys.time()
    print(AutoTransQF(idat)[1])
    end_time = Sys.time()
    Time_used = end_time - start_time
    cat(paste('Took', Time_used, 'secs.'))
  
} else if(itest == 11) {
    # Visually Study Exponential Input
    idat = t(read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest11.csv', header = FALSE))
    #output qqplot of the original data
    par(mfrow=c(1,2))
    
    qqnorm(c(idat), main = "qqplot for original data")
    qqline(c(idat))
  
    #output qqplot of the transformed data 
    transformed_data = unlist(AutoTransQF(t(idat))[1])
    qqnorm(c(transformed_data), main = "qqplot for transformed data")
    qqline(c(transformed_data))
  
} else if(itest == 12) {
    #Visually Study Shifted Exponential Input
    idat = t(read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest12.csv', header = FALSE))
    par(mfrow=c(1,2))
    qqnorm(c(idat), main = "qqplot for original data")
    qqline(c(idat))
  
    #output qqplot of the transformed data 
    transformed_data = unlist(AutoTransQF(t(idat))[1])
    qqnorm(c(transformed_data), main = "qqplot for transformed data")
    qqline(c(transformed_data))
  

    
} else if(itest ==13) {
    #Visually Study Negative Exponential Input
    idat = t(read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest13.csv', header = FALSE))
    #output qqplot of the original data
    par(mfrow=c(1,2))
    qqnorm(c(idat), main = "qqplot for original data")
    qqline(c(idat))
  
    #output qqplot of the transformed data 
    transformed_data = unlist(AutoTransQF(t(idat))[1])
    qqnorm(c(transformed_data), main = "qqplot for transformed data")
    qqline(c(transformed_data))
  
} else if(itest ==14) {
    #Visually Study Gaussian Input
    idat = t(read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest14.csv', header = FALSE))
    #output qqplot of the original data
    par(mfrow=c(1,2))
    qqnorm(c(idat), main = "qqplot for original data")
    qqline(c(idat))
  
    #output qqplot of the transformed data 
    transformed_data = unlist(AutoTransQF(t(idat))[1])
    qqnorm(c(transformed_data), main = "qqplot for transformed data")
    qqline(c(transformed_data))
  
} else if(itest == 15) {
    #Visually Study Large Gaussian Input
    idat = t(read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest15.csv', header = FALSE))
    #output qqplot of the original data
    par(mfrow=c(1,2))
    qqnorm(c(idat), main = "qqplot for original data")
    qqline(c(idat))
  
    #output qqplot of the transformed data 
    transformed_data = unlist(AutoTransQF(t(idat))[1])
    qqnorm(c(transformed_data), main = "qqplot for transformed data")
    qqline(c(transformed_data))
  
} else if(itest == 16) {
    #Visually Study Mean Mixture Input
    idat = t(read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest16.csv', header = FALSE))
    #output qqplot of the original data
    par(mfrow=c(1,2))
    qqnorm(c(idat), main = "qqplot for original data")
    qqline(c(idat))
  
    #output qqplot of the transformed data 
    transformed_data = unlist(AutoTransQF(t(idat))[1])
    qqnorm(c(transformed_data), main = "qqplot for transformed data")
    qqline(c(transformed_data))
  
} else if(itest == 17) {
    #Visually Study Scale Mixture Input
    idat = t(read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest17.csv', header = FALSE))
    #output qqplot of the original data
    par(mfrow=c(1,2))
    qqnorm(c(idat), main = "qqplot for original data")
    qqline(c(idat))
  
    #output qqplot of the transformed data 
    transformed_data = unlist(AutoTransQF(t(idat))[1])
    qqnorm(c(transformed_data), main = "qqplot for transformed data")
    qqline(c(transformed_data))
  
} else if(itest == 18) {
    #Visually Gaussian and two large outliers
    idat = t(read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest18.csv', header = FALSE))
    #output qqplot of the original data
    par(mfrow=c(1,2))
    qqnorm(c(idat), main = "qqplot for original data")
    qqline(c(idat))
  
    #output qqplot of the transformed data 
    transformed_data = unlist(AutoTransQF(t(idat))[1])
    qqnorm(c(transformed_data), main = "qqplot for transformed data")
    qqline(c(transformed_data))
  
} else if(itest == 19) {
    #Input data with missing values
    idat = t(read.csv('/Users/ko/Documents/GitHub/Project/Auto Transformation Matlab_R/Matlab Version/itest19.csv', header = FALSE))
    idat[500] = NA
    start_time = Sys.time()
    AutoTransQF(t(idat))[1]
    end_time = Sys.time()
    Time_used = end_time - start_time
    cat(paste('Took', Time_used, 'secs.'))
  
}
  
  
