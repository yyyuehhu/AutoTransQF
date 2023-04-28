source('autotransformationTest.R')
#1...9 if ifunc == 2 
#1...4 if ifunc ==1
itest = 4

ifunc = 1 #1 represents ADStatQFtest, #2 represents autotransfuncQFtest

if(ifunc==1){
  test.func = ADStatQFtest
}else{
  test.func = autotransfuncQFtest
}


if(ifunc == 2) {
  if(itest == 1) {
    #autotransfuncQF in R tested on 1000 data points from standard normal distribution.
    set.seed(100)
    test2.1 = rnorm(1000, 0, 1)
    test.func(test2.1)
    
  } else if(itest == 2) {
    #autotransfuncQF in R tested on 1000 data points from normal distribution with mean zero and standard deviation 100.
    set.seed(300)
    test2.2 = rnorm(1000, 0, 100)
    test.func(test2.2)
    
  } else if(itest == 3) {
    #autotransfuncQF in R tested on 1000 data points generated from the gamma distribution with shape = 1 and scale = 2.
    set.seed(209)
    test2.3 = rgamma(1000, shape = 1, scale = 2)
    test.func(test2.3)
    
  } else if(itest == 4) {
    #autotransfuncQF in R tested on 1000 data points generated from gamma distribution with shape = 2 and scale = 2. 
    set.seed(209)
    test2.4 = rgamma(1000, shape = 2, scale = 2)
    test.func(test2.4)
    
  } else if(itest == 5) {
    #function autotransfuncQF in R tested on 1000 data points generated from log normal distribution exp(N(0,1))
    set.seed(383)
    test_no_log2.5 = rnorm(1000, 0, 1)
    test2.5 = exp(test_no_log2.5)
    test.func(test2.5)
    
  } else if(itest == 6) {
    #function autotransfuncQF in R tested on 1000 data points generated from log normal distribution exp(N(-1,1))
    set.seed(383)
    test_no_log2.6 = rnorm(1000, -1, 1)
    test2.6 = exp(test_no_log2.6)
    test.func(test2.6)
    
  } else if(itest == 7) {
    #function autotransfuncQF in R tested on 1000 data points generated from log normal distribution exp(N(1,1))
    set.seed(383)
    test_no_log2.7 = rnorm(1000, 1, 1)
    test2.7 = exp(test_no_log2.7)
    test.func(test2.7)
    
  } else if(itest == 8) {
    #autotransfuncQF in R tested on a mixture of 500 data points generated from normal distribution 
    #with mean 0 and standard deviation 4
    #and 500 data points generated from normal distribution with mean 16 and standard deviation 4
    set.seed(100)
    test2.8 = c(rnorm(500, 0, 4), rnorm(500, 16, 4))
    test.func(test2.8)
    
  } else if(itest == 9) {
    # autotransfuncQF in R tested on a mixture of 500 data points generated 
    #from normal distribution with mean 0 and standard deviation 1
    #and 500 data points generated from normal distribution with mean 0 and standard deviation 4.
    set.seed(100)
    test2.9 = c(rnorm(500, 0, 1), rnorm(500, 0, 4))
    test.func(test2.9)
    
  } else if(itest == 10) {
    #autotransfuncQF in R tested on 1000 data points generated from the beta distribution with
    #alpha = 0.5 and beta =0.5
    set.seed(327)
    test2.10 = rbeta(1000, 0.5, 0.5)
    test.func(test2.10)
    
  } else if(itest == 11) {
    #autotransfuncQF in R tested on 1000 data points generated from uniform distribution 
    #with parameters min = 0 and max = 5.
    set.seed(500)
    test2.11 = runif(1000, min = 0, max = 5)
    test.func(test2.11)
    
  } else if (itest == 12) {
    #autotransfuncQF in R tested on 1000 data points generated from 
    #log normal distribution exp(N(1,5))
    set.seed(383)
    test_no_log2.12 = rnorm(1000, 1, 5)
    test2.12 = exp(test_no_log2.12)
    test.func(test2.12)
  
  }
  
} else if(ifunc == 1) {
    if (itest == 1) {
      #ADStatQF tested on N(0,1)
      set.seed(100)
      test1.1 = t(rnorm(1000, 0, 1))
      test.func(test1.1)
      
  } else if(itest == 2) {
      #ADStatQF tested on N(0, 100)
      set.seed(300)
      test1.2 = t(rnorm(1000, 0, 100))
      test.func(test1.2)
      
  } else if(itest == 3) {
      #ADStatQF tested on 1/2*N(0, 4) and 1/2*N(16, 4)
      set.seed(100)
      test1.3 = t(c(rnorm(500, 0, 4), rnorm(500, 16, 4)))
      test.func(test1.3)
      
  } else if(itest == 4) {
    #ADStatQF tested on 1/2*N(0, 1) and 1/2*N(0, 4)
      set.seed(100)
      test1.4 = t(c(rnorm(500, 0, 1), rnorm(500, 0, 4)))
      test.func(test1.4)
  }
}

