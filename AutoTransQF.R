#install packages
library(matlab2r)
library(moments)

#The program will automatically transform each row of input data set to
#make the distribution of each row vector closer to the standard normal distribution

#The transformation function belongs to a new parametrization of the shifted logarithm family,
#where the transformation parameter is chosen to minimize the skewness/Anderson-Darling Statistic
#of Transformed data vector

#mdata: d * n matrix
#d rows: each row represents a feature.
#n columns: each column is a sample (data object). We need n > 7 to compute Anderson-Darling Statistic

#paramstruct: a list of input parameters, misspecified values will be reverted to defaults

# Default Values
#istat: criterion for selecting the parameter of transformation function
#1: Anderson-Darling Test Statistics (Default)
#2: Skewness

# iscreenwrite: whether there is screenwrites
# iscreenwrite = 0 for no screenwrites (default)
# iscreenwrite = 1 to write progress to screen

# FeatureNames: Feature names of each data vector
# default setting: 'Feature1'
AutoTransQF = function(mdata, paramstruct = NULL) {
  mdata = as.matrix(mdata)
  row_num = NROW(mdata)
  col_num = NCOL(mdata)
  
  transformed_data = matrix(NA, nrow = row_num, ncol = col_num)
  transformation = list()
  
  #Set all parameters to defaults
  istat = 1
  iscreenwrite = 0
  FeatureNames = c()
  beta = -1
  alpha = -1
  all_alpha = c()
  all_beta = c()
  
  
  #Extract Feature Names
  for (i in 1:row_num) {
    feature = paste("Feature",i, sep = '')
    FeatureNames = append(FeatureNames, feature)
  }
  
  if(length(paramstruct) == 0){
    istat = istat
    iscreenwrite = iscreenwrite
    FeatureNames = FeatureNames} 
  
  if(length(paramstruct)!=0){
    if (!is.null(paramstruct$istat) && !is.null(paramstruct$iscreenwrite) && !is.null(paramstruct$FeatureNames)) {
      istat = paramstruct$istat
      iscreenwrite = paramstruct$iscreenwrite
      FeatureNames = paramstruct$FeatureNames
    } else if (is.null(paramstruct$istat) && !is.null(paramstruct$iscreenwrite) && !is.null(paramstruct$FeatureNames)) {
      iscreenwrite = paramstruct$iscreenwrite
      FeatureNames = paramstruct$FeatureNames
    } else if (!is.null(paramstruct$istat) && is.null(paramstruct$iscreenwrite) && !is.null(paramstruct$FeatureNames)) {
      istat = paramstruct$istat
      FeatureNames = paramstruct$FeatureNames
    } else if (!is.null(paramstruct$istat) && !is.null(paramstruct$iscreenwrite) && is.null(paramstruct$FeatureNames)) {
      istat = paramstruct$istat
      iscreenwrite = paramstruct$iscreenwrite
    } else if (is.null(paramstruct$istat) && is.null(paramstruct$iscreenwrite) && !is.null(paramstruct$FeatureNames)) {
      FeatureNames = paramstruct$FeatureNames
    } else if (is.null(paramstruct$istat) && !is.null(paramstruct$iscreenwrite) && is.null(paramstruct$FeatureNames)) {
      iscreenwrite = paramstruct$iscreenwrite
    } else if (!is.null(paramstruct$istat) && is.null(paramstruct$iscreenwrite) && is.null(paramstruct$FeatureNames)) {
      istat = paramstruct$istat
    } 
    
    #when the number of feature names is unmatched with number of features
    #needed to perform transformation, use default feature names
    
    if (row_num != length(FeatureNames)){
      
      message("\n")
      warning('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', '\n', 
              '!!!!!!!!!! Warning: Number of Feature Names!!!!!!!!!!!!!!!!', '\n', 
              '!!!!!!!!!! Unmatched with Number of Features!!!!!!!!!!!!!!!', '\n',
              '!!!!!!!!!! Use Default Set for Feature Names!!!!!!!!!!!!!!!', '\n',
              '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', '\n')
      
      FeatureNames = c()
      for (i in 1:row_num){
        feature = paste('Feature', i, sep = '')
        FeatureNames = append(FeatureNames, feature)
      }
    }
    
  }
  
  #loop through every vari(row)
  for(i in 1:row_num){
    vari = mdata[i,]
    
    if (sum(is.na(vari)!= 0)) {
      #if vari contains missing value
      
      message("\n")
      warning('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', '\n',
              '!!!!!!!!!!!!!!   Warning from AutoTransQF.R:   !!!!!!!!!!!!', '\n',
              '!!!!!!!!!!', FeatureNames[i], ' Contains Missing Value!!!!!!', '\n',
              '!!!!!       Returning Orignial Data          !!!!!!!!!!!!!!', '\n',
              '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!','\n')
      
      final_vari = vari
      text_k = "return original vector"
      
      transformed_data[i,] = final_vari
      
      transformation_info = paste(FeatureNames[i], ": ", text_k, sep = '')
      transformation = append(transformation, transformation_info)
      all_beta = append(all_beta, beta)
      all_alpha = append(all_alpha, alpha)
      
    } else if(abs(sd(vari)) < 1e-6 | (is.na(sd(vari)) == TRUE)){
      #if number of unique values is greater than 2
      
      message("\n")
      warning('!!!!!!!!!!!!!!!!!!!!!!!!!!!!!IIIIIIIIIIIIIIIII!!!!!!!!!!', '\n',
              '!!!    Warning from AutoTransQF.R:   !!!', '\n',
              '!!!          Standard deviation = 0            !!!', '\n',
              '!!!              Returning all zeros              !!!', '\n',
              '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!IIIIIIIIIIIIIIIII!!!!!!!!!!', '\n')
      
      final_vari = matrix(0, nrow = row_num, ncol = col_num)
      text_k =  'Return zero vector'
      
      transformed_data[i,] = final_vari[i,]
      transformation_info = paste('Feature', i, ': ', text_k, sep = '')
      transformation = append(transformation, transformation_info)
      all_beta = append(all_beta, beta)
      all_alpha = append(all_alpha, alpha)
      
    } else if(length(unique(vari)) <= 2){
      #test for binary data
      message("\n")
      warning(' !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!', '\n',
              '!!!!!!!!!!!!!!    Warning from AutoTransQF.R:    !!!!!!!!!!!', '\n',
              '!!!!!!!!!!!!!!!   Binary Variable               !!!!!!!!!!!', '\n',
              '!!!!!!!!!!!!!!!   Return original values         !!!!!!!!!!!', '\n',
              '!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!')
      final_vari = vari
      text_k = 'No Transformation'
      
      transformed_data[i,] = final_vari
      transformation_info = paste('Feature', i, ': ', text_k, sep = '')
      transformation = append(transformation, transformation_info)
      all_beta = append(all_beta, beta)
      all_alpha = append(all_alpha, alpha)
      
    } else {
      
      if (skewness(c(t(vari))) > 0){
        arrayindx = seq(0, 9, 0.01)
      } else {
        arrayindx = -seq(0, 9, 0.01)
      }
      
      func = function(paraindex) {
        autotransfuncQF(vari, istat, paraindex)
      }
      
      evals = list()
      for (k in arrayindx) {
        final_vari = func(k)
        
        if(istat == 1) {
          eval = ADStatQF(final_vari) 
        }else {
          #also need to implement skewness 
          eval = skewness(final_vari) 
        }
        evals = append(evals, eval)
      }
      
      #get the interquartile range of the values in the data vector
      iqrange_vari = IQR(vari, type = 2)
      
      #use the interquartile range as the data range if not zero 
      #otherwise use the full data range
      if(iqrange_vari == 0) {
        range_vari = range(vari)
      }else{
        range_vari = iqrange_vari
      }
      
      index = min(unlist(evals))$idx
      beta = sign(arrayindx[index]) * (exp(abs(arrayindx[index])) - 1)
      alpha = abs(1/beta) * range_vari
      
      final_vari = autotransfuncQF(vari, istat, arrayindx[index])
      value = evals[index]
      
      text_k = paste('Transformation Parameter Beta = ', beta, sep = '')
      
      transformed_data[i,] = final_vari
      transformation_info = paste('Feature', i, ': ', text_k, sep = '')
      transformation = append(transformation, transformation_info)
      all_beta = append(all_beta, beta)
      all_alpha = append(all_alpha, alpha)
    }
    
    #Display transformation information on screen
    if (iscreenwrite == 1) {
      message("\n")
      if(istat == 1) {
        message('************ Transformation of ', FeatureNames[i],' **********','\n',
                'Transformation Criterion: Minimize Log ', '(Anderson_Darling Test Statistic) (Standard Normal)', '\n')
      } else if(istat == 2) {
        message('************ Transformation of ', FeatureNames[i],' **********','\n',
                'Transformation Criterion: Minimize Skewness', '\n')
      }
      
      message('Log A-D Stat Before Transformation:', num2str(log(ADStatQF(vari))),'\n',
              'Skewness Before Transformation: ', num2str(skewness(vari)),'\n',
              'Selected Transformation: ', text_k,'\n',
              'Selected Transformation: Transformation Parameter alpha = ', alpha, '\n',
              'Skewness After Transformation: ', num2str(skewness(final_vari)),'\n',
              'Log A-D Stat After Transformation: ', num2str(log(ADStatQF(final_vari))),'\n',
              '*****************************************************','\n')
    }
    
  }
  return_value = list(data = transformed_data, beta =  all_beta, alpha = all_alpha)
  return(return_value)
}


