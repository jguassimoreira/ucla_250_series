###Function Examples

#Write a function to return a mean
user_mean = function(userData) {
  
  ##this function takes in data, computes their mean, and returns it
  
  um = sum(userData)/length(userData)
  return(um)
  
}

#Now write a function that doesn't take missing data
user_mean_no_na = function(userData) {
  
  ##this function takes a vector of data, computes their mean, and returns a scalar value
  ##Does not support missing data, 
  
  if (sum(is.na(x)) > 0) {
    
    stop("function does not take missing values") #error message
    
  } else {
    
    um = sum(userData)/length(userData) #compute mean 
    return(um) #return mean
    
  }
  
}