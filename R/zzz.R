abs_biased <- function(x,y,bias){
    diff <- (x-y)
    diff <- ifelse(diff<0, abs(diff+bias), abs(diff))
    return(diff)
}

vars_id_obs <- function(data,...){
  N <- nrow(data)
  K <- data %>% 
    ungroup() %>% 
    select(...) %>% 
    distinct() %>% 
    nrow()
  if(N==K){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

mean_w <- function(x,w){
    sum(x*w)/sum(w)
    }

sum_w <- function(x,w){
    sum(x*w)
    }
