set.seed(42)
random_numbers = rnorm(1000)

#
add_then_power = function(a,b,c){
  result = (a+b)^c
  return (result)
}

#
calc_slash_line = function(single, double, triple, hr, bb, hbp, sf, ab) {
  avg = c((single + double + triple + hr) / ab)
  obp = c((single + double + triple + hr + bb + hbp) / (ab + bb + hbp + sf)) 
  slg = c((single + (2 * double) + (3 * triple) + (4 * hr)) / ab)  
  
  vector_ = c(avg = round(avg, digits = 3),obp = round(obp, digits = 3),slg = round(slg, digits = 3))
  vector_
}

#
a_silly_list = list(
  aa = matrix(1:4, nrow = 2),
  bb = 8,
  cc = list(x = c("Hello!"),y=c("World!"),z=42),
  dd = c(1:100)
  
)

#
between = function(x,a,b){
  arr = c()
  for(i in x){
    if((i>a) & (i<b)){
      arr = append(arr,i)
    }
  }
  if(length(arr)==0)
  {
    return(numeric(0))
  }
  return(arr)
}

outside = function(x,a,b){
  arr = c()
  for(i in x){
    if(i<a | i>b){
      arr = append(arr,i)
    }
  }
  return(arr)    
}

#
list_to_mod_df = function(lst,row_mod,row_delete){
  lst$df$a[row_mod] = lst$rep_1 
  lst$df$b[row_mod] = lst$rep_2
  lst$df$c[row_mod] = lst$rep_3
  lst$df = lst$df[-row_delete,]
  return(lst$df)
}

#
split_odd_even = function(x){
  odd = x[seq_along(x) %% 2 == 1]
  even = x[seq_along(x) %% 2 == 0]
  lst = list(odd = odd, even = even)
  return(lst)
}

#
calc_olympic_score = function(judges_scores) {
  
  # delete this comment and place your code here
  max_ = max(judges_scores)
  min_ = min(judges_scores)
  index_min = which(judges_scores %in% min_)
  
  judges_scores = judges_scores[-index_min[1]]
  index_max = which(judges_scores %in% max_)
  judges_scores = judges_scores[-index_max[1]]
  return(mean(judges_scores))
  
}

#
replace_an_element = function(x, to_rep, rep_with){
  idx = which(x %in% to_rep)
  x[idx] = rep_with
  return(x)
}

#
contain_word = function(x, word){
  return(any(x %in% word))
}

count_word = function(x, word){
  idx = which(x %in% word)
  return(length(idx))
}

#
assign_letter_grade = function(x, type = "decimal") {
  
  if (length(x) != 1) {
    stop("x must be a vector of length 1.")
  }
  
  if (!is.numeric(x)) {
    stop("x must be a numeric vector.")
  }
  
  # delete this comment and place your code here
  if(x>1){
    x = x/100.0
  }
  
  if(x>=0.90){
    return("A")
  }
  if(x<0.90 & x>=0.80){
    return("B")
  }
  if(x<0.70 & x>=0.60){
    return("D")
  }
  if(x<0.80 & x>=0.70){
    return("C")
  }
  else{
    return("F")
  }
  
}

#
fib_seq = function(length){
  if(length==1){
    return(c(1))  
  }
  if(length==2){
    return(c(1,1))
  }
  arr = c(1,1)
  i=3
  while((length-i+1)!=0){
    arr = append(arr,arr[i-1]+arr[i-2])
    i=i+1
  }
  return(as.integer(arr))
}

#
next_day_weather = function(current){
  if(current <= 70){
    return(c(current+3))
  }
  if(current<=72 & current>70){
    return(c(current-1))
  }
  if(current<=74 & current>72){
    return(c(current+1))
  }
  if(current<=76 & current>74){
    return(c(current+1))
  }
  if(current<=78 & current>76){
    return(c(current+3))
  }
  if(current<=80 & current>78){
    return(c(current-5))
  }
  if(current>80){
    return(c(current-2))
  }
}




generate_forecast = function(first_day,num_days){
  if(num_days==1){
    return(c(first_day))
  }
  arr = c(first_day)
  for(i in c(1:(num_days-1))){
    arr = append(arr,next_day_weather(arr[i]) )
  }
  return(arr)
}

#
solve_quadratic = function(a, b, c) {
  
  # delete this comment and place your code here
  
  deta = b^2-4*a*c
  if(deta<0){
    x = -b/2.0/a+(-deta)^0.5/a/2.0i
    y = -b/2.0/a-(-deta)^0.5/a/2.0i   
    return(c(y,x))
  }
  if(deta==0){
    return(c(-b/2.0/a))
  }
  
  x = -b/2.0/a+(deta)^0.5/a/2.0
  y = -b/2.0/a-(deta)^0.5/a/2.0
  
  return(c(x,y))
}

#
count_collatz_steps = function(n) {
  
  if (length(n) != 1) {
    stop("n must be a vector of length 1.")
  }
  
  if (n < 1 | (n %% 1) != 0) {
    stop("n must be a positive integer.")
  }
  
  # delete this comment and place your code here
  step = 0
  while(n!=1){
    if(n%%2==0){
      n=n/2
    }
    else{
      n = n*3+1
    }
    step=step+1
  }
  return(step)
  
  
  
}

#
is_prime = function(x) {
  
  # check vector length and mode
  if (!(is.numeric(x) & length(x) == 1)) {
    stop("x must be a numeric vector of length 1")
  }
  
  # trick to allow integers input as doubles to be accepted
  if (as.integer(x) != x) {
    stop("values that cannot be represented as an integer are not allowed")
  }
  
  # only consider non-negative numbers
  if (x < 0) {
    stop("x must be non-negative")
  }
  
  # delete this comment and place your code here
  if(x==1){return(FALSE)}
  if(x==2){return(TRUE)}
  for (i in 2:(x-1)) {
    if(x%%i==0){return(FALSE)}
  }
  return(TRUE)  
}

find_primes = function(x) {
  
  if (!is.numeric(x)) {
    stop("x must be a numeric vector")
  }
  
  # delete this comment and place your code here
  arr = c()
  for(i in x){
    if(is_prime(i)){
      arr = append(arr,i)
    }
  }
  return(arr)  
}

#
selection_sort = function(x) {
  
  if (!is.numeric(x)) {
    stop("x must be a numeric vector")
  }
  
  # delete this comment and place your code here
  arr = vector(,length(x));
  for (i in 1:length(x)) {
    idx = which.min(x)
    arr[i] = x[idx]
    x = x[-idx]
  }
  return(arr)
  
}

#
check_div = function(x, y) {
  
  # delete this comment and place your code here
  if(!is.numeric(x) || !is.numeric(y)){
    stop("x and y must be a numeric vectors")
  }
  if(length(x)!=length(y)){
    stop("x and y must have the same length")
  }
  if(length(x)>1){
    warning("doing element-by-element comparison")
  }
  return(x %% y == 0)
  
}

#
is_prime = function(x) {
  
  # check vector length and mode
  if (!(is.numeric(x) & length(x) == 1)) {
    stop("x must be a numeric vector of length 1")
  }
  
  # trick to allow integers input as doubles to be accepted
  if (as.integer(x) != x) {
    stop("values that cannot be represented as an integer are not allowed")
  }
  
  # only consider non-negative numbers
  if (x < 0) {
    stop("x must be non-negative")
  }
  
  # delete this comment and place your code here
  if(x==1){return(FALSE)}
  if(x==2){return(TRUE)}
  
  ans = TRUE
  ans = lapply(2:(x-1), function(f){
    if(x%%f==0){return(FALSE)}
    return(TRUE)
  })
  return(!any(ans==FALSE))
}

find_primes = function(x) {
  
  if (!is.numeric(x)) {
    stop("x must be a numeric vector")
  }
  
  # delete this comment and place your code here
  
  ans = sapply(x,function(g){
    if(is_prime(g)){
      return(g)
    }
    return(-1)
  })
  ans = ans[which(ans!=-1)]
  
  return(ans)  
}


#
assign_letter_grade = function(x, type = "decimal") {
  
  if (length(x) != 1) {
    stop("x must be a vector of length 1.")
  }
  
  if (!is.numeric(x)) {
    stop("x must be a numeric vector.")
  }
  
  if (type == "percent") {
    x = x / 100
  }
  
  if (x >= 0.90) {
    "A"
  } else if (x >= 0.80) {
    "B"
  } else if (x >= 0.70) {
    "C"
  } else if (x >= 0.60) {
    "D"
  } else {
    "F"
  }
  
}

# delete this comment and place your code here
create_gradebook = function(num_grades, type){
  if(type=="decimal"){
    decimal = num_grades
    percent = num_grades*100
    letter = sapply(num_grades, assign_letter_grade, type=type)
    df <- data.frame(decimal,percent,letter)
  }
  if(type=="percent"){
    decimal = num_grades/100
    percent = num_grades
    letter = sapply(num_grades, assign_letter_grade,type=type)
    df <- data.frame(decimal,percent,letter)
  }
  
  return(df)
}

#
calc_cumulative = function(x, operation = sum) {
  
  if (!is.numeric(x)) {
    stop("x must be a numeric vector")
  }
  
  if (!is.function(operation)) {
    stop("operation must be a function")
  } else if (!is.numeric(operation(x))) {
    stop("operation must return a numeric vector")
  } else if (length(operation(x)) != 1) {
    stop("operation must return a numeric vector of length 1")
  }
  
  # delete this comment and place your code here
  arr = c()
  for (i in 1:length(x)) {
    arr = append(arr,operation(x[1:i]))
  }
  return(arr)  
}
