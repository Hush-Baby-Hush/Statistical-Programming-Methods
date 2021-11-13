ex = list(
  df = data.frame(a = 1:10,
                  b = "Hello!",
                  c = c(TRUE, FALSE)),
  rep_1 = 42,
  rep_2 = "World!",
  rep_3 = NA
)





list_to_mod_df = function(lst,row_mod,row_delete){
  lst$df$a[row_mod] = lst$rep_1 
  lst$df$b[row_mod] = lst$rep_2
  lst$df$c[row_mod] = lst$rep_3
  lst$df = lst$df[-row_delete,]
  return(lst$df)
}


lst = ex
row_mod = 4
row_delete = 8

lst$df$a[row_mod] = lst$rep_1 



list_to_mod_df(lst = ex, row_mod = 4, row_delete = 8)


i = 42
j = 55
k = 11

if (i == 42) {
  j = -i
} else {
  i = 66
}

if (i != j) {
  i = j
} else {
  i = i + j
  j = 0
}

if (i > 42) {
  k = 42
  j = 0
} else if (j < 42) {
  j = i + k
  k = -i
} else {
  i = "hello world"
}


x = 0
i = 0
while(x >= 0) {
  x = x + 2
  if (x > 100) {
    x = -x
  }
  i = i + 1
}


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


generate_forecast(first_day = 90, num_days = 10)




solve_quadratic = function(a, b, c) {
  
  # delete this comment and place your code here
  
  deta = b^2-4*a*c
  if(deta<0){
    return()
  }
  if(deta==0){
    return(c(-b/2.0/a))
  }
  
  x = -b/2.0/a+(-deta+0i)^0.5/a/2.0i
  y = -b/2.0/a-(-deta+0i)^0.5/a/2.0i   
  
  return(c(x,y))
}

solve_quadratic(4,10,3)






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
  
find_primes(x = 1:10)

eval_polynomial = function(coefs, x){
  i = 0
  ans = 0
  for (m in coefs) {
    ans = ans + m*x^i
    i=i+1
  }
  return(ans)
}
some_coefs = c(3, 4, 5, 6)
eval_polynomial(coefs = some_coefs, x = 2)

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

calc_cumulative(x = c(2, 4, 6), operation = prod)
calc_cumulative(x = 1:10, operation = sum)
calc_cumulative(x = c(10, 11, 10, 5, 12), operation = max)

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

selection_sort(x = c(2, 2, 1, 1, 4, 6, 8, 2, 10))
selection_sort(x = 10:1)

calculate_pi = function(xs, ys) {
  
  # delete this comment and place your code here
  count = 0
  for (i in 1:length(xs)) {
    if(xs[i]^2+ys[i]^2<=1){
      count = count+1
    }
  }
  
  return(4*count/length(xs))
}


set.seed(42)
x = runif(n = 1000)
y = runif(n = 1000)
calculate_pi(xs = x, ys = y)



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





create_gradebook(num_grades = c(0.77, 0.88, 0.99), type = "decimal")

library(dplyr)
starwars = dplyr::starwars
# delete this comment and place your code here
sw_df = starwars %>% 
  count(homeworld)













