#Exercise 1

find_divisors = function(x){
  if(x==1){
    return(NULL)
  }
  arr = 1:(x[1]-1)
  idx = which(x%%arr==0)
  return(arr[idx])
}

find_divisors((7))
find_divisors((1))
find_divisors((2))
find_divisors((20))
find_divisors((25))

is_perfect = function(x){
  arr = find_divisors(x)
  if(sum(arr)==x){
    return((TRUE))
  }
  return((FALSE))
}
is_perfect((6))
is_perfect((7))
is_perfect((1))
is_perfect((2))
is_perfect((20))
is_perfect((25))

is_abundant = function(x){
  arr = find_divisors(x)
  if(sum(arr)>x){
    return((TRUE))
  }
  return((FALSE))  
}
is_abundant((7))
is_abundant((1))
is_abundant((2))
is_abundant((20))
is_abundant((25))

is_deficient  = function(x){
  arr = find_divisors(x)
  if(sum(arr)<x){
    return((TRUE))
  }
  return((FALSE))  
}
is_deficient((7))
is_deficient((1))
is_deficient((2))
is_deficient((20))
is_deficient((25))

data.frame(
  x = 1:25,
  perfect = sapply(1:25, is_perfect),
  abundant = sapply(1:25, is_abundant),
  deficient = sapply(1:25, is_deficient)
)

#Exercise 2

split_digits = function(x) {
  
  if (!is.numeric(x) | length(x) != 1 | trunc(x) != x) {
    stop("x stop be a numeric vector (that represents an \"integer\") of length 1")
  }
  
  return(as.numeric(strsplit(as.character(x), split = "")[[1]]))
  
}

is_valid = function(num) {
  
  if (length(split_digits(x = num)) < 2) {
    stop("input numbermust have at least two digits")
  }
  
  # delete this comment and place your code here
  check_digit = num%%10
  remain = (num-check_digit)/10
  arr = rev(split_digits(remain))

  multiplier2 = arr[seq_along(arr)%%2==1]*2
  multiplier1 = arr[seq_along(arr)%%2==0]
  idx = which(multiplier2>9)
  origin = multiplier2[which(multiplier2<10)]
  s = 0
  for (i in idx) {
    s = s + sum(split_digits(multiplier2[i]))
  }
  s = s + sum(multiplier1) + sum(origin)
  result = 10-s%%10

  if(result==check_digit){
    return(TRUE)
  }
  return(FALSE)
  
}

is_valid(num = 79927398713)      # should return TRUE
is_valid(num = 4539319503436467) # should return TRUE
is_valid(num = 8273123273520569) # should return FALSE












