calc_volume_sphere = function(radius=4){
  vol = 4/3*pi*radius^3
  return(vol)
}

calc_volume_sphere(1)



set.seed(42)
vec = round(10 * runif(n = 100), 2)

positive = vec[c(2,17,21)]
negative = vec[-c(6,13,30)]

rep_and_concat_string = function(string, times) {
  
  # delete this comment and place your code here
  
  if(!is.character(string) || length(string)!=1){
    stop("string must be a character vector of length 1")
  }
  
  if(!is.numeric(times) || length(times)!=1){
    stop("times must be a numeric vector of length 1")
  }
  
  if(length(string)*times>100){
    warning("this will produce a very long string")
  }  
  
  
  strrep(string, times)
  
}
rep_and_concat_string("abc", times = 50)

some_df = data.frame(
  f = rep(415,31),
  h = rep("STAT 385", 31),
  r = 1:31
  
)


gen_seq = function(first,len){
  if(len==1){
    return(first)
  }
  
  arr = vector(,length = len)
  arr[1] = first
  for(i in 2:len){
    val = 4*arr[i-1]+15
    arr[i]=log(val)
  }
  
  return(arr)
  
}

gen_seq(4,10)



some_vector = c(7,172,1001,1039,1042)

set.seed(42)
long_vec = round(100 * runif(n = 200), 2)

even = long_vec[seq_along(long_vec)%%2==0]
large = long_vec[long_vec>75]


check_vec_total = function(x){
  if(sum(x)<=13){
    return("small total")
  }
  
  if(sum(x)>13 & sum(x)<=28){
    return("medium total")
  }
  if(sum(x)>28){
    return("large total")
  }
  
  
  
}

check_vec_total(1:5)


some_list = list(
  v = 42,
  a = "Exams are fun!",
  r = 6:255
)



