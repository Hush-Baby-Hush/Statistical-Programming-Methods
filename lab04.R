#exercise 1

fizz_buzz = function(x){
  new = vector(length = length(x))
  for (i in 1:length(x)){
    if(x[i]%%3==0 && x[i]%%5==0){
      new[i] = "fizzbuzz"
    }else if (x[i]%%3==0){
      new[i] = "fizz"
    }else if(x[i]%%5==0){
      new[i] = "buzz"
    }else{
      new[i] = as.character(x[i])
    }
  }
  return(new)

}

fizz_buzz(x = 1:5)
fizz_buzz(x = 10:20)

#exercise 2

is_leap = function(year){
  for (ele in year){
    if(ele%%4==0 && ele%%400==0)
    {
      return(TRUE)
    }else if(ele%%4==0 && ele%%100!=0){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
}

is_leap((2012))

is_leap((2019))

is_leap((2020))

is_leap((2016))

is_leap((2000))

is_leap((2500))

#exercise 3

gen_mult_seq = function(first, multi, length){
  new = vector(length = length[1])
  if(length(new)>=1){
    new[1] = first[1]
  }
  if(length(new)>=2){
    new[2] = new[1]*multi[1]
  }
  if(length(new)>=3){
    for (i in 3:length[1]){
      new[i] = new[i-1]*multi[1]
    }
  }
  
  return(new)

}



gen_mult_seq((4.1),(2.5),(1))

gen_mult_seq((4.1),(2.5),(2))

gen_mult_seq((4.1),(2.5),(3))

gen_mult_seq((4.1),(2.5),(0))

gen_mult_seq((4.1),(2.5),(5))




