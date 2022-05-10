pnorm(2,25*0.1,sqrt(25*0.1*(1-0.1)))
pbinom(2,25,0.1)

num_data <- c(43,59,22,25,36,47,19,21)
mean_lst <- c()
i = 1
tot_mean = repeat{
  random_data = sample(x=num_data,size=10,replace = TRUE)
  random_mean = mean(random_data)
  mean_lst[i] <- random_mean
  i <- i+1
  if(i > 20) {
    break
    }
}
print(mean_lst)
print(mean(mean_lst))
