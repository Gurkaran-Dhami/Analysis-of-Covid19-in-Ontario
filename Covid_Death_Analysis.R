#Simply reading in the data file, and creating a data frame with the Age group and Outcome 
#data points
data <- read.csv("conposcovidloc.csv", header = TRUE)
outcome <- data$Outcome1
age <- data$Age_Group
table <- data.frame(outcome, age)

#Understanding age group of the fatalities that occured, and total number of fatalities 
#that occured as of September 29th 2020
deaths <- 0
for (values in data$Outcome1){
  if (values == "Fatal") {
    deaths = deaths + 1
  }
}
print(deaths)

#Now that we have the total number of deaths, we can create a new list which stores the age group
# of those patients that passed away.
desired_length <- deaths
fatality_age_list_raw <-vector(mode = "list", length = desired_length)
index <- 1
counter <- 1
for (values in data$Outcome1){
  if (values == "Fatal") {
    fatality_age_list_raw[counter] <- table$age[index]
    index = index + 1
    counter = counter + 1
  }
  else {
    index = index + 1
  }
}

#The list created above isn't very clear since each element is a string, so we will 
#create a "clean" list consisting of integer values which allows our calculations to be easier.
fatality_age_list <- vector(mode = "list", length = desired_length)
counter <- 1
for (values in fatality_age_list_raw) {
  if (values == "20s" | values == "20") {
    fatality_age_list[counter] <- 20
    counter = counter + 1
  }
  if (values == "30s" | values == "30") {
    fatality_age_list[counter] <- 30
    counter = counter + 1
  }
  if (values == "40s" | values == "40") {
    fatality_age_list[counter] <- 40
    counter = counter + 1
  }
  if (values == "50s" | values == "50") {
    fatality_age_list[counter] <- 50
    counter = counter + 1
  }
  if (values == "60s" | values == "60") {
    fatality_age_list[counter] <- 60
    counter = counter + 1
  }
  if (values == "70s" | values == "70") {
    fatality_age_list[counter] <- 70
    counter = counter + 1
  }
  if (values == "80s" | values == "80") {
    fatality_age_list[counter] <- 80
    counter = counter + 1
  }
  if (values == "90s" | values == "90") {
    fatality_age_list[counter] <- 90
    counter = counter + 1
  }
  if (values == "<20s") {
    fatality_age_list[counter] <- 20
    counter = counter + 1
  }
}
#Get rid of any possible null values
fatality_age_list <- fatality_age_list[!sapply(fatality_age_list,is.null)] 
#Lets take a look at the distribution of deaths in ontario, lets turn our fatality age list to a vector
vec_list <- unlist(fatality_age_list)
hist(vec_list)
#The number of deaths seem very skewed towards a more older generation which is typical. Lets find the exact mean age of death
fatality_age_list <- fatality_age_list[!sapply(fatality_age_list,is.null)]
sum_xi <- 0
n <- length(fatality_age_list)
for (values in fatality_age_list) {
  sum_xi = sum_xi + values
}
mean_fatality_age <- sum_xi/n
print(mean_fatality_age)

#As we can see, the mean age of fatlity is 78.0197, however the real number is higher 
#since the given data from Ontario's COVID19 page didn't give the exact age of the patients 
#to account for confidentiality. Instead data was given in the "80s" meaning the age of the 
#patient could be anywhere from 81 to 89, hence resulting in a smaller mean fatality age. 
#Next, lets find the standard deviation.
var_sum <- 0
for (values in fatality_age_list) {
  var_sum = var_sum + (values - mean_fatality_age)^2
}
standard_dev_deaths <- sqrt(var_sum/n-1)
print(standard_dev_deaths)

#Now, since we have the mean and standard deviation we can assume normality (explained later) 
#and lets try plotting this data to give a better, and more visual representation 
#of fatality rates, and COVID19. In order to do this though we need to convert our numerical 
#list into a vector.
y <- pnorm(vec_list, mean_fatality_age, standard_dev_deaths)
plot(vec_list, y, main = "COVID19 and Probability of Fatality in Ontario")

#Lets conduct a hypothesis test now to see if with 95% confidence the mean age of death from 
#COVID19 in Ontario is 78 years old 
#Since the sample size is large either a Z test or a T test is fine. So in this case
#we will use a Z test.
#H_0 = 78
#H_a != 78
test_statistic <- sqrt(deaths)*(mean_fatality_age - 78)/standard_dev_deaths
p_value <- 2*pnorm(-abs(test_statistic))
print(p_value)
#Since the p-value = 0.9287 which is larger than the alpha = 0.05, 
#we must reject the null. Hence, the mean age of fatality is not 78

#Since this is a two tailed test,lets make a 95% confidence interval around 
#our calculated mean to see what the lower/upper bound is.
lower_bound <- mean_fatality_age - 1.96*(standard_dev_deaths/deaths)
upper_bound <- mean_fatality_age + 1.96*(standard_dev_deaths/deaths)
confidence_int <- c(lower_bound, upper_bound)
print(confidence_int)
#Hence, with 95% confidence we can say that the mean age of death occuring 
#from COVID19 is within (78.0116, 78.0278).

