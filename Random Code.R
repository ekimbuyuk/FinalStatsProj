
# Dataframe 1: House + Cold Weather + New Owner Doggos
```{r}
data1 <- final_stars_data %>% 
  filter(GoodForNoviceOwners == 0 | GoodForNoviceOwners == 1 | GoodForNoviceOwners == 2) %>% 
  filter(ToleratesColdWeather == 0 | ToleratesColdWeather == 1 | ToleratesColdWeather == 2)
```

# Dataframe 2: House + Hot Weather + New Owner Doggos
```{r}
data2 <- final_stars_data %>% 
  filter(GoodForNoviceOwners == 0 | GoodForNoviceOwners == 1 | GoodForNoviceOwners == 2) %>% 
  filter(ToleratesHotWeather == 0 | ToleratesHotWeather == 1 | ToleratesHotWeather == 2)
```

# Dataframe 3: House + Cold Weather + Old Owner Doggos
```{r}
data3 <- final_stars_data %>% 
  filter(ToleratesColdWeather == 0 | ToleratesColdWeather == 1 | ToleratesColdWeather == 2)
```

# Dataframe 4: House + Hot Weather + Old Owner Doggos
```{r}
data4 <- final_stars_data %>%
  filter(ToleratesHotWeather == 0 | ToleratesHotWeather == 1 | ToleratesHotWeather == 2)
```

# Dataframe 5: Apartment/Dorm + Hot Weather + New Owner Doggos
```{r}
data5 <- final_stars_data %>% 
  filter(AdaptsWelltoApartmentLiving == 0 | AdaptsWelltoApartmentLiving == 1 | AdaptsWelltoApartmentLiving == 2) %>%
  filter(GoodForNoviceOwners == 0 | GoodForNoviceOwners == 1 | GoodForNoviceOwners == 2) %>% 
  filter(ToleratesHotWeather == 0 | ToleratesHotWeather == 1 | ToleratesHotWeather == 2)
```

# Dataframe 6: Apartment/Dorm + Cold Weather + New Owner Doggos
```{r}
data6 <- final_stars_data %>% 
  filter(AdaptsWelltoApartmentLiving == 0 | AdaptsWelltoApartmentLiving == 1 | AdaptsWelltoApartmentLiving == 2) %>%
  filter(GoodForNoviceOwners == 0 | GoodForNoviceOwners == 1 | GoodForNoviceOwners == 2) %>% 
  filter(ToleratesColdWeather == 0 | ToleratesColdWeather == 1 | ToleratesColdWeather == 2)
```


# Dataframe 7: Apartment/Dorm + Hot Weather + Old Owner Doggos
```{r}
data7 <- final_stars_data %>% 
  filter(AdaptsWelltoApartmentLiving == 0 | AdaptsWelltoApartmentLiving == 1 | AdaptsWelltoApartmentLiving == 2) %>%
  filter(ToleratesHotWeather == 0 | ToleratesHotWeather == 1 | ToleratesHotWeather == 2)
```

# Dataframe 8: Apartment/Dorm + Cold Weather + Old Owner Doggos
```{r}
data8 <- final_stars_data %>% 
  filter(AdaptsWelltoApartmentLiving == 0 | AdaptsWelltoApartmentLiving == 1 | AdaptsWelltoApartmentLiving == 2) %>%
  filter(ToleratesColdWeather == 0 | ToleratesColdWeather == 1 | ToleratesColdWeather == 2)
```


```{r}
# Adjusting Dataframe to Form Clustering Algorithms

datalist = list()

for(i in names(clustered_groups)){
  dat <- order(clustered_groups[i],decreasing = T)
  datalist[[i]] <- dat
}
datalist <- as.data.frame(datalist)

datalist[datalist == 1] <- "AdaptsWelltoApartmentLiving"
datalist[datalist == 2] <- "GoodForNoviceOwners"
datalist[datalist == 3] <- "SensitivityLevel"
datalist[datalist == 4] <- "ToleratesBeingAlone"
datalist[datalist == 5] <- "ToleratesColdWeather"
datalist[datalist == 6] <- "ToleratesHotWeather"
datalist[datalist == 7] <- "AffectionatewithFamily"
datalist[datalist == 8] <- "IncrediblyKidFriendlyDogs"
datalist[datalist == 9] <- "DogFriendly"
datalist[datalist == 10] <- "FriendlyTowardStrangers"
datalist[datalist == 11] <- "AmountOfShedding"
datalist[datalist == 12] <- "DroolingPotential"
datalist[datalist == 13] <- "EasyToGroom"
datalist[datalist == 14] <- "GeneralHealth"
datalist[datalist == 15] <- "PotentialForWeightGain"
datalist[datalist == 16] <- "Size"
datalist[datalist == 17] <- "EasyToTrain"
datalist[datalist == 18] <- "Intelligence"
datalist[datalist == 19] <- "PotentialForMouthiness"
datalist[datalist == 20] <- "PreyDrive"
datalist[datalist == 21] <- "TendencyToBarkOrHowl"
datalist[datalist == 22] <- "WanderlustPotential"
datalist[datalist == 23] <- "EnergyLevel"
datalist[datalist == 24] <- "Intensity"
datalist[datalist == 25] <- "ExerciseNeeds"
datalist[datalist == 26] <- "PotentialForPlayfulness"

test <- datalist[1:5,]
test1 <- stack(test)
check <- as.data.frame(table(test1$values))
```
#Applying Mixed Membership Model To Data:

```{r}
#Initialize a mixedMem Model 
Total <- 216
J <- 26
Rj <- rep(1, J)
Nijr <- array(1, dim = c(Total, J, max(Rj))) #multinomial so rank is 1 for each
K <- 20 
Vj <- rep(5, J) #5 choices for each variable 
alpha <- rep(.2, K) #initalize alpha to .2
dist <- rep("multinomial", J) #all variables are multinomial 
obs <- array(0, dim = c(Total, J, max(Rj), max(Nijr)))
obs[, , 1, 1] <- as.matrix(final_stars_data)

# Initialize theta randomly with Dirichlet distributions 
set.seed(112718)
theta <- array(0, dim = c(J, K, max(Vj))) 
for (j in 1:J) {
  theta[j, , ] <- gtools::rdirichlet(K, rep(.8, Vj[j])) 
}

initial <- mixedMemModel(Total = Total, J = J, Rj = Rj,
                         Nijr = Nijr, K = K, Vj = Vj, alpha = alpha,
                         theta = theta, dist = dist, obs = obs)
```

```{r}
computeELBO(initial)
st = proc.time()
out <- mmVarFit(initial, printStatus = 1, printMod = 25)

summary(out)
out$alpha

new_data = list()

#Get the values for tendencies for each group:
for (i in 1:20) {
  popAgree <- out$theta[, i, 1]
  new_data[[i]] <- popAgree 
}

clustered_groups <- as.data.frame(new_data)
rownames(clustered_groups) <- colnames(final_stars_data)
colnames(clustered_groups) <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)

lambda.point <- out$phi / rowSums(out$phi)
test <- as.data.frame(lambda.point)
rownames(test) <- rownames(final_stars_data)
colnames(test) <- c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
```

```{r}
pop1VarOrder <- colnames(final_stars_data)[order(out$theta[, 1, 1], decreasing = F)]
pop1VarAgree <- sort(out$theta[, 1, 1], decreasing = F)
barplot(height = pop1VarAgree, names.arg = pop1VarOrder,
        main = "Propensity to Agree",
        cex.names = .7, las = 2, xlab = "Value Statements",
        ylab = expression(paste(theta["j,14,1"])),
        col = ifelse(pop1VarAgree > .5, "forestgreen", "darkred"))

pop2VarOrder <- colnames(final_stars_data)[order(out$theta[, 2, 1], decreasing = F)]
pop2VarAgree <- sort(out$theta[, 2, 1], decreasing = F)
barplot(height = pop2VarAgree,
        names.arg = pop2VarOrder, main = "Propensity to Agree",
        cex.names = .7, las = 2, xlab = "Value Statements",
        ylab = expression(paste(theta["j,2,1"])),
        col = ifelse(pop2VarAgree > .5, "forestgreen", "darkred"))


lambda.point <- out$phi / rowSums(out$phi)
sum( (lambda.point[, 1] > .4) & (lambda.point[, 2] > .4))
relativeFrequency <- out$alpha / sum(out$alpha)
hellingerDist <- (1/sqrt(2)) * sqrt(rowSums( (sqrt(out$theta[, 1, ])
                                              - sqrt(out$theta[, 2, ]))^2))

barplot(sort(hellingerDist, decreasing = T),
        names.arg = colnames(final_stars_data)[order(hellingerDist, decreasing = T)],
        main = "Hellinger Distance",
        cex.names = .7, las = 2, ylab = "Hellinger Distance",
        ylim = c(0, 1))

plot(out, type = "membership", indices = c(1:20), nrow = 5, ncol = 4, fitNames = c("mixedMem"))

index <- order(lambda.point[, 1])
var.Mem <- out$phi[, 1] * (rowSums(out$phi) -
                             out$phi[, 1]) /
  (rowSums(out$phi)^2 * (rowSums(out$phi) + 1))

plot(sort(lambda.point[, 1]))

ci_up <- qbeta(.975, out.permute$phi[index, 1], rowSums(out$phi[index, c(2:3)])) 
ci_low <- qbeta(.025, out$phi[index, 1], rowSums(out$phi[index, c(2:3)]))

#goal is to drop in the actual points im interested in into the different groups identified. So if u answer a certain way, which group do u fall into? 
```

```{r}
mixedMemfunction = function(Total, J, K, dataset) {
  Rj <- rep(1, J)
  Nijr <- array(1, dim = c(Total, J, max(Rj))) #multinomial so rank is 1 for each
  Vj <- rep(5, J) #5 choices for each variable 
  alpha <- rep(.2, K) #initalize alpha to .2
  dist <- rep("multinomial", J) #all variables are multinomial 
  obs <- array(0, dim = c(Total, J, max(Rj), max(Nijr)))
  obs[, , 1, 1] <- as.matrix(dataset)
  set.seed(112718)
  theta <- array(0, dim = c(J, K, max(Vj))) 
  for (j in 1:J) {
    theta[j, , ] <- gtools::rdirichlet(K, rep(.8, Vj[j])) 
  }
  initial <- mixedMemModel(Total = Total, J = J, Rj = Rj,
                           Nijr = Nijr, K = K, Vj = Vj, alpha = alpha,
                           theta = theta, dist = dist, obs = obs)
  computeELBO(initial)
  st = proc.time()
  out <- mmVarFit(initial, printStatus = 1, printMod = 25)
  return(out)
}

#Initialize a mixedMem Model 
Total <- 216
J <- 9
Rj <- rep(1, J)
Nijr <- array(1, dim = c(Total, J, max(Rj))) #multinomial so rank is 1 for each
K <- 24
Vj <- rep(5, J) #5 choices for each variable 
alpha <- rep(.2, K) #initalize alpha to .2
dist <- rep("multinomial", J) #all variables are multinomial 
obs <- array(0, dim = c(Total, J, max(Rj), max(Nijr)))
obs[, , 1, 1] <- as.matrix(new_data)

# Initialize theta randomly with Dirichlet distributions 
set.seed(112718)
theta <- array(0, dim = c(J, K, max(Vj))) 
for (j in 1:J) {
  theta[j, , ] <- gtools::rdirichlet(K, rep(.8, Vj[j])) 
}

initial <- mixedMemModel(Total = Total, J = J, Rj = Rj,
                         Nijr = Nijr, K = K, Vj = Vj, alpha = alpha,
                         theta = theta, dist = dist, obs = obs)
computeELBO(initial)
st = proc.time()
out <- mmVarFit(initial, printStatus = 1, printMod = 25)

lambda.point <- out$phi / rowSums(out$phi)
```

