# this scenario investigates the statistics behind the Martingale Betting System,
# but in the case that all bets are the same amount, whether the previous was
# won or lost.
sim2<-function(m,c,n,iteration){
iteration = 1
itList = list()
moneyList = list()
roundList = list()
while (iteration <= 100){
	m = 200
	n = 200
	c = 5
	currRound = 1
	while (m-c >= 0 & currRound <= n){
		betChoice = floor(runif(1,1,3))
		if (betChoice == 1){
			m = m + c
			c = 5
		}else{
			m = m - c
			c = 5		
		}
		currRound = currRound + 1
	}
	itList[iteration] = iteration
	moneyList[iteration] = m
	roundList[iteration] = currRound - 1
	iteration = iteration + 1	
}

for (i in 1:100){
	print(paste("For iteration", itList[i], "the money remaining is", moneyList[i]))
}

mean = mean(unlist(moneyList))
variance = var(unlist(moneyList))
stanDev = sd(unlist(moneyList))
cat("The mean is", mean)
cat("The variance is", variance)
cat("The standard deviation is", stanDev)
hist(unlist(moneyList), ylim=c(1,50),
     main="Histogram for Money Remaining Using Martindale", 
     xlab="Money Remaining", 
     border="blue", 
     col="green",
     xlim=c(-200,1000),
     las=1, 
     breaks=50)
dataList = list(iteration, moneyList)
return(dataList)
}