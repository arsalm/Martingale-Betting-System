# this scenario investigates the statistics behind the Martingale Betting System,
# but in the case that if the player wins, then the bet amount returns to 5, and
# if the player loses, then the next bet is double the previous bet to recoup
# losses.
sim1<-function(m,c,n,iteration){
iteration = 1
itList = list()
moneyList = list()
roundList = list()
while (iteration <= 100){
	m = 200 #starting amount of money
	n = 200 #number of rounds
	c = 5 #initial bet amount
	currRound = 1
	while (m-c >= 0 & currRound <= n){
		betChoice = floor(runif(1,1,3)) #this will select 1 random integer from 1 to 2 and set it equal to betchoice
		if (betChoice == 1){ #if betchoice = 1 then the player wins, if not then the player loses the bet
			m = m + c #total money is now the bet plus the previous amount of money
			c = 5 #bet stays at initial amount if the player wins
		}else{
			m = m - c #the player will lose the bet amount
			c = 2*c #the new bet must be double the previous bet to make up for the losses
		}
		currRound = currRound + 1
	}
	itList[iteration] = iteration
	moneyList[iteration] = m
	roundList[iteration] = currRound - 1
	iteration = iteration + 1	
}

# this will dispaly the amount of money remaining after each iteration. an iteration ends when the player either runs out
# of money or when the player plays 200 rounds of betting, whichever comes first.
for (i in 1:100){
	print(paste("For iteration", itList[i], "the money remaining is", moneyList[i]))

}

mean = mean(unlist(moneyList))
variance = var(unlist(moneyList))
sd = sd(unlist(moneyList))
cat("The mean is", mean)
cat("The variance is", variance)
cat("The standard deviation is", stanDev)
hist(unlist(moneyList), ylim=c(1,100),
     main="Histogram for Money Remaining Using Martindale", 
     xlab="Money Remaining", 
     border="blue", 
     col="green",
     xlim=c(-200,1000),
     las=1, 
     breaks=100)
dataList = list(iteration, moneyList)
return(dataList)
}