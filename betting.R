#Read last year's results
results2017 = read.table('results2017-18.csv',sep=',',header=T,stringsAsFactors = F, nrow=1271)

#Run the regression from BWS data to get team scores
source('regression.R')

#Convert string to POSIX
results2017$Date = as.Date(results2017$Date)

#Add columns for new variable
results2017$Visitor.B2B = numeric(1271)
results2017$Home.B2B = numeric(1271)
results2017$Home.Win.Prob= numeric(1271)

for(i in 1:1271){
  results2017$Visitor.B2B[i] = if(results2017$Visitor[i] %in% results2017[results2017$Date == (results2017$Date[i]-1),2] | 
                                  results2017$Visitor[i] %in% results2017[results2017$Date == (results2017$Date[i]-1),4]) {1} else {0}
  results2017$Home.B2B[i] = if(results2017$Home[i] %in% results2017[results2017$Date == (results2017$Date[i]-1),2] | 
                               results2017$Home[i] %in% results2017[results2017$Date == (results2017$Date[i]-1),4]) {1} else{0}
  results2017$Home.Win.Prob[i] = exp(logitResultDF$value[logitResultDF$team == results2017$Home[i]]) / (exp(logitResultDF$value[logitResultDF$team == results2017$Home[i]]) + exp(logitResultDF$value[logitResultDF$team == results2017$Visitor[i]]))
}

results2017$Home.Win = results2017$G.1 > results2017$G

plusModel = glm(Home.Win ~ Home.Win.Prob + Visitor.B2B + Home.B2B, data=results2017)

results2017$plusModel = fitted(plusModel)

winProb = data.frame(p = seq(0,1,by=0.05),
                     expected = numeric(21),
                     expected.plus = numeric(21),
                     observed = numeric(21))

for(i in 2:21){
  pSample = results2017[results2017$Home.Win.Prob > winProb$p[i-1] & results2017$Home.Win.Prob < winProb$p[i],]
  winProb$expected[i] = mean(pSample$Home.Win.Prob)
  winProb$expected.plus[i] = mean(pSample$plusModel)
  winProb$observed[i] = sum(pSample$Home.Win) / length(pSample$G)
}

p = ggplot(winProb[2:21,])
p + geom_point(aes(x = observed, y=expected), color='blue') +
  geom_point(aes(x = observed, y=expected.plus), color='red') +
  xlim(c(0,1)) + ylim(c(0,1)) +
  geom_abline(slope=1,intercept=0,linetype='dotdash')

threshold = 0.6
(sum(results2017$plusModel > threshold & results2017$Home.Win == T) + sum(results2017$plusModel < (1-threshold) & results2017$Home.Win == F)) / (sum(results2017$plusModel > threshold) + sum(results2017$plusModel < (1-threshold)))
(sum(results2017$Home.Win.Prob > threshold & results2017$Home.Win == T) + sum(results2017$Home.Win.Prob < (1-threshold) & results2017$Home.Win == F)) / (sum(results2017$Home.Win.Prob > threshold) + sum(results2017$Home.Win.Prob < (1-threshold)))

#Read this year's schedule
results2018 = read.table('results2018-19.csv',sep=',',header=T,stringsAsFactors = F, nrow=1271)

#Convert dates to date type
results2018$Date = as.Date(results2018$Date)

#Cut down to just today's games (or whatever interval we're predicting)
today = as.Date("2018-10-03")
todaysGames = results2018[results2018$Date %in% today,]

#Add new columns
nGames = sum(results2018$Date %in% today)
todaysGames$Visitor.B2B = numeric(nGames)
todaysGames$Home.B2B = numeric(nGames)
todaysGames$Home.Win.Prob = numeric(nGames)
todaysGames$Bet = numeric(nGames)
todaysGames$Payoff = numeric(nGames)

#Add column for win estimate
todaysGames$Prediction = numeric(nGames)

#Fill in predictions for the next set of games
#This is limited by how far ahead BCLC puts out odds
for(i in 1:nGames){
  todaysGames$Visitor.B2B[i] = if(todaysGames$Visitor[i] %in% todaysGames[todaysGames$Date == (todaysGames$Date[i]-1),2] | 
                                  todaysGames$Visitor[i] %in% todaysGames[todaysGames$Date == (todaysGames$Date[i]-1),4]) {1} else {0}
  
  todaysGames$Home.B2B[i] = if(todaysGames$Home[i] %in% todaysGames[todaysGames$Date == (todaysGames$Date[i]-1),2] | 
                               todaysGames$Home[i] %in% todaysGames[todaysGames$Date == (todaysGames$Date[i]-1),4]) {1} else{0}
  
  todaysGames$Home.Win.Prob[i] = exp(logitResultDF$value[logitResultDF$team == todaysGames$Home[i]]) / (exp(logitResultDF$value[logitResultDF$team == todaysGames$Home[i]]) + exp(logitResultDF$value[logitResultDF$team == todaysGames$Visitor[i]]))
  
  todaysGames$Prediction[i] = predict(plusModel, newdata = todaysGames[i,])
}

#Calculate risk-adjusted value per dollar bet on each team
#Calculated as the BCLC payoff premium over the plusModel change of winning
todaysGames$Visitor.Payoff = todaysGames$Visitor.Odds - 1/(1-todaysGames$Prediction) 
todaysGames$Home.Payoff = todaysGames$Home.Odds - 1/(todaysGames$Prediction) 

#Specify which bets to make
for(i in 1:nGames){
  todaysGames$Bet[i] = if(todaysGames$Visitor.Payoff[i] > 0){"Visitor"} else if(todaysGames$Home.Payoff[i] > 0){"Home"} else {"None"}
  todaysGames$Payoff[i] = if(todaysGames$Visitor.Payoff[i] > 0){todaysGames$Visitor.Payoff[i]} else if(todaysGames$Home.Payoff[i] > 0){todaysGames$Home.Payoff[i]} else {0}
}

#Record the model prediction to a CSV to check later
bets = data.frame(Date = today,
                  Visitor = todaysGames$Visitor,
                  Home = todaysGames$Home,
                  VisitorOdds = todaysGames$Visitor.Odds,
                  HomeOdds = todaysGames$Home.Odds,
                  Bet = todaysGames$Bet,
                  ExpectedPayoff = todaysGames$Payoff
                  )

write.table(bets,'predictions2018-19.csv',sep=',',append=T,quote=F,row.names = F,col.names=F)



