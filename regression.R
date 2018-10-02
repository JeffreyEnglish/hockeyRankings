#Read the data from the clipboard
#This is an inelegant way of getting information out of Google Sheets
pollOutput = read.table('pollResponses_sept18.csv',sep=',',header=T,stringsAsFactors = F)

#Get the number of trials (responses to the survey)
nTrials = length(pollOutput[,1])

#Initialize the data frame to pass to mlogit
#More information on this is at http://www.surveyanalysis.org/wiki/Analyzing_Max-Diff_Using_Standard_Logit_Models_Using_R
logitDF = data.frame(ID = 1,
                     Block = rep(1:nTrials,each=8),
                     Set = rep(1:(2*nTrials),each=4),
                     Choice = rep(c(1,0,0,0,0,0,0,1),nTrials),
                     matrix(0,8*nTrials,31))

#Rename columns to match team names
colnames(logitDF)[5:length(logitDF)] = unique(unlist(pollOutput[,1:4]))

#Loop through every trial to get it in the format for mlogit
for(i in 1:nTrials){
  trialData = unlist(pollOutput[i,])
  bestTeam = trialData[5]
  wrstTeam = trialData[6]
  othrTeamA = trialData[which(!(trialData[1:4] %in% c(bestTeam,wrstTeam)))][1]
  othrTeamB = trialData[which(!(trialData[1:4] %in% c(bestTeam,wrstTeam)))][2]
  
  teamOrder = rep(c(bestTeam,othrTeamA,othrTeamB,wrstTeam),2)
  #Set logitDF values for the best team
  for(k in 1:4){
    index = (i-1)*8+k
    logitDF[index,which(colnames(logitDF) == teamOrder[k])] = 1
  }
  #Set logitDF values for the worst team
  for(j in 5:8){
    index = (i-1)*8+j
    logitDF[index,which(colnames(logitDF) == teamOrder[j])] = -1
  }  
}

#Create a map to convert team names to letter IDs
#This makes it easier to write out the mlogit function
nameMap = data.frame(ID = c(LETTERS, sapply(LETTERS, function(x) paste0(x, LETTERS)))[1:31],
                     Team = colnames(logitDF)[5:length(logitDF)],
                     stringsAsFactors = F)

#Rename columns to letter IDs
colnames(logitDF)[5:length(logitDF)] = nameMap$ID

#Remove sets with no valid responses
validSet = !(pollOutput[,5] == "" & pollOutput[,6] == "")
logitDF = logitDF[rep(validSet,each=8),]

#Run the logistic regression
library(mlogit)
logitResult = mlogit(Choice ~ B + C + D + E + F + G + H + I + J + K + L + M + N + O + P + Q + R + S + T + U + V + W + X + Y + Z + AA + AB + AC + AD + AE| 0, 
              data=logitDF, 
              format='wide',
              alt.levels=paste(1:4))

#Convert the results into a usable data frame
logitResultDF = data.frame(team = nameMap$Team, #Team name
                           value = c(0,unlist(logitResult$coefficients)), #Regression value
                           prob = numeric(31), #Probability of being the best team
                           count = numeric(31), #Number of times this team appeared
                           txtLoc = numeric(31), #Location to place text in the plot
                           stringsAsFactors = F)

#Read team abbreviated and colour palette and join it to the data frame
abbv = read.csv('abbreviations.csv',stringsAsFactors = F,header=T)
logitResultDF = merge(logitResultDF,abbv,by.y='Team',by.x='team')

#Normalize regression results so that the mean is 0
logitResultDF$adj.value = logitResultDF$value - mean(logitResultDF$value)

#Specify the limit of the y-axis in the graph
yLimit = c(-max(c(max(logitResultDF$adj.value),-min(logitResultDF$adj.value)))*1.3,
           max(c(max(logitResultDF$adj.value),-min(logitResultDF$adj.value)))*1.3)

#Fill in counting values in the results data frame
for(i in 1:31){
  logitResultDF$probBest[i] = exp(logitResultDF$value[i]) / sum(exp(logitResultDF$value)) #Probability of being the best of all teams
  logitResultDF$probWorst[i] = exp(-logitResultDF$value[i]) / sum(exp(-logitResultDF$value)) #Probability of being the worst of all teams
  logitResultDF$count[i] = sum(unlist(pollOutput[,1:4]) == logitResultDF$team[i]) #Number of times the team appeared in the survey
  logitResultDF$best[i] = sum(unlist(pollOutput[,5]) == logitResultDF$team[i]) #Number of times the team was selected as best
  logitResultDF$worst[i] = sum(unlist(pollOutput[,6]) == logitResultDF$team[i]) #Number of times the team was selected as worst
  logitResultDF$txtLoc[i] = if(logitResultDF$adj.value[i] > 0){logitResultDF$adj.value[i] + yLimit[2]/8} else{logitResultDF$adj.value[i] - yLimit[2]/8} #Location to place the team label in the graph
}

#Add a field to sort the results based on regression results
logitResultDF$order = rank(logitResultDF$value)

#Set up colour palettes for the graphs
fillColours = logitResultDF$Colour1
names(fillColours) = logitResultDF$team
brdrColours = logitResultDF$Colour2
names(brdrColours) = logitResultDF$team

library(ggplot2)
#Plot regression scores
p = ggplot(logitResultDF)
p + geom_bar(stat='identity',aes(x=-order, y=adj.value, fill=team, color=team)) + 
  geom_text(aes(x=-order, y=txtLoc, label=Abbv),angle=90) +
  theme_minimal() +
  guides(fill = F, color=F) +
  scale_fill_manual(values=fillColours) +
  scale_color_manual(values=brdrColours) +
  geom_hline(aes(yintercept=0)) + 
  scale_y_continuous(limits = yLimit,
                     name = 'Adjusted Score',
                     breaks = scales::pretty_breaks(n = 10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.background = element_rect(fill = '#DDDDDD', color='#DDDDDD'))

#Plot regression likelihood for best team
p = ggplot(logitResultDF)
p + geom_bar(stat='identity',aes(x=-order, y=probBest, fill=team, color=team)) + 
  geom_text(aes(x=-order, y=probBest+0.015, label=Abbv),angle=90) +
  theme_minimal() +
  guides(fill = F, color=F) +
  scale_fill_manual(values=fillColours) +
  scale_color_manual(values=brdrColours) +
  geom_hline(aes(yintercept=0)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.background = element_rect(fill = '#DDDDDD', color='#DDDDDD')) +
  ylim(c(0,0.25)) + ylab('Probability of being the best\nout of all 31 teams')


#Plot regression likelihood for worst team
p = ggplot(logitResultDF)
p + geom_bar(stat='identity',aes(x=-order, y=probWorst, fill=team, color=team)) + 
  geom_text(aes(x=-order, y=probWorst+0.015, label=Abbv),angle=90) +
  theme_minimal() +
  guides(fill = F, color=F) +
  scale_fill_manual(values=fillColours) +
  scale_color_manual(values=brdrColours) +
  geom_hline(aes(yintercept=0)) + 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.background = element_rect(fill = '#DDDDDD', color='#DDDDDD')) +
  ylim(c(0,0.25)) + ylab('Probability of being the worst\nout of all 31 teams')
