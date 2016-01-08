


#############################
#############################
##### Betfair arbitrage #####
#############################
#############################
#############################


cat("\014")
rm(list = ls())
setwd("/Users/renucciflorent/Documents/Google Drive/Projets/Business/A venir/Betfair/Arbitrage")

library(plyr)



###########################
### 1. Prepare the data ###
###########################
###########################


path = "/Volumes/750/Betfair/csv/"
files = list.files(path,pattern = "csv")
nfiles = length(files)

i = 1

for (file in files) {
  
  name = gsub(".csv","",file)
  
  if(! (paste0(name,".RData") %in% list.files())) {
    
    data = read.csv(paste0(path,file), header = T)
    names(data) = tolower(names(data))
    
    data$sports_id = 1
    data = data[data$event %in% c("Match Odds","Correct Score"),]
    data = data[data$selection %in% c("The Draw","0 - 0"),]
    data = data[,c("event_id","full_description","event", "selection", "scheduled_off","latest_taken","odds","volume_matched","win_flag","in_play")]
    
    data$scheduled_off = as.POSIXct(data$scheduled_off, format = "%d-%m-%Y %H:%M")
    data$latest_taken = as.POSIXct(data$latest_taken, format = "%d-%m-%Y %H:%M:%S")
    
    # compute the total volumes
    vols = ddply(data,.(full_description), summarise, vol = sum(volume_matched,na.rm = T))
    vols = vols[order(-vols$vol),]  
    
    print(paste0(i,"/",nfiles))
    print(file)  
    
    save.image(paste0(name,".RData"))
  }
  i=i+1  
}

rm(list = ls())
Data = NULL
Vols = NULL


for (file in list.files()[grep("2015.*RData",list.files())]) {
  load(file)
  Data = rbind(Data,data)
  Vols = rbind(Vols,vols)
  print(file)
  print(dim(Data))
} 


data = Data
vols = Vols
rm(file, files, i, nfiles, path, Data, Vols)
save.image("Prepared.RData")



#########################################################
### 2. Bet on "the Draw" at the beginning, wait, sell ###
#########################################################
#########################################################


# the draw : au début le 0-0 est moins probable qu'au bout de 10'. Donc plus rémunérateur. 
# Donc odd décroit au cours du temps.
# Stratégie : achat, attente, vente. 


# rm(list = setdiff(ls(),c("data","vols")))

cat("\014")
rm(list = ls())
load("Prepared.RData")

### Parameters ###
threshold_volume = 10
buy_at = 0
sell_at = 10
portion = 1/1000

events =  sample(unique(data$event_id), size = length(unique(data$event_id))*portion)
data = data[which(data$event_id %in% events),]
vols = vols[which(vols$event_id %in% events),]

data = data[-which(is.na(data$scheduled_off) | is.na(data$latest_taken)),]
event = unique(data$event_id)[3]

data = data[which(data$event_id %in% vols[which(vols$vol > threshold_volume),"event_id"]),]

result = NULL
for(sell_at in seq(60,105*60,60)) {
  
  gains = NULL
  for(event in unique(data$event_id)) {
    used = data[data$event_id == event,]
    scheduled = used[1,"scheduled_off"]
    used = used[which(used$latest_taken >= scheduled),]
    
    if(nrow(used) >0) {
      used = used[order(used$latest_taken),c("latest_taken","odds","win_flag")]
      
      # if re-sold
      startOdd = used[1,"odds"]
      endOdd = head(used[which(used$latest_taken > used[1,"latest_taken"] + sell_at),"odds"],n=1)
      gain = (startOdd - endOdd)/endOdd
      
      # if not re-sold
      #       if(used[1,"win_flag"]==1) {gain = startOdd-1} else {gain = -1}
      
      gains = c(gains,gain)}
    #     print(gains)
  }
  
  tmp = c(sell_at/60,sum(gains,na.rm = T)/length(unique(data$event_id)))
  result = rbind(result,tmp)
  print(tmp)
}



#################################
### 3. Bet on "the Draw" only ###
#################################
#################################



cat("\014")
rm(list = ls())
load("Prepared.RData")

vols = ddply(
  data,
  .(event_id),
  summarise,
  vol = sum(volume_matched,na.rm = T)
  )

save.image("Prepared.RData")

cat("\014")
rm(list = ls())
load("Prepared.RData")

### Parameters ###
threshold_volume = 1000000
minOdd = 1

data = data[data$selection %in% c("0 - 0"),]
data = data[,c("event_id","full_description", "scheduled_off","latest_taken","odds","win_flag","volume_matched")]

data = data[sample(1:nrow(data),nrow(data)),]
data = data[which(data$full_description %in% vols[which(vols$vol > threshold_volume),"full_description"]),]

data = data[which(data$scheduled_off<=data$latest_taken),]
data = data[order(data$event_id,data$latest_taken),]
data = data[which(data$odds>minOdd),]

data$win_flag = as.numeric(data$win_flag)
data$odds = as.numeric(data$odds)

results = ddply(data,.(event_id),summarise,
                startOdd = head(odds,n=1)*(head(volume_matched,n=1)>1),
                win = head(win_flag,n=1),
                scheduled_off = head(scheduled_off, n=1),
                gain = (startOdd >0)*(win*(startOdd*0.95-1)-(1-win))           
)

results = results[order(results$scheduled_off),]
plot(results$scheduled_off,cumsum(results[,"gain"]),type="l")

sum(results$gain)/nrow(results)
sd(results$gain)

sum(results[round(runif(10)*nrow(results)),"gain"])



##########################
### 3. Bet on 0-0 only ###
##########################
##########################




cat("\014")
rm(list = ls())
load("Prepared.RData")

library(data.table)


### Parameters ###
threshold_volume = 0
portion = 0.1
minOdd = 1
maxOdd = 100000
type = "0 - 0"

data <- data.table(data)
data <- subset(data, selection %in% type)

events =  sample(unique(data$event_id), size = length(unique(data$event_id))*portion)
data = data[which(data$event_id %in% events),]

vols = data[which(data$latest_taken < data$scheduled_off),]
vols = data[, sum(volume_matched), by = event_id]
names(vols) = c("event_id","vol")
setkey(vols,"event_id")

result = NULL

for(threshold_volume in seq(0,max(vols$vol),length.out = 1000)) {
  
  used = data[which(!is.na(data$scheduled_off) & !is.na(data$latest_taken)),]
  setkey(used,"event_id")
  
  used = used[which(used$event_id %in% 
                      vols[which(vols$vol > threshold_volume & vols$vol <= threshold_volume+max(vols$vol)/999),][["event_id"]]),]
  used = used[which(used$odds>minOdd & used$odds<maxOdd),]
  
  used = used[which(used$scheduled_off<=used$latest_taken),]
  
  order = data.frame( order = sample(1:length(unique(used$event_id))),event_id = unique(used$event_id))
  used = merge(used,order, by = "event_id")
  used = used[order(used$scheduled_off,used$order,used$latest_taken),]
  
  results = used[,.(
    startOdd = head(odds,n=1)*(head(volume_matched,n=1)>1),
    win = head(win_flag,n=1)), by = order]

  results$gain = (results$startOdd >0)*(results$win*(results$startOdd*0.95-1)-(1-results$win))

  result = rbind(result,c(threshold_volume, nrow(results), sum(results$gain,na.rm = T)))
}               

toplot = as.data.frame(result)
names(toplot) = c("vol","n","totgain")
# toplot$roi = toplot$totgain / toplot$n
toplot = toplot[order(-toplot$vol),]

plot(cumsum(toplot$n),cumsum(toplot$totgain)/cumsum(toplot$n), type = "l")
lines(c(0,max(toplot$vol)),c(0,0),col = 2)


# plot(cumsum(results[,"gain"]),type="l")
# lines(x = 1:nrow(results), y=rep(0,nrow(results)),col = 2)
# prop.table(table(results$gain>0))
# mean(results$startOdd)

# sd(results$gain)
# sum(results[round(runif(10)*nrow(results)),"gain"])



# threshold, minOdd, gain 0-0, gain draw,gain <3.5, gain = f(time)
# plot proportion true = f(odds)

# predict positive or negative, feature = teams.





##########################
### 34 Bet on 0-0 only ###
##########################
##########################




cat("\014")
rm(list = ls())
load("Prepared.RData")

library(data.table)


### Parameters ###
portion = 1
minOdd = 1
maxOdd = 100000
# type = "0 - 0"
type = "The Draw"

data <- data.table(data)
data <- subset(data, selection %in% type)

events =  sample(unique(data$event_id), size = length(unique(data$event_id))*portion)
used = data[which(data$event_id %in% events),]

vols = used[which(used$latest_taken < used$scheduled_off),]
vols = used[, sum(volume_matched), by = event_id]
names(vols) = c("event_id","vol")
setkey(vols,"event_id")

result = NULL

used = used[which(!is.na(used$scheduled_off) & !is.na(used$latest_taken)),]
setkey(used,"event_id")

used = used[which(used$odds>minOdd & used$odds<maxOdd),]
used = used[which(used$scheduled_off<=used$latest_taken),]

order = data.frame( order = sample(1:length(unique(used$event_id))),event_id = unique(used$event_id))
used = merge(used,order, by = "event_id")
used = used[order(used$scheduled_off,used$order,used$latest_taken),]

results = used[,.(
  startOdd = head(odds,n=1)*(head(volume_matched,n=1)>1),
  win = head(win_flag,n=1)), by = event_id]

# for the draw
# results = used[,.(
#   startOdd = head(odds,n=1)*(head(volume_matched,n=1)>1),
#   endOdd   = head(win_flag,n=1)), by = event_id]
# 

results$gainBack = (results$startOdd >0)*(results$win*(results$startOdd*0.95-1)-(1-results$win))
results$gainLay  = (results$startOdd >0)*(results$win*(1-results$startOdd)+0.95*(1-results$win))

# threshold_volume = 870000
min_vol = 500
max_vol = 180000

volsUsed = vols[which(vols$vol>min_vol & vols$vol<max_vol),]

toplot = merge(results,volsUsed, by = "event_id")
toplot = toplot[order(-toplot$vol),]

# plot(cumsum(toplot$gainBack),type = "l")
# plot(cumsum(toplot$gainLay),type = "l")

# plot(log(results$vol),type = "l")
sum(toplot$gainLay)/nrow(toplot)
sum(toplot$gainBack)/nrow(toplot)


nrow(toplot)/12
cor(toplot$win,toplot$vol)


##########################
### Spot check the 0-0 ###
##########################
##########################




cat("\014")
rm(list = ls())
load("Prepared.RData")
data = data.table(data)
save.image("Prepared2.RData")



cat("\014")
rm(list = ls())
load("Prepared2.RData")

library(data.table)

type = "The Draw"
type = "0 - 0"


# data = data.table(data)
data = subset(data, selection %in% type)

vols = data[which(data$latest_taken < data$scheduled_off),]
vols = vols[, sum(volume_matched), by = event_id]
names(vols) = c("event_id","vol")
setkey(vols,"event_id")

kept = vols[which(vols$vol > 10000),]$event_id

result = NULL

used = data[which(!is.na(data$scheduled_off) & !is.na(data$latest_taken)),]
setkey(used,"event_id")

# used = used[which(used$scheduled_off<=used$latest_taken),c("evend_id","odds","win_flag")]
used = used[order(used$latest_taken),]
used = used[which(!duplicated(used$event_id)),]

used = used[which(used$event_id %in% kept),]


# stats


# dim(used)
# sum(used[which(used$win_flag == 1),]$odds-1)*0.95 - sum(used$win_flag==0)
prop.table(table(used$win_flag)) # 90.9% pertes, 9.1% gains
# mean(used$odds) # 13.4
# median(used$odds) # 12
# 
# mean(used[which(used$win_flag == 0),]$odds) # 14,2
# mean(used[which(used$win_flag == 1),]$odds) # 13,2
# 
# median(used[which(used$win_flag == 0),]$odds) # 13
# median(used[which(used$win_flag == 1),]$odds) # 12


used = used[which(used$volume_matched >1),]
used[which(used$win_flag == 1),"gain"] = (used[which(used$win_flag == 1),]$odds-1)*0.95
used[which(used$win_flag == 0),"gain"] = -1

# used[which(used$win_flag == 1),"gain"] = -(used[which(used$win_flag == 1),]$odds-1)
# used[which(used$win_flag == 0),"gain"] = 1*0.95


plot(cumsum(used$gain),type = "l")
lines(rep(0,nrow(used)), col = 2)

plot(cumsum(used[40000:40824,]$gain),type = "l")
lines(rep(0,nrow(used)), col = 2)

sum(cumsum(used$gain)<0)


plot(mav(used$odds,n=5000),type = "l")
plot(mav(used$win_flag,n=10000),type = "l")

used = merge(used,vols,by="event_id")
used = used[order(used$scheduled_off),]
plot(mav(used$vol,n=10000),type = "l")
plot(mav(used$vol,n=1000),type = "l")

#1 in used
# event_id full_description
#  2199736 English Soccer/Barclays Premiership/Fixtures 01 November/Man City v Norwich
#         event selection       scheduled_off        latest_taken
# Correct Score     0 - 0 2004-11-01 20:00:00 2004-10-30 16:08:51
# odds volume_matched win_flag in_play gain      vol
#   13             40        0      PE   -1 26087.56

# 50824 in used
#  event_id full_description
# 116988602 Bolivian Soccer/Bolivian Liga Nacional A/Fixtures 18 January  /Real Potosi v Bolivar
#         event selection       scheduled_off        latest_taken
# Correct Score     0 - 0 2015-01-18 21:15:00 2015-01-18 21:01:04
# odds volume_matched win_flag in_play gain     vol
#   15          16.04        0      PE   -1 5926.58

d = data[which(data$event_id == 117000926),]
d = d[order(d$latest_taken),]


# 1) c'est vraiment la vol avant le match ? 
# 2) vérifier les scores et backtester sur 10 matchs a) en utilisant la data b) avec les résultats officiels. 


