library(plyr)
setwd( "C:/Users/Mike/Desktop/R slides/Coursera Files/RepData_PeerAssessment 2"  )

StormData=read.csv("repdata-data-StormData.csv")

dim(StormData)
names(StormData)




Injuries=aggregate(StormData$INJURIES, by=list(StormData$EVTYPE), FUN=sum)
Fatalities=aggregate(StormData$FATALITIES, by=list(StormData$EVTYPE), FUN=sum)


Order.Injuries=Injuries[order(Injuries[,2],decreasing=TRUE),]
Order.Fatalities=Fatalities[order(Fatalities[,2],decreasing=TRUE),]

HOrder.Injuries= head(Order.Injuries)
HOrder.Fatalities= head(Order.Fatalities)

y= c(5633 , 504 , 470 , 1903, 816, 937)
plot(head(Order.Injuries[,2]),
     xlab = "Natural Disaster Type", 
     ylab = "Total Injuries", 
     bg = "green",
     cex = 1.1, # size of dots#
     pch = 21,  # type of dots#
     frame = FALSE, # frame or not#
     sub= "1=Tornado, 2=EX-HEAT , 3=F-FLOOD, 4=HEAT, 5=LIGHTNING , 6=TSTM WIND"
     )
    points(head(Order.Injuries[,2]) + y, col="red")

StormData$PROPDMGEXP=revalue(StormData$PROPDMGEXP, c("K"=1000,"k"=1000, "H"=100 , "h"=100 ,"m"=1000000 ,"M"=1000000, "B"=1000000000))
StormData$CROPDMGEXP=revalue(StormData$CROPDMGEXP, c("K"=1000,"k"=1000, "H"=100 , "h"=100 ,"m"=1000000 ,"M"=1000000, "B"=1000000000))


PROP.DMG.EXP=aggregate(as.numeric(StormData$PROPDMGEXP), by=list(StormData$EVTYPE), FUN=sum)
CROP.DMG.EXP=aggregate(as.numeric(StormData$CROPDMGEXP), by=list(StormData$EVTYPE), FUN=sum)

Head.Order.PROP.DMG.EXP=head(PROP.DMG.EXP[order(PROP.DMG.EXP[,2],decreasing=TRUE),])
Head.Order.CROP.DMG.EXP=head(CROP.DMG.EXP[order(PROP.DMG.EXP[,2],decreasing=TRUE),])



plot(Head.Order.PROP.DMG.EXP[,2] + Head.Order.CROP.DMG.EXP[,2],
     type="b",
     main = "Cost of Natural Disasters",
     xlab = "Natural Disaster Type",
     ylab = "Total Cost (in $)",
     sub = "1=HAIL   2=THUNDERWIND   3=TSTM_WIND   4=TORNADO   5=FL.FLOOD   6=FLOOD",
     bg = "blue",
     cex = 1.1, # size of dots#
     pch = 21,  # type of dots#
     frame = FALSE, # frame or not#
)
points(Head.Order.CROP.DMG.EXP[,2], col="red")
points(Head.Order.PROP.DMG.EXP[,2], col="green")

