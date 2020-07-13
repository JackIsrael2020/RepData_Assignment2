Data <- read.csv("repdata_data_StormData.csv.bz2")

X1 <- Data %>% select(STATE__, BGN_DATE, BGN_TIME, COUNTYNAME, STATE, EVTYPE, FATALITIES,
                      INJURIES, PROPDMG, PROPDMGEXP, LATITUDE, LONGITUDE)

#After all the mutation
Problem2 <- unique(X1$EVTYPE)
X2 <- filter(X1, EVTYPE %in% Problem2)



#How to get rid of summaries
TRY <- grep("SUMMARY", Problem2)
TRY2 <- grep("Summary", Problem2)
Remove1 <- Problem2[TRY]
Remove2 <- Problem2[TRY2]
Remove <- c(Remove1, Remove2)

L2 <- Problem2 %in% Remove
Problem3 <- Problem2[!L2]
#Or:
Problem3 <- Problem2[!(Problem2 %in% Remove)]
X3 <- filter(X2, EVTYPE %in% Problem3)


#The table for fatalities
Check1 <- aggregate(X3$FATALITIES, by = list('Event Type' = X3$EVTYPE), sum)
Check1 <- Check1 %>% arrange(x)
sum(Check1$x > 0) #117
sum(Check1$x > 1) #81
sum(Check1$x > 2) #69
sum(Check1$x > 3) #60
sum(Check1$x > 4) #52
sum(Check1$x > 5) #47
sum(Check1$x > 6) #46


#The table for property damage
# "H" "K" "M" "B" - Hundered, Thousand, Million and Billion
# "m" "h" - Million, Hundred
# "", "+", "0", "1" - nothing extra, ignore 
# "-" - Remove these values - Justification: negative costs don't make sense
# "?" - Remove these values - We can't be sure what they are
#  "2" "3" "4" "5" "6" "7" "8" - Number of zeroes - Multiply - Perhaps we should just
# remove these values, since there aren't many of them anyway, and the documentation 
# doesn't account for them. We can treat them as undefined.

X3 <- X3 %>% mutate(PROPDMG = replace(PROPDMG, PROPDMGEXP == "H", PROPDMG*100))
X3 <- X3 %>% mutate(PROPDMG = replace(PROPDMG, PROPDMGEXP == "K", PROPDMG*1000))
X3 <- X3 %>% mutate(PROPDMG = replace(PROPDMG, PROPDMGEXP == "M", PROPDMG*1000000))
X3 <- X3 %>% mutate(PROPDMG = replace(PROPDMG, PROPDMGEXP == "B", PROPDMG*1000000000))
X3 <- X3 %>% mutate(PROPDMG = replace(PROPDMG, PROPDMGEXP == "m", PROPDMG*1000000))
X3 <- X3 %>% mutate(PROPDMG = replace(PROPDMG, PROPDMGEXP == "h", PROPDMG*100))

Approve <- c("", "H", "K", "M", "B","m", "h", "0", "1")
X4 <- X3 %>% filter(PROPDMGEXP %in% Approve)

Check2 <- aggregate(X4$PROPDMG, by = list('Event Type' = X4$EVTYPE), sum)
Check2 <- Check2 %>% arrange(x)
sum(Check2$x > 0) #101
sum(Check2$x > 10^2) #101
sum(Check2$x > 10^4) # 88
sum(Check2$x > 10^5) #76
sum(Check2$x > 10^6) #47
sum(Check2$x > 10^7) #36
sum(Check2$x > 10^8) #24
sum(Check2$x > 10^9) #24
sum(Check2$x > 10^10) #24
sum(Check2$x > 10^11) #19
sum(Check2$x > 10^12) #15
sum(Check2$x > 10^13) #8
sum(Check2$x > 10^14) #4

# When you make the graph, you will need a log scale to compare the property damages.






