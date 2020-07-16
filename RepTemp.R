Data <- read.csv("repdata_data_StormData.csv.bz2")

X1 <- Data %>% select(STATE__, BGN_DATE, BGN_TIME, COUNTYNAME, STATE, EVTYPE, FATALITIES,
                      INJURIES, PROPDMG, PROPDMGEXP, LATITUDE, LONGITUDE)

#X1 - Original version of the database, involving all the relevant attributes
#X2 - version of the database involving all the collated event categories
#X3 - After removing summary event categories
#X4 - After setting the property damage attribute to its real value and removing those
# undefined values.

#After all the mutation
EventList1 <- unique(X1$EVTYPE)

#How to get rid of summaries
R1 <- grep("SUMMARY", EventList1)
R2 <- grep("Summary", EventList1)
Remove1 <- EventList1[R1]
Remove2 <- EventList1[R2]
Remove <- c(Remove1, Remove2)
EventList2 <- EventList1[!(EventList1 %in% Remove)]

X3 <- filter(X2, EVTYPE %in% EventList2)


# Prep for Property Damage analysis
# "H" "K" "M" "B" - Hundered, Thousand, Million and Billion
# "m" "h" - Million, Hundred
# "", "+", "0", "1" - nothing extra, ignore 
# "-" - Remove these values - Justification: negative costs don't make sense
# "?" - Remove these values - We can't be sure what they are
#  "2" "3" "4" "5" "6" "7" "8" - Number of zeroes - Multiply - Perhaps we should just
# remove these values, since there aren't many of them anyway, and the documentation 
# doesn't account for them. We can treat them as undefined.

X3 <- X3 %>% mutate(PROPDMG = replace(PROPDMG, PROPDMGEXP == "H", PROPDMG*100)) %>%
  mutate(PROPDMG = replace(PROPDMG, PROPDMGEXP == "K", PROPDMG*1000)) %>%
  mutate(PROPDMG = replace(PROPDMG, PROPDMGEXP == "M", PROPDMG*1000000)) %>% 
  mutate(PROPDMG = replace(PROPDMG, PROPDMGEXP == "B", PROPDMG*1000000000)) %>%
  mutate(PROPDMG = replace(PROPDMG, PROPDMGEXP == "m", PROPDMG*1000000)) %>%
  mutate(PROPDMG = replace(PROPDMG, PROPDMGEXP == "h", PROPDMG*100))

Approve <- c("", "H", "K", "M", "B","m", "h", "0", "1")
X4 <- X3 %>% filter(PROPDMGEXP %in% Approve) %>% select(STATE__, BGN_DATE, BGN_TIME, COUNTYNAME, STATE, EVTYPE,
                                                        FATALITIES, INJURIES, PROPDMG, LATITUDE, LONGITUDE)
# At this stage, we remove the entries whose values in PROPDMGEXP
# are not clearly defined


#The table for fatalities
Check1 <- aggregate(X4$FATALITIES, by = list('EventType' = X4$EVTYPE), sum) %>% arrange(x)
sum(Check1$x > 0) #111
sum(Check1$x > 1) #76
sum(Check1$x > 2) #65
sum(Check1$x > 3) #59
sum(Check1$x > 4) #52
sum(Check1$x > 5) #47
sum(Check1$x > 6) #46
sum(Check1$x > 20) #32

Temp1 <- Check1 %>% filter(Check1$x > 20)
Temp2 <- Check1 %>% filter(Check1$x <= 20)
LX <- data.frame(data = list('EventType' = "Other Events", x = sum(Temp2$x)))
colnames(LX) <- c("EventType", "x")

Present1 <- rbind(Temp1, LX)

# Play around with this to see how much detail you can add. Maybe also add an 'Other
# Events' collective measure to compare. This needs to be detailed in the report.

g <- ggplot(Present1, aes(x = EventType, y = x))
g + geom_bar(stat = "identity", color = "steel blue") +
  theme(axis.text.x = element_text(size=6, angle=90)) +
  scale_y_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000,
                                3500, 4000, 4500, 5000, 5500, 6000)) +
  ggtitle("Fatality Counts of US Weather Events, 1950 - 2011") +
  ylab("Fatalities") + xlab("Weather Event Type")


#The table for Property Damage
Check2 <- aggregate(X4$PROPDMG, by = list('Event Type' = X4$EVTYPE), sum) %>% arrange(x)
sum(Check2$x > 0) #96
sum(Check2$x > 10^2) #96
sum(Check2$x > 10^4) # 85
sum(Check2$x > 10^5) #72
sum(Check2$x > 10^6) #47
sum(Check2$x > 10^7) #36
sum(Check2$x > 10^8) #24
sum(Check2$x > 10^9) #24
sum(Check2$x > 10^10) #24
sum(Check2$x > 10^11) #19
sum(Check2$x > 10^12) #15
sum(Check2$x > 10^13) #8
sum(Check2$x > 10^14) #4


Temp3 <- Check2 %>% filter(Check2$x > 10^10)
Temp4 <- Check2 %>% filter(Check2$x <= 10^10)
X2 <- sum(Temp4$x)
LX2 <- data.frame(data = list('EventType' = "Other Events", x = X2))
colnames(LX2) <- c("EventType", "x")
colnames(Temp3) <- c("EventType", "x")

Present2 <- rbind(Temp3, LX2) %>% arrange(x)

# When you make the graph, you will need a log scale to compare the property damages.
# Make sure to explain and justify this descision.

g2 <- ggplot(Present2, aes(x = EventType, y = x))
g2 + geom_bar(stat = "identity", color = "steel blue") +
  scale_y_continuous(trans = 'log10') + 
  theme(axis.text.x = element_text(size=6, angle=90)) +
  ggtitle("Property Damage of US Weather Events, 1950 - 2011") +
  ylab("Property Damage in US Dollars") + xlab("Weather Event Type") 


# Recording the version of the Fatalities table using yearly means from 1991 - 2011

CheckX <- aggregate(X4$FATALITIES, by = list('EventType' = X4$EVTYPE, 'Year' = X4$Year), sum) %>% 
  arrange(Year) %>% filter(Year > 1990)
CheckX_2 <- aggregate(CheckX$x, by = list('EventType' = CheckX$EventType), mean)
CheckX_3 <- aggregate(CheckX$x, by = list('EventType' = CheckX$EventType),
                      function(x){return(sum(x)/20)}) %>% arrange(x)


g <- ggplot(CheckX_3, aes(x = EventType, y = x))
g + geom_bar(stat = "identity", color = "steel blue") +
  theme(axis.text.x = element_text(size=6, angle=90)) +
  scale_y_continuous(breaks = c(0, 500, 1000, 1500, 2000, 2500, 3000,
                                3500, 4000, 4500, 5000, 5500, 6000)) +
  ggtitle("Fatality Counts of US Weather Events, 1950 - 2011") +
  ylab("Fatalities") + xlab("Weather Event Type")




CheckY <- aggregate(X4$PROPDMG, by = list('EventType' = X4$EVTYPE, 'Year' = X4$Year), sum) %>% 
  arrange(Year) %>% filter(Year > 1990)
CheckY_2 <- aggregate(CheckY$x, by = list('EventType' = CheckY$EventType), mean)
CheckY_3 <- aggregate(CheckY$x, by = list('EventType' = CheckY$EventType),
                      function(x){return(sum(x)/20)}) %>% arrange(x)