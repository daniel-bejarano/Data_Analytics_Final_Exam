# FINAL, Problem 1: EDA on Permits
# Due Sunday December 21st, 2014
# By Daniel Bejarano

load("C:/Users/dbejarano/Dropbox/aFALL 2014/DATA ANALYTICS/FINAL/Permits2014.csv")

Permits=read.csv("C:/Users/dbejarano/Dropbox/aFALL 2014/DATA ANALYTICS/FINAL/Permits2014_formatted.csv",as.is=TRUE)

###  Data Manipulation  ###
Permits$CITY = as.factor(Permits$CITY)

mydate <- factor(Permits$FILE_DATE)
Permits$FILE_DATE = as.Date(mydate, format = "%m/%d/%Y")
mydate <- factor(Permits$STATUS_DATE)
Permits$STATUS_DATE = as.Date(mydate, format = "%m/%d/%Y")
mydate <- factor(Permits$EXPIRATION_DATE)
Permits$EXPIRATION_DATE = as.Date(mydate, format = "%m/%d/%Y")

### Histograms ###
hist(Permits$ESTIMATED.COST)
hist(Permits$REVISED.COST)
hist(Permits$CITY)

#Division by categories
recr_bldg = Permits[Permits$EXISTING.USE=="RECREATION BLDG",]

#Comparison of Proposed vs Existing Use
same_use = Permits[Permits$EXISTING.USE == Permits$PROPOSED.USE,]
hist(same_use$REVISED.COST, breaks = 20)
diff_use = Permits[Permits$EXISTING.USE != Permits$PROPOSED.USE,]
hist(diff_use$REVISED.COST, xlim = c(0, 100000), breaks = 50)
Permits$Colour[Permits$EXISTING.USE == Permits$PROPOSED.USE] = "red"
Permits$Colour[Permits$EXISTING.USE != Permits$PROPOSED.USE] = "blue"
plot(Permits$REVISED.COST, Permits$EXISTING.COST, col = Permits$Colour, ylim = c(0,50000000))

#Costs based on whether stories were added, remained the same, or reduced
same_stories = Permits[Permits$EXISTING.STORIES == Permits$PROPOSED.STORIES,]
build_stories = Permits[Permits$EXISTING.STORIES < Permits$PROPOSED.STORIES,]
reduce_stories = Permits[Permits$EXISTING.STORIES > Permits$PROPOSED.STORIES,]
Permits$Colour2[Permits$EXISTING.STORIES == Permits$PROPOSED.STORIES] = "red"
Permits$Colour2[Permits$EXISTING.STORIES < Permits$PROPOSED.STORIES] = "blue"
Permits$Colour2[Permits$EXISTING.STORIES > Permits$PROPOSED.STORIES] = "green"
boxplot(as.factor(Permits$CITY), Permits$REVISED.COST, col = Permits$Colour2, ylim = c(0,50000000))

#What may be causing differences between estimated and revised costs
Permits$Diff_Cost = Permits$REVISED.COST - Permits$ESTIMATED.COST 
plot(Diff_Cost~CITY, data=Permits)

largest = Permits[Permits$REVISED.COST == max(Permits$REVISED.COST),]


#San Francisco
SF = Permits[Permits$CITY == "SAN FRANCISCO",]
plot(Permits$REVISED.COST, na.omit(Permits$AVS_STREET_NAME), data = SF, )

# Plots
plot(REVISED.COST~ESTIMATED.COST, data = Permits)
plot(ESTIMATED.COST~EXISTING.USE, data=Permits, xlim = "LIBRARY")
plot(ESTIMATED.COST~CITY, data=Permits)


#### Question 1 plots ####
Permits$Colour_cost[Permits$REVISED.COST <= 10^2] = "green"
Permits$Colour_cost[10^2 < Permits$REVISED.COST & Permits$REVISED.COST <= 10^4] = "blue"
Permits$Colour_cost[10^4 < Permits$REVISED.COST & Permits$REVISED.COST <= 10^6] = "black"
Permits$Colour_cost[10^6 < Permits$REVISED.COST] = "red"
boxplot(REVISED.COST~as.factor(FORM_NUMBER), data = Permits, ylim = c(0, 50000000), xlab = "FORM_NUMBER", ylab = "REVISED_COST")
plot(PROPOSED.STORIES~EXISTING.STORIES, data = Permits, col = Colour_cost)
plot(PROPOSED.STORIES~EXISTING.STORIES, data = Permits, col = Colour_cost, xlim = c(0,20), ylim = c(0, 20))
plot(Diff_Cost~REVISED.COST, data=Permits, col = Permits$Colour, xlim = c(0, 6000000), ylim = c(-10000,3000000))
plot(Diff_Cost~log(REVISED.COST), data=Permits, col = Permits$Colour)#, xlim = c(0, 100000), ylim = c(-40000,100000))
pairs(~REVISED.COST + STATUS_CODE + EXISTING.UNITS + PLANSETS + NO.OF.PAGES, data = Permits)

plot(STATUS_CODE~FILE_DATE, data = Permits, ylab = "9 = Issued, 3 = Filed", xlab = "File Date")
pairs(~STATUS_CODE + FILE_DATE + STATUS_DATE + EXPIRATION_DATE, data = Permits)

hist(log10(Permits$REVISED.COST), breaks = 20)


hist(Permits$FORM_NUMBER,30, xlab=NA, ylab=NA, cex.axis=0.5, font.main=1, cex.main=0.8)


#### Question 2  ####

Permits$Colour_cost[Permits$REVISED.COST <= 10^2] = "green"
Permits$Colour_cost[10^2 < Permits$REVISED.COST & Permits$REVISED.COST <= 10^4] = "blue"
Permits$Colour_cost[10^4 < Permits$REVISED.COST & Permits$REVISED.COST <= 10^6] = "black"
Permits$Colour_cost[10^6 < Permits$REVISED.COST] = "red"

boxplot(as.factor(Permits$CITY), Permits$REVISED.COST, col = Permits$Colour2, ylim = c(0,50000000))
plot(PROPOSED.STORIES~EXISTING.STORIES, data = Permits, col = Colour_cost)
plot(PROPOSED.STORIES~EXISTING.STORIES, data = Permits, col = Colour_cost, xlim = c(0,20), ylim = c(0, 20))

#Creating the different categories of constructio work, based on proposed use of building
summary(as.factor(Permits$EXISTING.USE))
business = Permits[Permits$PROPOSED.USE == c("ANIMAL SALE OR CARE",
                                             "AUTO REPAIRS",
                                             "AUTOMOBILE SALES",
                                             "BUILDING MATERIALS",
                                             "CAR WASH",
                                             "CHRISTMAS TREE LOT",
                                             "DRY CLEANERS",
                                             "GARMENT SHOPS",
                                             "GREENHOUSE",
                                             "LENDING INSTITUTION",
                                             "MANUFACTURING",
                                             "MASSAGE",
                                             "MOVING & STORAGE",
                                             "MUNI CARBARN",
                                             "OFFICE",
                                             "PAINT STORE",
                                             "PARKING LOT",
                                             "PRINTING PLANT",
                                             "RETAIL SALES",
                                             "SOUND STUDIO",
                                             "STORAGE SHED",
                                             "VACANT LOT WAREHOUSE",
                                             "FURNITURE WAREHOUSE",
                                             "NO FRNITUR",
                                             "WHOLESALE SALES",
                                             "WORKSHOP COMMERCIAL"),]

education = Permits[Permits$PROPOSED.USE == c("DAY CARE HOME GT 12",
                                              "ORPHANAGE",
                                              "PRSON'L SVC TUTOR",
                                              "SCHOOL",
                                              "DAY CARE NON-RES",
                                              "DAY CARE CENTER"),]
entertainment = Permits[Permits$PROPOSED.USE == c("ADULT ENTERTAINMENT",
                                                  "AMUSEMENT CENTER",
                                                  "CLUB",
                                                  "NITE CLUB",
                                                  "RECREATION BLDG",
                                                  "THEATER"),]
food = Permits[Permits$PROPOSED.USE == c("FOOD/BEVERAGE HNDLNG",
                                         "MEAT/PRODUCE MARTS"),]
hospital = Permits[Permits$PROPOSED.USE == c("AMBULANCE SERVICE",
                                             "CLINICS-MEDIC/DENTAL",
                                             "CONVALESCENT HOME",
                                             "HEALTH STUDIOS & GYM",
                                             "HOSPITAL",
                                             "NURSING HOME GT 6",
                                             "NURSING HOME NON AMB",
                                             "R-3(DWG) NURSING",
                                             "SOCIAL CARE FACILITY"),]
housing = Permits[Permits$PROPOSED.USE == c("1 FAMILY DWELLING",
                                            "2 FAMILY DWELLING", 
                                            "APARTMENTS"),]
public_utilities = Permits[Permits$PROPOSED.USE == c("LIBRARY",
                                                       "MUSEUM",
                                                       "POWER PLANT",
                                                       "SEWAGE PLANT",
                                                       "SFPD OR SFFD STATION",
                                                       "CHEMICAL PROCESSING"),]
telecommunications = Permits[Permits$PROPOSED.USE == c("ANTENNA",
                                                       "PHONE",
                                                       "RADIO & TV STATIONS"),]
tourism = Permits[Permits$PROPOSED.USE == c("RESIDENTIAL HOTEL",
                                            "TOURIST HOTEL/MOTEL",
                                            "TOWER"),]

#Getting the average to make two tiers for each category
avg_business = mean(na.omit(business$REVISED.COST))
avg_education = mean(na.omit(education$REVISED.COST))
avg_entertainment = mean(na.omit(entertainment$REVISED.COST))
avg_food = mean(na.omit(food$REVISED.COST))
avg_hospital = mean(na.omit(hospital$REVISED.COST))
avg_housing = mean(na.omit(housing$REVISED.COST))
avg_public = mean(na.omit(public_utilities$REVISED.COST))
avg_tele = mean(na.omit(telecommunications$REVISED.COST))
avg_tour = mean(na.omit(tourism$REVISED.COST))

business_high = business[business$REVISED.COST > avg_business,]
business_low = business[business$REVISED.COST < avg_business,]
sum(na.omit(business_high$REVISED.COST))*.001
sum(na.omit(business_low$REVISED.COST))*.001

#Revenue obtained by category
sum(na.omit(business$REVISED.COST)) * 0.001
sum(na.omit(education$REVISED.COST)) * 0.001
sum(na.omit(entertainment$REVISED.COST)) * 0.001
sum(na.omit(food$REVISED.COST)) * 0.001
sum(na.omit(hospital$REVISED.COST)) * 0.001
sum(na.omit(housing$REVISED.COST)) * 0.001
sum(na.omit(public_utilities$REVISED.COST)) * 0.001
sum(na.omit(telecommunications$REVISED.COST)) * 0.001
sum(na.omit(tourism$REVISED.COST)) * 0.001
