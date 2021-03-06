library(readxl)
# ********************************************************************************************************************************************

# Below is a lengthy command which cleans and transforms the data in preparation for analysis.
# Copy and paste it all into the R command box.

# ********************************************************************************************************************************************

VCCDataFull <- read_excel("Documents/GitHub/TREEAID/VCCDataFull.xlsx")
View(VCCDataFull)
VCCDATA<-VCCDataFull

columnValues<- c(VCCDATA$voice_hh_food, VCCDATA$voice_hh_spending, VCCDATA$voice_hh_crops, VCCDATA$voice_hh_confidence, VCCDATA$voice_hh_suggestions, 
                 VCCDATA$voice_comm_speaking, VCCDATA$voice_public_suggestions, VCCDATA$voice_comm_meetings, VCCDATA$voice_comm_activities, VCCDATA$choice_hh_training,
                 VCCDATA$choice_hh_decisions, VCCDATA$choice_hh_allocation, VCCDATA$choice_hh_income_women, VCCDATA$choice_comm_market, VCCDATA$choice_comm_committee, 
                 VCCDATA$control_hh_farm_land, VCCDATA$control_hh_comm_land, VCCDATA$control_hh_assets, VCCDATA$control_hh_livestock, VCCDATA$control_hh_trees, VCCDATA$control_hh_savings,
                 VCCDATA$control_comm_resources, VCCDATA$control_comm_leadership, VCCDATA$control_comm_by_laws)

for (topic in columnValues) {
    topic<- factor(topic, levels = c("more_than", "equal", "moderate", "little", "none"))
}

VCCDATA$ voice_hh_suggestions <- factor(VCCDATA$voice_hh_suggestions, levels = c("yes", "sometimes", "no"))

VCCDATA$Respondent<- factor(VCCDATA$Respondent, levels = c("Young_woman", "Female_adult", "Young_man", "Male_adult"))
VCCDATA$"VTE member"[is.na(VCCDATA$"VTE member")] <- "N"
VCCDATA$"Jardins Nutritifs Member"[is.na(VCCDATA$"Jardins Nutritifs Member")] <- "N"
VCCDATA$"VTE & Jardins Nutritifs Member"[is.na(VCCDATA$"VTE & Jardins Nutritifs Member")] <- "N"
VCCDATA$"Participation"[VCCDATA$ "VTE member"=="Y"] <- "VTE"
VCCDATA$"Participation"[ VCCDATA $ "Jardins Nutritifs Member"=="Y"] <- "JNM"
VCCDATA$"Participation"[ VCCDATA $ "VTE & Jardins Nutritifs Member"=="Y"] <- "JNM+VTE"
VCCDATA$"Participation"[ VCCDATA$ "Jardins Nutritifs Member"=="N"& VCCDATA $ "VTE member"=="N"] <- "None"
VCCDATA$Participation<-factor(VCCDATA$Participation,levels=c("JNM+VTE", "JNM", "VTE", "None"))
VCCDATA<-subset(VCCDATA, Gender=="M" & Respondent!="Female_adult" | Gender=="F" & Respondent!="Male_adult")
VCCM<-subset(VCCDATA, Gender!="F")
VCCF<-subset(VCCDATA, Gender!="M")
View(VCCDATA)
VCC2019<-VCCDATA[!(VCCDATA$Year=="2020"),]
VCC2020<-VCCDATA[!(VCCDATA$Year=="2019"),]
VCCF2019<-VCCF[!(VCCF$Year=="2020"),]
VCCF2020<-VCCF[!(VCCF$Year=="2019"),]
VCCM2019<-VCCM[!(VCCM$Year=="2020"),]
VCCM2020<-VCCM[!(VCCM$Year=="2019"),]


# ********************************************************************************************************************************************
#                                                                  TABLES
# ********************************************************************************************************************************************

# *At this point you will need the 'kableExtra', 'ggplot2', and 'tidyverse' packages installed and opened*

# Below are various commands to produce tables, similar to those on the Excel.
# There are basic count and percentage tables, as well as a some extras. T
# I haven't really stylised the tables, but they can be customized easily using the 'kableExtra' package. 
# Each table has been made for Question 1, or column 'topic', and compared by 'Year'. 

# *The variables can be changed very easily. For example, to create Count and Percentage tables for 2 variables ... *

# TABLENAMECOUNT<-table(DATASET$VARIABLE1, DATASET$VARIABLE2) 
# TABLENAMECOUNT<-addmargins(TABLENAMECOUNT)
# TABLENAMEPERCENTAGE<-prop.table(table(DATASET$VARIABLE1, DATASET$VARIABLE2), margin=1)*100
# TABLENAMECOUNT%>%
# kbl(caption = "Title") %>%
# kable_classic(full_width = F, html_font = "Font Name")
# TABLENAMEPERCENTAGE %>%
# kbl(caption = "Title Percentage") %>%
# kable_classic(full_width = F, html_font = "Font Name")


# I have used a code system; T1Q1 refers to Table 1 Question 1, whilst T1Q1PER is the same table given as a Percentage
# The title refers to the name of the JPEG file uploaded onto Github.

# ***********************************************************************************************************************************************************************

# T1Q1, T1Q1PER: Q1 RESPONSES BY YEAR (Also produces a third percentage table called T1Q1PER2, the mean % across both years, as I couldn't bind it to T1Q1PER)

# ***********************************************************************************************************************************************************************

# columnValues<- c('voice_hh_food', 'voice_hh_spending', 'voice_hh_crops', 'voice_hh_confidence', 'voice_hh_suggestions', 
# 'voice_comm_speaking', 'voice_public_suggestions', 'voice_comm_meetings', 'voice_comm_activities', 'choice_hh_training',
# 'choice_hh_decisions', 'choice_hh_allocation', 'choice_hh_income_women', 'choice_comm_market', 'choice_comm_committee', 
# 'control_hh_farm_land', 'control_hh_comm_land', 'control_hh_assets', 'control_hh_livestock', 'control_hh_trees', 
# 'control_hh_savings', 'control_comm_resources', 'control_comm_leadership', 'control_comm_by_laws')

for (topic in columnValues) {

    T1Q1<-table(VCCDATA$Year, topic) 
    T1Q1<-addmargins(T1Q1)
    T1Q1PER<-prop.table(table(VCCDATA$Year, topic), margin=1)*100
    T1Q1 %>%
    kbl(caption = "Q1 Voice HH Food Production, Female (Count)") %>%
    kable_classic(full_width = F, html_font = "Cambria")
    T1Q1PER %>%
    kbl(caption = "Q1 Voice HH Food Production, Female (%)") %>%
    kable_classic(full_width = F, html_font = "Cambria")
    T1Q1PER2<-prop.table(table(topic))*100 
    T1Q1PER2 %>%
    kbl(caption = "Q1 Voice HH Food Production, Overall (%)") %>%
    kable_classic(full_width = F, html_font = "Cambria")

    # ***********************************************************************************************************************************************************************

    # T2Q1, T2Q1PER: Q1 RESPONSES BY GENDER (Also produces a third percentage tables called T2Q1PER, a not-very-pretty reference table displaying response x year x gender).

    # ***********************************************************************************************************************************************************************

    T1Q1<-table(VCCDATA$Gender, topic)
    T1Q1%>%
    kbl(caption = "Q1 Voice HH Food Production, Gender (Count)") %>%
    kable_classic(full_width = F, html_font = "Cambria")
    T1Q1PER<-prop.table(table(VCCDATA$Gender, topic), margin=1)*100
    T1Q1PER %>%
    kbl(caption = "Q1 Voice HH Food Production, Gender (%)") %>%
    kable_classic(full_width = F, html_font = "Cambria")
    T2Q1PER<-prop.table(table(VCCDATA$Year, VCCDATA$Gender, topic), margin=1)*100
    T2Q1PER %>%
    kbl(caption = "Q1 Voice HH Food Production, Gender + Year (%)") %>%
    kable_classic(full_width = F, html_font = "Cambria")

    # **********************************************************************************************************************************************************************

    # T3Q1, T3Q1PER: Q1 RESPONSES BY PROJECT PARTICIPATION. (Uses dataframe VCC2020, as no data for VTE, JNM participation in 2019).

    # ***********************************************************************************************************************************************************************

    T3Q1<-table(VCC2020$Participation, VCC2020$topic)
    T3Q1<-addmargins(T3Q1)
    T3Q1%>%
    kbl(caption = "Q1 Voice HH Food Production, Participation (Count)") %>%
    kable_classic(full_width = F, html_font = "Cambria")
    T3Q1PER<-prop.table(table(VCC2020$Participation, VCC2020$topic), margin=1)*100
    T3Q1PER %>%
    kbl(caption = "Q1 Voice HH Food Production, Participation (%)")%>%
    kable_classic(full_width = F, html_font = "Cambria")

    # **********************************************************************************************************************************************************************
    #                                                                     GRAPHS
    # **********************************************************************************************************************************************************************
    # Below are commands for various graphs. Again, I have tried to make them similar to those on Excel, but they can easily be furthered customized using the 
    # 'ggplot2' package I have used to write them. 

    # Plot name correlates to the JPEG file name. 

    # Write >plotname in command box to view plot.

    # **********************************************************************************************************************************************************************

    # P1Q1, P1Q1PER: Q1 RESPONSES BY YEAR

    # **********************************************************************************************************************************************************************
    # *P1Q1PER - PERCENTAGE, 2019 AND 2020 COMPARED*

    P1Q1PER<-ggplot(data=subset(VCCDATA,!is.na(topic)),aes(x=topic,group=Year))+geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
    geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",vjust=-.5,size=3)+ggtitle("Q1 Voice HH Food Production, Year (%)") +
    labs(y="Percent",x="Response")+facet_grid(~Year) +scale_y_continuous(labels=scales::percent) + scale_fill_discrete(name = "Response", 
    labels = c("More Than", "Equal", "Moderate", "Little", "None"))
    P1Q1PER


    # *P1Q1PER2020 - PERCENTAGE, 2020*

    P1Q1PER2020<-ggplot(data=subset(VCC2019,!is.na(topic)),aes(x=topic,group=Year))+geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+geom_text(aes(label=scales::percent(..prop..),y=..prop..),
    stat="count",vjust=-.5)+ggtitle("Q1 Voice HH Food Production, 2020 (%)")+labs(y="Percent",fill="Response")+facet_grid(~Year)+scale_y_continuous(labels=scales::percent)+ scale_fill_discrete(name = "Response", 
    labels = c("More Than", "Equal", "Moderate", "Little", "None"))
    P1Q1PER2020


    # *P1Q1PER2019 - PERCENTAGE, 2019*

    P1Q1PER2020<-ggplot(data=subset(VCC2019,!is.na(topic)),aes(x=topic,group=Year))+geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+geom_text(aes(label=scales::percent(..prop..),y=..prop..),
    stat="count",vjust=-.5)+ggtitle("Q1 Voice HH Food Production, 2019 (%)")+labs(y="Percent",fill="Response")+facet_grid(~Year)+scale_y_continuous(labels=scales::percent)+ scale_fill_discrete(name = "Response", 
    labels = c("More Than", "Equal", "Moderate", "Little", "None"))
    P1Q1PER2019


    # *P1Q1 - COUNT 2019 AND 2020 COMPARED*

    P1Q1<-ggplot(data=subset(VCCDATA,!is.na(topic)),aes(x=topic,group=Year))+geom_bar(aes(y=..count..,fill=factor(..x..)),stat="count",position="dodge")+
    geom_text(stat="count",aes(label=..count..),vjust=-.5, size=3)+ggtitle("Q1 Voice HH Food Production, Year (Count)") +labs(y="Count",fill="Response")+facet_grid(~Year) + scale_fill_discrete(name = "Response", 
    labels = c("More Than", "Equal", "Moderate", "Little", "None"))
    P1Q1
summ

    # *P1Q1a - COUNT 2019 AND 2020 COMPARED, SIDE BY SIDE*

    P1Q1a<-ggplot(data=subset(VCCDATA,!is.na(topic)),aes(x = topic, fill=as.factor(Year))) + geom_bar(position = "dodge")+geom_text(stat="count",aes(label=..count..,group=Year), 
    position = position_dodge(width = 1), vjust = -0.5, size = 3) +ggtitle("Q1 Voice HH Food Production, Year (Count)") +labs(y="Count",fill="Year")
    P1Q1a

    # ***********************************************************************************************************************************************************************

    # P2Q1, P2Q1PER: Q1 RESPONSES BY GENDER BREAKDOWN

    # ***********************************************************************************************************************************************************************

    # *P2Q1PER - PERCENTAGE, MALE V FEMALE*

    P2Q1PER<-ggplot(data=subset(VCCDATA,!is.na(Gender)),aes(x=topic,group=Gender))+geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",size=3,vjust=-.5)+ggtitle("Q1 Voice HH Food Production, Gender (%)") +labs(y="Percent",fill="Response")+facet_grid(~Gender)+scale_y_continuous(labels=scales::percent)+ scale_fill_discrete(name = "Response", labels = c("More Than", "Equal", "Moderate", "Little", "None"))
    P2Q1PER


    # *P2Q1a - PERCENTAGE, GENDER x AGE*

    P2Q1a<-ggplot(data=subset(VCCDATA,!is.na(Respondent)),aes(x=topic,group=Respondent))+geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",size=3,vjust=-.5)+ggtitle("Q1 Voice HH Food Production, Gender and Age (%)") +labs(y="Percent",fill="Response")+facet_grid(~Respondent)+scale_y_continuous(labels=scales::percent)+ scale_fill_discrete(name = "Response", labels = c("More Than", "Equal", "Moderate", "Little", "None"))
    P2Q1a


    # *P2Q1 - COUNT, MALE V FEMALE*

    P2Q1<-ggplot(data=subset(VCCDATA,!is.na(topic)),aes(x=topic,group=Gender))+geom_bar(aes(y=..count..,fill=factor(..x..)),stat="count",position="dodge")+ geom_text(stat="count",aes(label=..count..),vjust=-.5, size=3)+ggtitle("Q1 Voice HH Food Production, Gender (Count)") +labs(y="Count",fill="Gender")+facet_grid(~Gender) + scale_fill_discrete(name = "Response", labels = c("More Than", "Equal", "Moderate", "Little", "None"))
    P2Q1


    # ***********************************************************************************************************************************************************************

    # *P3Q1, P31PER: Q1 RESPONSES BY GROUP PARTICIPATION

    # ***********************************************************************************************************************************************************************

    # *P3Q1 - COUNT, GROUP PARTICIPATION* 

    P3Q1<-ggplot(data=subset(VCC2020,!is.na(topic)), aes(x = Participation, fill=as.factor(topic))) + geom_bar(position = "dodge")+geom_text(stat="count",aes(label=..count..,group=topic), position = position_dodge(width = 1),vjust = -0.5, size = 3)+ggtitle("Q1 Voice HH Food Production, Group Participation 2020 (Count)") +labs(y="Count",fill="Response")
    P3Q1


    # *P3Q1PERa - PERCENTAGE, GROUP PARTICIPATION, STACKED*

    P3Q1PERa<-ggplot(data=subset(VCC2020,!is.na(topic)), aes(x = Participation, y = 100, fill = topic)) + geom_bar(position = position_fill(), stat = "identity") + scale_y_continuous(labels = scales::percent_format())+ggtitle("Q1 Voice HH Food Production, Group Participation 2020 (%)")+labs(y="Percent",fill="Response")
    P3Q1PERa


    # *P3Q1PERa - PERCENTAGE, GROUP PARTICIPATION*

    P3Q1PER<-ggplot(VCC2020,aes(x=topic,group=Participation))+geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",vjust=-.5,size=3)+ggtitle("Q1 Voice HH Food Production, Group Participation 2020 (%)")+ labs(y="Percent",x="")+facet_grid(~Participation) +scale_y_continuous(labels=scales::percent) + scale_fill_discrete(name = "Voice", labels = c("More Than", "Equal", "Moderate", "Little", "None"))
    P3Q1PER
}
