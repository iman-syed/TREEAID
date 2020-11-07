
********************************************************************************************************************************************

Below is a lengthy command which cleans and transforms the data in preparation for analysis.
Copy and paste it all into the R command box.

********************************************************************************************************************************************

VCCDataFull <- read_excel("aaTreeAid/VCCDataFull.xlsx")
View(VCCDataFull)
VCCDATA<-VCCDataFull
VCCDATA$voice_hh_food<- factor(VCCDATA$voice_hh_food, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ voice_hh_spending <- factor(VCCDATA$voice_hh_spending, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ voice_hh_crops <- factor(VCCDATA$voice_hh_crops, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ voice_hh_confidence <- factor(VCCDATA$voice_hh_confidence, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ voice_hh_suggestions <- factor(VCCDATA$voice_hh_suggestions, levels = c("yes", "sometimes", "no"))
VCCDATA$ voice_comm_speaking <- factor(VCCDATA$voice_comm_speaking, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ voice_comm_meetings <- factor(VCCDATA$voice_comm_meetings, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ voice_comm_activities <- factor(VCCDATA$voice_comm_activities, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ choice_hh_training <- factor(VCCDATA$choice_hh_training, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ choice_hh_decisions <- factor(VCCDATA$choice_hh_decisions, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ choice_hh_allocation <- factor(VCCDATA$choice_hh_allocation, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ choice_hh_income_women <- factor(VCCDATA$choice_hh_income_women, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ choice_comm_market <- factor(VCCDATA$choice_comm_market, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ choice_comm_committee <- factor(VCCDATA$choice_comm_committee, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ control_hh_farm_land <- factor(VCCDATA$control_hh_farm_land, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ control_hh_comm_land <- factor(VCCDATA$control_hh_comm_land, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ control_hh_assets <- factor(VCCDATA$control_hh_assets, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ control_hh_livestock <- factor(VCCDATA$control_hh_livestock, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ control_hh_trees <- factor(VCCDATA$control_hh_trees, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ control_hh_savings <- factor(VCCDATA$control_hh_savings, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ control_comm_resources <- factor(VCCDATA$control_comm_resources, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ control_comm_leadership <- factor(VCCDATA$control_comm_leadership, levels = c("more_than", "equal", "moderate", "little", "none"))
VCCDATA$ control_comm_by_laws <- factor(VCCDATA$control_comm_by_laws, levels = c("more_than", "equal", "moderate", "little", "none"))
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


********************************************************************************************************************************************
                                                                 TABLES
********************************************************************************************************************************************

*At this point you will need the 'kableExtra', 'ggplot2', and 'tidyverse' packages installed and opened*

Below are various commands to produce tables, similar to those on the Excel.
There are basic count and percentage tables, as well as a some extras. T
I haven't really stylised the tables, but they can be customized easily using the 'kableExtra' package. 
Each table has been made for Question 1, or column 'voice_hh_food', and compared by 'Year'. 

*The variables can be changed very easily. For example, to create Count and Percentage tables for 2 variables ... *

TABLENAMECOUNT<-table(DATASET$VARIABLE1, DATASET$VARIABLE2) 
TABLENAMECOUNT<-addmargins(TABLENAMECOUNT)
TABLENAMEPERCENTAGE<-prop.table(table(DATASET$VARIABLE1, DATASET$VARIABLE2), margin=1)*100
TABLENAMECOUNT%>%
kbl(caption = "Title") %>%
kable_classic(full_width = F, html_font = "Font Name")
TABLENAMEPERCENTAGE %>%
kbl(caption = "Title Percentage") %>%
kable_classic(full_width = F, html_font = "Font Name")


I have used a code system; T1Q1 refers to Table 1 Question 1, whilst T1Q1PER is the same table given as a Percentage
The title refers to the name of the JPEG file uploaded onto Github.

***********************************************************************************************************************************************************************

T1Q1, T1Q1PER: Q1 RESPONSES BY YEAR (Also produces a third percentage table called T1Q1PER2, the mean % across both years, as I couldn't bind it to T1Q1PER)

***********************************************************************************************************************************************************************

T1Q1<-table(VCCDATA$Year, VCCDATA$voice_hh_food) 
T1Q1<-addmargins(T1Q1)
T1Q1PER<-prop.table(table(VCCDATA$Year, VCCDATA$voice_hh_food), margin=1)*100
T1Q1 %>%
kbl(caption = "Q1 Voice HH Food Production, Female (Count)") %>%
kable_classic(full_width = F, html_font = "Cambria")
T1Q1PER %>%
kbl(caption = "Q1 Voice HH Food Production, Female (%)") %>%
kable_classic(full_width = F, html_font = "Cambria")
T1Q1PER2<-prop.table(table(VCCDATA$voice_hh_food))*100 
T1Q1PER2 %>%
kbl(caption = "Q1 Voice HH Food Production, Overall (%)") %>%
kable_classic(full_width = F, html_font = "Cambria")

***********************************************************************************************************************************************************************

T2Q1, T2Q1PER: Q1 RESPONSES BY GENDER (Also produces a third percentage tables called T2Q1PER, a not-very-pretty reference table displaying response x year x gender).

***********************************************************************************************************************************************************************

T1Q1<-table(VCCDATA$Gender, VCCDATA$voice_hh_food)
T1Q1%>%
kbl(caption = "Q1 Voice HH Food Production, Gender (Count)") %>%
kable_classic(full_width = F, html_font = "Cambria")
T1Q1PER<-prop.table(table(VCCDATA$Gender, VCCDATA$voice_hh_food), margin=1)*100
T1Q1PER %>%
kbl(caption = "Q1 Voice HH Food Production, Gender (%)") %>%
kable_classic(full_width = F, html_font = "Cambria")
T2Q1PER<-prop.table(table(VCCDATA$Year, VCCDATA$Gender, VCCDATA$voice_hh_food), margin=1)*100
T2Q1PER %>%
kbl(caption = "Q1 Voice HH Food Production, Gender + Year (%)") %>%
kable_classic(full_width = F, html_font = "Cambria")

**********************************************************************************************************************************************************************

T3Q1, T3Q1PER: Q1 RESPONSES BY PROJECT PARTICIPATION. (Uses dataframe VCC2020, as no data for VTE, JNM participation in 2019).

***********************************************************************************************************************************************************************

T3Q1<-table(VCC2020$Participation, VCC2020$voice_hh_food)
T3Q1<-addmargins(T3Q1)
T3Q1%>%
kbl(caption = "Q1 Voice HH Food Production, Participation (Count)") %>%
kable_classic(full_width = F, html_font = "Cambria")
T3Q1PER<-prop.table(table(VCC2020$Participation, VCC2020$voice_hh_food), margin=1)*100
T3Q1PER %>%
kbl(caption = "Q1 Voice HH Food Production, Participation (%)")%>%
kable_classic(full_width = F, html_font = "Cambria")

**********************************************************************************************************************************************************************
                                                                    GRAPHS
**********************************************************************************************************************************************************************
Below are commands for various graphs. Again, I have tried to make them similar to those on Excel, but they can easily be furthered customized using the 
'ggplot2' package I have used to write them. 

Plot name correlates to the JPEG file name. 

Write >plotname in command box to view plot.

**********************************************************************************************************************************************************************

P1Q1, P1Q1PER: Q1 RESPONSES BY YEAR

**********************************************************************************************************************************************************************
*P1Q1PER - PERCENTAGE, 2019 AND 2020 COMPARED*

P1Q1PER<-ggplot(data=subset(VCCDATA,!is.na(voice_hh_food)),aes(x=voice_hh_food,group=Year))+geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+
geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",vjust=-.5,size=3)+ggtitle("Q1 Voice HH Food Production, Year (%)") +
labs(y="Percent",x="Response")+facet_grid(~Year) +scale_y_continuous(labels=scales::percent) + scale_fill_discrete(name = "Response", 
labels = c("More Than", "Equal", "Moderate", "Little", "None"))
P1Q1PER


*P1Q1PER2020 - PERCENTAGE, 2020*

P1Q1PER2020<-ggplot(data=subset(VCC2019,!is.na(voice_hh_food)),aes(x=voice_hh_food,group=Year))+geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+geom_text(aes(label=scales::percent(..prop..),y=..prop..),
stat="count",vjust=-.5)+ggtitle("Q1 Voice HH Food Production, 2020 (%)")+labs(y="Percent",fill="Response")+facet_grid(~Year)+scale_y_continuous(labels=scales::percent)+ scale_fill_discrete(name = "Response", 
labels = c("More Than", "Equal", "Moderate", "Little", "None"))
P1Q1PER2020


*P1Q1PER2019 - PERCENTAGE, 2019*

P1Q1PER2020<-ggplot(data=subset(VCC2019,!is.na(voice_hh_food)),aes(x=voice_hh_food,group=Year))+geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+geom_text(aes(label=scales::percent(..prop..),y=..prop..),
stat="count",vjust=-.5)+ggtitle("Q1 Voice HH Food Production, 2019 (%)")+labs(y="Percent",fill="Response")+facet_grid(~Year)+scale_y_continuous(labels=scales::percent)+ scale_fill_discrete(name = "Response", 
labels = c("More Than", "Equal", "Moderate", "Little", "None"))
P1Q1PER2019


*P1Q1 - COUNT 2019 AND 2020 COMPARED*

P1Q1<-ggplot(data=subset(VCCDATA,!is.na(voice_hh_food)),aes(x=voice_hh_food,group=Year))+geom_bar(aes(y=..count..,fill=factor(..x..)),stat="count",position="dodge")+
geom_text(stat="count",aes(label=..count..),vjust=-.5, size=3)+ggtitle("Q1 Voice HH Food Production, Year (Count)") +labs(y="Count",fill="Response")+facet_grid(~Year) + scale_fill_discrete(name = "Response", 
labels = c("More Than", "Equal", "Moderate", "Little", "None"))
P1Q1


*P1Q1a - COUNT 2019 AND 2020 COMPARED, SIDE BY SIDE*

P1Q1a<-ggplot(data=subset(VCCDATA,!is.na(voice_hh_food)),aes(x = voice_hh_food, fill=as.factor(Year))) + geom_bar(position = "dodge")+geom_text(stat="count",aes(label=..count..,group=Year), 
position = position_dodge(width = 1), vjust = -0.5, size = 3) +ggtitle("Q1 Voice HH Food Production, Year (Count)") +labs(y="Count",fill="Year")
P1Q1a

***********************************************************************************************************************************************************************

P2Q1, P2Q1PER: Q1 RESPONSES BY GENDER BREAKDOWN

***********************************************************************************************************************************************************************

*P2Q1PER - PERCENTAGE, MALE V FEMALE*

P2Q1PER<-ggplot(data=subset(VCCDATA,!is.na(Gender)),aes(x=voice_hh_food,group=Gender))+geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",size=3,vjust=-.5)+ggtitle("Q1 Voice HH Food Production, Gender (%)") +labs(y="Percent",fill="Response")+facet_grid(~Gender)+scale_y_continuous(labels=scales::percent)+ scale_fill_discrete(name = "Response", labels = c("More Than", "Equal", "Moderate", "Little", "None"))
P2Q1PER


*P2Q1a - PERCENTAGE, GENDER x AGE*

P2Q1a<-ggplot(data=subset(VCCDATA,!is.na(Respondent)),aes(x=voice_hh_food,group=Respondent))+geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",size=3,vjust=-.5)+ggtitle("Q1 Voice HH Food Production, Gender and Age (%)") +labs(y="Percent",fill="Response")+facet_grid(~Respondent)+scale_y_continuous(labels=scales::percent)+ scale_fill_discrete(name = "Response", labels = c("More Than", "Equal", "Moderate", "Little", "None"))
P2Q1a


*P2Q1 - COUNT, MALE V FEMALE*

P2Q1<-ggplot(data=subset(VCCDATA,!is.na(voice_hh_food)),aes(x=voice_hh_food,group=Gender))+geom_bar(aes(y=..count..,fill=factor(..x..)),stat="count",position="dodge")+ geom_text(stat="count",aes(label=..count..),vjust=-.5, size=3)+ggtitle("Q1 Voice HH Food Production, Gender (Count)") +labs(y="Count",fill="Gender")+facet_grid(~Gender) + scale_fill_discrete(name = "Response", labels = c("More Than", "Equal", "Moderate", "Little", "None"))
P2Q1


***********************************************************************************************************************************************************************

*P3Q1, P31PER: Q1 RESPONSES BY GROUP PARTICIPATION

***********************************************************************************************************************************************************************

*P3Q1 - COUNT, GROUP PARTICIPATION* 

P3Q1<-ggplot(data=subset(VCC2020,!is.na(voice_hh_food)), aes(x = Participation, fill=as.factor(voice_hh_food))) + geom_bar(position = "dodge")+geom_text(stat="count",aes(label=..count..,group=voice_hh_food), position = position_dodge(width = 1),vjust = -0.5, size = 3)+ggtitle("Q1 Voice HH Food Production, Group Participation 2020 (Count)") +labs(y="Count",fill="Response")
P3Q1


*P3Q1PERa - PERCENTAGE, GROUP PARTICIPATION, STACKED*

P3Q1PERa<-ggplot(data=subset(VCC2020,!is.na(voice_hh_food)), aes(x = Participation, y = 100, fill = voice_hh_food)) + geom_bar(position = position_fill(), stat = "identity") + scale_y_continuous(labels = scales::percent_format())+ggtitle("Q1 Voice HH Food Production, Group Participation 2020 (%)")+labs(y="Percent",fill="Response")
P3Q1PERa


*P3Q1PERa - PERCENTAGE, GROUP PARTICIPATION*

P3Q1PER<-ggplot(VCC2020,aes(x=voice_hh_food,group=Participation))+geom_bar(aes(y=..prop..,fill=factor(..x..)),stat="count")+geom_text(aes(label=scales::percent(..prop..),y=..prop..),stat="count",vjust=-.5,size=3)+ggtitle("Q1 Voice HH Food Production, Group Participation 2020 (%)")+ labs(y="Percent",x="")+facet_grid(~Participation) +scale_y_continuous(labels=scales::percent) + scale_fill_discrete(name = "Voice", labels = c("More Than", "Equal", "Moderate", "Little", "None"))
P3Q1PER
