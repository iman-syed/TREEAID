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