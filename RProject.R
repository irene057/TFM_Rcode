install.packages("devtools")
install.packages("ggplot2")
install.packages("scales")
install.packages("cowplot")
install.packages("broom")
install.packages("DescTools")
install.packages("referenceIntervals")
devtools::install_github("kassambara/easyGgplot2")
install.packages("Matrix")
install.packages("arules")
install.packages( arules , scatterplot3d, vcd, seriation, igraph,"grid","cluster","TSP","gclus", "colorspace")
install.packages("arulesViz")
install.packages("plotrix")
install.packages("rattle")
install.packages("rfUtilities")
install.packages("NeuralNetTools")
install.packages("pROC")
install.packages("neuralnet")

library(plotrix)
library(Matrix)
library(arulesViz)
library(arules)
library(cowplot)
library(dplyr)
library(usethis)
library(devtools)
library(ggplot2)
library(easyGgplot2)
library(scales)
library(forcats)
library(gridExtra)
library(grid)
require(ggplot2)
library(gtable)
library(broom)
library(DescTools)
library(referenceIntervals)
library(car)
library(nnet)
library(FactoMineR)
library(factoextra)
library(e1071)  
library(caret)  
library(rattle)
library(randomForest)
library(neuralnet)
library(rfUtilities)
library(rpart)
library(rpart.plot)
library(FSelector)
library(pROC)
library(NeuralNetTools)
library(NeuralNetTools)

setwd("/Users/irene/Documents/TFM/DATASET")
df_patient_integrated_info <- read.csv("patient_integrated_info.csv",header=TRUE,sep=";",stringsAsFactors = FALSE)
df_test <- df_patient_integrated_info
glimpse(df_test)
sort(sapply(df_test,function(x) sum(is.na(x))),decreasing = FALSE)
sort(sapply(df_test,function(x) sum(x=="")),decreasing = FALSE)
dim(df_test)   #1039x324

# Exploratory analysis ---------------------------------------------------


#drugs for each treatment
library(tidyr)
df_drugsForEachTreatment<- bind_cols(df_test[,grepl("_type",names(df_test))],df_test[,grepl("_drugs",names(df_test))])
df_drugsForEachTreatment1<- bind_cols(gather(data = df_drugsForEachTreatment,key ="TreatmentLine",value="TreatmentType",1:3 ),gather(data = df_drugsForEachTreatment,key ="TreatmentLine",value="Drugs",6:8 ))
df_drugsForEachTreatment1<-df_drugsForEachTreatment1[,-c(1:7,10:16)]
df_drugsForEachTreatment1$LineOfTreatment<-substr(df_drugsForEachTreatment1$TreatmentLine,1,15)
df_drugsForEachTreatment1<-df_drugsForEachTreatment1[,-c(1,3)]

df_drugsForEachTreatment1<-df_drugsForEachTreatment1%>%
  group_by(TreatmentType)%>%
  count(Drugs=factor(Drugs),LineOfTreatment)
  ggplot(df_drugsForEachTreatment1[df_drugsForEachTreatment1$TreatmentType!="",],aes(x=Drugs,y=n,fill=TreatmentType))+geom_col()+coord_flip()+labs(y="nº of patients")+facet_grid(. ~ LineOfTreatment)+theme(legend.position = "bottom")
  

#smoker vs gender
ggplot(data=df_test[!(df_test$smoker==""),])+geom_bar(aes(x=smoker_yesNo,fill=gender))+ ggtitle(label =" Gender vs smoker")
ggplot(data=df_test[!(df_test$stage_at_dx==""),])+geom_bar(aes(x=stage_at_dx,fill=categorized_age_at_dx)) + ggtitle(label ="Stage at dx VS Categroized age at dx")
ggplot(data=df_test[!(df_test$dead=="0"),])+geom_bar(aes(x=death_cause)) + ggtitle(label ="Stage at dx VS Categroized age at dx")
#
ggplot(data=df_test)+geom_smooth(aes(x=startAge,y=smoking_CxY))
## death cause
df_new <- df_test %>% 
  filter(dead!="0",death_cause!="")%>%
  group_by(death_cause)%>%
  group_by(af)%>%
  summarize(count = n())%>%   # count records by species
  mutate(pct = count/sum(count))  # find percent of total

ggplot(df_new, aes(death_cause,pct,fill=death_cause)) + 
  geom_bar(stat='identity') + 
  geom_text(aes(label=scales::percent(pct)), position = position_stack(vjust = .5))+
  scale_y_continuous(labels = scales::percent)+
  scale_x_discrete(labels=c("obstruccion intestinal" = "obst. intest."))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(y="Percentage of patient")+ggtitle(label ="Death cause in case of death")
  
# FUCNTION for number of patientes whos has comorbity, cormorbitie_jke, drugGroup,Mutatted,symptoms,toxicity
dfToxicity <- df_test[,grepl("toxicities2_",names(df_test))]
dfComorbity<- df_test[,grepl("comorbidity_redcap_",names(df_test))]
dfSymptoms <- df_test[,grepl("symptom_redcap_",names(df_test))]
dfComorbityJkes<- df_test[,grepl("comorb_predx_jkes_",names(df_test))]
dfDrugGroup<- df_test[,grepl("drugGroup_",names(df_test))]
dfMutated <- df_test[,grepl("Mutated_",names(df_test))]

TransformData = function(dataF) {
  Types <- colnames(dataF)
  NumberOfPacientes =c()
  
  for(i in 1:length(Types)){
    NumberOfPacientes<-c(NumberOfPacientes,length(which("1" == dataF[i])))
  }
  newDF<- data.frame(Types,NumberOfPacientes)
  ggplot(newDF,aes(x=NumberOfPacientes,y=Types))+geom_bar(stat="identity")
  #barplot(newDF$VectorNumber,newDF$colnamesComorbity)geom_bar(stat="identity")
}
 
    TransformData(dfComorbity)
    TransformData(dfComorbityJkes)
    TransformData(dfDrugGroup)
    TransformData(dfMutated)
    TransformData(dfSymptoms)
    TransformData(dfToxicity)

#suvival days en funcion de categeized age y categorized stage
g1<-ggplot(data = df_test[!(df_test$categorized_stage_at_dx=="") & !(df_test$dead==0),], mapping = aes(x = categorized_stage_at_dx, y = survivalDays,color=categorized_age_at_dx)) +
  geom_boxplot()+theme(legend.position = "top") #variance of the mean of differente groups,gender,smoker,survivalDays

g2<-ggplot(data = df_test[!(df_test$categorized_stage_at_dx=="") & !(df_test$dead==1),], mapping = aes(x = categorized_stage_at_dx, y = survivalDays,color=categorized_age_at_dx)) +
  geom_boxplot() +theme(legend.position="none")#variance of the mean of differente groups,gender,smoker,survivalDays

legend <-  get_legend(g1)
g1 <- g1 + theme(legend.position="none")
df_plot_arranged <-grid.arrange(arrangeGrob(g1, g2, nrow = 1),
                                 arrangeGrob(nullGrob(), legend, nullGrob(), nrow = 1), ncol = 1, heights = c(4,0.5),
                                 top = textGrob("surival days of dead pacientes / alive pacientes", gp = gpar(fotsize = 12, font = 2)))
View(df_test[!(df_test$categorized_stage_at_dx=="") & !(df_test$dead==1),])
##show treatments
vectorTrearmnt<-c("chemotherapy","radiationtherapy","immunotherapy","antiangiogenic","tki","surgery","palliative_care")
dfTreatment<-NULL
for(i in vectorTrearmnt){
  dfTreatment[i]<- as.data.frame(table(df_test[i]))["Freq"]
}
dfTreatment<-as.data.frame(dfTreatment)
vectorStage<-c("I","II","III","IIIA","IIIB","IV")
dfTreatment["TreatmentYesNo"]<-c(0,1)
dfTreatment<-dfTreatment[dfTreatment$TreatmentYesNo==1,]
dfTreatment<-gather(
  dfTreatment,
  key="Treatment",
  value = "Frequecy",
  1:7
)
for(i in vectorStage){
  dfTreatment[i]<- 0
}

for (j in vectorTrearmnt){
  x<-as.data.frame(table(df_test[df_test[j]==1,]["stage"]))
  for(i in vectorStage){
    dfTreatment[dfTreatment$Treatment==j,i] <- ifelse(nrow(x[x$Var1==i,]["Freq"])>0,x[x$Var1==i,]["Freq"],0)
  }
}
dfTreatment<-gather(
  dfTreatment,
  key="Stage",
  value = "FrequecyPerStage",
  4:9
)

df_cumsum1 <- ddply(dfTreatment, "Treatment",
                    transform, label_ypos=cumsum(FrequecyPerStage))

ggplot(df_cumsum1,aes(x=Treatment,y=FrequecyPerStage,fill=forcats::fct_rev(Stage)))+geom_bar(stat = "identity")+labs(x="Type of treatment",y="Nº of patients",fill="Stage")+
  scale_fill_manual(values = c("#1c3770","#2c56af","#4672d0","#7596dc", "#a4bae8","#d3ddf4"))+
  theme(axis.text=element_text(size=14),
        axis.title=element_text(size=16),legend.title=element_text(size=16), 
        legend.text=element_text(size=14))+
  geom_text(aes(y=label_ypos,label = FrequecyPerStage), vjust=1.6, color="white", size = 3.5)
  
##Has systemic progression

progrsedpatientsDF <- df_test[df_test$has_systemic_progression==1,]

ggplot(progrsedpatientsDF[progrsedpatientsDF$categorized_stage_at_dx!="",],aes(x=categorized_stage_at_dx,fill=categorized_age_at_dx))+geom_bar()


##   posology de grupos de medicamento en las lineas 1 y 2
xOri<-df_test[(!is.na(df_test$TreatmentLine_1_posology) & (df_test$TreatmentLine_1_drugs!="")) | (!is.na(df_test$TreatmentLine_2_posology) & (df_test$TreatmentLine_2_drugs!="")) ,]
x<-xOri[,c("TreatmentLine_1_posology","TreatmentLine_1_drugs")]
x1<-xOri[,c("TreatmentLine_2_posology","TreatmentLine_2_drugs")]

x<-gather(
  x,
  key="TreatmentLineOfPosology",
  value = "Posology",
  1
)

x<-gather(
  x,
  key="TreatmentLineOfDrug",
  value = "Drug",
  1
)
x1<-gather(
  x1,
  key="TreatmentLineOfPosology",
  value = "Posology",
  1
)
x1<-gather(
  x1,
  key="TreatmentLineOfDrug",
  value = "Drug",
  1
)
dfNew <- rbind(x, x1)
ggplot(dfNew,aes(x=Drug,y=Posology,color=TreatmentLineOfPosology))+geom_boxplot() +coord_flip()+
  theme(legend.position = "top")

#ps_at_diagnosis
table(df_test$ps_at_diagnosis)
x<-df_test[!is.na(df_test$ps_at_diagnosis),]
table(x$palliative_care)



xx<-as.data.frame(table(df_test[!(df_test$categorized_stage_at_dx=="") & !is.na(df_test$ps_at_diagnosis),"categorized_stage_at_dx"]))        
names(xx)[names(xx)=="Var1"]<-"categorized_stage_at_dx"

x<-df_test[!is.na(df_test$ps_at_diagnosis) & !(df_test$categorized_stage_at_dx==""),] %>%
  count(categorized_stage_at_dx=factor(categorized_stage_at_dx),ps_at_diagnosis=factor(ps_at_diagnosis))
xCombined <- left_join(x,xx,by="categorized_stage_at_dx") 
xCombined$pct<-xCombined$n/xCombined$Freq


g1<-ggplot(xCombined,aes(x=factor(categorized_stage_at_dx,levels=c("Early", "Intermediate", "Advanced")),y=pct,fill=as.factor(ps_at_diagnosis),label = scales::percent(pct)))+geom_bar(stat = "identity")+
  labs(x="categorized_stage_at_dx",y="pct of patients",fill="ps_at_diagnosis")+theme(legend.position = "bottom")+
  scale_y_continuous(labels = scales::percent)

g2<-ggplot(df_test[!is.na(df_test$ps_at_diagnosis),],aes(factor(palliative_care),fill=factor(ps_at_diagnosis)))+geom_bar(position = "dodge2")+theme(legend.position = "none")+
   labs(x="palliative treatment", y="nº of patients")+ scale_x_discrete(labels=c( "0"= "NoTreated","1"="Treated"))

legend <-get_legend(g1)
g1 <- g1 + theme(legend.position="none")
df_plot_arranged <-grid.arrange(arrangeGrob(g1, g2, nrow = 1),
                                arrangeGrob(nullGrob(), legend, nullGrob(), nrow = 1), ncol = 1, heights = c(4,0.5),
                                top = textGrob("nº of patients of each performance status according to stage at diagnosis / palliative care ", gp = gpar(fotsize = 12, font = 2)))

#hay outliers, analizamos los perfiles de los casos
MorePosology<-xOri[xOri$TreatmentLine_1_posology>10 & xOri$TreatmentLine_1_posology>10,]
View(MorePosology[,c("numberOfdrugs_beforedx","categorized_numberOfdrugs_beforedx","TreatmentLine_1_posology","TreatmentLine_2_posology")])

##Duration days for each treatment type in line 1 and 2
DDDF<-df_test[df_test$TreatmentLine_1_duration_days!="",]
DDDF<-DDDF[!is.na(DDDF$TreatmentLine_1_type),]
View(DDDF[DDDF$TreatmentLine_1_type=="imm",c("TreatmentLine_1_type","TreatmentLine_1_duration_days")])

g1<-ggplot(DDDF[!is.na(DDDF$TreatmentLine_1_type),],aes(x=TreatmentLine_1_type,y=TreatmentLine_1_duration_days))+ggtitle("Treatment line 1")+labs(x="",y="Duration days")+geom_boxplot()+theme(axis.text=element_text(size=10),
                                                                                                                                                           axis.title=element_text(size=10),plot.title = element_text(size=12,face="bold"))
DDDF<-df_test[df_test$TreatmentLine_2_duration_days!="",]
g2<-ggplot(DDDF[!is.na(DDDF$TreatmentLine_2_type),],aes(x=TreatmentLine_2_type,y=TreatmentLine_2_duration_days))+ggtitle("Treatment line 2")+labs(x="",y="")+geom_boxplot()+theme(axis.text=element_text(size=10),
                                                                                                                                                                              axis.title=element_text(size=10),plot.title = element_text(size=12,face="bold"))
DDDF<-df_test[df_test$TreatmentLine_3_duration_days!="",]
g3<-ggplot(DDDF[!is.na(DDDF$TreatmentLine_3_type),],aes(x=TreatmentLine_3_type,y=TreatmentLine_3_duration_days))+ggtitle("Treatment line 3")+labs(x="",y="Duration days")+geom_boxplot()+theme(axis.text=element_text(size=10),                                                                                                                                                                                           axis.title=element_text(size=10),plot.title = element_text(size=12,face="bold"))

DDDF<-df_test[df_test$TreatmentLine_4_duration_days!="",]
g4<-ggplot(DDDF[!is.na(DDDF$TreatmentLine_4_type),],aes(x=TreatmentLine_4_type,y=TreatmentLine_4_duration_days))+ggtitle("Treatment line 4")+labs(x="Treatment type",y="")+geom_boxplot()+theme(axis.text=element_text(size=10),
                                                                                                                                                                                            axis.title=element_text(size=10),plot.title = element_text(size=12,face="bold"))

DDDF<-df_test[df_test$TreatmentLine_5_duration_days!="",]
g5<-ggplot(DDDF[!is.na(DDDF$TreatmentLine_5_type),],aes(x=TreatmentLine_5_type,y=TreatmentLine_5_duration_days))+ggtitle("Treatment line 5")+labs(x="Treatment type",y="Duration days")+geom_boxplot()+theme(axis.text=element_text(size=10),
                                                                                                                                                                                                         axis.title=element_text(size=10),plot.title = element_text(size=12,face="bold"))
ggplot2.multiplot(g1,g2,g3,g4,g5)

#non one of the five treatemnt
NonTreatmentPat<-df_test[,c("treatment_order","categorized_stage_at_dx")]
NonTreatmentPat$treatmentYesNo<-"Treated"
NonTreatmentPat[NonTreatmentPat$treatment_order=="",]["treatmentYesNo"]<-"NoTreated"
NonTreatmentPat<-NonTreatmentPat[-1]

xx<-as.data.frame(table(NonTreatmentPat[!(NonTreatmentPat$categorized_stage_at_dx==""),"categorized_stage_at_dx"]))        
names(xx)[names(xx)=="Var1"]<-"categorized_stage_at_dx"
x<-NonTreatmentPat[!(NonTreatmentPat$categorized_stage_at_dx==""),] %>%
  count(treatmentYesNo=factor(treatmentYesNo),categorized_stage_at_dx=factor(categorized_stage_at_dx))
xCombined <- left_join(x,xx,by="categorized_stage_at_dx") 
xCombined$pct<-xCombined$n/xCombined$Freq

 ggplot(data=xCombined,aes(x=categorized_stage_at_dx,y=pct,fill=treatmentYesNo,label = scales::percent(pct)))+geom_bar(stat = "identity",position = "dodge") + 
  geom_text(position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) + 
  scale_y_continuous(labels = scales::percent)+ggtitle("Percentage of Treated/NoTreated patiens per stage")


# non surgery..
Nonsurgery <-NonTreatmentPat[NonTreatmentPat$surgery==0,]
View(Nonsurgery[Nonsurgery$has_systemic_progression==1,])

#non palliative
NonPalliative<-Nonsurgery[Nonsurgery$palliative_care==0,]

CountElments1 = function(dataF) {
  
  Types <- colnames(dataF)
  comorbityToTreat=c()
  
  for(j in 1:nrow(dataF)){
   for(i in 1:length(Types)){
     if("1" == dataF[j,i]){
       comorbityToTreat <- c(comorbityToTreat,Types[i])
     }
     else
       next()
   }
  }
  x<-as.data.frame(table(comorbityToTreat))
return(x)
}




cardiopatia <- df_test %>%
  filter(numberOfComorbidities>0)
dfComorbity<- cardiopatia[,grepl("comorbidity_redcap",names(cardiopatia))]

dfComorbity<-dfComorbity[-1]
dfComorbity$TreatmentLine_2_type  <-cardiopatia$TreatmentLine_2_type
table(dfComorbity$TreatmentLine_2_type)
imm<-dfComorbity[dfComorbity$TreatmentLine_2_type=="imm",]

x<-CountElments1(imm)
x$type<-"qt"
gl1<-ggplot(x,aes(x=Freq,y=comorbityToTreat))+geom_bar(stat="identity")
gl2<-ggplot(x,aes(x=Freq,y=comorbityToTreat))+geom_bar(stat="identity")
gl3<-ggplot(x,aes(x=Freq,y=comorbityToTreat))+geom_bar(stat="identity")
gl4<-ggplot(x,aes(x=Freq,y=comorbityToTreat))+geom_bar(stat="identity")
gl5<-ggplot(x,aes(x=Freq,y=comorbityToTreat))+geom_bar(stat="identity")
ggplot2.multiplot(gl1,gl2,gl3,gl4,gl5, cols = 2)
x$type<-"imm"
glimm1<-ggplot(x,aes(x=Freq,y=comorbityToTreat))+geom_bar(stat="identity")
glimm2<-ggplot(x,aes(x=Freq,y=comorbityToTreat))+geom_bar(stat="identity")
glimm3<-ggplot(x,aes(x=Freq,y=comorbityToTreat))+geom_bar(stat="identity")
glimm4<-ggplot(x,aes(x=Freq,y=comorbityToTreat))+geom_bar(stat="identity")
glimm5<-ggplot(x,aes(x=Freq,y=comorbityToTreat))+geom_bar(stat="identity")
ggplot2.multiplot(glimm1,glimm2,glimm3,glimm4,glimm5, cols = 2)


xx<-df_test[,c("categorized_age_at_dx","stage","numberOfComorbidities","num_of_onco_lines","treatment_order","immunotherapy","chemotherapy","antiangiogenic","radiationtherapy","TreatmentLine_1_type","TreatmentLine_2_type","TreatmentLine_3_type","TreatmentLine_4_type","TreatmentLine_5_type","num_of_qt_lines","num_of_tki_lines","num_of_imm_lines","num_of_ant_lines")]
ggplot(xx,aes(numberOfComorbidities,num_of_onco_lines))+geom_point(aes(colour=categorized_age_at_dx))


sum(x$Freq)


View(df_test[,c("TreatmentLine_1_type","TreatmentLine_2_type","TreatmentLine_3_type","TreatmentLine_4_type","TreatmentLine_5_type","treatment_order")])
ggplot(df_test,aes(x=surgery,fill=as.factor(toxicities_GENERALES)))+geom_bar()


surgeryDFt<- df_test[df_test$surgery==0,]
surgeryDFF<- df_test[df_test$surgery==1,]
g1<-ggplot(surgeryDFt,aes(x=surgery,fill=as.factor(FRCV)))+geom_bar()
g2<-ggplot(surgeryDFF,aes(x=surgery,fill=as.factor(FRCV)))+geom_bar()



myplots <- list()  # new empty list
for (i in vectorTrearmnt) {
  x<-df_test[df_test[i]==1,]
  p1 <- eval(substitute(
    ggplot(data=x,aes(x=as.factor(dfToxicity[ ,i]),fill=as.factor(dfToxicity[ ,i])))+ 
      geom_bar(aes(y = (..count..)/sum(..count..))) + theme(legend.position = "none")+
      xlab(colnames(dfToxicity)[i])+ylab("Percent")+scale_y_continuous(labels=scales::percent)
    ,list(i = i)))
  
  myplots[[i]] <- p1  # add each plot into plot list
}
ggplot2.multiplot(plotlist = myplots, cols = 4)

require(scales)

ggplot(dfTreatment,aes(x=Treatment,y=Frequecy))+geom_bar(stat="identity")+
theme(legend.position = "top")+
labs(x="TypeOfTreatment",y="Number Of Pacientes")



#TRATMIENTO CON LAS FECHAS
x["TreatmentLine_2_init"]<-as.Date(x$TreatmentLine_2_init)
 x["DfrenceDays"]<-x$TreatmentLine_2_init-x$TreatmentLine_1_finish
 View(x[,c("TreatmentLine_2_init","TreatmentLine_1_finish","DfrenceDays")])
 
install.packages("ggmosaic")
library(ggmosaic)

ggplot(df_test[!(df_test$stage==""),]) + 
  geom_mosaic(aes(x=product(as.factor(numberOfComorbidities),as.factor(numberOfdrugs_beforedx)), fill = stage),
              divider = ddecker()) + theme(axis.text.x = element_text(angle = 15, hjust = 1))


ggplot()
ggplot(df_test,aes(TreatmentLine_1_numOfToxicities))+geom_histogram()
ggplot2.barplot(data=df_test,x=df_test$TreatmentLine_1_numOfToxicities)



# Data wrangling ----------------------------------------------------------

#death_cause
df_test[df_test$dead==1 & df_test$other_death_cause=="hemoptisis",]["death_cause"]<-"hemoptisis"
df_test[df_test$dead==1 & df_test$death_cause=="",]["death_cause"]<-"otra"
df_test$other_death_cause<-NULL

#comorbidity_redcap_otra
df_test[df_test$comorbidity_redcap_otra!="",]["comorbidity_redcap_otra"]<-1
df_test[df_test$comorbidity_redcap_otra=="",]["comorbidity_redcap_otra"]<-0

#symptom_redcap_otro
df_test[df_test$symptom_redcap_otro!="",]["symptom_redcap_otro"]<-1
df_test[df_test$symptom_redcap_otro=="",]["symptom_redcap_otro"]<-0

#smoker, smoker_yesNo
df_test[df_test$smoker=="",][c("smoker","smoker_yesNo")]<-"no"


#fix data error of TreatmentLine_1_short_tto
df_test[!is.na(df_test$TreatmentLine_1_duration_days) & df_test$TreatmentLine_1_duration_days<7 & df_test$TreatmentLine_1_short_tto=="no",]["TreatmentLine_1_short_tto"] <- "yes"
df_test[!is.na(df_test$TreatmentLine_1_duration_days) & df_test$TreatmentLine_1_duration_days>7 & df_test$TreatmentLine_1_short_tto=="yes",]["TreatmentLine_1_short_tto"]<-"no"


#remove variables numberOftoxicities_before2018, numberOftoxicities_after2018
df_test$numberOftoxicities_after2018<-NULL
df_test$numberOftoxicities_before2018<-NULL

#delete survivalDays_from_progression
df_test$survivalDays_from_progression<-df_test$survivalDays
df_test$survivalDays_from_progression<-NULL
df_test$survivalMonths_from_progression<-NULL
#x<-df_test[,c("survivalDays","survivalDays_from_1tto","survivalDays_from_2tto","survivalDays_from_3tto","survivalDays_from_progression")]
#View(x[x$survivalDays!=x$survivalDays_from_progression,c("survivalDays","survivalDays_from_progression")])

#delete redundant variables

df_test[,c("city","distancia","other_death_cause","firstDocumentDate","firstNoteDate","firstReportDate","numberOfNotes","numberOfReports",
           "clinical_trial","numberOftoxicities_before2018","numberOftoxicities_after2018","survival_12m","survival_3_months",
           "survivalMonths","survivalDays_from_progression")]<-NULL

df_test[,startsWith(names(df_test),"toxicities_")]<-NULL
#df_test[,grepl("Mutated_",names(df_test)) | grepl("time_from_",names(df_test)) | grepl("_mutation_date",names(df_test))] <-NULL
df_test[,grepl("time_from_",names(df_test)) | grepl("_mutation_date",names(df_test))] <-NULL

# Statistical test --------------------------------------------------------
  DF_TEST_NEW<-df_test

# compare means of groups============
### Smoking vs gender using wilcox.test(alternative of t-tes) ######
#assump2: samples are drawn form population with same variance
var.test(df_test$smoking_CxY~df_test$gender) #do logarith transformation on the continuos variable if not met
df_test$smoking_CxY<-log(df_test$smoking_CxY)
##assump3: samples are drwan fom population with a normal distribution, if not met, we can use non parametric test, like Man..
male<-subset(DF_TEST_NEW,gender=="Male")[,c("smoking_CxY","gender")]
Female<-subset(DF_TEST_NEW,gender=="Female")[,c("smoking_CxY","gender")]
#normal quantile plot
qqMale<-as.data.frame(qqnorm(male$smoking_CxY,main="Normal Q-Q plot for Males"))
qqline(male$smoking_CxY,col="red")
qqFemale<-as.data.frame(qqnorm(Female$smoking_CxY,main="Normal Q-Q plot for Females"))
qqline(Female$smoking_CxY,col="red")

plotNormalHistogram(Female$smoking_CxY)
plotNormalHistogram(male$smoking_CxY)
T_tuk = 
  transformTukey(male$smoking_CxY,
                 plotit=FALSE)
plotNormalHistogram(T_tuk)
T_cub = sign(male$smoking_CxY) * abs(male$smoking_CxY)^(1/3)
plotNormalHistogram(T_cub)
T_cub = sign(Female$smoking_CxY) * abs(Female$smoking_CxY)^(1/3)
plotNormalHistogram(T_cub)

outMale<- head(sort(qqMale$y,decreasing = TRUE),2)
male<-male[-which(qqMale$y %in% outMale),]

outFemale<- head(sort(qqFemale$y,decreasing = TRUE),4)
Female<-Female[-which(qqFemale$y %in% outFemale),]
df<-rbind(male,Female)
#mu=0 because the null hypothesis is there is no diffrence between two groups
wilcox.test(df_test$smoking_CxY~df_test$gender,mu=0,alt="two.sided",correct=TRUE,paired=FALSE,conf.int=TRUE,data=df_test)



### survivadays vs categorzied stage #######
# anova one way 
#(one x variable, one y variable) or alternatives of anova

x<-df_test[df_test$dead==1 & df_test$categorized_stage_at_dx!="",]

AVOModel <- aov(survivalDays_box~factor(categorized_stage_at_dx),data=df_test)
summary(AVOModel)
res=AVOModel$residuals

df_test[df_test$survivalDays==0,"survivalDays"]<-0.001

advanced<-subset(df_test,categorized_stage_at_dx=="Advanced")[,c("survivalDays","categorized_stage_at_dx")]
early<-subset(df_test,categorized_stage_at_dx=="Early")[,c("survivalDays","categorized_stage_at_dx")]
intermediate<-subset(df_test,categorized_stage_at_dx=="Intermediate")[,c("survivalDays","categorized_stage_at_dx")]

plotNormalHistogram(df_test$survivalDays,title("normal histogram before transformation"))

par(mfrow=c(1,3))
qqnorm(advanced$survivalDays,main="SurvivalDays of advanced groups",cex.main=1.2)
qqline(advanced$survivalDays,col="red")
qqnorm(early$survivalDays,main="SurvivalDays of early groups",cex.main=1.2)
qqline(early$survivalDays,col="red")
qqnorm(intermediate$survivalDays,main="SurvivalDays of intermediate groups",cex.main=1.2)
qqline(intermediate$survivalDays,col="red")


install.packages("rcompanion")
library(rcompanion)

plotNormalHistogram(advanced$survivalDays)
#Square root transformation
plotNormalHistogram(sqrt(advanced$survivalDays))
#Cube root transformation
T_cub = sign(advanced$survivalDays) * abs(advanced$survivalDays)^(1/3)
plotNormalHistogram(T_cub)
skewness(T_cub)
kurtosis(T_cub)

#advanced[advanced$survivalDays==0,"survivalDays"]<-0.001
## Log transformation
#plotNormalHistogram(log(advanced$survivalDays))
## Tukey’s Ladder of Powers transformation
T_tuk = 
  transformTukey(advanced$survivalDays,
                 plotit=FALSE)
plotNormalHistogram(T_tuk)

# BOXCOX
  library(MASS)
  
  Box = boxcox(survivalDays ~ categorized_stage_at_dx,
               data = df_test,
               lambda = seq(-6,6,0.1)
  )
  
  Cox = data.frame(Box$x, Box$y)
  
  Cox2 = Cox[with(Cox, order(-Cox$Box.y)),]
  
  Cox2[1,]
  
  lambda = Cox2[1, "Box.x"]
  
  df_test$survivalDays_box = (df_test$survivalDays ^ lambda - 1)/lambda  
  plotNormalHistogram(df_test$survivalDays_box,title("normal histogram after transformation"))



par(mfrow=c(1,3))
qqnorm(df_test[df_test$categorized_stage_at_dx=="Advanced",]$survivalDays_box,main="SurvivalDays of advanced groups",cex.main=1.2)
qqline(df_test[df_test$categorized_stage_at_dx=="Advanced",]$survivalDays_box,col="red")
qqnorm(df_test[df_test$categorized_stage_at_dx=="Early",]$survivalDays_box,main="SurvivalDays of early groups",cex.main=1.2)
qqline(df_test[df_test$categorized_stage_at_dx=="Early",]$survivalDays_box,col="red")
qqnorm(df_test[df_test$categorized_stage_at_dx=="Intermediate",]$survivalDays_box,main="SurvivalDays of intermediate groups",cex.main=1.2)
qqline(df_test[df_test$categorized_stage_at_dx=="Intermediate",]$survivalDays_box,col="red")

##test equal variance
bartlett.test(survivalDays_box~factor(categorized_stage_at_dx),data=df_test)
#perform Welch's anoxa
oneway.test(survivalDays_box~factor(categorized_stage_at_dx),data=df_test,
            var.equal = FALSE)
### Compute post-hoc statistics using the games-howell method
posthocTGH(df_test$survivalDays_box,df_test$categorized_stage_at_dx)

leveneTest(df_test$survivalDays_box,df_test$categorized_stage_at_dx)
skewness(df_test$survivalDays_box)
kurtosis(df_test$survivalDays_box)



TukeyHSD(AVOModel)#check differences in means
plot(TukeyHSD(AVOModel),las=1,cex.lab=0.3)
# comment https://www.r-graph-gallery.com/84-tukey-test.html

kruskal.test(survivalDays~categorized_stage_at_dx,data=x)
AVOModel2 <- aov(survivalDays~.,data=df_test)
summary(AVOModel2)

## duration days vs type
df_test[,c("TreatmentLine_1_type","TreatmentLine_1_duration_days","TreatmentLine_2_type","TreatmentLine_2_duration_days","TreatmentLine_3_type","TreatmentLine_3_duration_days","TreatmentLine_4_type","TreatmentLine_4_duration_days","TreatmentLine_5_type","TreatmentLine_5_duration_days")]

# Find relationships =======

### Binomial Logistic regression for survival ######

# check linearytiy
x<-DF_TEST_NEW
xFactorizedColumnsNames<- colnames(x[,!names(x) %in% c("EHR","birthDate","startAge","deathDate","diagnosisDate","lastDocumentDate","monthsInTheSystem","smoking_CxY","diagnosisYear","age_of_diagnosis","age_of_death","survivalDays","numberOfComorbidities","numberOfsymptoms","numberOfdrugs_beforedx","numberOftoxicities")])
x[xFactorizedColumnsNames]<- lapply(x[xFactorizedColumnsNames],factor)
df<-x[x$categorized_stage_at_dx!="",]

xtabs(~TreatmentLine_1_yes.no+numberOfComorbidities,data =x )
df_logisticz<-df[c("categorized_stage_at_dx","startAge","gender","smoker_yesNo","Riesgo_Radon","numberOfComorbidities","numberOfsymptoms","numberOfdrugs_beforedx","numberOftoxicities","has_systemic_progression","brain_metastasis","af","FRCV","survival")]
df_logisticz<-df_logisticz[-c(81,303,676),]
logistic <-glm(TreatmentLine_1_yes.no~.,data = df_logisticz,family = "binomial")
logistic <-glm(survival~.,data = df_logisticz,family = "binomial")
summary(logistic)


probabilities <- predict(logistic, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, 1, 0)
head(predicted.classes)

# Select only numeric predictors
mydata <- df_logisticz %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
library(tidyr)
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")


plot(logistic, which = 4, id.n = 3)

model.data <- augment(logistic) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = survival), alpha = .5) +
  theme_bw()
model.data %>% 
  filter(abs(.std.resid) > 3)


dfToxicity <- x[,startsWith(names(x),"toxicities2_")]
dfToxicity$toxicities2_OTOLARYNGOLOGY_new<- NULL  #all instances are 0
dfToxicity$survival<-x$survival
logistic <-glm(survival~.,data = dfToxicity,family = "binomial")
xtabs(~TreatmentLine_5_yes.no+toxicities2_GENERALES_new+toxicities2_GASTROINTESTINALES_new,data =dfToxicity )


dfComor<-x[,startsWith(names(x),"comorbidity_redcap_")]
dfComor$survival<-x$survival
logistic <-glm(survival~.,data = dfComor,family = "binomial")
chisq.test(dfComor$TreatmentLine_5_yes.no,dfComor$comorbidity_redcap_hipertension)


dfSymptoms<-x[,startsWith(names(x),"symptom_redcap")]#no influence
dfSymptoms$survival<-x$survival
logistic <-glm(survival~.,data = dfSymptoms,family = "binomial")


dfdrugGroup<-x[,startsWith(names(x),"drugGroup_")] #no influence
dfdrugGroup$drugGroup_analgesico<-NULL
dfdrugGroup$TreatmentLine_2_yes.no<-x$TreatmentLine_2_yes.no
logistic <-glm(TreatmentLine_2_yes.no~.,data = dfdrugGroup,family = "binomial")
summary(logistic)

probabilities <- predict(logistic, type = "response")
confusionMatrixTrain<-table(dfdrugGroup$TreatmentLine_2_yes.no,probabilities >= 0.5)
AccuracyTrain<-(confusionMatrixTrain[1,1]+confusionMatrixTrain[2,2])/nrow(train)


### TreatmentLine_X_catNumOfToxicities for numerical variables #######
library(nnet)
x<-DF_TEST_NEW
xFactorizedColumnsNames<- colnames(x[,!names(x) %in% c("EHR","birthDate","startAge","deathDate","diagnosisDate",
                                                       "lastDocumentDate","monthsInTheSystem","smoking_CxY","diagnosisYear",
                                                       "age_of_diagnosis","age_of_death","survivalDays","numberOfComorbidities",
                                                       "numberOfsymptoms","numberOfdrugs_beforedx","numberOftoxicities","TreatmentLine_1_duration_days",
                                                       "TreatmentLine_1_posology","num_of_onco_lines","TreatmentLine_2_duration_days","TreatmentLine_3_duration_days",
                                                       "TreatmentLine_4_duration_days","TreatmentLine_5_duration_days")])
x[xFactorizedColumnsNames]<- lapply(x[xFactorizedColumnsNames],factor)
df1<-x[x$categorized_stage_at_dx!="" & x$TreatmentLine_1_type!="",]
df2<-x[x$categorized_stage_at_dx!="" & x$TreatmentLine_2_type!="",]
df3<-x[x$categorized_stage_at_dx!="" & x$TreatmentLine_3_type!="",]
df4<-x[x$categorized_stage_at_dx!="" & x$TreatmentLine_4_type!="",]
df5<-x[x$categorized_stage_at_dx!="" & x$TreatmentLine_5_type!="",]

xtabs(~TreatmentLine_1_catNumOfToxicities+startAge,data =df)
#---
df_logisticz<-df1[c("startAge","numberOfComorbidities","numberOfsymptoms","numberOfdrugs_beforedx","numberOftoxicities","TreatmentLine_1_duration_days","TreatmentLine_1_type")]
#df_logisticz<-df_logisticz[-c(81,303,676),]
logistic <-multinom(TreatmentLine_1_type~.,data = df_logisticz,Hess = TRUE)
summary(logistic)

probabilities <- predict(logistic, type = "response")

  
  if(probabilities <0.25){
    predicted.classes<-"ant"
    
  }else if(0.25<probabilities<0.5){
  predicted.classes<-"imm"
  } else if(0.5<probabilities<0.75){
  predicted.classes<-"tki"
  } else if(0.75<probabilities<1){
  predicted.classes<-"qt"
  }
head(predicted.classes)

# Select only numeric predictors
mydata <- df_logisticz %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
# Bind the logit and tidying the data for plot
library(tidyr)
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

plot(logistic, which = 4, id.n = 3)

model.data <- augment(logistic) %>% 
  mutate(index = 1:n()) 
model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = TreatmentLine_1_yes.no), alpha = .5) +
  theme_bw()
model.data %>% 
  filter(abs(.std.resid) > 3)






multinom <-multinom(TreatmentLine_5_catNumOfToxicities~startAge+
                      numberOfComorbidities+numberOfsymptoms+numberOfdrugs_beforedx+numberOftoxicities+
                      TreatmentLine_5_duration_days,data = df5,Hess = TRUE)
summary(multinom)

# 2- tailed Z-test
z<- summary(multinom)$coefficients/summary(multinom)$standard.errors
pValues1 <- (1-pnorm(abs(z),0,1))*2

pValues2 <- (1-pnorm(abs(z),0,1))*2
pValues3 <- (1-pnorm(abs(z),0,1))*2
pValues4 <- (1-pnorm(abs(z),0,1))*2
pValues5 <- (1-pnorm(abs(z),0,1))*2

df5<-df5[,c("TreatmentLine_5_catNumOfToxicities","startAge","numberOfComorbidities","numberOfsymptoms","numberOfdrugs_beforedx","numberOftoxicities","TreatmentLine_5_duration_days")]
library(lmtest)
library(mlogit)        
data3 <-mlogit.data(df5,varing=NULL,choice="TreatmentLine_5_catNumOfToxicities",shape="wide")
head(data3) 
model.5<-mlogit(TreatmentLine_5_catNumOfToxicities~1 | startAge+numberOfComorbidities+numberOfsymptoms+numberOfdrugs_beforedx+numberOftoxicities+
                  TreatmentLine_5_duration_days,data=data3,reflevel = "0")
                  
summary(multinom)


### TreatmentLine_X_type for numerical variables #######
multinom <-multinom(TreatmentLine_4_type~startAge+
                      numberOfComorbidities+numberOfsymptoms+numberOfdrugs_beforedx+numberOftoxicities+
                      TreatmentLine_4_duration_days,data = df4,Hess = TRUE)
summary(multinom)

# 2- tailed Z-test
z<- summary(multinom)$coefficients/summary(multinom)$standard.errors
pValues1 <- (1-pnorm(abs(z),0,1))*2
pValues2 <- (1-pnorm(abs(z),0,1))*2
pValues3 <- (1-pnorm(abs(z),0,1))*2
pValues4 <- (1-pnorm(abs(z),0,1))*2


### pvalue using chic-square or alternatices for TreatmentLine_X_catNumOfToxicities and TreatmentLine_X_type  ######
library(tidyr)
CalulateExpectedCell = function(ve) {
 #formulate margin table
  newVe<-spread(ve,colnames(ve)[2],Freq)
  dim<-dim(newVe)

  vector<-c("Sum")
  for(i in 2:dim[2]){
  vector<-c(vector,sum(newVe[,i]))
  }
  xx<-data.frame(matrix(vector,nrow = 1))
  names(xx)<-colnames(newVe)
  newVe<- rbind(newVe,xx)
  
  vectorCol<-c()
  newVe[,-1]<- lapply(newVe[,-1], as.numeric)
  for(i in 1:dim(newVe)[1]){
    vectorCol<-c(vectorCol,sum(newVe[i,2:dim(newVe)[2]]))
  }
  newVe$sum<-vectorCol
 
   #Calulate espected cell to test chic-square assumption
  Edf<-newVe[-dim(newVe)[1],-dim(newVe)[2]]
  for(i in 2:dim(Edf)[2]){#col 2:5
    for(j in 1:dim(Edf)[1]){#row 1:4
      Edf[j,i]=(as.numeric(newVe[dim(newVe)[1],i])*as.numeric(newVe[j,dim(newVe)[2]]))/as.numeric(newVe[dim(newVe)[1],dim(newVe)[2]])
    }
  }
 return(Edf)
}
CategoricalAttributes<-c("categorized_stage_at_dx","gender","smoker_yesNo","has_systemic_progression","brain_metastasis","af","Riesgo_Radon", "FRCV", "stage","ps_at_diagnosis")

pvalueTreatmentType<- function(y,responseVar,specialCol){
df<-y[y["categorized_stage_at_dx"]!="" & y[responseVar]!="",]
Line1Toxi<-df[,c(responseVar,CategoricalAttributes,specialCol)]

ve<-apply(Line1Toxi[,-1], 2, function(x) as.data.frame(xtabs(~Line1Toxi[[responseVar]]+x)))
expectedCell<-lapply(ve,function(x) CalulateExpectedCell(x))

colmnna<-colnames(Line1Toxi)[-1]
pvalue2<-Line1Toxi %>% 
  select(colmnna) %>%
  summarise_all(funs(chisq.test(.,Line1Toxi[[responseVar]])$p.value))%>%
  as.data.frame()
#colnames(pvalue1)[7]<-"TreatmentLine_X_type"
#colnames(pvalue1)[8]<-"TreatmentLine_X_short_tto"
return(list(pvalue2,expectedCell))
}
#result1<-pvalueTreatmentType(x,"TreatmentLine_1_type","TreatmentLine_1_short_tto")
result1<-pvalueTreatmentType(x,"TreatmentLine_1_catNumOfToxicities",c("TreatmentLine_1_type","TreatmentLine_1_short_tto"))
pvalue1<-as.data.frame(result1[1])
expectedCell1<-result1[2]
#names(pvalue1)[length(pvalue1)]<-"TreatmentLine_X_short_tto"
names(pvalue1)[c(length(pvalue1)-1,length(pvalue1))]<-c("TreatmentLine_X_type","TreatmentLine_X_short_tto")

#result2<-pvalueTreatmentType(x,"TreatmentLine_2_type","TreatmentLine_2_short_tto")
result2<-pvalueTreatmentType(x,"TreatmentLine_2_catNumOfToxicities",c("TreatmentLine_2_type","TreatmentLine_2_short_tto"))
pvalue2<-as.data.frame(result2[1])
result2[2]
#names(pvalue2)[length(pvalue2)]<-"TreatmentLine_X_short_tto"
names(pvalue2)[c(length(pvalue2)-1,length(pvalue2))]<-c("TreatmentLine_X_type","TreatmentLine_X_short_tto")

result3<-pvalueTreatmentType(x,"TreatmentLine_3_type","TreatmentLine_3_short_tto")
result3<-pvalueTreatmentType(x,"TreatmentLine_3_catNumOfToxicities",c("TreatmentLine_3_type","TreatmentLine_3_short_tto"))
pvalue3<-as.data.frame(result3[1])
expectedCell3<-result3[2]
#names(pvalue3)[length(pvalue3)]<-"TreatmentLine_X_short_tto"
names(pvalue3)[c(length(pvalue3)-1,length(pvalue3))]<-c("TreatmentLine_X_type","TreatmentLine_X_short_tto")


#result4<-pvalueTreatmentType(x,"TreatmentLine_4_type","TreatmentLine_4_short_tto")
result4<-pvalueTreatmentType(x,"TreatmentLine_4_catNumOfToxicities",c("TreatmentLine_4_type","TreatmentLine_4_short_tto"))
pvalue4<-as.data.frame(result4[1])
expectedCell4<-result4[2]
#names(pvalue4)[length(pvalue4)]<-"TreatmentLine_X_short_tto"
names(pvalue4)[c(length(pvalue4)-1,length(pvalue4))]<-c("TreatmentLine_X_type","TreatmentLine_X_short_tto")

#result5<-pvalueTreatmentType(x,"TreatmentLine_5_type","TreatmentLine_5_short_tto")
result5<-pvalueTreatmentType(x,"TreatmentLine_5_catNumOfToxicities",c("TreatmentLine_5_type","TreatmentLine_5_short_tto"))
pvalue5<-as.data.frame(result5[1])
expectedCell5<-result5[2]
#names(pvalue5)[length(pvalue5)]<-"TreatmentLine_X_short_tto"
names(pvalue5)[c(length(pvalue5)-1,length(pvalue5))]<-c("TreatmentLine_X_type","TreatmentLine_X_short_tto")

pvalue<-NULL
pvalue<-rbind(pvalue1,pvalue2,pvalue3,pvalue4,pvalue5)
#pvalue$TreatmentLine_X_type<-c("TreatmentLine_1_type","TreatmentLine_2_type","TreatmentLine_3_type",
                                 #            "TreatmentLine_4_type","TreatmentLine_5_type")
pvalue$TreatmentLine_X_catNumOfToxicities<-names(x[,grepl("_catNumOfToxicities",names(x))])

#colReorder<-c("TreatmentLine_X_type",colnames(pvalue[,-length(pvalue)]))
colReorder<-c("TreatmentLine_X_catNumOfToxicities",colnames(pvalue[,-length(pvalue)]))
pvalue<-pvalue[,colReorder]


#


#TreatmentLine_1_catNumOfToxicities
df<-x[x$categorized_stage_at_dx!="" & x$TreatmentLine_1_type!="",]
Line1Toxi<-df[,c("TreatmentLine_1_catNumOfToxicities","categorized_stage_at_dx","gender","smoker_yesNo","has_systemic_progression","brain_metastasis","af","TreatmentLine_1_type","TreatmentLine_1_short_tto","Riesgo_Radon", "FRCV", "stage")]
  ve<-apply(Line1Toxi[,-1], 2, function(x) as.data.frame(xtabs(~Line1Toxi$TreatmentLine_1_catNumOfToxicities+x)))
  lapply(ve,function(x) CalulateExpectedCell(x))

colmnna<-colnames(Line1Toxi)[-1]
pvalue1<-Line1Toxi %>% 
  select(colmnna) %>%
  summarise_all(funs(chisq.test(.,Line1Toxi$TreatmentLine_1_catNumOfToxicities)$p.value))%>%
  as.data.frame()
colnames(pvalue1)[7]<-"TreatmentLine_X_type"
colnames(pvalue1)[8]<-"TreatmentLine_X_short_tto"
#TreatmentLine_2_catNumOfToxicities
df<-x[x$categorized_stage_at_dx!="" & x$TreatmentLine_2_type!="",]
Line2Toxi<-df[,c("TreatmentLine_2_catNumOfToxicities","categorized_stage_at_dx","gender","smoker_yesNo","has_systemic_progression","brain_metastasis","af","TreatmentLine_2_type","TreatmentLine_2_short_tto","Riesgo_Radon", "FRCV", "stage")]
ve<-apply(Line2Toxi[,-1], 2, function(x) as.data.frame(xtabs(~Line2Toxi$TreatmentLine_2_catNumOfToxicities+x)))
lapply(ve,function(x) CalulateExpectedCell(x))

colmnna<-colnames(Line2Toxi)[-1]
pvalue2<-Line2Toxi %>% 
  select(colmnna) %>%
  summarise_all(funs(chisq.test(.,Line2Toxi$TreatmentLine_2_catNumOfToxicities)$p.value))%>%
  as.data.frame()
colnames(pvalue2)[7]<-"TreatmentLine_X_type"
colnames(pvalue2)[8]<-"TreatmentLine_X_short_tto"

#cramers <- apply(ve[[1]], 2, cramersV)

#TreatmentLine_3_catNumOfToxicities
df<-x[x$categorized_stage_at_dx!="" & x$TreatmentLine_3_type!="",]
Line3Toxi<-df[,c("TreatmentLine_3_catNumOfToxicities","categorized_stage_at_dx","gender","smoker_yesNo","has_systemic_progression","brain_metastasis","af","TreatmentLine_3_type","TreatmentLine_3_short_tto","Riesgo_Radon", "FRCV", "stage")]
ve<-apply(Line3Toxi[,-1], 2, function(x) as.data.frame(xtabs(~Line3Toxi$TreatmentLine_3_catNumOfToxicities+x)))
lapply(ve,function(x) CalulateExpectedCell(x))

colmnna<-colnames(Line3Toxi)[-1]
pvalue3<-Line3Toxi %>% 
  select(colmnna) %>%
  summarise_all(funs(chisq.test(.,Line3Toxi$TreatmentLine_3_catNumOfToxicities)$p.value))%>%
  as.data.frame()
colnames(pvalue3)[7]<-"TreatmentLine_X_type"
colnames(pvalue3)[8]<-"TreatmentLine_X_short_tto"
chisq.test(Line3Toxi$categorized_stage_at_dx,Line3Toxi$TreatmentLine_3_catNumOfToxicities)

##TreatmentLine_4_catNumOfToxicities
df<-x[x$categorized_stage_at_dx!="" & x$TreatmentLine_4_type!="",]
Line4Toxi<-df[,c("TreatmentLine_4_catNumOfToxicities","categorized_stage_at_dx","gender","smoker_yesNo","has_systemic_progression","brain_metastasis","af","TreatmentLine_4_type","TreatmentLine_4_short_tto","Riesgo_Radon", "FRCV", "stage")]
ve<-apply(Line4Toxi[,-1], 2, function(x) as.data.frame(xtabs(~Line4Toxi$TreatmentLine_4_catNumOfToxicities+x)))
lapply(ve,function(x) CalulateExpectedCell(x))

colmnna<-colnames(Line4Toxi)[-1]
pvalue4<-Line4Toxi %>% 
  select(colmnna) %>%
  summarise_all(funs(chisq.test(.,Line4Toxi$TreatmentLine_4_catNumOfToxicities)$p.value))%>%
  as.data.frame()
colnames(pvalue4)[7]<-"TreatmentLine_X_type"
colnames(pvalue4)[8]<-"TreatmentLine_X_short_tto"
pvalue4[7]<-GTest(as.matrix(xtabs(~TreatmentLine_4_catNumOfToxicities+TreatmentLine_4_type,data =Line4Toxi)),correct="none")$p.value

## TreatmentLine_5_catNumOfToxicities
df<-x[x$categorized_stage_at_dx!="" & x$TreatmentLine_5_type!="",]
Line5Toxi<-df[,c("TreatmentLine_5_catNumOfToxicities","categorized_stage_at_dx","gender","smoker_yesNo","has_systemic_progression","brain_metastasis","af","TreatmentLine_5_type","TreatmentLine_5_short_tto","Riesgo_Radon", "FRCV", "stage")]
ve<-apply(Line5Toxi[,-1], 2, function(x) as.data.frame(xtabs(~Line5Toxi$TreatmentLine_5_catNumOfToxicities+x)))
lapply(ve,function(x) CalulateExpectedCell(x))
pValue5<-c()
for(i in 1:length(ve)){
  pValue5<- c(pValue5,GTest(as.matrix(spread(ve[[7]],colnames(ve[[7]])[2],Freq)[-1]),correct="none")$p.value)
}
pValue5<-data.frame(matrix(pValue5,nrow = 1))
names(pValue5)<-colnames(Line5Toxi[,-1])
colnames(pValue5)[7]<-"TreatmentLine_X_type"
colnames(pValue5)[8]<-"TreatmentLine_X_short_tto"

#newVe<-xtabs(~TreatmentLine_5_catNumOfToxicities+TreatmentLine_5_type,data =Line5Toxi )
#Matriz = as.matrix(xtabs(~TreatmentLine_5_catNumOfToxicities+TreatmentLine_5_type,data =Line5Toxi ))
#GTest(Matriz,correct="none") 
pvalue<-NULL
pvalue<-rbind(pvalue1,pvalue2,pvalue3,pvalue4,pValue5)
pvalue$TreatmentLine_X_catNumOfToxicities<-c("TreatmentLine_1_catNumOfToxicities","TreatmentLine_2_catNumOfToxicities","TreatmentLine_3_catNumOfToxicities",
                                             "TreatmentLine_4_catNumOfToxicities","TreatmentLine_5_catNumOfToxicities")

colReorder<-c("TreatmentLine_X_catNumOfToxicities",colnames(pvalue[,-length(pvalue)]))
pvalue<-pvalue[,colReorder]


# Modeling -----------
# check linearytiy
DF_TEST_NEW<-df_test[df_test$survival!="",]
# DF_TEST_NEW$survival<-df_patient_integrated_info[DF_TEST_NEW]
x<-DF_TEST_NEW

# plot pie chart of observations to be analyzed=========
tableSurvival<- data.frame(
  group=names(table(x$survival)),
  value=c(table(x$survival)[[1]],table(x$survival)[[2]])
)
blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

bp<- ggplot(tableSurvival, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
pie <- bp + coord_polar("y", start=0)+ggtitle("Distribution of survival population
             with sample size")
pie + scale_fill_brewer("Survival types")+ blank_theme +
  theme(axis.text.x=element_blank())+
  geom_text(aes(y = value/3 + c(0, value[-length(value)]), 
                label = value), size=5)+labs(fill="Survival")

# filter survival valid information ======
Comor<-colnames(x[,startsWith(names(x),"comorbidity_redcap_")])
Mutated<-colnames(x[,startsWith(names(x),"Mutated_")])
Gen<-c("gender","Riesgo_Radon","stage_at_dx","af","brain_metastasis","smoker_yesNo","FRCV","stage","categorized_age_at_dx","numberOfComorbidities","surgery","radiationtherapy","survival","palliative_care_in_1mon_to_death","palliative_care_before_1mon_to_death","overtreated","has_stage_IV_date","has_systemic_progression","ps_at_diagnosis")
TreatmentLine1<-colnames(x[,startsWith(names(x),"TreatmentLine_1_")])[-c(1,2,4,7,24,28,6)]
TreatmentLine2<-colnames(x[,startsWith(names(x),"TreatmentLine_2_")])[-c(1,2,4,7,24,28,6)]

AttributesForClassification<- c(Comor,Mutated,Gen,TreatmentLine1,TreatmentLine2)
dfClass <- x[,AttributesForClassification]
#dfClass<-dfClass[dfClass$TreatmentLine_1_type!="",]
dfClass[,c("ps_at_diagnosis")]<-sapply(dfClass[,c("ps_at_diagnosis")], function(x) ifelse(is.na(x),"",x))
xFactorizedColumnsNames<- colnames(dfClass[,!names(dfClass) %in% c("TreatmentLine_1_duration_days","TreatmentLine_2_duration_days","TreatmentLine_1_LGID","TreatmentLine_2_LGID","numberOfComorbidities","TreatmentLine_2_numOfToxicities","TreatmentLine_1_numOfToxicities")])
dfClass[xFactorizedColumnsNames]<- sapply(dfClass[xFactorizedColumnsNames],factor)

df<-removeAttributesZeroOnes(dfClass)


dfClass[xFactorizedColumnsNames]<- lapply(dfClass[xFactorizedColumnsNames],factor)

dfClassnUEVO<-as.data.frame(lapply(dfClass[,! names(dfClass) %in% c("survival")],function(x) as.numeric(x)))
dfClassnUEVO$survival<-dfClass$survival


# FAMD======
dfClassFAMD<-dfClass[,! names(dfClass) %in% c("survival")]
res.famd<-FAMD(dfClassFAMD,graph = FALSE,ncp = 165)
summary(res.famd)
get_eigenvalue(res.famd)[1:10,]
plot(get_eigenvalue(res.famd)[1:62,3],xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
      type="b")

res <- get_famd_ind(res.famd)
var.ind<-res$coord[,1:62]
newDataSet<-data.frame(var.ind,survival=dfClass$survival)
head(newDataSet,5)

fviz_screeplot(res.famd,ncp=62)
fviz_eig(res.famd, choice = "eigenvalue", addlabels=TRUE,ncp = 12,hjust = -0.3)
var <- get_famd_var(res.famd)
var.ind<-res.famd$ind
newDataSet<-head(var[,1:62],14)
var.ind<-get_famd(res.famd, element ="ind")


# Coordinates of variables
head(var$coord)
# Cos2: quality of representation on the factore map
head(var$cos2)
# Contributions to the  dimensions
head(var$contrib[,1:6])

# Plot of variables
fviz_famd_var(res.famd, repel = TRUE)


# Contribution to the first dimension
fviz_contrib(res.famd, "var", axes = 1)
fviz_contrib(res.famd, "var", axes = 2)
fviz_contrib(res.famd, "var", axes = 3)
# Contribution to the second dimension
# Variable contributions on axes 1》62
fviz_contrib(res.famd, "var", axes = 1:62)

summary(res)



# cfs feature selection ========
result<- as.vector(cfs(survival ~., dfClass))
dfCFS<-  dfClass[, c(result,"survival")]
#dfCFST for neuralNetwork
dfCFST<- as.data.frame(lapply(dfCFS[,-length(dfCFS)],function(x) as.numeric(x)))
dfCFST$survival<-dfCFS$survival

# modelling classifiers for survival========

folds = createFolds(newDataSet$survival, k = 10)
folds = createFolds(dfCFS$survival, k = 10)
folds = createFolds(dfCFST$survival, k = 10)#transform to numerical value for neural network
MatrixFocAUC<-list()
# in cv we are going to applying a created function to our 'folds'
cv = lapply(folds, function(x) { 

  training_fold = dfCFST[-x, ] 
  test_fold = dfCFST[x, ]
 # training_fold = dfCFS[-x, ] 
 # test_fold = dfCFS[x, ] 
  
  # now apply (train) the classifer on the training_fold
 
    # classifier<- svm(survival~., data = training_fold,probability=TRUE)    
    # prob = attr(predict(classifier, newdata = test_fold,probability=TRUE),"probabilities")[,"short"]
    
    
   # classifier<- randomForest(survival~.,data=training_fold,ntree=500)
    classifier<- rpart(survival~.,data=training_fold,method = 'class')
    prob = predict(classifier, newdata = test_fold,type="pro")[,"short"]
   
    

  #neural network
    # classifier = neuralnet((survival=="short") ~ .,
    #                        data = training_fold,hidden = 3, linear.output = FALSE, threshold=0.01)
    # 
    #  y_pred=compute(classifier,test_fold[,-length(test_fold)])
    #  prob<-y_pred$net.result
      y_pred<- ifelse(prob>0.5,"short","long")
     
     cm = table(y_pred,test_fold$survival)
  
  accuracy = (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
  Precision=cm[1,1]/(cm[1,1]+cm[1,2])
  Recall=cm[1,1]/(cm[1,1]+cm[2,1])
  MatrixFocAUC<-list(prob,as.numeric(as.factor(test_fold$survival)))
  mm<-matrix(c(accuracy,Precision,Recall))
  return(list(mm,MatrixFocAUC))
})


cvm<-matrix(,nrow=3,ncol = 10)
predicted<-list()
observ<-list()
##Accuracy,Precision,Recall
for (i in 1:length(cv)) {
  cvm[,i]<-cv[i][[1]][[1]]
  
}

for (i in 1:length(cv)) {
  predicted[[i]]<-cv[i][[1]][[2]][[1]]
  
}
for (i in 1:length(cv)) {
  observ[[i]]<-cv[i][[1]][[2]][[2]]
  
}

out<-cvAUC(predicted,observ)
outRFcfs<-out
outRFfamd<-out
outDTfamd<-out
outSVMfamd<-out
outNNfamd<-out
outNNCFS<-out
outSVMCFS<-out
outDTCFS<-out
#Plot fold AUCs
plot(out$perf, col="grey82", lty=3, main="10-fold CV AUC")

#Plot CV AUC
plot(out$perf, col="red", avg="vertical", add=TRUE)

plot(outRFfamd$perf, col="red", avg="vertical", add=TRUE)
plot(outSVM$perf, col="red", avg="vertical", add=TRUE)
abline(a=0,b=1)
legend(.7,.3,round(out$cvAUC,4),title="cv AUC",cex=0.4)


accuracy = mean(as.numeric(cvm[1,]))
Precision = mean(as.numeric(cvm[2,]))
Recall = mean(as.numeric(cvm[3,]))
par(mfrow=c(1,3))

boxplot(cv[1,],main="Accuracy")
boxplot(cv[2,],main="Precision")
boxplot(cv[3,],main="Recall")


classifier<- rpart(survival~.,data=newDataSet,method = 'class')
rpart.plot(classifier,main="Classification tree")
summary(classifier)
printcp(classifier)
plotcp(classifier)
ptree<- prune(classifier, cp= classifier$cptable[which.min(classifier$cptable[,"xerror"]),"CP"])
rpart.plot(ptree, main="Pruned Classification Tree")

classifier = neuralnet((survival=="short") ~ .,
                       data = dfCFST,hidden = 3, linear.output = FALSE, threshold=0.01) 
plot(classifier)


classifier = nnet((survival=="short") ~ ., data = dfCFS,size=2 ) 

plot.nnet(classifier,pos.col='darkgreen',neg.col='darkblue',alpha.val=0.7,rel.rsc=15,
          circle.cex=10,cex=1.4,
          circle.col='brown')
par(mar=numeric(4),family='serif')
plot.nnet(classifier, alpha.val = 0.5, circle.col = list('lightgray', 'white'), bord.col = 'black')
plotnet(classifier)
dev.off()
plot(classifier)

