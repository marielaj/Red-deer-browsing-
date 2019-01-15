############START ###############




# Packages
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)


cdat <- read_excel("~/Master/Data/Species 2009_2018 (2).xlsx")
View(cdat)

names(cdat)


#  regions
table(cdat$Region)






# calculating year since exclosure (yse)
table(as.factor(cdat$Year)) # fences came up 2009
yse <- cdat$Year - 2009
cdat <- cbind(yse, cdat)
table(as.factor(cdat$yse))

table(as.factor(cdat$yse), cdat$LocalityName)
table(cdat$Plot)


# remove some columns that are not vascular plant species

# Obs, these column dont match the current dataset
other_types <- c("Bare ground",
                 "Bare ground/branch", 
                 "Bare ground-stone",
                 "Bryophyta",
                 "Cow shit",
                 "Lichens",
                 "Litter",
                 "No occurrence",
                 "Sphagnum sp",
                 "Stone",
                 "Not identified")

mosses <- c("Cirriphyllum piliferum",
            "Dicranum sp",
            "Hylocomiastrum umbratum",
            "Hylocomium splendens",
            "Marchantiophyta",
            "Plagiochila asplenioides",
            "Plagiomnium ellipticum",
            "Plagiomnium undulatum",
            "Plagiothecium laetum/curvifolium",
            "Plagiothecium undulatum",
            "Pleurozium schreberi",
            "Polytrichum/-astrum",
            "Ptilidium ciliare",
            "Ptilium crista-castrensis",
            "Rhodobryum roseum",
            "Rhytidiadelphus loreus",
            "Rhytidiadelphus squarrosus/subpinnatus",
            "Rhytidiadelphus triquetrus",
            "Sciuro-hypnum reflexum",
            "Sciuro-hypnum starkei")

trees <- c("Alnus incana",
           "Betula pubescens",
           "Quercus sp_",
           "Salix caprea",
           "Betula pendula",
           "Corylus avellana",
           "Picea abies",
           "Pinus sylvestris",
           "Populus tremula",
           "Prunus padus",
           "Sorbus aucuparia",
           "Juniperus communis",
           "Sambucus sp_")

cdat2 <- cdat[,!colnames(cdat) %in% c(other_types, mosses, trees)]
cdat <- cdat2
rm(cdat2)



# Turning all species into numeric
#to_be_numbers <- c(16:ncol(cdat))
#cdat[,to_be_numbers] <- as.numeric(as.character(unlist(cdat[,to_be_numbers])))
#summary(colSums(cdat[,16:ncol(cdat)], na.rm = T))





# removing 'observed' species and retaining only measured species
#cdat4 <- cdat[cdat$Method=="Point_Intercept",]
#cdat <- cdat4
#rm(cdat4)




# removing species with no records

# loking for zeros
names(cdat)
colSums(cdat[,16:ncol(cdat)])

#cdat2 <- cdat[,c(rep(TRUE, times = 15),
#                 colSums(cdat[,16:ncol(cdat)], na.rm = T) > 0)]
#summary(colSums(cdat2[,16:ncol(cdat2)], na.rm=T) == 0)
#cdat <- cdat2;rm(cdat2)






# look for empty plots
#summary(rowSums(cdat[,16:ncol(cdat)], na.rm = T))  # obs - note some non-species at the end of df
#cdat2 <- cdat[rowSums(cdat[,16:ncol(cdat)], na.rm = T) > 0,]
#cdat <- cdat2;rm(cdat2)




# Balance between treatment?
table(cdat$LocalityName, cdat$Treatment)
# balance good, even numbers between observations 






# Vascular plant species, number of species - biomass 
cdatBM <- cdat
vasc_names <- names(cdatBM[,16:ncol(cdatBM)])




#Species richness
#summerer alle celler > 0
cdatBM$SR <- rowSums(cdatBM[,vasc_names]>0, na.rm=T)

#Abundance of each species
#Sum ocurrence in all subplots per plot 

#Hente inn data på "hjort" statestikk 
library(readxl)
Sette_dyr_Tingvoll <- read_excel("~/Master/Data/Sette dyr Tingvoll.xlsx")
View(Sette_dyr_Tingvoll)

xyplot(Sette_dyr_Tingvoll$`Sum sette hjort`~ Sette_dyr_Tingvoll$År)

#Henter inn data på sett hjort per lokalisjon og vald 

library(readxl)
Sett_hjort <- read_excel("~/Master/Data/Sett hjort.xlsx")
View(Sett_hjort)

p <- xyplot(Sett_hjort$`Sett hjort`~ Sett_hjort$Year,
            main = "Changes in population ",
            xlab = "Year",
            ylab = "Number of deer count")
            

#Fjerner NA?  
Sett_hjort$`Sett hjort` [is.na(Sett_hjort$`Sett hjort`)] <- 0 

# identifying locations in the xyplot
#identify(p, what= Sett_hjort$Hjorteviltvald, labels=splist, col="red")

#plotnames <- as.character(allmerged$plot_ID)

#cdatBM <- colSums(Sett_hjort$`Sett hjort`)

library(vegan)
# Diversity index
cdat$Shannon <- diversity(cdatBM[,vasc_names], index = "shannon")
cdat$simpson <- diversity(cdatBM[,vasc_names], index = "simpson")
plot(cdat$simpson, cdat$Shannon)

plot(as.factor(cdatBM$Treatment), cdatBM$SR)


plot(cdatBM$Year, cdatBM$SR)

meanSR<-aggregate(data=cdatBM,
          SR~ LocalityName+Year+yse+Treatment,
          FUN = mean)
#Boxplot som illusterer SR mot år + behandling 
boxplot(meanSR$SR~as.factor(meanSR$Year)+meanSR$Treatment,las=2)
boxplot(meanSR$SR~meanSR$Treatment+as.factor(meanSR$Year),las=2, col=c("white", "grey"))


# ALK: Making line graph for species richness
# First we calculate means and SE
meanSR2 <- aggregate(data=meanSR,
                     SR~ Year+yse+Treatment,
                     FUN = mean)
meanSRsd <- aggregate(data=meanSR,
                     SR~ Year+yse+Treatment,
                     FUN = sd)
meanSR2$sd <- meanSRsd$SR
meanSR2$se <- meanSR2$sd/sqrt(10)   # standard error of the mean (sd delt på rot av N)

library(ggplot2)
pd <- position_dodge(width=0.4)
sr_plot <- ggplot(data = meanSR2, aes(x= yse, y = SR, group = Treatment, linetype = Treatment))+
  geom_line(size=1, position=pd)+
  geom_point(position=pd)+
  geom_errorbar(aes(ymax = SR+se, ymin = SR-se), position=pd)+
  xlab("Years since exclosure")+
  ylab("Species richness")+
  scale_x_continuous(breaks = seq(-1,9,2))+
  theme_cleveland()
  
sr_plot

boxplot(meanSR$SR~as.factor(meanSR$Year)) 

boxplot(meanSR$SR~meanSR$Treatment,
        main = "Species richness - treatment",
        xlab = "Treatment",
        ylab = "Species richness",
        col = "orange",
        border = "green",
        #horizontal = TRUE,
        notch = TRUE)

boxplot(meanSR$SR ~as.factor(meanSR$LocalityName), las=2)
boxplot(meanSR$SR ~meanSR$Treatment + as.factor(meanSR$LocalityName), las=2)

hist(meanSR$SR, main= " ", xlab= "Species richness", ylab="Frequency", 
     col="lightgrey", xlim=c(0.0,9), ylim=c(0,30))

#Boxplot som viser endringen i Shannon div. index mot år + behandling 
boxplot(cdat$Shannon~cdat$Year+cdat$Treatment, las=2, col=c("white", "grey"))
boxplot(cdat$Shannon~cdat$Treatment+cdat$Year, las=2, col=c("white", "grey"))

boxplot(cdat$Shannon ~cdat$Treatment + as.factor(cdat$LocalityName), las=2)

hist(cdat$Shannon, main= " ", xlab= "Shannon's diversity", ylab="Frequency", 
     col="lightgrey", xlim=c(0.0,3), ylim=c(0,200)) 
#Shannon diversity appears to be normally distributed. 


#Boxplot som viser endringen i Simpson div. index mot år + behandling 
boxplot(cdat$simpson~cdat$Year+cdat$Treatment, las=2, col=c("white", "grey"))
boxplot(cdat$simpson~cdat$Treatment+cdat$Year, las=2, col=c("white", "grey"))

boxplot(cdat$simpson ~cdat$Treatment + as.factor(cdat$LocalityName), las=2)

#Enkle boksplot
boxplot(cdat$Shannon)
boxplot(cdat$simpson)
boxplot(cdatBM$SR)


#Validate data using linear mixed effect model 

install.packages("lme4")
install.packages("arm")
library(lme4)
library(arm)
library(lmerTest)

#inserting deer density data
cdatBM$sett_hjort <- Sett_hjort$`Sett hjort`[match(paste0(cdatBM$LocalityCode, cdatBM$Year), 
                                                   paste0(Sett_hjort$LocalityCode, Sett_hjort$Year))]

model1 <- lmer(cdatBM$SR ~ cdatBM$yse * cdatBM$Treatment + (1|cdatBM$LocalityCode), data = cdatBM) 
summary(model1)


levels(as.factor(cdatBM$Treatment))
cdatBM$Treatment_f <- factor(cdatBM$Treatment, levels = c("B", "UB"))
model1ALK <- lmer(cdatBM$SR ~ cdatBM$Treatment_f * cdatBM$sett_hjort + (1|cdatBM$LocalityCode), data = cdatBM[cdatBM$yse==7,]) 
model2ALK <- lmer(cdatBM$SR ~ cdatBM$Treatment_f + (1|cdatBM$LocalityCode), data = cdatBM[cdatBM$yse==7,]) 
model3ALK <- lmer(cdatBM$SR ~ cdatBM$yse * cdatBM$sett_hjort + (1|cdatBM$LocalityCode), data = cdatBM[cdatBM$Treatment=="B",]) 

summary(model1ALK)
summary(model3ALK)


AIC(model1)

model2 <- lmer(cdatBM$SR ~ cdatBM$Year * cdatBM$Treatment + Sett_hjort$`Sett hjort` + (1|cdatBM$LocalityCode)) 
summary(model2)

#Likelihood Ratio Test as a means to attain p-values
#The null model 

model1.null = lmer(cdatBM$SR ~ cdatBM$Treatment + (1|cdatBM$LocalityCode), data = cdatBM,
                              REML = FALSE)
anova(model1.null, model1)




# the coefficients of the model

coef(model1)
resid(model1)
fitted(model1)




# Our model is what is called a random intercept model. In this
#model, we account for baseline-differences in pitch, but we assume that whatever
#the effect of diversity is, it’s going to be the same for all treatments and years.


#Undersøke korrelasjon mellom variabler? 

# checking correlations between the constrainig variables to identify possibly redundant variables

library("ggpubr")
ggscatter(cdatBM, x = "LocalityCode", y = "Treatment", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Species richness", ylab = "treatment")


cor(as.factor(meanSR$LocalityName), as.factor(meanSR$Year))
cor(as.factor(meanSR$LocalityName, meanSR$Treatment))
cor(as.factor(meanSR$LocalityName, meanSR$SR))

cor(as.factor(meanSR$Year, meanSR$Treatment))
cor(as.factor(meanSR$Year, meanSR$SR))

cor(meanSR$Treatment, meanSR$SR)




# the following is not adapted to the current dataset, but included for reference:
# ***************************************#
# ***************************************#
# Betadiversity ####
# ***************************************#
# ***************************************#
library(vegan)



#***********************************
# VEGDIST

# betadiversity and how it changes with time since exlosure

head(cdat)
head(cdatBM)


pairs <- interaction(cdatBM$LocalityName, cdatBM$yse, cdatBM$Treatment)
dist_out <- cbind(pairs, cdatBM)
dist_out2 <- NULL

str(dist_out)
dist_out$LocalityCode <- as.numeric(dist_out$LocalityCode)

for(i in unique(dist_out$pairs)){
  temp <- vegdist(x = subset(dist_out[,17:ncol(dist_out)], pairs == i), method="jaccard")     # dissimilarity
  temp2 <- cbind( 
    unique(dist_out$Region[dist_out$pairs == i]),             
    unique(dist_out$LocalityName[dist_out$pairs == i]), 
    unique(dist_out$yse[dist_out$pairs == i]),
    unique(dist_out$Treatment[dist_out$pairs == i]),
    mean(as.numeric(temp))
  )
  dist_out2 <- rbind(dist_out2, temp2)
}

class(dist_out2)
dist_out3 <- data.frame(dist_out2)
names(dist_out3) <- c("Region", "LocalityName", "yse", "Treatment", "Jaccard")
dist_out3$nJaccard <- as.numeric(as.character(dist_out3$Jaccard))
levels(dist_out3$Treatment)[levels(dist_out3$Treatment) == "B"] <- "Open plots"
levels(dist_out3$Treatment)[levels(dist_out3$Treatment) == "UB"] <- "Exclosures"



# get mean Jaccard distance per location and year
Jacc <- aggregate(data = dist_out3, nJaccard~yse+Treatment, FUN = mean)
Jacc$SE <- aggregate(data = dist_out3, nJaccard~yse+Treatment, FUN = std)[,3]
summary(Jacc)

Jacc$nyse <- as.numeric(as.character(Jacc$yse))


jacc <- ggplot(data = Jacc, aes(x=nyse, y=nJaccard, group=Treatment, linetype=Treatment))+
  theme_bw()+
  xlab("Years since exclusion")+
  ylab("Mean within-plot\nJaccard dissimilarity")+
  geom_line(size=1, position=pd)+ 
  geom_point(size =3, position=pd, aes(shape=Treatment))+
  geom_errorbar(aes(ymax=nJaccard+SE, ymin=nJaccard-SE, linetype = NULL), size=1.1, width=0.5, position=pd)+
  guides(linetype = FALSE, shape=FALSE)+
  scale_linetype_manual(breaks = c("B", "UB"), labels = c("Open plots", "Exclosures"), values=c(1,2))





# plot treatment difference against site productivity:
Jacc <- aggregate(data = dist_out3, nJaccard~yse+Treatment+LocalityName, FUN = mean)
Jacc2 <- Jacc[Jacc$yse=="8",]
Jacc$nyse <- as.numeric(as.character(Jacc$yse))

Jacc3 <- aggregate(data = Jacc2,
                   nJaccard~Treatment+LocalityName,
                   FUN = mean)

Jacc4 <- dcast(data = Jacc3, 
               LocalityName~Treatment,
               value.var = "nJaccard")
Jacc4$diff <- Jacc4$`Open plots` - Jacc4$Exclosures
Jacc4$productivity <- productivity$productivity[match(Jacc4$LocalityName, productivity$LocalityName)]


jacc2 <- ggplot(data = Jacc4, aes(x = productivity, y = diff))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_rect(data=NULL,aes(xmin=-Inf,xmax=Inf,ymin=-Inf,ymax=0),
            fill="lightgreen")+
  #theme(text = element_text(size=15))+
  geom_point(size = 3, stroke = 2, shape =1)+
  #geom_smooth(method = "lm", se = T, size = 2, colour = "black")+
  ylab("")+
  xlab("")
#ggtitle("Grasses")


