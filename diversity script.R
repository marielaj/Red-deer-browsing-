############START ###############




# Packages
library(readr)
library(readxl)
library(dplyr)
library(ggplot2)


Species_data <- read_excel("~/Master/Data/Species 2009_2018 (2).xlsx")
View(Species_2009_2018_2_)

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




# Balance?
table(cdat$LocalityName, cdat$Treatment)
# balance good






# Vascular plant species 
vasc_names <- names(cdatBM[,16:ncol(cdatBM)])


cdatBM <- cdat


#Species richness
#summerer alle celler > 0
cdatBM$SR <- rowSums(cdatBM[,vasc_names]>0, na.rm=T)


library(vegan)
# Diversity index
cdat$Shannon <- diversity(cdatBM[,vasc_names], index = "shannon")
cdat$simpson <- diversity(cdatBM[,vasc_names], index = "simpson")
plot(cdat$simpson, cdat$Shannon)
plot(as.factor(cdatBM$Treatment), cdatBM$SR)


plot(cdatBM$Year, cdatBM$SR)

meanSR<-aggregate(data=cdatBM,
          SR~ LocalityName+Year+Treatment,
          FUN = mean)

boxplot(meanSR$SR~as.factor(meanSR$Year)+meanSR$Treatment,las=2)
boxplot(meanSR$SR~meanSR$Treatment+as.factor(meanSR$Year),las=2, col=c("white", "grey"))


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

head(wide)
head(wide2)


pairs <- interaction(wide2$LocalityName, wide2$yse, wide2$Treatment)
dist_out <- cbind(pairs, wide2)
dist_out2 <- NULL

for(i in unique(dist_out$pairs)){
  temp <- vegdist(x = subset(dist_out[,7:ncol(dist_out)], pairs == i), method="jaccard")     # dissimilarity
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


