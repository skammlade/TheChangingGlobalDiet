#### DATA as of April 2013, ALL YEARS 1961-2009 ####

# Import data in short-list form, this data include plants AND total animal data
# data file called "all_1961_2009_final_analysis_data_v2.csv" can be found here: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/HYOWIC

dataRaw <-read.csv("/Sara/SaraGitHub/skammlade.github.io/projects/tableau/ChangingGlobalDiet_RawData.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA",""))
head(dataRaw)
tail(dataRaw)

#Rename "Element" column contents for simplicity's sake
dataRaw$Element[dataRaw$Element=="Protein supply quantity (g/capita/day)"]<-"Protein"
dataRaw$Element[dataRaw$Element=="Fat supply quantity (g/capita/day)"]<-"Fat" 
dataRaw$Element[dataRaw$Element=="Food supply quantity (g/capita/day)"]<-"Weight"
dataRaw$Element[dataRaw$Element=="Food supply (kcal/capita/day)"]<-"Calories"

# Convert to long-form data
# Using the reshape2 package because it is so handy
library(reshape2)
dataLong<-melt(dataRaw,id=c("Item","Country","Element","Unit"))

colnames(dataLong)<-c("Item","Country","Measurement","Unit","XYear","Value")
dataLong$Year<-substr(dataLong$XYear,2,5)
str(dataLong)
dataLong<-within(dataLong,Year<-as.numeric(Year))

# save  Country as a factor in column fCountry, as some of the statistical methods require factors
dataLong<-within(dataLong,fCountry<-as.factor(Country))

# REMOVE the Plant Commodities Sum data from this worksheet
unique(dataLong$Item)
dataRemoveVegTotalAndGrandTotal<-subset(raw.long, Item %in% c("Vegetal Products (Total)","Grand Total")==FALSE)

# Calculate total value of (kcal, grams, etc.) across all crops for each country in each year
dataCountryYearSums<-aggregate(dataRemoveVegTotalAndGrandTotal$Value, 
                           by=list(dataRemoveVegTotalAndGrandTotal$Country,
                                   dataRemoveVegTotalAndGrandTotal$Year,
                                   dataRemoveVegTotalAndGrandTotal$Measurement),
                           FUN=sum,na.rm=TRUE)

colnames(dataCountryYearSums)<-c("Country",
                                 "Year",
                                 "Measurement",
                                 "Sum_Value")
dataMerge<-merge(dataRemoveVegTotalAndGrandTotal,
                 dataCountryYearSums)

head(dataMerge)

#Grand total of calories in each year, all countries combined
countriesCombinedTotal<-aggregate(dataMerge$Value,
                                          by=list(dataMerge$Measurement,
                                                  dataMerge$Year),
                                          FUN=sum,na.rm=TRUE)

colnames(countriesCombinedTotal)<-c("Measurement",
                                            "Year",
                                            "World_Sum")
dataCountryTotal<-merge(dataMerge,
                        countriesCombinedTotal,
                        by=c("Year","Measurement"))

#Calculate proportion total for each crop
dataCountryTotal$Proportion<-dataCountryTotal$Value/dataCountryTotal$Sum_Value

#Add presence-absence - NOTE THAT THIS ASSUMES NA's ARE 0's!
dataCountryTotal$pres.abs<-0
dataCountryTotal$pres.abs[dataCountryTotal$Value>0]<-1

### CHANGES IN SPECIES COMPOSITION -- MULTIVARIATE ANALYSES ###

#NMDS -- this is using ABSOLUTE ABUNDANCES (NMDS analysis uses rank)

dataCalories<-subset(dataCountryTotal,Measurement=="Calories")

#HAVE to convert missing values to zeros
dataCalories$Value[is.na(dataCalories$Value)]<-0


#Extract 3 years
dataCalories618509<-subset(crops.kcal, Year %in% c(1961,1985,2009))
head(dataCalories618509)
dataPivotCalories618509<-dcast(dataCalories618509, Year+Country ~ Item ,value.var="Value")
head(dataPivotCalories618509)
dataNMDSCalories618509<-dataPivotCalories618509[,3:54]
head(dataNMDSCalories618509)
library(vegan)


cat("metaMDS Output", 
    file="metaMDS_3yrs_kcal.txt")
cat("\n\n", 
    file="metaMDS_3yrs_kcal.txt")

cat("3 years k=2\n", 
    file="metaMDS_3yrs_kcal.txt")
capture.output((outputMDSCalories618509k2<-metaMDS(dataNMDSCalories618509,
                                        distance="bray",
                                        k=2)), 
               file="metaMDS_3yrs_kcal.txt", 
               append=TRUE) # can play around with options here to ensure convergence
cat("\n\n", 
    file="metaMDS_3yrs_kcal.txt")


#stressplot
jpeg('stressplot_k2.jpg')
stressplot(outputMDSCalories618509K2)
dev.off()

head(outputMDSCalories618509k2$diss)

# USING NORMAL PLOT CODE

#Set up the color coding
shrink<-FALSE

cols.3yrs<-rep(c("firebrick3","midnightblue","black"),c(152,152,152))

#Get the species and site scores
spp.sc<-scores(crops.mds.3yrs,display="species",shrink=shrink)
country.sc<-scores(crops.mds.3yrs,display="sites")
#Work out the ranges
ylimy<-range(spp.sc[,2],country.sc[,2])
xlimx<-range(spp.sc[,1],country.sc[,1])

#Set up plotting region
plot(country.sc,xlim=xlimx,ylim=ylimy,type="n",asp=1,xlab="NMDS 1",ylab="NMDS 2",main="NMDS of crop species in 1961, 1985 and 2009\n with 95% confidence interval around mean")

#Add the site/species labeling info (be careful to use the column labels from the file AFTER the first column was deleted, otherwise species labels will be off by one)
text(country.sc,col=cols.3yrs,labels="*",cex=1.5)
#text(spp.sc,labels=colnames(data4nmds),cex=0.5)

#ORDIELLIPSE (this draws the black circles around the clusters of points; ordiellipse is like a 95% confidence interval around the "mean composition", so if the circles are big it means a lot of variation in the species composition (diet) of the different countries, small cirles mean little variation)

groups2<-factor(c(rep(1,152),rep(2,152),rep(3,152)),labels=c("1961","1985","2009"))

ordi.3yrs<-ordiellipse(crops.mds.3yrs,groups2,display="sites",kind="sd",conf=0.95,draw="lines",label=TRUE,col=c("black"))
hullellipse

str(ordi.3yrs)
summary(ordi.3yrs)

#       1961         1985        2007
#NMDS1 0.03800092 -0.001564185 -0.03643673
#NMDS2 0.02060166  0.009773507 -0.03037516
#Area  2.85734142  1.795182417  0.89167196

##--Sara results
#       1961         1985        2007
#NMDS1 0.04139983 -0.003269378 -0.03813045
#NMDS2 0.02158073  0.013592489 -0.03517321
#Area  2.84492549  1.799840856  0.89123276

#Function ordiellipse draws lines or polygons for dispersion ellipse using either standard deviation of point scores or standard error of the (weighted) average of scores, and the (weighted) correlation defines the direction of the principal axis of the ellipse. An ellipsoid hull can be drawn with function ellipsoidhull of package cluster.
#Functions ordihull and ordiellipse return the invisible plotting structure. In ordihull this is a list of coordinates of the hull and in ordiellipse a list of covariance matrices and scales used in drawing the ellipses. These result objects have a summary method that returns the coordi- nates of the centres of the ellipses or hulls and their surface areas in user units. The centres of the hulls may differ from the location of the label which is the centre of the points instead of the centre of the polygon. With draw = "none" only the result object is returned and nothing is drawn.

#AREA given by ORDIELLIPSE

(summary(ordi.3yrs)[3,3]-summary(ordi.3yrs)[3,1])/summary(ordi.3yrs)[3,1] #-0.6879 or a 68.79% decrease in ellipse area between yr 1 and 3

##--Sara results: -0.686729

# USING GGPLOT CODE

nmds.groups.3yrs<-pivot.61.85.09[,1:2]
#nmds.groups.3yrs<-factor(c(rep(1,152),rep(2,152),rep(3,152)),labels=c("1961","1985","2007"))

#NMDS<-data.frame(MDS1 = crops.mds.3yrs$points[,1], MDS2 = crops.mds.3yrs$points[,2])
NMDS<-data.frame(MDS1 = crops.mds.3yrs$points[,1], MDS2 = crops.mds.3yrs$points[,2],group=nmds.groups.3yrs$Year,Country=nmds.groups.3yrs$Country)

NMDS<-within(NMDS, group<-as.factor(group))
write.csv(NMDS, "NMDS__3yrs_k2")

NMDS.mean<-aggregate(NMDS[,1:2],list(group=NMDS$group),mean) # if you want to plot the center point


library(ggplot2)

#FINAL GRAPH for NMDS!

ggplot(NMDS, aes(x=NMDS$MDS1, y=NMDS$MDS2, colour=group))+
  geom_point()+
  stat_ellipse()+
  scale_colour_manual(values=c("red","blue","black"),name="Year")+
  theme_bw()+
  xlab("\n NMDS 1")+
  ylab("NMDS 2 \n")+
  theme(axis.text.x=element_text(size=20),
        axis.text.y=element_text(hjust=1,size=20),
        axis.title.x=element_text(size=20,
                                  margin=margin(15,0,0,0)),
        axis.title.y=element_text(angle=90,
                                  size=24,
                                  face="plain",
                                  margin=margin(0,15,0,0)),
        axis.ticks = element_blank(), 
        panel.grid.major.x=element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y=element_blank(),
        panel.grid.major.y=element_blank(),
        legend.key = element_blank(), 
        legend.title=element_text(size=20), 
        legend.text=element_text(size=16))+
  guides(color = guide_legend(override.aes = list(size = 5, linetype=0)))

ggsave("NMDS_3ellipse_17May2013.png", width=9, height=7, dpi=300)

#Plotting country names
ggplot(NMDS, 
       aes(x=MDS1, 
           y=MDS2, 
           colour=group,
           label=Country))
ggplot <- ggplot + geom_text(size=3.5)
ggplot <- ggplot + stat_ellipse()
ggplot <- ggplot + scale_colour_manual(values=c("red","blue","black"),
                                       name="Year")
ggplot <- ggplot + theme_bw()
ggplot <- ggplot + xlab("\n NMDS 1")
ggplot <- ggplot + ylab("NMDS 2 \n")
ggplot <- ggplot + theme(legend.title=element_text(size=16),
                         legend.text=element_text(size=12),
                         panel.grid.major.y = element_blank(), 
                         panel.grid.minor.y = element_blank(),
                         panel.grid.major.x = element_blank(),
                         panel.grid.minor.x = element_blank())

#actual centroids are:

NMDS.mean

NMDS$meanMDS1<-NMDS.mean$MDS1[match(NMDS$group,NMDS.mean$group)]
NMDS$meanMDS2<-NMDS.mean$MDS2[match(NMDS$group,NMDS.mean$group)]

#manually compute distances to centroids

NMDS$DistMDS1<-abs(NMDS$MDS1-NMDS$meanMDS1)
NMDS$DistMDS2<-abs(NMDS$MDS2-NMDS$meanMDS2)

NMDS$PyDist<-sqrt((NMDS$DistMDS1^2)+(NMDS$DistMDS2^2))

NMDS<-within(NMDS,Year<-factor(NMDS$group,levels=c("1961","1985","2009")))

#Connect countries with lines
ggplot(NMDS, 
       aes(x=MDS1, 
           y=MDS2, 
           colour=group)) +
  geom_point() +
  stat_ellipse() +
  scale_colour_manual(values=c("red","blue","black"),
                      name="Year") + 
  theme_bw()+ 
  xlab("\n NMDS 1")+
  ylab("NMDS 2 \n")+
  theme(legend.title=element_text(size=16),
        legend.text=element_text(size=12),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  geom_path(aes(group=Country),
            colour="purple",
            stat="identity")

#Connect specific countries with lines
ggplot(NMDS, 
       aes(x=MDS1, 
           y=MDS2, 
           colour=group)
)+
  geom_point()+
  stat_ellipse()+
  theme_bw()+
  xlab("\n NMDS 1")+
  ylab("NMDS 2 \n")+
  theme(legend.title=element_text(size=16),
        legend.text=element_text(size=12),
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())+
  geom_path(data=NMDS[NMDS$Country %in% c("China",
                                          "India",
                                          "Republic of Korea"),],
            aes(x=MDS1, 
                y=MDS2, 
                group=Country,
                colour=Country), 
            size=2)+
  scale_colour_manual(values=c("red","blue","black","orange","purple","green"))

