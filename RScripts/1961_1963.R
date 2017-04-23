#### NEW DATA (as of April 2013), ALL YEARS 1961-2009 ####

# NOTE: Cote d'Ivoire name is screwy


# Import data in short-list form, this data include plants AND total animal data
# I am used the file in the dropbox folder called "all_1961_2009_final_analysis_data_v2.csv" (not sure if it's still there)

raw <-read.csv("/Sara/SaraGitHub/skammlade.github.io/projects/tableau/ChangingGlobalDiet_RawData.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA",""))
#raw <- all_1961_2009_final_analysis_data_v2
head(raw)
tail(raw)

raw2<-raw

#Rename "Element" column contents for simplicity's sake
raw2$Element[raw2$Element=="Protein supply quantity (g/capita/day)"]<-"Protein"
raw2$Element[raw2$Element=="Fat supply quantity (g/capita/day)"]<-"Fat" 
raw2$Element[raw2$Element=="Food supply quantity (g/capita/day)"]<-"Weight"
raw2$Element[raw2$Element=="Food supply (kcal/capita/day)"]<-"Calories"

# Convert to long-form data
# Using the reshape2 package because it is so handy
library(reshape2)
raw.long<-melt(raw2,id=c("Item","Country","Element","Unit"))

colnames(raw.long)<-c("Item","Country","Measurement","Unit","XYear","Value")
raw.long$Year<-substr(raw.long$XYear,2,5)
str(raw.long)
raw.long<-within(raw.long,Year<-as.numeric(Year))

# saving Country as a factor in column fCountry, as some of the statistical methods require factors
raw.long<-within(raw.long,fCountry<-as.factor(Country))

# REMOVE the Plant Commodities Sum and Animal Commodities Sum data from this worksheet
unique(raw.long$Item)
crops.misc<-subset(raw.long, Item %in% c("Vegetal Products (Total)","Animal Products (Total)","Grand Total")==FALSE)

# Also remove the "Miscellaneous" category from this dataset (crops1)
crops1<-subset(raw.long, Item %in% c("Vegetal Products (Total)","Animal Products (Total)","Grand Total","Miscellaneous")==FALSE)

# Calculate total value of (kcal, grams, etc.) across all crops for each country in each year
country.year.sums<-aggregate(crops1$Value, by=list(crops1$Country,crops1$Year,crops1$Measurement),FUN=sum,na.rm=TRUE)

colnames(country.year.sums)<-c("Country","Year","Measurement","Sum_Value")
crops2<-merge(crops1,country.year.sums)

head(crops2)

#Grand total of calories in each year, all countries combined
world.sum.value<-aggregate(crops2$Value,by=list(crops2$Measurement,crops2$Year),FUN=sum,na.rm=TRUE)

colnames(world.sum.value)<-c("Measurement","Year","World_Sum")
crops3<-merge(crops2,world.sum.value,by=c("Year","Measurement"))

crops<-crops3

#Calculate proportion total for each crop
crops$Proportion<-crops$Value/crops$Sum_Value

#Add presence-absence - NOTE THAT THIS ASSUMES NA's ARE 0's!
crops$pres.abs<-0
crops$pres.abs[crops$Value>0]<-1

### CHANGES IN SPECIES COMPOSITION -- MULTIVARIATE ANALYSES ###

#NMDS -- this is using ABSOLUTE ABUNDANCES (NMDS analysis uses rank)

crops.kcal<-subset(crops,Measurement=="Calories")

#HAVE to convert missing values to zeros
crops.kcal$Value[is.na(crops.kcal$Value)]<-0


#Extract 3 years
crops.kcal.61.62<-subset(crops.kcal, Year %in% c(1961,1962))
head(crops.kcal.61.62)
pivot.61.62<-dcast(crops.kcal.61.62, Year+Country ~ Item ,value.var="Value")

data4nmds.61.62<-pivot.61.62[,3:54]
head(data4nmds.61.62)
library(vegan)

crops.mds.61.62<-metaMDS(data4nmds.61.62,distance="bray",k=2) # can play around with options here to ensure convergence
data <- as.data.frame(crops.mds.61.62)

#Square root transformation
#Wisconsin double standardization

head(crops.mds.61.62$diss)

# USING NORMAL PLOT CODE

#Set up the color coding
shrink<-FALSE

cols.61.62<-rep(c("firebrick3","midnightblue"),c(152,152))

#Get the species and site scores
spp.sc<-scores(crops.mds.61.62,display="species",shrink=shrink)
country.sc<-scores(crops.mds.61.62,display="sites")
#Work out the ranges
ylimy<-range(spp.sc[,2],country.sc[,2])
xlimx<-range(spp.sc[,1],country.sc[,1])


#Set up plotting region
plot(country.sc,xlim=xlimx,ylim=ylimy,type="n",asp=1,xlab="NMDS 1",ylab="NMDS 2",main="NMDS of crop species in 1961, 1962 and 1963\n with 95% confidence interval around mean")

#Add the site/species labeling info (be careful to use the column labels from the file AFTER the first column was deleted, otherwise species labels will be off by one)
text(country.sc,col=cols.61.62,labels="*",cex=1.5)
#text(spp.sc,labels=colnames(data4nmds),cex=0.5)

#ORDIELLIPSE (this draws the black circles around the clusters of points; ordiellipse is like a 95% confidence interval around the "mean composition", so if the circles are big it means a lot of variation in the species composition (diet) of the different countries, small cirles mean little variation)

groups2<-factor(c(rep(1,152),rep(2,152)),labels=c("1961","1962"))

ordi.61.62<-ordiellipse(crops.mds.61.62,groups2,display="sites",kind="sd",conf=0.95,draw="lines",label=FALSE,col=c("black"))

str(ordi.61.62)
summary(ordi.61.62)

#       1961         1985        2007
#NMDS1 0.03800092 -0.001564185 -0.03643673
#NMDS2 0.02060166  0.009773507 -0.03037516
#Area  2.85734142  1.795182417  0.89167196

##--Sara results
#       1961         1985        2007
#NMDS1 0.04139983 -0.003269378 -0.03813045
#NMDS2 0.02158073  0.013592489 -0.03517321
#Area  2.84492549  1.799840856  0.89123276

#         1961         1962          1963
#NMDS1 0.002663982 -0.001714070 -0.0009499114
#NMDS2 0.002475580 -0.001440153 -0.0010354270
#Area  2.312739527  2.229639885  2.1844858026

#Function ordiellipse draws lines or polygons for dispersion ellipse using either standard deviation of point scores or standard error of the (weighted) average of scores, and the (weighted) correlation defines the direction of the principal axis of the ellipse. An ellipsoid hull can be drawn with function ellipsoidhull of package cluster.
#Functions ordihull and ordiellipse return the invisible plotting structure. In ordihull this is a list of coordinates of the hull and in ordiellipse a list of covariance matrices and scales used in drawing the ellipses. These result objects have a summary method that returns the coordi- nates of the centres of the ellipses or hulls and their surface areas in user units. The centres of the hulls may differ from the location of the label which is the centre of the points instead of the centre of the polygon. With draw = "none" only the result object is returned and nothing is drawn.

#AREA given by ORDIELLIPSE

(summary(ordi.61.62)[3,2]-summary(ordi.61.62)[3,1])/summary(ordi.61.62)[3,1] #-0.6879 or a 68.79% decrease in ellipse area between yr 1 and 3

##--Sara results: -0.686729

# USING GGPLOT CODE

nmds.groups.61.62<-pivot.61.62[,1:2]
#nmds.groups.61.62<-factor(c(rep(1,152),rep(2,152),rep(3,152)),labels=c("1961","1962","1963"))

#NMDS<-data.frame(MDS1 = crops.mds.61.62$points[,1], MDS2 = crops.mds.61.62$points[,2])
NMDS<-data.frame(MDS1 = crops.mds.61.62$points[,1], MDS2 = crops.mds.61.62$points[,2],group=nmds.groups.61.62$Year,Country=nmds.groups.61.62$Country)

NMDS<-within(NMDS, group<-as.factor(group))

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

