#### NEW DATA (as of April 2013), ALL YEARS 1961-2009 ####

# NOTE: Cote d'Ivoire name is screwy


# Import data in short-list form, this data include plants AND total animal data
# I am used the file in the dropbox folder called "all_1961_2009_final_analysis_data_v2.csv" (not sure if it's still there)

raw <-read.csv("/home/owner/Downloads/ChangingGlobalDiet_RawData.csv", stringsAsFactors = FALSE, strip.white = TRUE, na.strings = c("NA",""))
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
#unique(raw.long$Item)
#crops.misc<-subset(raw.long, Item %in% c("Vegetal Products (Total)","Animal Products (Total)","Grand Total")==FALSE)

# REMOVE the Plant Commodities Sum and Grand Total Sum data from this worksheet
crops1<-subset(raw.long, Item %in% c("Animal Products (Total)","Grand Total")==FALSE)

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

crops.weight<-subset(crops,Measurement=="Protein")

#HAVE to convert missing values to zeros
crops.protein$Value[is.na(crops.protein$Value)]<-0


#Extract 3 years
#crops.protein.61.85.09<-subset(crops.protein, Year %in% c(1961,1985,2009))

#Extract all years
#pivot
pivot.allyrs<-dcast(crops.protein, Year+Country ~ Item ,value.var="Value")

data4nmds.allyrs<-pivot.allyrs[,3:56]

library(vegan)

crops.mds.allyrs<-metaMDS(data4nmds.allyrs,distance="bray",k=2) # can play around with options here to ensure convergence

#Square root transformation
#Wisconsin double standardization

head(crops.mds.allyrs$diss)

#stressplot
jpeg('stressplot_allyrs_protein_k2_rep1.jpg')
stressplot(crops.mds.allyrs)
dev.off()

# USING NORMAL PLOT CODE

#Set up the color coding
shrink<-FALSE

cols.allyrs<-rep(c(152,152,152,152,152,
                   152,152,152,152,152,
                   152,152,152,152,152,
                   152,152,152,152,152,
                   152,152,152,152,152,
                   152,152,152,152,152,
                   152,152,152,152,152,
                   152,152,152,152,152,
                   152,152,152,152,152,
                   152,152,152,152))

#Get the species and site scores
spp.sc<-scores(crops.mds.allyrs,display="species",shrink=shrink)
country.sc<-scores(crops.mds.allyrs,display="sites")
#Work out the ranges
ylimy<-range(spp.sc[,2],country.sc[,2])
xlimx<-range(spp.sc[,1],country.sc[,1])

#Set up plotting region
plot(country.sc,xlim=xlimx,ylim=ylimy,type="n",asp=1,xlab="NMDS 1",ylab="NMDS 2",main="NMDS of crop species in 1961, 1985 and 2009\n with 95% confidence interval around mean")

#Add the site/species labeling info (be careful to use the column labels from the file AFTER the first column was deleted, otherwise species labels will be off by one)
text(country.sc,col=cols.allyrs,labels="*",cex=1.5)
#text(spp.sc,labels=colnames(data4nmds),cex=0.5)

#ORDIELLIPSE (this draws the black circles around the clusters of points; ordiellipse is like a 95% confidence interval around the "mean composition", so if the circles are big it means a lot of variation in the species composition (diet) of the different countries, small cirles mean little variation)

groups2<-factor(c(rep(1,152),
                  rep(2,152),
                  rep(3,152),
                  rep(4,152),
                  rep(5,152),
                  rep(6,152),
                  rep(7,152),
                  rep(8,152),
                  rep(9,152),
                  rep(10,152),
                  rep(11,152),
                  rep(12,152),
                  rep(13,152),
                  rep(14,152),
                  rep(15,152),
                  rep(16,152),
                  rep(17,152),
                  rep(18,152),
                  rep(19,152),
                  rep(20,152),
                  rep(21,152),
                  rep(22,152),
                  rep(23,152),
                  rep(24,152),
                  rep(25,152),
                  rep(26,152),
                  rep(27,152),
                  rep(28,152),
                  rep(29,152),
                  rep(30,152),
                  rep(31,152),
                  rep(32,152),
                  rep(33,152),
                  rep(34,152),
                  rep(35,152),
                  rep(36,152),
                  rep(37,152),
                  rep(38,152),
                  rep(39,152),
                  rep(40,152),
                  rep(41,152),
                  rep(42,152),
                  rep(43,152),
                  rep(44,152),
                  rep(45,152),
                  rep(46,152),
                  rep(47,152),
                  rep(48,152),
                  rep(49,152)),
                labels=c("1961",
                         "1962",
                         "1963",
                         "1964",
                         "1965",
                         "1966",
                         "1967",
                         "1968",
                         "1969",
                         "1970",
                         "1971",
                         "1972",
                         "1973",
                         "1974",
                         "1975",
                         "1976",
                         "1977",
                         "1978",
                         "1979",
                         "1980",
                         "1981",
                         "1982",
                         "1983",
                         "1984",
                         "1985",
                         "1986",
                         "1987",
                         "1988",
                         "1989",
                         "1990",
                         "1991",
                         "1992",
                         "1993",
                         "1994",
                         "1995",
                         "1996",
                         "1997",
                         "1998",
                         "1999",
                         "2000",
                         "2001",
                         "2002",
                         "2003",
                         "2004",
                         "2005",
                         "2006",
                         "2007",
                         "2008",
                         "2009"))

ordi.allyrs<-ordiellipse(crops.mds.allyrs,groups2,display="sites",kind="sd",conf=0.95,draw="lines",label=FALSE,col=c("black"))

str(ordi.allyrs)
summaryOrdiAllyrs<-summary(ordi.allyrs)
write.csv(summaryOrdiAllyrs, "ordiAllyrsproteinRep1.csv")



#Function ordiellipse draws lines or polygons for dispersion ellipse using either standard deviation of point scores or standard error of the (proteined) average of scores, and the (proteined) correlation defines the direction of the principal axis of the ellipse. An ellipsoid hull can be drawn with function ellipsoidhull of package cluster.
#Functions ordihull and ordiellipse return the invisible plotting structure. In ordihull this is a list of coordinates of the hull and in ordiellipse a list of covariance matrices and scales used in drawing the ellipses. These result objects have a summary method that returns the coordi- nates of the centres of the ellipses or hulls and their surface areas in user units. The centres of the hulls may differ from the location of the label which is the centre of the points instead of the centre of the polygon. With draw = "none" only the result object is returned and nothing is drawn.

#AREA given by ORDIELLIPSE

diffEllipse<-(summary(ordi.allyrs)[3,56]-summary(ordi.3yrs)[3,1])/summary(ordi.3yrs)[3,1] #-0.6879 or a 68.79% decrease in ellipse area between yr 1 and 3
write.csv(diffEllipse, "diffEllipseAllyrsproteinRep1.csv")

# USING GGPLOT CODE

nmds.groups.allyrs<-pivot.allyrs[,1:2]

NMDS<-data.frame(MDS1 = crops.mds.allyrs$points[,1], 
                 MDS2 = crops.mds.allyrs$points[,2],
                 group=nmds.groups.allyrs$Year,
                 Country=nmds.groups.allyrs$Country)

NMDS<-within(NMDS, group<-as.factor(group))

NMDS.mean<-aggregate(NMDS[,1:2],list(group=NMDS$group),mean) # if you want to plot the center point

write.csv(NMDS.mean, "centerEllipseAllyrsproteinRep1.csv")

library(ggplot2)

#FINAL GRAPH for NMDS!

ggplot(NMDS, aes(x=NMDS$MDS1, y=NMDS$MDS2, colour=group))+
  geom_point()+
  stat_ellipse()+
  scale_colour_manual(values=c("red","blue","black"),name="Year")+
  theme_bw()+xlab("\n NMDS 1")+ylab("NMDS 2 \n")+theme(axis.text.x=element_text(size=20),axis.text.y=element_text(hjust=1,size=20),axis.title.x=element_text(size=20,margin=margin(15,0,0,0)),axis.title.y=element_text(angle=90,size=24,face="plain",margin=margin(0,15,0,0)),axis.ticks = element_blank(), panel.grid.major.x=element_blank(),panel.grid.minor.x=element_blank(),panel.grid.minor.y=element_blank(),panel.grid.major.y=element_blank(),legend.key = element_blank(), legend.title=element_text(size=20), legend.text=element_text(size=16))+
  guides(color = guide_legend(override.aes = list(size = 5, linetype=0)))

ggsave("NMDS_ellipse__allyrs_protein_rep1_3Nov2016.png", width=9, height=7, dpi=300)

#Plotting country names
ggplot(NMDS, aes(x=MDS1, y=MDS2, colour=group,label=Country))+geom_text(size=3.5)+stat_ellipse()+scale_colour_manual(values=c("red","blue","black"),name="Year")+theme_bw()+xlab("\n NMDS 1")+ylab("NMDS 2 \n")+theme(legend.title=element_text(size=16),legend.text=element_text(size=12),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())

#actual centroids are:

NMDS.mean

NMDS$meanMDS1<-NMDS.mean$MDS1[match(NMDS$group,NMDS.mean$group)]
NMDS$meanMDS2<-NMDS.mean$MDS2[match(NMDS$group,NMDS.mean$group)]

#manually compute distances to centroids

MDS1Dist<-NMDS$DistMDS1<-abs(NMDS$MDS1-NMDS$meanMDS1)
MDS2Dist<-NMDS$DistMDS2<-abs(NMDS$MDS2-NMDS$meanMDS2)

write.csv(MDS1Dist, "MDS1Dist_allyrs_protein_rep1.csv")
write.csv(MDS2Dist, "MDS2Dist_allyrs_protein_rep1.csv")

NMDS$PyDist<-sqrt((NMDS$DistMDS1^2)+(NMDS$DistMDS2^2))
write.csv(NMDS$PyDist, "NMDS$PyDist_allyrs_protein_rep1.csv")

NMDS<-within(NMDS,Year<-factor(NMDS$group))
write.csv(NMDS, "NMDSellipseDist_allyrs_protein_rep1.csv")

#Connect countries with lines
ggplot(NMDS, aes(x=MDS1, y=MDS2, colour=group))+geom_point()+stat_ellipse()+scale_colour_manual(values=c("red","blue","black"),name="Year")+theme_bw()+xlab("\n NMDS 1")+ylab("NMDS 2 \n")+theme(legend.title=element_text(size=16),legend.text=element_text(size=12),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())+geom_path(aes(group=Country),colour="purple",stat="identity")

#Connect specific countries with lines
ggplot(NMDS, aes(x=MDS1, y=MDS2, colour=group))+geom_point()+stat_ellipse()+theme_bw()+xlab("\n NMDS 1")+ylab("NMDS 2 \n")+theme(legend.title=element_text(size=16),legend.text=element_text(size=12),panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),panel.grid.major.x = element_blank(),panel.grid.minor.x = element_blank())+
  geom_path(data=NMDS[NMDS$Country %in% c("China","India","Republic of Korea"),],aes(x=MDS1, y=MDS2, group=Country,colour=Country), size=2)+scale_colour_manual(values=c("red","blue","black","orange","purple","green"))

