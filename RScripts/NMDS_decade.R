#-----------------------------------------------------
#---------Data Import and Transformation--------------
#-----------------------------------------------------

# Import data in longform
rawData <-read.csv("/Sara/SaraGitHub/skammlade.github.io/projects/tableau/datafiles/QuickCropDataAvgValueByDecade.csv", 
               stringsAsFactors = FALSE,
               strip.white = TRUE,
               na.strings = c("NA",""))

head(rawData)
tail(rawData)

colnames(rawData)<-c("CountryName","ItemName", "ElementName", "Decade","AvgValueForDecade")

# save Country as a factor in new column 'fCountry', as some of the statistical methods require factors
rawData<-within(rawData, 
            fCountry<-as.factor(CountryName))

# Calculate total value of (kcal, grams, etc.) across all crops for each country in each year
countryDecadeSums<-aggregate(rawData$AvgValueForDecade, 
                             by=list(rawData$CountryName, 
                                     rawData$Decade, 
                                     rawData$ElementName), 
                             FUN=sum,
                             na.rm=TRUE)

head(countryDecadeSums)

colnames(countryDecadeSums)<-c("CountryName","Decade","ElementName","CountryDecadeSumValue")
crops<-merge(rawData,countryDecadeSums)

head(crops)

#Grand total of calories in each year, all countries combined
globalSum<-aggregate(crops$AvgValueForDecade, 
                           by=list(crops$ElementName, 
                                   crops$Decade), 
                           FUN=sum,na.rm=TRUE)

colnames(globalSum)<-c("ElementName","Decade","GlobalSum")
crops<-merge(crops,
             globalSum,
             by=c("Decade","ElementName"))

head(crops)
tail(crops)

#Calculate proportion total for each crop
crops$Proportion<-crops$AvgValueForDecade/crops$CountryDecadeSumValue

#Add presence-absence - NOTE THAT THIS ASSUMES NA's ARE 0's!
crops$pres.abs<-0
crops$pres.abs[crops$AvgValueForDecade>0]<-1

#-------------------------------------------------------
#--------- Change in species Composition----------------
#-------------------------------------------------------

#create data subsets by ElementName
cropsCalories<-subset(crops, ElementName=="Calories")
cropsFat<-subset(crops, ElementName=="Fat")
cropsProtein<-subset(crops, ElementName=="Protein")
cropsFoodWeight<-subset(crops, ElementName=="Food Weight")

head(cropsCalories)
  
#convert missing values to zeros
cropsCalories$AvgValueForDecade[is.na(cropsCalories$AvgValueForDecade)]<-0
cropsFat$AvgValueForDecade[is.na(cropsFat$AvgValueForDecade)]<-0
cropsProtein$AvgValueForDecade[is.na(cropsProtein$AvgValueForDecade)]<-0
cropsFoodWeight$AvgValueForDecade[is.na(cropsFoodWeight$AvgValueForDecade)]<-0

#pivot datatable
pivotCropsCalories<-dcast(cropsCalories, Decade+CountryName ~ ItemName ,value.var="AvgValueForDecade")
pivotCropsFat<-dcast(cropsFat, Decade+CountryName ~ ItemName ,value.var="AvgValueForDecade")
pivotCropsProtein<-dcast(cropsProtein, Decade+CountryName ~ ItemName ,value.var="AvgValueForDecade")
pivotCropsFoodWeight<-dcast(cropsFoodWeight, Decade+CountryName ~ ItemName ,value.var="AvgValueForDecade")

#remove country and decade columns
dataNMDSDecadesCropsCalories<-pivotCropsCalories[,3:56]
dataNMDSDecadesCropsFat<-pivotCropsFat[,3:56]
dataNMDSDecadesCropsProtein<-pivotCropsProtein[,3:56]
dataNMDSDecadesCropsFoodWeight<-pivotCropsFoodWeight[,3:56]

#open vegan library
#https://cran.r-project.org/web/packages/vegan/vegan.pdf
library(vegan)

#Nonmetric multidimensional scaling with stable solution from random starts, axis scaling, and species scores
MDSCalories<-metaMDS(dataNMDSDecadesCropsCalories,
                        distance="bray",
                        k=2, 
                        trymax = 20, 
                        stratmax=0.9999999999 )

MDSFat<-metaMDS(dataNMDSDecadesCropsFat,
                     distance="bray",
                     k=2, 
                     trymax = 20, 
                     stratmax=0.9999999999 )

MDSProtein<-metaMDS(dataNMDSDecadesCropsProtein,
                distance="bray",
                k=2, 
                trymax = 20, 
                stratmax=0.9999999999 )

MDSFoodWeight<-metaMDS(dataNMDSDecadesCropsFoodWeight,
                    distance="bray",
                    k=2, 
                    trymax = 20, 
                    stratmax=0.9999999999 )


#stressplot to visualize fit of ordination
StressplotCaloriesDecade <- stressplot(MDSCalories) 
StressplotFatDecade <- stressplot(MDSFat)
StressplotProteinDecade <- stressplot(MDSProtein)
StressplotFoodWeightDecade <- stressplot(MDSFoodWeight)

#Stress test to show decrease in ordination stress with an increas in the number of dimensions
#https://cran.r-project.org/web/packages/goeveg/goeveg.pdf
library(goeveg)
#StressTestCaloriesDecade <- dimcheckMDS(dataNMDSDecadesCropsCalories, distance = "bray", k = 8, trymax = 20, autotransform = TRUE)
#StressTestFatDecade <- dimcheckMDS(dataNMDSDecadesCropsFat, distance = "bray", k = 8, trymax = 20, autotransform = TRUE)
#StressTestProteinDecade <- dimcheckMDS(dataNMDSDecadesCropsProtein, distance = "bray", k = 8, trymax = 20, autotransform = TRUE)
#StressTestFoodWeightDecade <- dimcheckMDS(dataNMDSDecadesCropsFoodWeight, distance = "bray", k = 8, trymax = 20, autotransform = TRUE)

#create variable 'NMDSGroups' with Country and Decade information
NMDSGroups<-pivotCropsCalories[,1:2]

#return NMDS coordinates for all Measurements including Country and Decade information
NMDSCoordinatesCalories<-data.frame(MDS1 = MDSCalories$points[,1], 
                                     MDS2 = MDSCalories$points[,2],
                                     group=NMDSGroups$Decade,
                                     Country=NMDSGroups$CountryName)
NMDSCoordinatesFat<-data.frame(MDS1 = MDSFat$points[,1], 
                                    MDS2 = MDSFat$points[,2],
                                    group=NMDSGroups$Decade,
                                    Country=NMDSGroups$CountryName)
NMDSCoordinatesProtein<-data.frame(MDS1 = MDSProtein$points[,1], 
                                    MDS2 = MDSProtein$points[,2],
                                    group=NMDSGroups$Decade,
                                    Country=NMDSGroups$CountryName)
NMDSCoordinatesFoodWeight<-data.frame(MDS1 = MDSFoodWeight$points[,1], 
                                    MDS2 = MDSFoodWeight$points[,2],
                                    group=NMDSGroups$Decade,
                                    Country=NMDSGroups$CountryName)

#add column 'ElementName' to dataframes
NMDSCoordinatesCalories$ElementName <- rep("Calories", nrow(NMDSCoordinatesCalories))
NMDSCoordinatesFat$ElementName <- rep("Fat", nrow(NMDSCoordinatesFat))
NMDSCoordinatesProtein$ElementName <- rep("Protein", nrow(NMDSCoordinatesProtein))
NMDSCoordinatesFoodWeight$ElementName <- rep("Food Weight", nrow(NMDSCoordinatesFoodWeight))

#merge all measurement coordinates dataframes vertically into one dataframe
NMDSCoordinatesDecadesAllMeasurements <- rbind(NMDSCoordinatesCalories,
                                NMDSCoordinatesFat,
                                NMDSCoordinatesProtein,
                                NMDSCoordinatesFoodWeight)

#export dataframe to csv file
#write.csv(NMDSCoordinatesDecadesAllMeasurements, "NMDSCoordinatesDecadesAllMeasurements.csv")


#--------------------------------------------------------------------------
#---------Additional scripts to obtain ellipses on 2 dimensions------------
#--------------------------------------------------------------------------

groupsNMDSDecadesCropsCalories<-pivotCropsCalories[,1:2]
groupsNMDSDecadesCropsFat<-pivotCropsFat[,1:2]
groupsNMDSDecadesCropsProtein<-pivotCropsProtein[,1:2]
groupsNMDSDecadesCropsFoodWeight<-pivotCropsFoodWeight[,1:2]


NMDSCaloriesDecade <- data.frame(MDS1 = MDSCalories$points[,1], 
                                 MDS2 = MDSCalories$points[,2],
                                 group = groupsNMDSDecadesCropsCalories$Decade,
                                 Country = groupsNMDSDecadesCropsCalories$CountryName)
NMDSFatDecade <- data.frame(MDS1 = MDSFat$points[,1], 
                                 MDS2 = MDSFat$points[,2],
                                 group = groupsNMDSDecadesCropsFat$Decade,
                                 Country = groupsNMDSDecadesCropsFat$CountryName)
NMDSProteinDecade <- data.frame(MDS1 = MDSProtein$points[,1], 
                                 MDS2 = MDSProtein$points[,2],
                                 group = groupsNMDSDecadesCropsProtein$Decade,
                                 Country = groupsNMDSDecadesCropsProtein$CountryName)
NMDSFoodWeightDecade <- data.frame(MDS1 = MDSFoodWeight$points[,1], 
                                 MDS2 = MDSFoodWeight$points[,2],
                                 group = groupsNMDSDecadesCropsFoodWeight$Decade,
                                 Country = groupsNMDSDecadesCropsFoodWeight$CountryName)


NMDSCaloriesDecadeGroupAsFactor <- within(NMDSCaloriesDecade, 
                                          group<-as.factor(group))
NMDSFatDecadeGroupAsFactor <- within(NMDSFatDecade, 
                                          group<-as.factor(group))
NMDSProteinDecadeGroupAsFactor <- within(NMDSProteinDecade, 
                                          group<-as.factor(group))
NMDSFoodWeightDecadeGroupAsFactor <- within(NMDSFoodWeightDecade, 
                                          group<-as.factor(group))


NMDSCaloriesDecadeGroupAsFactor$ElementName <- rep("Calories", 
                                                   nrow(NMDSCaloriesDecadeGroupAsFactor))
NMDSFatDecadeGroupAsFactor$ElementName <- rep("Fat", 
                                              nrow(NMDSFatDecadeGroupAsFactor))
NMDSProteinDecadeGroupAsFactor$ElementName <- rep("Protein", 
                                                  nrow(NMDSProteinDecadeGroupAsFactor))
NMDSFoodWeightDecadeGroupAsFactor$ElementName <- rep("FoodWeight", 
                                                     nrow(NMDSFoodWeightDecadeGroupAsFactor))
library(ggplot2)

#VIew NMDS plots and connect specific countries with lines
plotCaloriesDecade <- ggplot(NMDSCaloriesDecadeGroupAsFactor, 
                            aes(x=NMDSCaloriesDecadeGroupAsFactor$MDS1, 
                                y=NMDSCaloriesDecadeGroupAsFactor$MDS2, 
                                colour=group))+
                      geom_point()+
                      stat_ellipse()+
                      scale_colour_manual(values=c("#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84", 
                                                   "#bd0026", "#f03b20", "#fd8d3c"),
                                          name="Decade")+
                      theme_bw()+
                      xlab("NMDS 1")+
                      ylab("NMDS 2")+
                      labs(title="Calories 2 dimensions")+
                      geom_path(data=NMDSCaloriesDecadeGroupAsFactor[NMDSCaloriesDecadeGroupAsFactor$Country %in% c("China","Mongolia", "Maldives"),],
                                aes(x=MDS1, 
                                    y=MDS2, 
                                    group=Country, 
                                    colour=Country), 
                                size=1)

plotFatDecade <- ggplot(NMDSFatDecadeGroupAsFactor, 
                             aes(x=NMDSFatDecadeGroupAsFactor$MDS1, 
                                 y=NMDSFatDecadeGroupAsFactor$MDS2, 
                                 colour=group))+
                geom_point()+
                stat_ellipse()+
                scale_colour_manual(values=c("#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84", 
                                             "#bd0026", "#f03b20", "#fd8d3c"),
                                    name="Decade")+
                theme_bw()+
                xlab("NMDS 1")+
                ylab("NMDS 2")+
                labs(title="Fat 2 dimensions")+
                geom_path(data=NMDSFatDecadeGroupAsFactor[NMDSFatDecadeGroupAsFactor$Country %in% c("China","Mongolia", "Maldives"),],
                          aes(x=MDS1, 
                              y=MDS2, 
                              group=Country, 
                              colour=Country), 
                          size=1)

plotProteinDecade <- ggplot(NMDSProteinDecadeGroupAsFactor, 
                             aes(x=NMDSProteinDecadeGroupAsFactor$MDS1, 
                                 y=NMDSProteinDecadeGroupAsFactor$MDS2, 
                                 colour=group))+
                     geom_point()+
                     stat_ellipse()+
                     scale_colour_manual(values=c("#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84", 
                                                  "#bd0026", "#f03b20", "#fd8d3c"),
                                         name="Decade")+
                     theme_bw()+
                     xlab("NMDS 1")+
                     ylab("NMDS 2")+
                     labs(title="Protein 2 dimensions")+
                     geom_path(data=NMDSProteinDecadeGroupAsFactor[NMDSProteinDecadeGroupAsFactor$Country %in% c("China","Mongolia", "Maldives"),],
                               aes(x=MDS1, 
                                   y=MDS2, 
                                   group=Country, 
                                   colour=Country), 
                               size=1)
                    

plotFoodWeightDecade <- ggplot(NMDSFoodWeightDecadeGroupAsFactor, 
                             aes(x=NMDSFoodWeightDecadeGroupAsFactor$MDS1, 
                                 y=NMDSFoodWeightDecadeGroupAsFactor$MDS2, 
                                 colour=group))+
                        geom_point()+
                        stat_ellipse()+
                        scale_colour_manual(values=c("#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84", 
                                                     "#bd0026", "#f03b20", "#fd8d3c"),
                                            name="Decade")+
                        theme_bw()+
                        xlab("NMDS 1")+
                        ylab("NMDS 2")+
                        labs(title="FoodWeight 2 dimensions")+
                        geom_path(data=NMDSFoodWeightDecadeGroupAsFactor[NMDSFoodWeightDecadeGroupAsFactor$Country %in% c("China","Mongolia", "Maldives"),],
                                  aes(x=MDS1, 
                                      y=MDS2, 
                                      group=Country, 
                                      colour=Country), 
                                  size=1)
                       


#--------------------------------------------------------------------------
#---------Additional scripts to obtain ellipses on 3 dimensions------------
#--------------------------------------------------------------------------

#Nonmetric multidimensional scaling with stable solution from random starts, axis scaling, and species scores
MDSCalories3Dim<-metaMDS(dataNMDSDecadesCropsCalories,
                       distance="bray",
                       k=3, 
                       trymax = 20, 
                       stratmax=0.9999999999 )

MDSFat3Dim<-metaMDS(dataNMDSDecadesCropsFat,
                  distance="bray",
                  k=3, 
                  trymax = 20, 
                  stratmax=0.9999999999 )

MDSProtein3Dim<-metaMDS(dataNMDSDecadesCropsProtein,
                      distance="bray",
                      k=3, 
                      trymax = 20, 
                      stratmax=0.9999999999 )

MDSFoodWeight3Dim<-metaMDS(dataNMDSDecadesCropsFoodWeight,
                         distance="bray",
                         k=3, 
                         trymax = 20, 
                         stratmax=0.9999999999 )


#create variable 'NMDSGroups' with Country and Decade information
NMDSGroups3Dim<-pivotCropsCalories[,1:2]

#return NMDS coordinates for all Measurements including Country and Decade information
NMDSCoordinatesCalories3Dim<-data.frame(MDS1 = MDSCalories3Dim$points[,1], 
                                    MDS2 = MDSCalories3Dim$points[,2],
                                    group=NMDSGroups3Dim$Decade,
                                    Country=NMDSGroups3Dim$Country)
NMDSCoordinatesFat3Dim<-data.frame(MDS1 = MDSFat3Dim$points[,1], 
                               MDS2 = MDSFat3Dim$points[,2],
                               group=NMDSGroups3Dim$Decade,
                               Country=NMDSGroups3Dim$Country)
NMDSCoordinatesProtein3Dim<-data.frame(MDS1 = MDSProtein3Dim$points[,1], 
                                   MDS2 = MDSProtein3Dim$points[,2],
                                   group=NMDSGroups3Dim$Decade,
                                   Country=NMDSGroups3Dim$Country)
NMDSCoordinatesFoodWeight3Dim<-data.frame(MDS1 = MDSFoodWeight3Dim$points[,1], 
                                      MDS2 = MDSFoodWeight3Dim$points[,2],
                                      group=NMDSGroups3Dim$Decade,
                                      Country=NMDSGroups3Dim$Country)

#add column 'ElementName' to dataframes
NMDSCoordinatesCalories3Dim$ElementName <- rep("Calories", nrow(NMDSCoordinatesCalories3Dim))
NMDSCoordinatesFat3Dim$ElementName <- rep("Fat", nrow(NMDSCoordinatesFat3Dim))
NMDSCoordinatesProtein3Dim$ElementName <- rep("Protein", nrow(NMDSCoordinatesProtein3Dim))
NMDSCoordinatesFoodWeight3Dim$ElementName <- rep("Food Weight", nrow(NMDSCoordinatesFoodWeight3Dim))

#merge all measurement coordinates dataframes vertically into one dataframe
NMDSCoordinatesDecadesAllMeasurements3Dim <- rbind(NMDSCoordinatesCalories3Dim,
                                               NMDSCoordinatesFat3Dim,
                                               NMDSCoordinatesProtein3Dim,
                                               NMDSCoordinatesFoodWeight3Dim)

#elipses

groupsNMDSDecadesCropsCalories3Dim<-pivotCropsCalories[,1:2]
groupsNMDSDecadesCropsFat3Dim<-pivotCropsFat[,1:2]
groupsNMDSDecadesCropsProtein3Dim<-pivotCropsProtein[,1:2]
groupsNMDSDecadesCropsFoodWeight3Dim<-pivotCropsFoodWeight[,1:2]


NMDSCaloriesDecade3Dim <- data.frame(MDS1 = MDSCalories3Dim$points[,1], 
                                 MDS2 = MDSCalories3Dim$points[,2],
                                 group = groupsNMDSDecadesCropsCalories3Dim$Decade,
                                 Country = groupsNMDSDecadesCropsCalories3Dim$CountryName)
NMDSFatDecade3Dim <- data.frame(MDS1 = MDSFat3Dim$points[,1], 
                            MDS2 = MDSFat3Dim$points[,2],
                            group = groupsNMDSDecadesCropsFat3Dim$Decade,
                            Country = groupsNMDSDecadesCropsFat3Dim$CountryName)
NMDSProteinDecade3Dim <- data.frame(MDS1 = MDSProtein3Dim$points[,1], 
                                MDS2 = MDSProtein3Dim$points[,2],
                                group = groupsNMDSDecadesCropsProtein3Dim$Decade,
                                Country = groupsNMDSDecadesCropsProtein3Dim$CountryName)
NMDSFoodWeightDecade3Dim <- data.frame(MDS1 = MDSFoodWeight3Dim$points[,1], 
                                   MDS2 = MDSFoodWeight3Dim$points[,2],
                                   group = groupsNMDSDecadesCropsFoodWeight3Dim$Decade,
                                   Country = groupsNMDSDecadesCropsFoodWeight3Dim$CountryName)


NMDSCaloriesDecadeGroupAsFactor3Dim <- within(NMDSCaloriesDecade3Dim, 
                                          group<-as.factor(group))
NMDSFatDecadeGroupAsFactor3Dim <- within(NMDSFatDecade3Dim, 
                                     group<-as.factor(group))
NMDSProteinDecadeGroupAsFactor3Dim <- within(NMDSProteinDecade3Dim, 
                                         group<-as.factor(group))
NMDSFoodWeightDecadeGroupAsFactor3Dim <- within(NMDSFoodWeightDecade3Dim, 
                                            group<-as.factor(group))


NMDSCaloriesDecadeGroupAsFactor3Dim$ElementName <- rep("Calories", 
                                                   nrow(NMDSCaloriesDecadeGroupAsFactor3Dim))
NMDSFatDecadeGroupAsFactor3Dim$ElementName <- rep("Fat", 
                                              nrow(NMDSFatDecadeGroupAsFactor3Dim))
NMDSProteinDecadeGroupAsFactor3Dim$ElementName <- rep("Protein", 
                                                  nrow(NMDSProteinDecadeGroupAsFactor3Dim))
NMDSFoodWeightDecadeGroupAsFactor3Dim$ElementName <- rep("FoodWeight", 
                                                     nrow(NMDSFoodWeightDecadeGroupAsFactor3Dim))
library(ggplot2)

#VIew NMDS plots and connect specific countries with lines
plotCaloriesDecade3Dim <- ggplot(NMDSCaloriesDecadeGroupAsFactor3Dim, 
                             aes(x=NMDSCaloriesDecadeGroupAsFactor3Dim$MDS1, 
                                 y=NMDSCaloriesDecadeGroupAsFactor3Dim$MDS2, 
                                 colour=group))+
  geom_point()+
  stat_ellipse()+
  scale_colour_manual(values=c("#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84", 
                               "#bd0026", "#f03b20", "#fd8d3c"),
                      name="Decade")+
  theme_bw()+
  xlab("NMDS 1")+
  ylab("NMDS 2")+
  labs(title="Calories 3 dimensions (plot first 2 of 3 dimensions)")+
  geom_path(data=NMDSCaloriesDecadeGroupAsFactor3Dim[NMDSCaloriesDecadeGroupAsFactor3Dim$Country %in% c("China","Mongolia", "Maldives"),],
            aes(x=MDS1, 
                y=MDS2, 
                group=Country, 
                colour=Country), 
            size=1)

plotFatDecade <- ggplot(NMDSFatDecadeGroupAsFactor3Dim,
                        aes(x=NMDSFatDecadeGroupAsFactor3Dim$MDS1, 
                            y=NMDSFatDecadeGroupAsFactor3Dim$MDS2, 
                            colour=group))+
  geom_point()+
  stat_ellipse()+
  scale_colour_manual(values=c("#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84", 
                               "#bd0026", "#f03b20", "#fd8d3c"),
                      name="Decade")+
  theme_bw()+
  xlab("NMDS 1")+
  ylab("NMDS 2")+
  labs(title="Fat 3 dimensions (plot first 2 of 3 dimensions)")+
  geom_path(data=NMDSFatDecadeGroupAsFactor3Dim[NMDSFatDecadeGroupAsFactor3Dim$Country %in% c("China","Mongolia", "Maldives"),],
            aes(x=MDS1, 
                y=MDS2, 
                group=Country, 
                colour=Country), 
            size=1)

plotProteinDecade <- ggplot(NMDSProteinDecadeGroupAsFactor3Dim, 
                            aes(x=NMDSProteinDecadeGroupAsFactor3Dim$MDS1, 
                                y=NMDSProteinDecadeGroupAsFactor3Dim$MDS2, 
                                colour=group))+
  geom_point()+
  stat_ellipse()+
  scale_colour_manual(values=c("#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84", 
                               "#bd0026", "#f03b20", "#fd8d3c"),
                      name="Decade")+
  theme_bw()+
  xlab("NMDS 1")+
  ylab("NMDS 2")+
  labs(title="Protein 2 dimensions")+
  geom_path(data=NMDSProteinDecadeGroupAsFactor3Dim[NMDSProteinDecadeGroupAsFactor3Dim$Country %in% c("China","Mongolia", "Maldives"),],
            aes(x=MDS1, 
                y=MDS2, 
                group=Country, 
                colour=Country), 
            size=1)


plotFoodWeightDecade <- ggplot(NMDSFoodWeightDecadeGroupAsFactor3Dim, 
                               aes(x=NMDSFoodWeightDecadeGroupAsFactor3Dim$MDS1, 
                                   y=NMDSFoodWeightDecadeGroupAsFactor3Dim$MDS2, 
                                   colour=group))+
  geom_point()+
  stat_ellipse()+
  scale_colour_manual(values=c("#7fcdbb", "#41b6c4", "#1d91c0", "#225ea8", "#0c2c84", 
                               "#bd0026", "#f03b20", "#fd8d3c"),
                      name="Decade")+
  theme_bw()+
  xlab("NMDS 1")+
  ylab("NMDS 2")+
  labs(title="FoodWeight 2 dimensions")+
  geom_path(data=NMDSFoodWeightDecadeGroupAsFactor3Dim[NMDSFoodWeightDecadeGroupAsFactor3Dim$Country %in% c("China","Mongolia", "Maldives"),],
            aes(x=MDS1, 
                y=MDS2, 
                group=Country, 
                colour=Country), 
            size=1)







































#Nonmetric multidimensional scaling with stable solution from random starts, axis scaling, and species scores
3DimMDSCalories<-metaMDS(dataNMDSDecadesCropsCalories,
                     distance="bray",
                     k=3, 
                     trymax = 20, 
                     stratmax=0.9999999999 )

3DimMDSFat<-metaMDS(dataNMDSDecadesCropsFat,
                distance="bray",
                k=3, 
                trymax = 20, 
                stratmax=0.9999999999 )

3DimMDSProtein<-metaMDS(dataNMDSDecadesCropsProtein,
                    distance="bray",
                    k=3, 
                    trymax = 20, 
                    stratmax=0.9999999999 )

3DimMDSFoodWeight<-metaMDS(dataNMDSDecadesCropsFoodWeight,
                       distance="bray",
                       k=3, 
                       trymax = 20, 
                       stratmax=0.9999999999 )

#create variable 'NMDSGroups' with Country and Decade information
#NMDSGroups<-pivotCropsCalories[,1:2]

#return NMDS coordinates for all Measurements including Country and Decade information
3DimNMDSCoordinatesCalories<-data.frame(MDS1 = 3DimMDSCalories$points[,1], 
                                    MDS2 = 3DimMDSCalories$points[,2],
                                    group=NMDSGroups$Decade,
                                    Country=NMDSGroups$Country)
3DimNMDSCoordinatesFat<-data.frame(MDS1 = 3DimMDSFat$points[,1], 
                               MDS2 = 3DimMDSFat$points[,2],
                               group=NMDSGroups$Decade,
                               Country=NMDSGroups$Country)
3DimNMDSCoordinatesProtein<-data.frame(MDS1 =3DimMDSProtein$points[,1], 
                                   MDS2 = 3DimMDSProtein$points[,2],
                                   group=NMDSGroups$Decade,
                                   Country=NMDSGroups$Country)
3DimNMDSCoordinatesFoodWeight<-data.frame(MDS1 = 3DimMDSFoodWeight$points[,1], 
                                      MDS2 = 3DimMDSFoodWeight$points[,2],
                                      group=NMDSGroups$Decade,
                                      Country=NMDSGroups$Country)

#add column 'ElementName' to dataframes
3DimNMDSCoordinatesCalories$ElementName <- rep("Calories", nrow(NMDSCoordinatesCalories))
3DimNMDSCoordinatesFat$ElementName <- rep("Fat", nrow(NMDSCoordinatesFat))
3DimNMDSCoordinatesProtein$ElementName <- rep("Protein", nrow(NMDSCoordinatesProtein))
3DimNMDSCoordinatesFoodWeight$ElementName <- rep("Food Weight", nrow(NMDSCoordinatesFoodWeight))

#merge all measurement coordinates dataframes vertically into one dataframe
3DimNMDSCoordinatesDecadesAllMeasurements <- rbind(3DimNMDSCoordinatesCalories,
                                               3DimNMDSCoordinatesFat,
                                               3DimNMDSCoordinatesProtein,
                                               3DimNMDSCoordinatesFoodWeight)

groupsNMDSDecadesCropsCalories<-pivotCropsCalories[,1:2]
groupsNMDSDecadesCropsFat<-pivotCropsFat[,1:2]
groupsNMDSDecadesCropsProtein<-pivotCropsProtein[,1:2]
groupsNMDSDecadesCropsFoodWeight<-pivotCropsFoodWeight[,1:2]


3DimNMDSCaloriesDecade <- data.frame(MDS1 = MDSCalories$points[,1], 
                                     MDS2 = MDSCalories$points[,2],
                                     group = MDSCalories$Year,
                                     Country = MDSCalories$Country)
3DimNMDSFatDecade <- data.frame(MDS1 = MDSFat$points[,1], 
                                MDS2 = MDSFat$points[,2],
                                group = MDSFat$Year,
                                Country = MDSFat$Country)
3DimNMDSProteinDecade <- data.frame(MDS1 = MDSProtein$points[,1], 
                                    MDS2 = MDSProtein$points[,2],
                                    group = MDSProtein$Year,
                                    Country = MDSProtein$Country)
3DimNMDSFoodWeightDecade <- data.frame(MDS1 = MDSFoodWeight$points[,1], 
                               MDS2 = MDSFoodWeight$points[,2],
                               group = MDSFoodWeight$Year,
                               Country = MDSFoodWeight$Country)


3DimNMDSCaloriesDecadeGroupAsFactor <- within(NMDSCaloriesDecade, 
                                          group<-as.factor(group))
3DimNMDSFatDecadeGroupAsFactor <- within(NMDSFatDecade, 
                                     group<-as.factor(group))
3DimNMDSProteinDecadeGroupAsFactor <- within(NMDSProteinDecade, 
                                         group<-as.factor(group))
3DimNMDSFoodWeightDecadeGroupAsFactor <- within(NMDSFoodWeightDecade, 
                                            group<-as.factor(group))


3DimNMDSCaloriesDecadeGroupAsFactor$ElementName <- rep("Calories", 
                                                   nrow(NMDSCaloriesDecadeGroupAsFactor))
3DimNMDSFatDecadeGroupAsFactor$ElementName <- rep("Fat", 
                                              nrow(NMDSFatDecadeGroupAsFactor))
3DimNMDSProteinDecadeGroupAsFactor$ElementName <- rep("Protein", 
                                                  nrow(NMDSProteinDecadeGroupAsFactor))
3DimNMDSFoodWeightDecadeGroupAsFactor$ElementName <- rep("FoodWeight", 
                                                     nrow(NMDSFoodWeightDecadeGroupAsFactor))


#VIew NMDS plots and connect specific countries with lines
3DimPlotCaloriesDecade <- ggplot(3DimNMDSCaloriesDecadeGroupAsFactor, 
                             aes(x=NMDS$MDS1, 
                                 y=NMDS$MDS2, 
                                 colour=group))+
  geom_point()+
  stat_ellipse()+
  scale_colour_manual(values=c("#ffffcc", #a1dab4", #41b6c4", "#2c7fb8", "#253494", 
                               "#bd0026", "#f03b20", "#fd8d3c"),
                      name="Decade")+
  theme_bw()+
  xlab("NMDS 1")+
  ylab("NMDS 2")+
  labs(title="Calories 3 dimensions")+
  geom_path(data=NMDS[NMDS$Country %in% c("China","Mongolia", "Maldives"),],
            aes(x=MDS1, 
                y=MDS2, 
                group=Country, 
                colour=Country), 
            size=1)

3DimPlotFatDecade <- ggplot(3DimNMDSFatDecadeGroupAsFactor, 
                        aes(x=NMDS$MDS1, 
                            y=NMDS$MDS2, 
                            colour=group))+
  geom_point()+
  stat_ellipse()+
  scale_colour_manual(values=c("#ffffcc", #a1dab4", #41b6c4", "#2c7fb8", "#253494", 
                               "#bd0026", "#f03b20", "#fd8d3c"),
                      name="Decade")+
  theme_bw()+
  xlab("NMDS 1")+
  ylab("NMDS 2")+
  labs(title="Fat 3 dimensions")+
  geom_path(data=NMDS[NMDS$Country %in% c("China","Mongolia", "Maldives"),],
            aes(x=MDS1, 
                y=MDS2, 
                group=Country, 
                colour=Country), 
            size=1)

3DimPlotProteinDecade <- ggplot(3DimNMDSProteinDecadeGroupAsFactor, 
                            aes(x=NMDS$MDS1, 
                                y=NMDS$MDS2, 
                                colour=group))+
  geom_point()+
  stat_ellipse()+
  scale_colour_manual(values=c("#ffffcc", #a1dab4", #41b6c4", "#2c7fb8", "#253494", 
                               "#bd0026", "#f03b20", "#fd8d3c"),
                      name="Decade")+
  theme_bw()+
  xlab("NMDS 1")+
  ylab("NMDS 2")+
  labs(title="Protein 3 dimensions")+
  geom_path(data=NMDS[NMDS$Country %in% c("China","Mongolia", "Maldives"),],
            aes(x=MDS1, 
                y=MDS2, 
                group=Country, 
                colour=Country), 
            size=1)

3DimPlotFoodWeightDecade <- ggplot(3DimNMDSFoodWeightDecadeGroupAsFactor, 
                               aes(x=NMDS$MDS1, 
                                   y=NMDS$MDS2, 
                                   colour=group))+
  geom_point()+
  stat_ellipse()+
  scale_colour_manual(values=c("#ffffcc", #a1dab4", #41b6c4", "#2c7fb8", "#253494", 
                               "#bd0026", "#f03b20", "#fd8d3c"),
                      name="Decade")+
  theme_bw()+
  xlab("NMDS 1")+
  ylab("NMDS 2")+
  labs(title="FoodWeight 3 dimensions")+
  geom_path(data=NMDS[NMDS$Country %in% c("China","Mongolia", "Maldives"),],
            aes(x=MDS1, 
                y=MDS2, 
                group=Country, 
                colour=Country), 
            size=1)































#set up colors for  each ellipse
EllipseColorsDecade<-rep(c("firebrick3","midnightblue","black"),c(152,152,152,152,152))

#Get species and site scores
speciesScoreCalories<-scores(MDSCalories,display="species",shrink=shrink)
speciesScoreFat<-scores(MDSFat,display="species",shrink=shrink)
speciesScoreProtein<-scores(MDSProtein,display="species",shrink=shrink)
speciesScoreFoodWeight<-scores(MDSFoodWeight,display="species",shrink=shrink)

countryScoreCalories<-scores(MDSCalories,display="sites")
countryScoreFat<-scores(MDSFat,display="sites")
countryScoreProtein<-scores(MDSProtein,display="sites")
countryScoreFoodWeight<-scores(MDSFoodWeight,display="sites")


#Work out ranges
ylimyCalories<-range(speciesScoreCalories[,2],countryScoreCalories[,2])
xlimxCalories<-range(speciesScoreCalories[,1],countryScoreCalories[,1])

ylimyFat<-range(speciesScoreFat[,2],countryScoreFat[,2])
xlimxFat<-range(speciesScoreFat[,1],countryScoreFat[,1])

ylimyProtein<-range(speciesScoreProtein[,2],countryScoreProtein[,2])
xlimxProtein<-range(speciesScoreProtein[,1],countryScoreProtein[,1])

ylimyFoodWeight<-range(speciesScoreFoodWeight[,2],countryScoreFoodWeight[,2])
xlimxFoodWeight<-range(speciesScoreFoodWeight[,1],countryScoreFoodWeight[,1])
