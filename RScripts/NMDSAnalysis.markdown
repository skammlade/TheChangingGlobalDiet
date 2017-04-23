## NMDS Analysis

An interactive multivariate ordination of crop commodity composition can be found [here](http://skammlade.github.io/projects/tableau/NMDSInteractive.html).

#### About the analyses

NMDS coordinates were found using the *metaMDS()* function from the *vegan* package.
```{r, eval=FALSE}
library(vegan)
metaMDS(comm = data, 
        distance = "bray", 
        k = 2,      
        trymax = 20, 
        stratmax = 0.9999999999) 
```

<br/>

**Stressplots** (or Shepard diagrams) are used to visualize the fit of the ordination. 

```{r, eval=FALSE}
library(vegan)
stressplot(data)
```
Using [*stressplot()*](http://cc.oulu.fi/~jarioksa/opetus/metodi/vegantutor.pdf) from R package *vegan*. 

<br/>

**Stress Value Tests** are used to visualize stress values for a given number of tested dimensions. The plot shows the decrease in ordination distance with an increase in the number of ordination dimensions.
```{r, eval=FALSE}
library(goeveg)
dimcheckMDS(data, 
            distance = "bray", 
            k = 8, 
            trymax = 20, 
            autotransform = TRUE)
```
Using [*dimcheckMDS()*](https://cran.r-project.org/web/packages/goeveg/goeveg.pdf) from R package *goeveg*.  

<br/>

Below you will find diagnostic tests of the analysis for each Measurement.

#### Calories

#### -- Stress Value Tests

Stress Value Tests indicate ordination should be done over 3 dimensions.

![Calories using decade dataset](https://raw.githubusercontent.com/skammlade/skammlade.github.io/master/projects/tableau/RScripts/StressValueTestNMDSDecadesCalories.png)

#### -- Stressplots

Stressplots performed over 2 dimensions

![Calories using decade dataset](https://raw.githubusercontent.com/skammlade/skammlade.github.io/master/projects/tableau/RScripts/StressplotNMDSDecadesCalories.png)

#### Fat

#### -- Stress Value Tests

![Fat using decade dataset](https://raw.githubusercontent.com/skammlade/skammlade.github.io/master/projects/tableau/RScripts/StressValueTestNMDSDecadesFat.png)

#### -- Stressplots

![Fat using decade dataset](https://raw.githubusercontent.com/skammlade/skammlade.github.io/master/projects/tableau/RScripts/StressplotNMDSDecadesFat.png) 

#### Protein

#### -- Stress Value Tests

![Protein using decade dataset](https://raw.githubusercontent.com/skammlade/skammlade.github.io/master/projects/tableau/RScripts/StressValueTestNMDSDecadesProtein.png)

#### -- Stressplots

![Protein using decade dataset](https://raw.githubusercontent.com/skammlade/skammlade.github.io/master/projects/tableau/RScripts/StressplotNMDSDecadesProtein.png) 

#### FoodWeight

#### -- Stress Value Tests

![Food weight using decade dataset](https://raw.githubusercontent.com/skammlade/skammlade.github.io/master/projects/tableau/RScripts/StressValueTestNMDSDecadesFoodWeight.png)

#### -- Stressplots

![Food weight using decade dataset](https://raw.githubusercontent.com/skammlade/skammlade.github.io/master/projects/tableau/RScripts/StressplotNMDSDecadesFoodWeight.png) 

