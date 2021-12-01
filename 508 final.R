library(tidyverse)
library(sf)
library(RSocrata)
library(viridis)
library(spatstat)
library(raster)
library(spdep)
library(FNN)
library(grid)
library(gridExtra)
library(knitr)
library(kableExtra)
library(tidycensus)
# functions
root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"
source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

cin_overdose<- st_read("C:/Users/zheng/Desktop/508 final/Cinci_Overdoses.geojson") %>%
  st_transform('ESRI:102258')

#build prediction dataset with test=0 (year2016)
drug16<-cin_overdose%>%
  filter(test==0)%>%
  st_transform('ESRI:102258')

#build test dataset with test=0 (year2017)
drug17<-cin_overdose%>%
  filter(test==1)%>%
  st_transform('ESRI:102258')

cin_boundary <- st_read("https://opendata.arcgis.com/datasets/ed78f4754b044ac5815d0a9efe9bb336_1.geojson") %>%
  st_transform('ESRI:102258')

grid.arrange(ncol=2,
             ggplot() + 
               geom_sf(data = cin_boundary) +
               geom_sf(data = drug16, colour="red", size=0.1, show.legend = "point") +
               labs(title= "Heroin, Cincinnati - 2016",caption="Figure 1. Visualizing Heroin as points and density") +
               mapTheme(title_size = 14),
             
             ggplot() + 
               geom_sf(data = cin_boundary, fill = "grey40") +
               stat_density2d(data = data.frame(st_coordinates(drug16)), 
                              aes(X, Y, fill = ..level.., alpha = ..level..),
                              size = 0.01, bins = 40, geom = 'polygon') +
               scale_fill_viridis() +
               scale_alpha(range = c(0.00, 0.35), guide = FALSE) +
               labs(title = "Density of Heroin") +
               mapTheme(title_size = 14) + theme(legend.position = "none"))

#Creating the fishnet
fishnet <- 
  st_make_grid(cin_boundary,
               cellsize = 500, 
               square = TRUE) %>%
  .[cin_boundary] %>%            # <- MDH Added
  st_sf() %>%
  mutate(uniqueID = rownames(.))

#Data wrangling: Joining burglaries to the fishnet
## add a value of 1 to each crime, sum them with aggregate
drug_net <- 
  dplyr::select(drug16) %>% 
  mutate(countDrug = 1) %>% 
  aggregate(., fishnet, sum) %>%
  mutate(countDrug = replace_na(countDrug, 0),
         uniqueID = rownames(.),
         cvID = sample(round(nrow(fishnet) / 24), 
                       size=nrow(fishnet), replace = TRUE))

ggplot() +
  geom_sf(data = drug_net, aes(fill = countDrug), color = NA) +
  scale_fill_viridis() +
  labs(title = "Figure 2. Count of Heroin for the fishnet") +
  mapTheme()

#Wrangling risk factors
request311 <- read.socrata("https://data.cincinnati-oh.gov/resource/4cjh-bm8b.json")%>%
  filter(requested_date >= "2016-01-01T00:00:00.00Z" & requested_date <= "2016-12-31T00:00:00.00Z") #select only year 2016 for prediction

#multiple map of your risk factors in the fishnet (counts, distance, and/or other feature engineering approaches).
#junk vehicle
junk_cars<-request311%>%
  filter(stringr::str_detect(service_code, "ABAN-VNA"))%>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X","Y"), crs=4326, agr="constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Junk_Cars")

graffiti<-request311%>%
  filter(stringr::str_detect(service_code, "GRFITI"))%>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X","Y"), crs=4326, agr="constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Graffiti")

streetLightsOut <-request311%>%
  filter(stringr::str_detect(service_code, "STRTLITE"))%>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X","Y"), crs=4326, agr="constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Street_Lights_Out")

dead_ani<-request311%>%
  filter(stringr::str_detect(service_code, "DAPUB1"))%>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X","Y"), crs=4326, agr="constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Dead_Animal")

pot_hole<-request311%>%
  filter(stringr::str_detect(service_code, "PTHOLE"))%>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X","Y"), crs=4326, agr="constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Pot_Hole")

vac_building<-request311%>%
  filter(stringr::str_detect(service_code, "BLD_VACR"))%>%
  dplyr::select(Y = latitude, X = longitude) %>%
  na.omit() %>%
  st_as_sf(coords = c("X","Y"), crs=4326, agr="constant") %>%
  st_transform(st_crs(fishnet)) %>%
  mutate(Legend = "Vacant_Building")

neighborhoods <-
  st_read(paste0("https://opendata.arcgis.com/datasets/572561553c9e4d618d2d7939c5261d46_0.geojson")) %>%
  st_transform('ESRI:102258')

#Feature engineering - Count of risk factors by grid cell
vars_net <-
  rbind(junk_cars,streetLightsOut,dead_ani,
        vac_building, graffiti, pot_hole)%>%
  st_join(., fishnet, join=st_within) %>%
  st_drop_geometry() %>%
  group_by(uniqueID, Legend) %>%
  summarize(count = n()) %>%
  full_join(fishnet, by = "uniqueID") %>%
  spread(Legend, count, fill=0) %>%
  st_sf() %>%
  dplyr::select(-`<NA>`) %>%
  na.omit() %>%
  ungroup()

vars_net.long<-gather(vars_net,Variable,value,-geometry,-uniqueID)
vars<-unique(vars_net.long$Variable)
mapList<-list()
for(i in vars)
{mapList[[i]]<-
  ggplot()+
  geom_sf(data=filter(vars_net.long,Variable==i),
          aes(fill=value),colour=NA)+
  scale_fill_viridis(name="")+
  labs(title=i)+
  mapTheme()}

do.call(grid.arrange,c(mapList,ncol=3,top="Figure 3. Risk Factors by Fishnet"))

##Feature engineering - Nearest Neighbor Feature
nn_function <- function(measureFrom,measureTo,k) {
  measureFrom_Matrix <-
    as.matrix(measureFrom)
  measureTo_Matrix <-
    as.matrix(measureTo)
  nn <-   
    get.knnx(measureTo, measureFrom, k)$nn.dist
  output <-
    as.data.frame(nn) %>%
    rownames_to_column(var = "thisPoint") %>%
    gather(points, point_distance, V1:ncol(.)) %>%
    arrange(as.numeric(thisPoint)) %>%
    group_by(thisPoint) %>%
    summarize(pointDistance = mean(point_distance)) %>%
    arrange(as.numeric(thisPoint)) %>% 
    dplyr::select(-thisPoint) %>%
    pull()
  
  return(output)  
}

st_c    <- st_coordinates
st_coid <- st_centroid

vars_net <-
  vars_net %>%
  mutate(
    junk_cars.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(junk_cars),3),
    streetLightsOut.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(streetLightsOut),3),
    dead_ani.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(dead_ani),3),
    vac_building.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(vac_building),3),
    graffiti.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(graffiti),3),
    pot_hole.nn =
      nn_function(st_c(st_coid(vars_net)), st_c(pot_hole),3))

## Visualize the NN feature
vars_net.long.nn <- 
  dplyr::select(vars_net, ends_with(".nn")) %>%
  gather(Variable, value, -geometry)

vars <- unique(vars_net.long.nn$Variable)
mapList <- list()

for(i in vars){
  mapList[[i]] <- 
    ggplot() +
    geom_sf(data = filter(vars_net.long.nn, Variable == i), aes(fill=value), colour=NA) +
    scale_fill_viridis(name="") +
    labs(title=i) +
    mapTheme()}

do.call(grid.arrange,c(mapList,ncol=3,top="Figure 4. Nearest Neighbor risk Factors by Fishnet"))

#create final_set
final_net<-left_join(drug_net,st_drop_geometry(vars_net),by="uniqueID")

final_net<-st_centroid(final_net)%>%
  st_join(dplyr::select(neighborhoods,SNA_NAME))%>%
  st_drop_geometry()%>%
  left_join(dplyr::select(final_net,geometry,uniqueID))%>%
  st_sf()%>%
  na.omit()

final_net.nb <- poly2nb(as_Spatial(final_net), queen=TRUE)
final_net.weights <- nb2listw(final_net.nb, style="W", zero.policy=TRUE)

## see local moran
local_morans <- localmoran(final_net$countDrug, final_net.weights, zero.policy=TRUE) %>% 
  as.data.frame()

# join local Moran's I results to fishnet
final_net.localMorans <- 
  cbind(local_morans, as.data.frame(final_net)) %>% 
  st_sf() %>%
  dplyr::select(Drug_Count = countDrug, 
                Local_Morans_I = Ii, 
                P_Value = `Pr(z != E(Ii))`) %>%
  mutate(Significant_Hotspots = ifelse(P_Value <= 0.001, 1, 0)) %>%
  gather(Variable, Value, -geometry)

vars<-unique(final_net.localMorans$Variable)
varList<-list()
for(i in vars){
  varList[[i]]<-
    ggplot()+
    geom_sf(data=filter(final_net.localMorans,Variable==i),
            aes(fill=Value),colour=NA)+
    scale_fill_viridis(name="")+
    labs(title=i)+
    mapTheme()+
    theme(legend.position="bottom")}
do.call(grid.arrange,c(varList,ncol=4,top="Figure 6. Local Morans I statistics, Drug"))

final_net <- final_net %>% 
  mutate(drug.isSig = 
           ifelse(local_morans[,5] <= 0.001, 1, 0)) %>%
  mutate(drug.isSig.dist = 
           nn_function(st_c(st_coid(final_net)),
                       st_c(st_coid(filter(final_net, 
                                           drug.isSig == 1))), 
                       k = 1))

#Correlation test
correlation.long<-st_drop_geometry(final_net)%>%
  dplyr::select(-uniqueID,-cvID,-SNA_NAME)%>%
  gather(Variable,Value,-countDrug)

correlation.cor<-correlation.long%>%
  group_by(Variable)%>%
  summarize(correlation=cor(Value,countDrug,use="complete.obs"))

ggplot(correlation.long,aes(Value,countDrug))+
  geom_point(size=0.1)+
  geom_text(data=correlation.cor,
            aes(label=paste("r=",round(correlation,2))),
            x=-Inf,y=Inf,vjust=1.5,hjust=-.1)+
  geom_smooth(method="lm",se=FALSE,colour="black")+
  facet_wrap(~Variable,ncol=2,scales="free")+
  labs(title="Figure 8. Heroin count as a function of risk factors")+
  plotTheme()

#A histogram of your dependent variable.
ggplot(final_net, aes(countDrug)) + 
  geom_histogram(binwidth = 1) +
  labs(title = "Figure 9. Heroin distribution")

#Cross-validated Poisson Regression
crossValidate <- function(dataset, id, dependentVariable, indVariables) {
  
  allPredictions <- data.frame()
  cvID_list <- unique(dataset[[id]])
  
  for (i in cvID_list) {
    
    thisFold <- i
    cat("This hold out fold is", thisFold, "\n")
    
    fold.train <- filter(dataset, dataset[[id]] != thisFold) %>% as.data.frame() %>% 
      dplyr::select(id, geometry, indVariables, dependentVariable)
    fold.test  <- filter(dataset, dataset[[id]] == thisFold) %>% as.data.frame() %>% 
      dplyr::select(id, geometry, indVariables, dependentVariable)
    
    regression <-
      glm(countDrug ~ ., family = "poisson", 
          data = fold.train %>% 
            dplyr::select(-geometry, -id))
    
    thisPrediction <- 
      mutate(fold.test, Prediction = predict(regression, fold.test, type = "response"))
    
    allPredictions <-
      rbind(allPredictions, thisPrediction)
    
  }
  return(st_sf(allPredictions))
}

reg.vars <- c("junk_cars.nn", "streetLightsOut.nn", "dead_ani.nn", 
              "pot_hole.nn", "graffiti.nn", "vac_building.nn")

reg.ss.vars <- c("junk_cars.nn", "streetLightsOut.nn", "dead_ani.nn", 
                 "pot_hole.nn", "graffiti.nn", "vac_building.nn", "drug.isSig", "drug.isSig.dist")

reg.cv <- crossValidate(
  dataset = final_net,
  id = "cvID",
  dependentVariable = "countDrug",
  indVariables = reg.vars) %>%
  dplyr::select(cvID = cvID, countDrug, Prediction, geometry)

reg.ss.cv <- crossValidate(
  dataset = final_net,
  id = "cvID",
  dependentVariable = "countDrug",
  indVariables = reg.ss.vars) %>%
  dplyr::select(cvID = cvID, countDrug, Prediction, geometry)

## RUN REGRESSIONS
reg.ss.spatialCV <- crossValidate(
  dataset = final_net,
  id = "SNA_NAME",                           
  dependentVariable = "countDrug",
  indVariables = reg.ss.vars) %>%
  dplyr::select(cvID = SNA_NAME, countDrug, Prediction, geometry)

reg.spatialCV <- crossValidate(
  dataset = final_net,
  id = "SNA_NAME",
  dependentVariable = "countDrug",
  indVariables = reg.vars) %>%
  dplyr::select(cvID = SNA_NAME, countDrug, Prediction, geometry)

#Accuracy & Generalzability
reg.summary <- 
  rbind(
    mutate(reg.cv,           Error = Prediction - countDrug,
           Regression = "Random k-fold CV: Just Risk Factors"),
    
    mutate(reg.ss.cv,        Error = Prediction - countDrug,
           Regression = "Random k-fold CV: Spatial Process"),
    
    mutate(reg.spatialCV,    Error = Prediction - countDrug,
           Regression = "Spatial LOGO-CV: Just Risk Factors"),
    
    mutate(reg.ss.spatialCV, Error = Prediction - countDrug,
           Regression = "Spatial LOGO-CV: Spatial Process")) %>%
  st_sf() 

error_by_reg_and_fold <- 
  reg.summary %>%
  group_by(Regression, cvID) %>% 
  summarize(Mean_Error = mean(Prediction - countDrug, na.rm = T),
            MAE = mean(abs(Mean_Error), na.rm = T),
            SD_MAE = mean(abs(Mean_Error), na.rm = T)) %>%
  ungroup()

error_by_reg_and_fold %>%
  ggplot(aes(MAE)) + 
  geom_histogram(bins = 30, colour="black", fill = "#FDE725FF") +
  facet_wrap(~Regression) +  
  geom_vline(xintercept = 0) + scale_x_continuous(breaks = seq(0, 8, by = 1)) + 
  labs(title="Figure 10. Distribution of MAE", subtitle = "k-fold cross validation vs. LOGO-CV",
       x="Mean Absolute Error", y="Count") +
  plotTheme()

# A table of MAE and standard deviation MAE by regression
st_drop_geometry(error_by_reg_and_fold) %>%
  group_by(Regression) %>% 
  summarize(Mean_MAE = round(mean(MAE), 2),
            SD_MAE = round(sd(MAE), 2)) %>%
  kable(caption = "Table 1. MAE and standard deviation MAE by regression") %>%
  kable_styling("striped", full_width = F) %>%
  row_spec(2, color = "black", background = "#FDE725FF") %>%
  row_spec(4, color = "black", background = "#FDE725FF") 

#map of model errors by random k-fold and spatial cross validation
map_name<-unique(error_by_reg_and_fold$Regression)
map_list<-list()

for(i in map_name){
  map_list[[i]] <- 
    ggplot() +
    geom_sf(data = filter(error_by_reg_and_fold, Regression == i), 
            aes(fill=MAE), colour=NA) +
    scale_fill_viridis(name="") +
    labs(title=i) +
    mapTheme()}

do.call(grid.arrange,c(map_list, ncol=4, top="Figure 11. model errors by random k-fold and spatial cross validation"))

#Generalizability by race context
#A table of raw errors by race context for a random k-fold vs. spatial cross validation regression.
tracts16 <- 
  get_acs(geography = "tract", variables = c("B01001_001E","B01001A_001E"), # B01001I_001E is Latino/Hispanic pop
          year = 2016, state=39, county=061, geometry=T) %>%
  st_transform('ESRI:102258')  %>% 
  dplyr::select(variable, estimate, GEOID) %>%
  spread(variable, estimate) %>%
  rename(TotalPop = B01001_001,
         NumberWhites = B01001A_001) %>%
  mutate(percentWhite = NumberWhites / TotalPop,
         raceContext = ifelse(percentWhite > .5, "Majority_White", "Majority_Non_White")) %>%
  .[neighborhoods,]

reg.summary %>% 
  filter(str_detect(Regression, "LOGO")) %>%
  st_centroid() %>%
  st_join(tracts16) %>%
  na.omit() %>%
  st_drop_geometry() %>%
  group_by(Regression, raceContext) %>%
  summarize(mean.Error = mean(Error, na.rm = T)) %>%
  spread(raceContext, mean.Error) %>%
  kable(caption = "Table 2. Mean Error by neighborhood racial context") %>%
  kable_styling("striped", full_width = F) 

#Density vs. prediction
drug_ppp <- as.ppp(st_coordinates(drug16), W = st_bbox(final_net))
drug_KD.1000 <- spatstat.core::density.ppp(drug_ppp, 1000)

drug_KDE_sf <- as.data.frame(drug_KD.1000) %>%
  st_as_sf(coords = c("x", "y"), crs = st_crs(final_net)) %>%
  aggregate(., final_net, mean) %>%
  mutate(label = "Kernel Density",
         Risk_Category = ntile(value, 100),
         Risk_Category = case_when(
           Risk_Category >= 90 ~ "90% to 100%",
           Risk_Category >= 70 & Risk_Category <= 89 ~ "70% to 89%",
           Risk_Category >= 50 & Risk_Category <= 69 ~ "50% to 69%",
           Risk_Category >= 30 & Risk_Category <= 49 ~ "30% to 49%",
           Risk_Category >= 1 & Risk_Category  <= 29 ~ "1% to 29%")) %>%
  cbind(
    aggregate(
      dplyr::select(drug17) %>% mutate(drugCount = 1), ., sum) %>%
      mutate(burgCount = replace_na(drugCount, 0))) %>%
  dplyr::select(label, Risk_Category, drugCount)

drug_risk_sf <-
  reg.ss.spatialCV %>%
  mutate(label = "Risk Predictions",
         Risk_Category = ntile(Prediction, 100),
         Risk_Category = case_when(
           Risk_Category >= 90 ~ "90% to 100%",
           Risk_Category >= 70 & Risk_Category <= 89 ~ "70% to 89%",
           Risk_Category >= 50 & Risk_Category <= 69 ~ "50% to 69%",
           Risk_Category >= 30 & Risk_Category <= 49 ~ "30% to 49%",
           Risk_Category >= 1 & Risk_Category <= 29 ~ "1% to 29%")) %>%
  cbind(
    aggregate(
      dplyr::select(drug17) %>% mutate(drugCount = 1), ., sum) %>%
      mutate(burgCount = replace_na(drugCount, 0))) %>%
  dplyr::select(label,Risk_Category, drugCount)

rbind(drug_KDE_sf, drug_risk_sf) %>%
  na.omit() %>%
  gather(Variable, Value, -label, -Risk_Category, -geometry) %>%
  ggplot() +
  geom_sf(aes(fill = Risk_Category), colour = NA) +
  geom_sf(data = drug17, size = .5, colour = "black") +
  facet_wrap(~label, ) +
  scale_fill_viridis(discrete = TRUE) +
  labs(title="Figure 12. Comparison of Kernel Density and Risk Predictions",
       subtitle="2017 drug risk predictions; 2018 drug") +
  mapTheme(title_size = 14)

#The bar plot making this comparison.
rbind(drug_KDE_sf, drug_risk_sf) %>%
  st_set_geometry(NULL) %>% na.omit() %>%
  gather(Variable, Value, -label, -Risk_Category) %>%
  group_by(label, Risk_Category) %>%
  summarize(countDrug = sum(Value)) %>%
  ungroup() %>%
  group_by(label) %>%
  mutate(Rate_of_test_set_drug = countDrug / sum(countDrug)) %>%
  ggplot(aes(Risk_Category,Rate_of_test_set_drug)) +
  geom_bar(aes(fill=label), position="dodge", stat="identity") +
  scale_fill_viridis(discrete = TRUE) +
  labs(title = "Figure 13. Risk prediction vs. Kernel density, 2017 Heroin Overdose") +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5))

#user case?