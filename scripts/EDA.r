### Analyze results

## Libraries
library(tidyverse)

## Look at the model results
models <- list.files("out//models//", full.names = T)

listModels <- lapply(1:length(models), function(i)
  read.csv(models[i])
)
allModels <- do.call(rbind, listModels)

## Look at predictions for cities
futureclimate <- list.files("out//cityPredict//", full.names = T, pattern="Future")

listFuture <- lapply(1:length(futureclimate), function(i)
  read.csv(futureclimate[i])
)
allFuture <- do.call(rbind, listFuture)

currentclimate <- list.files("out//cityPredict//", full.names = T, pattern="Current")

listCurrent <- lapply(1:length(currentclimate), function(i)
  read.csv(currentclimate[i])
)
allCurrent <- do.call(rbind, listCurrent)

ggplot(allCurrent, aes(x=species, y= meanProb)) + geom_boxplot() +coord_flip()

## compare change
allClimate <- allCurrent %>% dplyr::select(City, species, currentProb = meanProb) %>% left_join(allFuture) %>% 
  mutate(changeProb = log(meanProb / currentProb))

## Differences in species
ggplot(allClimate, aes(x=species, y= changeProb, fill=Year)) + geom_boxplot() + coord_flip() +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) + theme_classic() + ylab("Change in Predicted Occurrence") +
  geom_hline(yintercept=0, lty=2)



## Differences in cities
ggplot(allClimate, aes(x=City, y= changeProb, fill=Year)) + geom_boxplot() + coord_flip() +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) + theme_classic() + ylab("Change in Predicted Occurrence") +
  geom_hline(yintercept=0, lty=2)