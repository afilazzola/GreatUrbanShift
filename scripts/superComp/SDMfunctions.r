#### SDM functions
## A list of functions to facilitate processing SDM 

### Function to use Kfold partioninig into 10 groups
kfoldPartionData <- function(occurrences, absences){
fold.p <- kfold(occurrences, k=10)
occtest.p <- occurrences[fold.p == 1, ]
occtrain.p <- occurrences[fold.p != 1, ]
fold.a <- kfold(absences, k=10)
occtest.a <- absences[fold.a == 1, ]
occtrain.a <- absences[fold.a != 1, ]

returnList <- list(occtest.p, occtrain.p, occtest.a, occtrain.a)
names(returnList) <- c("testingPresence","trainingPresence","testingAbsence","trainingAbsence")
return(returnList)
}


### find co-linear variables
FindColinearVariables <- function(occurrences, absences, climate){

pres <- extract(climate, occurrences)
abs <- extract(climate, absences)
allClim <- rbind(pres, abs) %>% data.frame()

## Check for covariance
colin <- usdm::vifcor(allClim[,-ncol(allClim)])
selectVars <-  colin@results$Variables
return(selectVars)
}