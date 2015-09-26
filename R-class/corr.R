corr <- function(directory, threshold=0){
    files <- list.files(directory, full.names=TRUE)
    
    files_with_complete_data <- complete(directory)
    files_meets_threashold_req <- files_with_complete_data[which(files_with_complete_data$nobs > threshold),]
    
    #print(as.vector(files_meets_threashold_req$id))
    
    cor_set <- c()
    for (i in as.vector(files_meets_threashold_req$id)){
        temp_data <- read.csv(files[i], header=TRUE)
        #sulfates <- temp_data[,2]
        #print(class(sulfates))
        sulfates <- temp_data[complete.cases(temp_data),][,2]
        #sulfates <- sulfates[sulfates != 0]
      
        nitrates <- temp_data[complete.cases(temp_data),][,3]
        #nitrates <- nitrates[nitrates != 0]
        #nitrates <- temp_data[,3]
        cor_val <- cor(sulfates, nitrates)
        #print(cor_val)
        cor_set <- c(cor_set, cor_val)
    }
   cor_set
}