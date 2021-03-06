corr <- function(directory, threshold=0){
    files <- list.files(directory, full.names=TRUE)
    
    files_with_complete_data <- complete(directory)
    files_meets_threashold_req <- files_with_complete_data[which(files_with_complete_data$nobs > threshold),]
    
    cor_set <- c()
    for (i in as.vector(files_meets_threashold_req$id)){
        #all_valid_data <- rbind(all_valid_data, read.csv(files[i], header=TRUE))
        temp_data <- read.csv(files[i], header=TRUE)
        sulfates <- temp_data[complete.cases(temp_data),][,2]
        nitrates <- temp_data[complete.cases(temp_data),][,3]
        cor_set <- c(cor_set, cor(sulfates, nitrates))
    }
    #sulfates <- all_valid_data[complete.cases(all_valid_data),][,2]
    #nitrates <- all_valid_data[complete.cases(all_valid_data),][,3] 
   cor_set
   
}