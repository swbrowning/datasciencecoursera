pollutantmean <- function (directory, pollutant, id = 1:332){
    files <- list.files(directory, full.names=TRUE)
    all_data <- data.frame()
    for (i in id){
        all_data <- rbind(all_data, read.csv(files[i], header=TRUE))
    }
    if ( pollutant == 'sulfate'){
        valid_data <- all_data[!is.na(all_data$sulfate),]
        mean(valid_data$sulfate)
    }
    else if ( pollutant == 'nitrate'){
        valid_data <- all_data[!is.na(all_data$nitrate),]
        mean(valid_data$nitrate)
    }
}
