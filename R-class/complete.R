complete <- function(directory, id=1:332){
    files <- list.files(directory, full.names=TRUE)
    all_data <- data.frame()
    complete_data <- data.frame()
    
    m <- matrix(NA, nrow=length(id), ncol=2)
    colnames(m) <- c("id", "nobs")
    
    x<-1
    for (i in id){
        all_data <- read.csv(files[i], header=TRUE)
        complete_data <- all_data[complete.cases(all_data),]
        m[x,1] <- i
        m[x,2] <- nrow(complete_data)
        x<- x+1
    }
    data.frame(m)
}