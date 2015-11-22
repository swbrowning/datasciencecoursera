calculate_mean <- function(){}
        ##load data
            setwd("~/datasciencecoursera/R-class/UCI HAR Dataset")
           
            labels <- read.table('activity_labels.txt')
            features <- read.table("features.txt")
            X_test <- read.table("test/X_test.txt")
            y_test <- read.table("test/y_test.txt")
            subject_test <- read.table("test/subject_test.txt")
            
            X_train <- read.table("train/X_train.txt")
            y_train <- read.table("train/y_train.txt")
            subject_train <- read.table("train/subject_train.txt")
        
        
        ##rbind each 
            
        #create each train set and test set then row bind otherwise loose records
        #as each subject in not in both test and train data sets
        
        #rename columns
        names(subject_train)[names(subject_train)=="V1"] <- "SubjectId"
        names(subject_test)[names(subject_test)=="V1"] <- "SubjectId"
        #change column names for the measures
        colnames(X_train) <- features[,2]
        colnames(X_test) <- features[,2]
            
        # add labels to the factor data
        TrainData <- cbind(subject_train, y_train)
        TrainData  <- merge(TrainData, labels, by="V1")
        names(TrainData)[names(TrainData)=='V2'] <- "ActivityName"
        
        TestData <- cbind(subject_test, y_test)
        TestData <- merge(TestData, labels, by="V1")
        names(TestData)[names(TestData)=='V2'] <- "ActivityName"
        
        #get the average subset
        groupByColumns <- c(colnames(TrainData[,2:3]))
        dots <- lapply(groupByColumns, as.symbol)
        countData <- TrainData %>% group_by_(.dots=dots) %>% summarise(n=n())
        
        groupByColumns <- c(colnames(TestData[,2:3]))
        dots <- lapply(groupByColumns, as.symbol)
        countData <- TestData %>% group_by_(.dots=dots) %>% summarise(n=n())
        
        #remove the id column
        rmColumn <- c("V1")
        TrainData <- TrainData[,!names(TrainData) %in% rmColumn]
        TestData <- TestData[,!names(TestData) %in% rmColumn]
        
        #combine subject factor with measures
        TrainData <- cbind(TrainData, X_train)
        TestData <- cbind(TestData, X_test)
        
        #get only columns that have std and mean
        TrainData <- TrainData[,grepl("SubjectId|ActivityName|std|mean", names(TrainData))]
        
        
        #sort the data
        AllDataSorted <- AllData[order(AllData$SubjectId, AllData$ActivityName),]
        #calculate the mean for each variables by subject and activity
        AllDataMean <- AllData %>% group_by_(.dots=dots) %>% summarise_each(funs(mean))
        
        
        #write the output
        write.table(AllDataMean, "meanData.txt", row.name=false)

}

