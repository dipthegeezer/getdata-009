library(dplyr)


load_set <- function(path, type){
    X <- read.table(file.path(path, type, paste("X_", type, ".txt", sep = "")), sep = "" , header = F, stringsAsFactors = F)
    Y <- read.table(file.path(path, type, paste("y_", type, ".txt", sep = "")), sep = "" , header = F, stringsAsFactors = F)
    subject <- read.table(file.path(path, type, paste("subject_", type, ".txt", sep = "")), sep = "" , header = F, stringsAsFactors = F)
    data <- cbind(cbind(subject,Y), X)
    rm(X, Y, subject)
    return(data)
}


load_files <- function(path){
    headers <- read.table(file.path(path, "features.txt"), sep = "" , header = F, stringsAsFactors = F)
    headers <- rbind(c("subject"),rbind(c("activity"), headers))
    headers <- headers[,c("V2")]
    
    activity <- read.table(file.path(path, "activity_labels.txt"), sep = "" , header = F, stringsAsFactors = F)
    colnames(activity) <- c("activity", "activity_name") 
    
    test <- load_set(path, "test")
    colnames(test) <- headers

    train <- load_set(path, "train")
    colnames(train) <- headers
    
    data <- rbind(test, train)
    data <- data[grep("subject|activity|mean\\(\\)|std\\(\\)", headers, perl=TRUE, value=FALSE)]
    data <- inner_join(activity,data)
    data <- select(data, -activity)
    final <- group_by(data, activity_name, subject) %>% summarise_each(funs(mean))
    write.table(final, row.names = FALSE, file = "final.txt")
    rm(test, train, headers, activity, data, final)
}





