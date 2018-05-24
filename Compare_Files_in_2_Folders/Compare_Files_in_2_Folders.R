# INIT

library(tools)

# Folder names

folder_original <- folder1
folder_target <- folder2

# get list of files 

list_files_original <- list.files(folder_original, all.files = TRUE, recursive = TRUE)
list_files_target <- list.files(folder_target, all.files = TRUE, recursive = TRUE)

# Compare list of files

table(
    unlist(
        lapply(
            list_files_original, 
            function(x) {file.exists(paste(folder_target, x, sep = "/"))})))


# Compare MD5 hash

table(
    unlist(
    lapply(
        list_files_original, 
           function(x) {
               md5sum(paste(folder_original, x, sep = "/")) == md5sum(paste(folder_target, x, sep = "/"))})))
