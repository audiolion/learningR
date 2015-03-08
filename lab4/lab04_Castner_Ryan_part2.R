#
# Script tests out the functions defined in lab04_Castner_Ryan.R and benchmarks
# them to determine which method is the fastest for extracting data from a dir.
#
# author: Ryan Castner rrc9704@rit.edu

# import functions
source("~/lab04_Castner_Ryan.R")

# benchmark the three methods
start1 <- proc.time()
df1 <- readFolderDF1("~/helaData")
end1 <- proc.time()

start2 <- proc.time()
df2 <- readFolderDF2("~/helaData")
end2 <- proc.time()

start3 <- proc.time()
df3 <- readFolderList("~/helaData")
end3 <- proc.time()

# print time results
print(end1[3]-start1[3])
print(end2[3]-start2[3])
print(end3[3]-start3[3])

# the list method was by far the slowest. I believe a lot of this to be
# because the list has no memory space allocated for it, therefore the
# list needs to be completely copied every time you append another vector
# to it and thus it becomes a very costly operation. 
# Data frames likely have more specific optimizations to reduce this kind
# of overhead and we are initializing the dataframes with given column or
# row lengths so that these optimizations can be made. I could see it be
# something like a row in a dataframe that needs to represent some new
# vector could simply have a pointer reassigned to the new vector in mem
# instead of copying the whole thing over in memory