## 1. set the path
setwd("D:/git-ware/feature_selection_on_gene_mutation")
getwd()

## 2. get the data
source("getdata.R")
all_data = getdata()

## 3. mrmr vector column from the Matlab
colnum_number = c(9017, 2100, 106, 5145, 5144, 7027, 382, 9787, 9239, 2618, 2248, 8811, 8372, 6823,
                  9547, 6614, 7301, 2596, 9957, 4587, 301, 3418, 6773, 7678, 422, 322, 5141, 4254,
                  7403, 2614, 6753, 4950, 9018, 4537, 984, 2948, 4954, 205, 10399, 2307, 7439, 9168,
                  4616, 9737, 2577, 10019, 2249, 330, 5259, 1194, 710, 2428, 6783, 6485, 10192, 7132,3258, 6589, 7037, 8032)

## 4. get the gene mutation data
nci60GeneMut = all_data[,-1]

## 5. get the selected columns
j = 1
m <- as.data.frame(nci60GeneMut[,colnum_number[1]])
names_it <- colnames(nci60GeneMut)[colnum_number[1]]
colnames(m)[1] <- names_it
for( i in colnum_number[2:55]) {
  j = j + 1
  m <- cbind(m, nci60GeneMut[,i])
  names_it <- colnames(nci60GeneMut)[i]
  colnames(m)[j] <- names_it
}

## 6. save the data
all_data_mrmr <- cbind(all_data[,1], m)
colnames(all_data_mrmr)[1] <- "IC50"

write.csv(all_data_mrmr, file = "selected_features/data_mrmr.csv")

