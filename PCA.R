## 1. set the path
setwd("D:/git-ware/feature_selection_on_gene_mutation")
getwd()

## 2. get the data
source("getdata.R")
all_data = getdata()

## 3. get the gene mutation data
nci60GeneMut = all_data[,-1]

## 4. do the pca to the gene mutation data
pca_genes = prcomp(nci60GeneMut, center = TRUE, scale. = TRUE)
names(pca_genes)

dim(pca_genes$x)

biplot(pca_genes, scale = 0)

std_dev <- pca_genes$sdev
pr_var <- std_dev^2
pr_var[1:10]

prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]

plot(prop_varex, xlab = "Principal Component",
     ylab = "Proportion of Variance Explained",
     type = "b")

plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative Proportion of Variance Explained",
     type = "b")

## 5. save the pca features
#add a training set with principal components
data_pca <- data.frame(pca_genes$x)

# we are interested in first 55 PCAs
data_pca <- data_pca[,1:55]

all_data_pca <- cbind(all_data[,1], data_pca)
colnames(all_data_pca)[1] <- "IC50"

write.csv(all_data_pca, file = "selected_features/data_pca.csv")

