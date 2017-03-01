
getdata <- function() {
  # load the library
  library(rcellminer)
  ## Get normalized (Z-score) NCI-60 gene mutation and drug activity data.
  # get the gene mutation
  nci60GeneMut <- getAllFeatureData(rcellminerData::molData)[["mut"]]
  # transpose
  nci60GeneMut <- t(nci60GeneMut)
  dim(nci60GeneMut)
  #[1] 60    10664
  
  # getFeatureAnnot() returns a named list of data frames with annotation data 
  drugAnnot <- getFeatureAnnot(rcellminerData::drugData)[["drug"]]
  dim(drugAnnot)
  # [1] 20861     8
  
  # get annotation data of drugs which fda approved
  fdaDrugAnnot <- drugAnnot[which(drugAnnot$FDA_STATUS == "FDA approved"), ]
  dim(fdaDrugAnnot)
  # [1] 158   8
  
  # get the drug act which fda approved
  nci60FdaDrugAct <- getDrugActivityData(nscSet = fdaDrugAnnot$NSC)
  dim(nci60FdaDrugAct)
  # [1] 158  60
  # drugs x patients
  
  nci60FdaDrugAct <- t(nci60FdaDrugAct)
  
  # all.equal(rownames(nci60GeneMut), rownames(nci60FdaDrugAct))
  
  ## combine the mutation data and drug sensitivity 
  all_data <- cbind(nci60FdaDrugAct[,2], nci60GeneMut)
  colnames(all_data)[1] <- "IC50"
  
  return (all_data)
}