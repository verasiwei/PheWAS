#' Add the exclustion criteria to Phecode Table
#' Add NA to exclude those who cannot be regarded as controls for each phecode
#'
#' @param pheno_data Phenotype data(sparse matrix)
#' @examples
#' add_exclusion(pheno_dat)


add_exclusion <- function(pheno_dat){
  #examine whether there is an initial "X" or other label of phecode in phenotype data
  if(is.na(as.integer(substring(colnames(pheno_dat),1,1)[1]))){
    #create the list of exclude codes for each phecode
    phecode_name=substring(colnames(pheno_dat),2)
    excludecodeslist=lapply(phecode_name, function(x) mapPhecodesToExclusions(x)[,2])
  } else {
    phecode_name=colnames(pheno_dat)
    excludecodeslist=lapply(colnames(pheno_dat), function(x) mapPhecodesToExclusions(x)[,2])
  }

  #for each phecode, NA those subjects who satisfy at least one code in exclusion codes
  for (col in 1:ncol(pheno_dat)) {
    if(nrow(excludecodeslist[[col]])!=0){
      if(nrow(excludecodeslist[[col]])!=1){
        NAsubjects=apply(pheno_dat[which(pheno_dat[,col]!="TRUE"),phecode_name %in% excludecodeslist[[col]]$exclusion],
                         1,function(row) "TRUE" %in% row)
        NAsubjects=names(which(NAsubjects))
        while (!identical(NAsubjects,character(0))) {
          pheno_dat[which(rownames(pheno_dat) %in% NAsubjects),col]=NA
        }

      } else {
        NAsubjects=apply(data.frame(pheno_dat[which(pheno_dat[,col]!="TRUE"),phecode_name %in% excludecodeslist[[col]]$exclusion]),
                         1,function(row) "TRUE" %in% row)
        NAsubjects=names(which(NAsubjects))
        while (!identical(NAsubjects,character(0))) {
          pheno_dat[which(rownames(pheno_dat) %in% NAsubjects),col]=NA
        }
      }
    }
    print(col)
  }

  return(pheno_dat_exclusion=pheno_dat)
}


