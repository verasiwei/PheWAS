#' This is used to prepare and clean phenome and covariates data
#'
#' @param pheno_dat Phenotype data(sparse matrix)
#' @param demo_dat Demographics dataframe with id and other covariates
#' @param number_cases The criteria of keeping the phecodes with at least how many individual have this code
#'
#' @example
#' phewas_prereq(pheno_dat, demo_dat, 50)
#'
#' @export
#' Will return a list of a dataframe of phenome and a dataframe of covariates data(add phenome_burden covariate)


phewas_prereq <- function(pheno_dat, demo_dat, number_cases){
  #examine whether there is an initial "X" or other label of phecode in phenotype data
  while(is.na(as.integer(substring(colnames(pheno_dat),1,1)[1]))){
    colnames(pheno_dat)=substring(colnames(pheno_dat),2)
  }
  #examine id in pheno_dat
  id_pheno=rownames(pheno_dat)
  if(length(id_pheno)==0){stop("There is no id information in phenome data")}
  #examine id column in demo_dat
  id_demo=names(demo_dat)
  if(!("id" %in% id_demo)){stop("There is no id column in demographics data")}

  #calculate phenome burden
  phenome_burden_dat=data.frame("id"=names(rowSums(pheno_dat)),
                                "phenome_burden"=unname(rowSums(pheno_dat)))

  #prepare the covariate dataframe
  pheno_dat=pheno_dat[which(id_pheno %in% demo_dat[,"id"]),]
  demo_dat=demo_dat[which(demo_dat$id %in% rownames(pheno_dat)),]
  demo_dat=merge(demo_dat,phenome_burden_dat,by="id",sort = FALSE)
  demo_dat$phenome_burden=as.integer(demo_dat$phenome_burden)
  rownames(demo_dat)=demo_dat$id
  cov_dat <- demo_dat

  #prepare the phenome dataframe
  id_pheno=rownames(pheno_dat)
  phecode=colnames(pheno_dat)
  pheno_dat=data.frame(as.matrix(pheno_dat))
  colnames(pheno_dat)=phecode
  pheno_dat$id=id_pheno
  phecode_keep=apply(pheno_dat[,-c("id")], 2, function(x) sum(x)>=as.numeric(number_cases))
  phecode_keep=which(phecode_keep=="TRUE")
  pheno_dat =pheno_dat[,c(phecode_keep,"id")]

  return(list(pheno_dat=pheno_dat,cov_dat=cov_dat))

}



