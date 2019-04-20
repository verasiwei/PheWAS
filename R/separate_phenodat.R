#' Separate large phenome table into chunks by phecodes
#'
#' @param pheno_dat Phenotype data(sparse matrix)
#' @param chunk_number Number of chunks
#' @param seed The seed for reproducibility
#'
#' @example
#' separate_phenome(pheno_dat, 10, 123)
#'
#' @export
#' Will return a list of separate Phenotype Tables


separate_phenome <- function(pheno_dat, chunk_number, seed){
  set.seed(seed)
  splits = sample(1:as.integer(chunk_number),ncol(pheno_dat),replace = T)

  separate_dat = list()
  for (i in as.integer(names(table(splits)))) {
    separate_dat[[i]] = pheno_dat[,which(splits == i)]
  }

  return(separate_dat=separate_dat)

}


