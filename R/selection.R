#' Identify the case/control based on ICD9 codes or Phecodes criteria
#'
#' @param pheno_data Phenotype data(sparse matrix)
#' @param demo_data Demographics data
#' @param code_case ICD9 codes or Phecodes used to define cases
#' @param code_control ICD9 codes or Phecodes used for exclusion of controls
#' @param icd9_phe Parameter to define whether the criteria decided by ICD8 codes or Phecodes
#' @examples
#' selection("GRID_ICD9.Aug2017.pivot.RData",c("238.72","238.73","238.74","238.75"),
#'                          c("238.72","238.73","238.74","238.75","238.76","205.10","205.11","205.12",
#'                            "238.71","238.4","205","205.0","205.00","205.01","205.02","205.2","205.20","205.21",
#'                            "205.22","205.8","205.80","205.81","205.82","205.9","205.92","V10.62","40.0",
#'                            "41.04","41.05","41.06","41.07","41.08","996.88","V42.82"),"icd9")
#' @export
#' Will return a dataframe with id and case/control status


selection <- function(pheno_dat,code_case,code_control,icd9_phe){
  #examine whether there is an initial "X" or other label of phecode in phenotype data
  while(is.na(as.integer(substring(colnames(pheno_dat),1,1)[1]))){
    colnames(pheno_dat)=substring(colnames(pheno_dat),2)
  }
  #find individuals satisfy codes
    if (icd9_phe=="icd9"){
      #satisfy at least one code "1" for cases to keep
      case_rows=apply(pheno_dat[,c(code_case)], 1, function(row) "1" %in% row)
      #satisfy at least one code "1" for controls to delete
      control_rows_del=apply(pheno_dat[,c(code_control)], 1, function(row) "1" %in% row)
    }
    else{
      #satisfy at least one code "1" for cases to keep
      case_rows=apply(pheno_dat[,c(code_case)], 1, function(row) "TRUE" %in% row)
      #satisfy at least one code "1" for controls to delete
      control_rows_del=apply(pheno_dat[,c(code_control)], 1, function(row) "TRUE" %in% row)
    }
    caselist=names(which(case_rows))
    controllist=names(which(!control_rows_del))
    totallist=names(c(which(case_rows),which(!control_rows_del)))
    total=pheno_dat[c(which(case_rows),which(!control_rows_del)),]
    genotype=data.frame("id"=totallist,"group"=ifelse(totallist %in% caselist,1,0))
    return("case_control_list"=genotype)
}


