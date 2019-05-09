#' This is used to arrange the result from phewas function
#'
#' @param result_dat a dataframe from the result of phewas function
#' @param phecode_description a dataframe of the phecode description
#'
#' @example
#' result(result_dat,phecode_description)
#'
#' @export
#' Will return a dataframe of a clear result

result <- function(result_dat, phecode_description){
  phecode_description=phecode_description[which(!duplicated(phecode_description$PheCode)),]
  colnames(result_dat)[1]="PheCode"
  results_description=merge(result_dat,phecode_description,by="PheCode",sort = FALSE)
  results_description=results_description[,c(1,2,19,10,11,4,5,6,7)]
  colnames(results_description)=c("phenotype","variable","Descripton",
                                  "n_cases","n_controls","beta","SE","OR","p")
  results_description=results_description[order(results_description$p),]

  return(results_description=results_description)

}

