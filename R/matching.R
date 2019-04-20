#' Propensity score matching to get matched controls
#'
#' @param pheno_dat Phenotype data(sparse matrix)
#' @param demo_dat Demographics dataframe with id and other covariates
#' @param status_dat A dataframe with id and case/control status
#' @param ratio 1:ratio matched controls
#'
#' @example
#' matching(pheno_dat, demo_dat, status_dat, 10)
#'
#' @export
#' Will return a data frame with matched id and case/control status


matching <- function(pheno_dat, demo_dat, status_dat,ratio){
  #examine whether there is an initial "X" or other label of phecode in phenotype data
  while(is.na(as.integer(substring(colnames(pheno_dat),1,1)[1]))){
    colnames(pheno_dat)=substring(colnames(pheno_dat),2)
  }
  #examine id column in status_dat
  id_status=names(status_dat)
  if(!("id" %in% id_status)){stop("There is no column of id in status data")}
  #examine id in pheno_dat
  id_pheno=rownames(pheno_dat)
  if(length(id_pheno)==0){stop("There is no id information in phenome data")}
  #examine id column in demo_dat
  id_demo=names(demo_dat)
  if(!("id" %in% id_demo)){stop("There is no id column in demographics data")}

  id=intersect(id_status,id_demo)
  covariates=demo_dat[,which(!(colnames(demo_dat) %in% id))]

  #calculate phenome burden
  phenome_burden_dat=data.frame("id"=names(rowSums(pheno_dat)),
                                "phenome_burden"=unname(rowSums(pheno_dat)))

  pheno_dat=pheno_dat[which(id_pheno %in% status_dat[,"id"]),]
  status_dat=status_dat[which(status_dat[,"id"] %in% id_pheno),]
  demo_dat=merge(demo_dat,status_dat,by="id",sort=FALSE)
  colnames(status_dat)[2]="group"
  demo_dat=join(demo_dat,status_dat,type="left",by="id",match="all")
  demo_dat=merge(demo_dat,phenome_burden_dat,by="id",sort = FALSE)
  demo_dat$phenome_burden=as.integer(demo_dat$phenome_burden)
  rownames(demo_dat)=demo_dat$id

  message("begin matching")
  start=Sys.time()
  ratio=as.numeric(ratio)
  cov=colnames(covariates)
  formula.string=paste0("group ~", paste(c(cov),collapse = "+"))
  my.formula = as.formula(formula.string)

  match_dat <- matchit(my.formula, data=demo_dat[,c(cov,"group",)],
                    method = "nearest",
                    ratio=ratio,
                    caliper = .10,
                    distance="logit")
  end=Sys.time()
  end-start
  message("finished matching")

  match_id=subset(match_dat$match.matrix,subset = !apply(is.na(match_dat$match.matrix),1,all))
  caselist=row.names(match_id)
  controllist=c()
  for (i in 1:ratio){
    controllist=unname(c(controllist,match_id[,i]))
  }

  controllist=controllist[which(!is.na(controllist))]
  totallist=c(caselist,controllist)
  case_control_list=data.frame("id"=totallist,"group"=ifelse(totallist %in% caselist,1,0))
  return(matched_status=case_control_list)

}




