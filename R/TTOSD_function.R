#' Run TTO Signal detection
#'
#' @param dataset Input dataset containing at least a column for the product, a column for the event and a column for the TTO (in days)
#' @param Product_var Name of the field containing the Product identifier
#' @param Product_val List of products for which the TTO Signal detection will run (e.g. c("ZOSTER (SHINGRIX)","ZOSTER (ZOSTAVAX)"))
#' @param Event_var Name of the field containing the Event identifier
#' @param TTO_var Name of the field containing the TTO information (in days). Needs to be numeric
#' @param interval Length (in days) of the interval for the TTO Signal detection
#' @param min Minimal number of rows with non-missing TTO information in the user-defined interval for performing TTO Signal Detection
#'
#' @return
#' @export
#'
#' @examples

# https://github.com/mvuorre/exampleRPackage followed for creating the package

TTOSD_function<-function(dataset,
                         Product_var,
                         Product_val,
                         Event_var,
                         TTO_var,
                         interval,
                         min){

  library(sqldf)

  vaers_TTOSD<-dataset[dataset[TTO_var]>=0 &
                       dataset[TTO_var]<=interval,]

  # Rename the variables so that I can reuse the code - Should add check to make sure the new names do not exist
  colnames(vaers_TTOSD)[which(colnames(vaers_TTOSD)==Product_var)]<-"var1"
  colnames(vaers_TTOSD)[which(colnames(vaers_TTOSD)==Event_var)]<-"var2"
  colnames(vaers_TTOSD)[which(colnames(vaers_TTOSD)==TTO_var)]<-"NUMDAYS"

  # Get the list of distinct PTs with at least inpu$min non-missing TTO values within the inpu$interval specified

  for (j in 1:length(Product_val)){
    str=paste('SELECT distinct var2
              FROM vaers_TTOSD
              WHERE (var1 == "',Product_val[j],'")
              GROUP BY var2
              HAVING count(*) >= ',min, sep="")
    LstEv <- sqldf(str)

    LstEv2 = as.character(LstEv[,1])
    LstEv$var1<-Product_val[j]

    for (i in 1:length(LstEv2)){
      y<-try(ks.test(x=vaers_TTOSD$NUMDAYS[(vaers_TTOSD$var1 == Product_val[j]) &
                                           (vaers_TTOSD$var2 == LstEv2[i])],
                     y=vaers_TTOSD$NUMDAYS[(vaers_TTOSD$var1 != Product_val[j]) &
                                           (vaers_TTOSD$var2 == LstEv2[i])],
                     alternative="two.sided",
                     exact=FALSE),silent=TRUE)
      if (!is.atomic(y)){
        LstEv$Pval[LstEv$var2==LstEv2[i]]<-as.numeric(y$p.value)
        LstEv$Stat[LstEv$var2==LstEv2[i]]<-as.numeric(y$statistic)
      }
    }
    if (j==1){
      TTO<-LstEv
    }
    else{
      TTO<-rbind(TTO,LstEv)
    }
  }

colnames(TTO)[which(colnames(TTO)=="var1")]<-Product_var
colnames(TTO)[which(colnames(TTO)=="var2")]<-Event_var
colnames(TTO)[which(colnames(TTO)=="NUMDAYS")]<-TTO_var

TTO

}

#vaers<-read.csv(paste("/home/mint/R/history/2020_05_14/vaers_all.csv",sep=""),header=T)

#vaers_TTOSD<-TTOSD_function(dataset = vaers[,
#                                            c("VAERS_ID","VAX_NAME","SYMPTOM","NUMDAYS")],
#                            Product_var = "VAX_NAME",
#                            Product_val = c("ZOSTER (SHINGRIX)"),
#                            Event_var="SYMPTOM",
#                            TTO_var='NUMDAYS',
#                            interval=30)

#check<-sqldf('SELECT distinct VAX_NAME, count(distinct VAERS_ID) as N FROM vaers GROUP BY VAX_NAME ORDER BY N desc')
