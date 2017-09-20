# install.packages("corrplot")
# install.packages("JWileymisc")

setwd("/home/alf/Scrivania/lav_zabi")


library(XLConnect)
library(MASS)
library(corrplot)
library(Hmisc)
library(xtable)
#library(JWileymisc)
library(sjt)

#########################################################################################################
# x is a matrix containing the data
# method : correlation method. "pearson"" or "spearman"" is supported
# removeTriangle : remove upper or lower triangle
# results :  if "html" or "latex"
  # the results will be displayed in html or latex format

corstars <-function(x, method=c("pearson", "spearman"), removeTriangle=c("upper", "lower"),
                     result=c("none", "html", "latex")){
    #Compute correlation matrix
    require(Hmisc)
    x <- as.matrix(x)
    correlation_matrix<-rcorr(x, type=method[1])
    R <- correlation_matrix$r # Matrix of correlation coeficients
    p <- correlation_matrix$P # Matrix of p-value 
    
    ## Define notions for significance levels; spacing is important.
    mystars <- ifelse(p < .0001, "****", ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    "))))
    
    ## trunctuate the correlation matrix to two decimal
    R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
    
    ## build a new matrix that includes the correlations with their apropriate stars
    Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
    diag(Rnew) <- paste(diag(R), " ", sep="")
    rownames(Rnew) <- colnames(x)
    colnames(Rnew) <- paste(colnames(x), "", sep="")
    
    ## remove upper triangle of correlation matrix
    if(removeTriangle[1]=="upper"){
      Rnew <- as.matrix(Rnew)
      Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove lower triangle of correlation matrix
    else if(removeTriangle[1]=="lower"){
      Rnew <- as.matrix(Rnew)
      Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
      Rnew <- as.data.frame(Rnew)
    }
    
    ## remove last column and return the correlation matrix
    Rnew <- cbind(Rnew[1:length(Rnew)-1])
    if (result[1]=="none") return(Rnew)
    else{
      if(result[1]=="html") print(xtable(Rnew), type="html")
      else print(xtable(Rnew), type="latex") 
    }
} 
#########################################################################################################

#wb = loadWorkbook("mat_zab.xlsx")
#df = readWorksheet(wb, sheet = "Foglio1")
#mat_zab$Score_VG_TXT=NULL
#mat_zab$ID=NULL
#saveRDS(mat_zab,"mat_zab.rds")
#########################################################################################################

mat_zab=readRDS("mat_zab.rds")
NA_risk_obj=which(is.na(mat_zab$RISCHIO.OGGETTIVO))
nozero_NA_Q11=which(mat_zab$Q11>0)

esp_mat_zab=mat_zab[nozero_NA_Q11,c(2:10,12:length(names(mat_zab)))]
noesp_mat_zab=mat_zab[,2:11]

###########################################################################################################àààà

corstars(noesp_mat_zab,"spearman","upper","none")
corstars(noesp_mat_zab,"pearson","upper","none")
capture.output(corstars(noesp_mat_zab,"spearman","upper","latex"), file = "matcorr_noesp.tex")
capture.output(corstars(noesp_mat_zab,"spearman","upper","html"), file = "matcorr_noesp.html")


corstars(esp_mat_zab,"spearman","upper","none")
corstars(esp_mat_zab,"pearson","upper","none")
capture.output(corstars(esp_mat_zab,"spearman","upper","latex"), file = "matcorr_esp.tex")
capture.output(corstars(esp_mat_zab,"spearman","upper","html"), file = "matcorr_esp.html")

###########################################################################################################àààà

# modello totale

tot=glm(SCORE_VG~.,data=noesp_mat_zab)
summary(tot)
sjt.glm(tot,show.aic = TRUE, show.family = TRUE, show.r2 = TRUE,file="tot_noesp.html")
capture.output(xtable(tot), file = "tot_model_noexp.tex")

# modello anagrafico
 
anagrafico=glm(SCORE_VG~Q1+Q2+Q3,data=noesp_mat_zab)
summary(anagrafico)
sjt.glm(anagrafico,show.aic = TRUE, show.family = TRUE, show.r2 = TRUE,file="anagrafico_noesp.html")
capture.output(xtable(anagrafico), file = "anagrafico_model_noexp.tex")


casa=glm(SCORE_VG~Q6+Q7+Q8N+Q9N+Q10,data=noesp_mat_zab)
summary(casa)
sjt.glm(casa,show.aic = TRUE, show.family = TRUE, show.r2 = TRUE,file="casa_noesp.html")
capture.output(xtable(casa), file = "casa_model_noexp.tex")


esperienza=glm(SCORE_VG~Q44+Q13N+Q14A+Q15N,data=esp_mat_zab)
summary(esperienza)
sjt.glm(esperienza,show.aic = TRUE, show.family = TRUE, show.r2 = TRUE,file="esperienza_noesp.html")
capture.output(xtable(esperienza), file = "esperienza_model_noexp.tex")

######################################################################################################
# [1] http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package


