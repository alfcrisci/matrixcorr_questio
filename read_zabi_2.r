# to install sjPlot
# install.packages("devtools")
# devtools::install_github("strengejacke/strengejacke")

setwd("/home/alf/alf_github/matrixcorr_questio")

library(XLConnect)
library(MASS)
library(corrplot)
library(Hmisc)
library(xtable)
library(sjPlot)

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

wb = loadWorkbook("mat_zab_new.xlsx")
df = readWorksheet(wb, sheet = "Foglio1")
df$ID=NULL
saveRDS(df,"mat_zab2.rds")
#########################################################################################################

mat_zab2=readRDS("mat_zab2.rds")

last_mat_zab=mat_zab2

###########################################################################################################àààà

corstars(mat_zab2,"spearman","upper","none")
corstars(last_mat_zab,"pearson","upper","none")
capture.output(corstars(last_mat_zab,"spearman","upper","latex"), file = "matcorr_last_spear.tex")
capture.output(corstars(last_mat_zab,"spearman","upper","html"), file = "matcorr_last_spear.html")



###########################################################################################################àààà

# modello totale

tot=glm(SCORE_VG~.,data=last_mat_zab)
summary(tot)
sjt.glm(tot,show.aic = TRUE, show.family = TRUE, show.r2 = TRUE,file="tot_last.html")
capture.output(xtable(tot), file = "tot_last.tex")


mod1_zab=tot=glm(Q31~Q29+SCORE_VG+Q33.N+Q41N,data=last_mat_zab)
summary(mod1_zab)
sjt.glm(mod1_zab,show.aic = TRUE, show.family = TRUE, show.r2 = TRUE,file="mod1_zab.html")
capture.output(xtable(mod1_zab), file = "mod1_zab.tex")

mod2_zab=tot=glm(Q32~Q29+SCORE_VG+Q33.N+Q41N,data=last_mat_zab)
summary(mod2_zab)
sjt.glm(mod2_zab,show.aic = TRUE, show.family = TRUE, show.r2 = TRUE,file="mod2_zab.html")
capture.output(xtable(mod2_zab), file = "mod2_zab.tex")





######################################################################################################
# [1] http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package


