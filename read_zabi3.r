setwd("/home/alf/alf_github/matrixcorr_questio")

library(XLConnect)
library(MASS)
library(corrplot)
library(Hmisc)
library(xtable)
library(sjPlot)

wb = loadWorkbook("MATRIX_esp.xls")
df_esp = readWorksheet(wb, sheet = "MATRIX_esp")

wb1 = loadWorkbook("MATRIX_FINAL.xls")
df_final = readWorksheet(wb1, sheet = "TUTTI")
wb_ogg = loadWorkbook("MATRIX_rogg.xls")
df_ogg = readWorksheet(wb_ogg, sheet = "oggettivo")

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

corstars(df_final,"spearman","upper","none")
corstars(df_final,"pearson","upper","none")
capture.output(corstars(df_final,"pearson","upper","html"), file = "matcorr_last_pearson.html")
capture.output(corstars(df_final,"spearman","upper","html"), file = "matcorr_last_spear.html")

corstars(df_esp,"spearman","upper","none")
corstars(df_esp,"pearson","upper","none")
capture.output(corstars(df_esp,"pearson","upper","html"), file = "matcorr_esp_pearson.html")
capture.output(corstars(df_esp,"spearman","upper","html"), file = "matcorr_esp_spear.html")

corstars(df_ogg,"spearman","upper","none")
corstars(df_ogg,"pearson","upper","none")
capture.output(corstars(df_ogg,"pearson","upper","html"), file = "matcorr_ogg_pearson.html")
capture.output(corstars(df_ogg,"spearman","upper","html"), file = "matcorr_ogg_spear.html")

#Model 1 (TUTTI)
#Variabile dipendente: RPScore

#Predittori: Age, Education, Gender, House's tipology, Length of time, #Ground floor, River proximity, Home ownership, Flood experience

#Model 3 (TUTTI)
#Variabile dipendente: Perceived preparedness

#Predittori: Responsibility in preparedness, RPScore, Level of feeling #informed, Training attendance


#Model 2 (SOTTOGRUPPO SI ESPERIENZA)
#Variabile dipendente: RPScore

#Predittori: Age, Education, Gender, House's tipology, Length of time, #Ground floor, River proximity, Home ownership, Flood recency, Flood #damages, Flood magnitude, Fear experienced

###########################################################################################################àààà
# [1] "RPScore"                                  
# [2] "Age"                                      
# [3] "Education"                                
# [4] "Gender"                                   
# [5] "House.s.tipology"                         
# [6] "Length.of.time.at.current.residence"      
# [7] "Ground.floor"                             
# [8] "River.proximity"                          
# [9] "Home.ownership."                          
# [10] "Previous.flood.experience."               
# [11] "Perceived.exposure.to.risks"              
# [12] "Perceived.preparedness."                  
# [13] "Personal.responsibility.in.preparedeness."
# [14] "Level.of.feeling.informed"                
# [15] "Knowledge.of.the.local.emergengy.plan"    
# [16] "Preparedeness.training.attendance"        
# [17] "Trust.in.experts"                         
# [18] "Trust.in.emergency.managers"

# modello totale

tot=glm(RPScore~Age+
          Education+
          Gender+
          House.s.tipology+
          Length.of.time.at.current.residence+
          Ground.floor+
          River.proximity+
          Home.ownership.+
          Previous.flood.experience.
          ,data=df_final)
summary(tot)
sjt.glm(tot,show.aic = TRUE, show.family = TRUE, show.r2 = TRUE,file="model_1.html")
capture.output(xtable(tot), file = "model_1.tex")

###########################################################################################################ààà
#Variabile dipendente: Perceived preparedness

#Predittori: Responsibility in preparedness, RPScore, Level of feeling #informed, Training attendance

tot3=glm(Perceived.preparedness.~
           Personal.responsibility.in.preparedeness.+RPScore+
          Level.of.feeling.informed+
          Preparedeness.training.attendance
        ,data=df_final)

summary(tot3)
sjt.glm(tot3,show.aic = TRUE, show.family = TRUE, show.r2 = TRUE,file="model_3.html")
capture.output(xtable(tot3), file = "model_3.tex")

#############################################################################################################################################################################################################à

#Model 2 (SOTTOGRUPPO SI ESPERIENZA)
#Variabile dipendente: RPScore

#Predittori: Age, Education, Gender, House's tipology, Length of time,
#Ground floor, River proximity, Home ownership, 
#Flood recency, Flood #damages, Flood magnitude, Fear experienced

tot2=glm(RPScore~Age+
          Education+
          Gender+
          House.s.tipology+
          Length.of.time.at.current.residence+
          Ground.floor+
          River.proximity+
          Home.ownership.+
          Flood.recency+
          Flood.damages+
          Flood.magnitude+
          Fear.experienced   
        ,data=df_esp)
summary(tot2)
sjt.glm(tot2,show.aic = TRUE, show.family = TRUE, show.r2 = TRUE,file="model_2.html")
capture.output(xtable(tot2), file = "model_2.tex")




