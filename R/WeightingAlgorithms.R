# Weighting Algorithms.R


#' @description A function to create weights via the AHP Method devolped by Prof. Thomas L. Staay. 
#' @title AHP_Weight  
#' @author Geoffrey Schuette \email{geoschuette@yahoo.com}
#' @param data, dataframe is a nxn+1 data frame where n is the number of attributes being compared. The first column is the name of the attributes begin compared and have class of character. The rest of the columns should be in the numeric class. 
#' @param name, Name of what what you are doing AHP on for displaying Consistentcy Ratio
#' @param number, Number attributes being compared by the AHP process, should be between 3 and 10. 
#' @keywords weighting, AHP, Analytical Heirarchy Process
#' @return Data frame
#' @examples 
#' @details 
#' @export
AHP_Weight <- function(data, name, number){
  #data should be nXn+1 data frame where the first column of the data frame is the names of the alternatives.

n = number
Comparison_Matrix = data
c_m_copy = c_m_wsv = as.matrix(Comparison_Matrix[,-1])
sum_c_m= colSums(c_m_copy)

for (i in 1:nrow(c_m_copy)){
  c_m_copy[,i] = c_m_copy[,i]/sum_c_m[i] 
}

weight = rowMeans(c_m_copy)
#Weighted Sum Vector(WSV)
WSV = c_m_wsv%*%weight

# Average of WSV divided by Weight
average = mean(WSV/weight)
#Consistency Index(CI)
CI = (average-nrow(c_m_copy))/(nrow(c_m_copy)-1)
#Random Index's for 3 or 4 Comparisons
RI3 = 0.58
RI4 = 0.90 
RI5 = 1.12
RI6 = 1.24
RI7 = 1.32
RI8 = 1.41
RI9 = 1.46
RI10 = 1.49
RI = c(0,0,RI3,RI4,RI5,RI6,RI7,RI8,RI9,RI10)
#Consistency Ration(CR)
if (n %in% c(3:10)) {
  CR = CI/RI[n] 
} else {
  print("Parameters Out of Bounds") 
}

print(paste("Consistency Ratio for",name, "is",CR))
weight <- cbind(Comparison_Matrix[,1],weight) 
colnames(weight)[1] <- "Attributes"

return(weight)

} 


#' @description A function to create weights via the Rank Order Centroid Method
#' @title ROC_Weight 
#' @author Geoffrey Schuette \email{geoschuette@yahoo.com}
#' @param data, dataframe is a nx2 data frame where n is the number of attributes being compared. The first column is the name of attributes being compared.
#' @param number, Number attributes being compared by the Rank Ordered Centroid Method. 
#' @keywords weighting, ROC, Rank-Ordered Centroid Method
#' @return Data frame
#' @examples 
#' @details 
#' @export

ROC_Weight <- function(data, number){
  # data should be a nX2 data frame with the first column being the attributes being compare 
  # and the second column being the associated rank. 
  
  n = number 
  ROC <- data.frame(Names = character(length = n),Ranks = numeric(length = n))
  ROC[,1] = data[,1]
  ROC[,2] = data[,2]
  Attr_Num = c(1:n)
  ROC = cbind(ROC,Attr_Num)
  ROC = ROC[order(ROC[,2]),]
  ROC[,2] <-as.numeric(ROC[,2])
  weight <- c()
  unit_vec = rep(c(1),times= n)
  for (i in 1:n){
    weight[i]=(1/n)*sum(unit_vec[i:n]/ROC[i:n,2]) 
  }
  
  ROC = cbind(ROC,weight)
  ROC = ROC[order(ROC[,3]),]
  ROC[,3] <- NULL
  return(ROC)
  
} 
