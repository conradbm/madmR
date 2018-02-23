#Sensitivity.R

#' 
#' 
#' Import libraries
#' 
#' 
#library(ggplot2)
#source("Globals/DB_Globals.R")
#source("Functions/Algorithms.R")



#' @description A function to perform the sensitivity on a data.frame in read data matrix format.
#' The spirit behind this function is to interpretively take in the given parameters
#' of a specified read data matrix style data.frame and populate a database.
#' From this database, the user can pivot, plot, and reshape for appropriate analysis
#' in deciding what different weighting schemes might have looked like for the decision maker.
#' This function is meant to scale between various methods as well as attributes.
#' Custom split separation is available for each sensitivity step.
#' Custom step sizes are also available to allow the user to do multiple sensitivity
#' runs and determine what fidelity the sensitivty is needed to make an impact on the 
#' overarching decision. 
#' @title sensitivity
#' @author Blake Conrad \email{bmc.cs@outlook.com}
#' @param data, read data matrix(data.frame), data.frame in data matrix format.
#' @param step, automatically set to 0.01 is the steps you want an attribute to climb up or down.
#' @param attr, automatically set to c() will trigger a full sensitivity accross all attributes.
#'  If a list of valid attribuets are provided, the sensitivity will only trigger these attributes
#'  in the sensitivity and reflect them only in the returned database.
#' @param window, double or vector or list, a sigle value represents how low to go for a weight on an attribute being studied, i.e weight-window. likewise for how high, weight+window. for the vector
#' and list situation it is nothing more than a substitute for the lower and upper bound of default, i.e., (step, 1-step)
#'  next will be assessed.
#' @param splitPercentages, automatically set to uniform. This means as you incrementally increase and
#' decrease an attributes weight value, the percetnage given to each other attribute is equal. An example would
#' be with 3 attributes, we toggle attribute2 down from 0.3 to 0.2. So we lost 0.1 from attribute2, so we will take
#' 0.1/(3-1) to give us 0.1/2==0.05, therefore attribuet1 and attribute2 will increase by 0.05 when attribute2 goes down 0.1.
#' The generalization to this is step/(1-N) in the algorithm. 
#' @param verbose, default to FALSE. As with most linux command line applications, this will give you a highly detailed picture of what type of iterations are taking place under the hood.
#' @param algs, vector to specify the algorithms with which to use,
#' @param algParams, list of named vectors,

#' @param plotLabels, boolean to specify how you would like the ggplot2 object to be returned (points or labels).

#' @import ggplot2
#' @return named list
#' 
#' @examples 
#' 
#' ## load up some test data
#' data(maut_dm)
#' data(topsis_dm)
#' 
#' ## Get a baseline for ranking
#' TOPSIS(maut_dm)$Results
#' MAUT(maut_dm)$Results
#' 
#' ## Run Sensitivity (I.e., step defaults, algs defaults, algParams defaults, attr 
#' #                   defaults. All of which can be tweaked.)
#' FinalDB <- sensitivity(data=maut_dm)
#' FinalDB$Results          # Output from all the runs
#' FinalDB$EdgeCasesResults # Show all of the cases in Final_DB$Results when rank changed
#' FinalDB$Plot
#' 
#' 
#' ## Take another look at sensitivity results
#' FinalDB <- sensitivity(data=topsis_dm,
#'                        step=0.1,
#'                        algs=c("TOPSIS","MAUT"),
#'                        algParams=list(MAUT=list(scales=list("linear",
#'                                                             "linear",
#'                                                             "linear",
#'                                                             "exponential",
#'                                                             "exponential",
#'                                                             "exponential",
#'                                                             "linear"))))
#' FinalDB$Plot
#' @export

sensitivity <- function(data=c(), 
                        algs=c("TOPSIS","MAUT"),
                        algParams=c(),
                        step=0.01,
                        window=c(), #TODO
                        attr=c(), 
                        splitPercentages="uniform", #TODO
                        verbose=FALSE,
                        plotLabels=FALSE
                        ){
  
  # Disable scientific notation
  #options(scipen=999)
  # Enable scientific notation
  #options(scipen=9)
  
  DM <- data
  plt<-ggplot()
  retList<-list()
  
  if(verbose) cat("step size: ", step, "\n")
  if(verbose) cat("percentageSplit: ", splitPercentages, "\n")
  
  if(is.vector(attr) || is.list(attr)){
    if(length(attr) == 0){
      if(verbose) cat("attr left empty will default to examine all attributes.\n")
      attr<-names(DM)
    }
    else{
      if(verbose) cat("attributes focus: ", attr, "\n")
    }
  }
  
  #if length(window) == 0 
  # if vector or list or double
  # set values for each attr
  
  if(splitPercentages == "uniform"){
    if(verbose) cat("default uniform splitting for weights of all non-specified attributes will be applied. The total number of attributes supplied is ", length(names(DM)), " meaning ", length(names(DM))-1, " attributes can acquire the split percentages, therefore " ,1/(length(names(DM))-1),"% will be split amongst each on every step through the sensitivity analysis.","\n")
    
    cat("Default splits assumed.\n")
    DB_Final <- default_sensitivity(DM, DB, attr, algs, algParams, step, verbose, window)
    
  }
  else{
    cat("Custom splits were decided at: ", splitPercentages, "\n")
    cat("Non-uniform split percentages is not yet supported in this package. Try a later version for this functionality.\n")
    #DB_Final <- custom_sensitivity(DM, DB, algs, algParams, step, attr, splitPercentages, verbose)
    # same as default just when increase/decreaseAttribute is called scale by the row in splitPerc that attr_i is in names(dm)
  }

  
  # Return a ggplot2 object of the database for quick visualization 
  
  # Get cases where ranks changed
  DB_Edges <- DB_Final[DB_Final$reportOut==TRUE,][,c((ncol(DM)+1):ncol(DB_Final))]
  
  # Customized plot
  DB_Edges$ranks <- as.factor(DB_Edges$ranks)
  
  if(nrow(DB_Edges) != 0)
  {
    plt <- ggplot(data=DB_Edges,
                  aes(x=DB_Edges$alts, y=DB_Edges$weight, color=DB_Edges$ranks)) 
    plt <- plt + theme_bw()
    plt <- plt + geom_point()
    plt <- plt + facet_wrap(~alg+attr_i) 
    plt <- plt + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    plt <- plt + theme(axis.text.y = element_text(angle = 25))
    plt <- plt + scale_y_continuous(breaks = round(seq(min(DB_Edges$weight), 1, by = step),1))
    if(plotLabels) plt <- plt + geom_label(aes(label= round(DB_Edges$weight,2)), show.legend = FALSE)
    plt <- plt + ggtitle("Sensitivity Analysis") + xlab("Alternative") + ylab("Weight")
    plt <- plt + theme(plot.title = element_text(color="#666666", 
                                                 face="bold",
                                                 size=32, 
                                                 hjust=0)) 
    plt <- plt + theme(axis.title = element_text(color="#666666",
                                                 size=22)) 
    plt <- plt + theme(axis.title.x = element_text(face="bold"),
                       axis.title.y = element_text(face="bold"))
    plt <- plt + + labs(fill='Rank Scoring')
  }
  else{
    cat("Sensitivity results: No change in weight will result in a change in rank between alternatives.\n")
  }
  
  # Concise list to capture important elements of analysis
  retList = list(Results=DB_Final,
                 EdgeCasesResults=DB_Edges,
                 Plot=plt)
  
  # Return all, a ggplot2 object, and a consolidated summary
  return(retList)
}


#' @description Helper function to sensitivity analysis
#' @title default_sensitivty
#' @author Blake Conrad \email{bmc.cs@outlook.com}
#' @param DM, the original decision matrix, in read.decision.matrix format
#' @param DB, wearhouse to store all of our iterations of data being created.
#' @param attr, specified attribute(s) to run sensitivty on.
#' @param algs, specified algorithms to run sensitivity on.
#' @param algParams, parameters to be passed around to each function in sensitivity
#' @param step, incremental step size each iteration.
#' @param verbose, echos print statements to understand input/output relation.
#' @param window, specified window size vector e.g., c(0,.5) only to allow weights to change between the given window range.
#' @keywords data.frame, ranking
#' @return data.frame

default_sensitivity <- function(DM, 
                                DB,
                                attr,
                                algs,
                                algParams,
                                step,
                                verbose,
                                window){ 
  
  
  cat("Default sensitivity beginning. \n")
  
  # If no algorithm specified, do all!
  if(length(algs)==0) algs <- c("TOPSIS","MAUT")
  
  # If no attributes specified, do all!
  if(length(attr)==0) attr <- names(DM)
  
  # If no window specified, do step!
  if(length(window) == 2){
    minStep    <- window[1]
    maxStep    <- window[2]
  }
  else{
    minStep <- step
    maxStep <- (1-step)
  }
  # Constants for DB & calculations
  iterid     <-1
  algid      <-1
  N          <- length(names(DM))
  split_step <- step/(N-1)


  # Keep a fresh copy of the DM so as we update we dont lose information
  DMCopy <- DM
  
  for(alg in algs){
    
    if(verbose) cat("Running method: ", alg, ".\n")
    
    for(attr_i in attr){
      
      cat(". ")
      
      # For debugging
      if(verbose) cat("Focus attribute: ", attr_i,"\n")
      if(verbose) cat("*** Analyzing from ", DMCopy["weight", attr_i], " to ",step,".*** \n")
      if(verbose) cat("Current weight...\t")
      
      # Initial run w/o inc/dec
      DB <- updateDB(DMCopy, DB, alg, algParams, attr_i, iterid, verbose)
      iterid<-iterid+1
      
      repeat{
        
        if(verbose) cat(DMCopy["weight",attr_i],"\t")
        
        # Update the DM
        DMCopy <- decreaseAttribute(DMCopy, attr_i, step, split_step)
        
        if(verbose) cat("attribute: ", attr_i,"\tvalue: ",DMCopy["weight",attr_i],"\ttype: ",class(DMCopy["weight",attr_i]),"\n")
        if(verbose) cat("attribute: ", "minStep","\tvalue: ", minStep, "\ttype: ",class(minStep),"\n")

        if(DMCopy["weight",attr_i] > minStep){
          #cat("Storing..\t")
          #cat("\n")
          #Sys.sleep(1)
          # Store the results of alg
          DB <- updateDB(DMCopy, DB, alg, algParams, attr_i, iterid, verbose)
          
          # update iter
          iterid<-iterid+1
        }else{
          
          #cat("Skipping..\t")
          #cat("\n")
          #Sys.sleep(5)
          break
        }
      } #endwhile
      
      # For debugging
      if(verbose) cat("\n")
      
      # Keep a fresh copy of the DM from points: attrVal->(1-step)
      DMCopy <- DM
      
      # For debugging
      if(verbose) cat("*** Analyzing from ", DMCopy["weight", attr_i], " to ",(1-step),".*** \n")
      if(verbose) cat("Current weight...\t")
      
      repeat{
        
        if(verbose) cat(DMCopy["weight",attr_i],"\t")
        
        # If next step is valid, while will engage
        DMCopy <- increaseAttribute(DMCopy, attr_i, step, split_step)
        
        if(verbose) cat("attribute: ", attr_i,"\tvalue: ",DMCopy["weight",attr_i],"\ttype: ",class(DMCopy["weight",attr_i]),"\n")
        if(verbose) cat("attribute: ", "minStep","\tvalue: ", minStep, "\ttype: ",class(minStep),"\n")
        
        if (DMCopy["weight",attr_i] < maxStep){
          
          #cat("Storing..\t")
          #cat("\n")
          #Sys.sleep(1)
          DB <- updateDB(DMCopy, DB, alg, algParams, attr_i, iterid, verbose)
          
          # update iter
          iterid<-iterid+1
          
          
        }else{
          
          #cat("Skipping..\t")
          #cat("\n")
          #Sys.sleep(5)
          break
        }
      } #endwhile
      
      
      
      # For debugging
      if(verbose) cat("\n")
      
      # Reset to original weights for the next attribute.
      DMCopy <- DM
      
    }#endforattri
    
  }#endforalg
  
  cat("\n")
  cat("Default sensitivity finished.\n")
  
  return(DB)
}#enddefault_sensitivity


#' @description Helper function to sensitivity analysis
#' @title decreaseAttribute
#' @author Blake Conrad \email{bmc.cs@outlook.com}
#' @param DMCopyj, the original decision matrix
#' @param attr_j, specified attribute being examined.
#' @param step, incremental step size
#' @param split_step, how much to add too other attributes as we increase this one
#' @keywords data.frame, ranking
#' @return data.frame
decreaseAttribute <- function(DMCopyj,
                              attr_j,
                              step, 
                              split_step){
  
  # Update just the target attributes weight -> 0
  DMCopyj["weight",attr_j] <- DMCopyj["weight",attr_j] - step
  
  # Update every other attribute's weight in the DM
  DMCopyj["weight",][which(names(DMCopyj) != attr_j)] <- DMCopyj["weight",][which(names(DMCopyj) != attr_j)] + split_step
  
  #if(any(as.list(DMCopyj["weight",])) < 0){
  #  sys.sleep(5)
  #}

    return(DMCopyj)
}


#' @description Helper function to sensitivity analysis
#' @title increaseAttribute
#' @author Blake Conrad \email{bmc.cs@outlook.com}
#' @param DMCopyk, the original decision matrix
#' @param attr_k, specified attribute being examined.
#' @param step, incremental step size
#' @param split_step, how much to take away from other attributes as we increase this one
#' @keywords data.frame, ranking
#' @return data.frame
increaseAttribute <- function(DMCopyk,
                              attr_k,
                              step,
                              split_step){
  
  # Update just the target attributes weight -> 0
  DMCopyk["weight",attr_k] <- DMCopyk["weight",attr_k] + step
  
  # Update every other attribute's weight in the DM
  DMCopyk["weight",][which(names(DMCopyk) != attr_k)] <- DMCopyk["weight",][which(names(DMCopyk) != attr_k)] - split_step
  
  #if(any(as.list(DMCopyk["weight",])) < 0){
  #  sys.sleep(5)
  #}
  
  return(DMCopyk)
}


#' @description Helper function to sensitivity analysis
#' @title updateDB
#' @author Blake Conrad \email{bmc.cs@outlook.com}
#' @param DMCopy, the original decision matrix
#' @param DB, wearhouse to store all of our iterations of data being created.
#' @param attr_i, specified attribute being examined.
#' @param alg, specified algorithms to run sensitivity on.
#' @param algParams, parameters to be passed around to each function in sensitivity
#' @param verbose, echos print statements to understand input/output relation.
#' @param iterid, the current unique identifier to this iteration.
#' @keywords data.frame, ranking
#' @return data.frame
updateDB <- function(DMCopy, 
                     DB,
                     alg,
                     algParams,
                     attr_i, 
                     iterid,
                     verbose){

  # Constants to store
  weights <- as.list(t(DMCopy["weight",]))
  names(weights) <- row.names(t(DMCopy["weight",]))
  res <- data.frame()
  
  
  # Run the appropriate algorithm
  if(alg == "TOPSIS") 
    res <- TOPSIS(data=DMCopy, algParams=algParams, verbose=verbose)
  else if(alg == "MAUT")
    res <- MAUT(data=DMCopy, algParams=algParams, verbose=verbose)
  else{
    cat("Invalid algs supplied.\n")
  }
  
  # Store algorithm results
  alts <- res$Results[,"Alternative"]#get alternatives
  ranks <- res$Results[,"Rank"]#get ranks
  
  container <- data.frame(weights, alts, ranks)
  container$alg <- alg
  container$attr_i <- attr_i
  container$iterid <- iterid
  container$weight <- DMCopy["weight", attr_i]
  #Beta testing for edge cases
  
  #print(DB[(nrow(DB)-(length(alts)-1)):(nrow(DB)),]$ranks)
  #print(container[,]$ranks)
  #print(any(DB[(nrow(DB)-(length(alts)-1)):(nrow(DB)),"ranks"] != container[, "ranks"]))
  #Sys.sleep(3)
  
  # If the attr just changed, it should not count as an edge case

  # If the ranks changed
  if(!is.null(DB[(nrow(DB)-(length(alts)-1)):(nrow(DB)),"ranks"])){
    if (any(DB[(nrow(DB)-(length(alts)-1)):(nrow(DB)),"ranks"] != container[, "ranks"])){
      
      # Attr change isn't edge case
      if(tail(DB,1)$attr_i != attr_i){
        container$reportOut <- FALSE
      }
      # It was the same attribute, and indeed is an edge case
      else{
        container$reportOut<-TRUE
      }
      
    }
    else{
      container$reportOut <- FALSE
      #print(DB[(nrow(DB)-(length(alts)-1)):(nrow(DB)),]$ranks)
      #print(container[,]$ranks)
      #print(any(DB[(nrow(DB)-(length(alts)-1)):(nrow(DB)),"ranks"] != container[, "ranks"]))
      #Sys.sleep(15)
    } 
  }
  else{
    container$reportOut <- FALSE
  }

  
  # Update DB

  DB <- rbind(DB, container)
  return(DB)
}



#' @description Helper function to sensitivity analysis
#' @title custom_sensitivty
#' @author Blake Conrad \email{bmc.cs@outlook.com}
#' @param DM, the original decision matrix
#' @param DB, wearhouse to store all of our iterations of data being created.
#' @param attr, specified attribute(s) to run sensitivty on.
#' @param algs, specified algorithms to run sensitivity on.
#' @param algParams, parameters to be passed around to each function in sensitivity
#' @param step, incremental step size each iteration.
#' @param verbose, echos print statements to understand input/output relation.
#' @param window, specified window size vector e.g., c(0,.5) only to allow weights to change between the given window range.
#' @param splitPercentages, a specified range of how each iteration will divey out the incrmented change to the other attributes. This can vary.
#' @keywords data.frame, ranking
#' @return list
custom_sensitivity <- function(DM,
                               DB,
                               attr,
                               algs,
                               algParams,
                               step,
                               verbose,
                               window,
                               splitPercentages){
  
  cat("Custom sensitivity beginning.\n")
  cat("Custom sensitivity not yet established ... \n")
  cat("Custom sensitivity finished.\n")
  
  return(data.frame())
}
