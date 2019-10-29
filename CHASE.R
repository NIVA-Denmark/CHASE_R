library("dplyr")
library("tidyr")

#===============================================================================
# function Assessment
Assessment<- function(assessmentdata,summarylevel=1){
  
   if(!length(assessmentdata)>0){
     message("length 0\n")
     return(NA)
   }else{
   
   requiredcols <- c("Matrix","Substance","Threshold","Status")
   extracols <- c("Waterbody","Response")
   
  
  #Check column names in the imported data
  cnames<-names(assessmentdata)
  nimp = ncol(assessmentdata)
  nreq = length(requiredcols)
  nextra = length(extracols)
  
  ok <- rep(0, nreq)
  okextra <- rep(0, nextra)
  foundresponse=FALSE
  
  for (i in 1:nimp){
    for (j in 1:nreq){
      if(toupper(requiredcols[j])==toupper(cnames[i])){
        names(assessmentdata)[i] <- requiredcols[j]
        ok[j]=1
      }
    }
    for (j in 1:nextra){
      if(toupper(extracols[j])==toupper(cnames[i])){
        names(assessmentdata)[i] <- extracols[j]
        okextra[j]=1
      }
    }
  }
  
  for(j in 1:nextra){
    if(okextra[j]==0){
      assessmentdata[[extracols[j]]]<-1
    }
  }

  n<-sum(ok, na.rm = TRUE)
  
  if(n<nreq){
    # The required columns were not found in the input data
    message("Error in CHASE Assessment. Required column(s) were not found in the input data:")
    for (j in 1:nreq){
      if(ok[j]!=1){
        message(paste("    ",requiredcols[j]))
      }
    }
    return(NA)
  }else{
    # The required columns are present - do the assessment
    
    
    # Change order of matrices factors
    mat1<-data.frame(unique(assessmentdata$Matrix))
    names(mat1)[1] <- 'Matrix'
    mat1$char<-as.character(mat1$Matrix)
    mat1$len<-nchar(mat1$char)
    mat1<-arrange(mat1,len)
    
    assessmentdata$Matrix <- factor(assessmentdata$Matrix, levels = mat1$char)

    # All combinations of matrices and waterbodies
    # This is used to ensure that a NA is returned where the combinations are missing
    waterbodies<-unique(assessmentdata$Waterbody)
    matrices<-unique(assessmentdata$Matrix)
    matrices<-expand.grid(waterbodies, matrices)
    names(matrices)[1] <- 'Waterbody'
    names(matrices)[2] <- 'Matrix'
    
    
    
    assessmentdata$CR<-ContaminationRatio(assessmentdata$Threshold,assessmentdata$Status,assessmentdata$Response)
    
    QEdata<-summarise(group_by(assessmentdata,Waterbody,Matrix), sumCR=sum(CR), Count=n())
    QEdata$ConSum<-QEdata$sumCR/sqrt(QEdata$Count)
    
    QEdata$sumCR <- NULL
    QEdata$Count <- NULL
    QEspr<-spread(QEdata,Matrix,ConSum)
    
    QEdata$QEStatus<-CHASEStatus(QEdata$ConSum)
    QEdata<-left_join(matrices,QEdata,c('Waterbody','Matrix'))
    QEdata<-arrange(QEdata,Waterbody,Matrix)
    
    CHASE<-summarise(group_by(QEdata,Waterbody), ConSum=max(ConSum, na.rm = TRUE))
    CHASE$Waterbody<-NULL
    CHASEQE<-inner_join(QEdata, CHASE, 'ConSum')
    CHASEQE<-rename(CHASEQE,Status=QEStatus,Worst=Matrix)
    assessmentdata<-left_join(assessmentdata,QEdata,c('Waterbody','Matrix'))
    QEspr<-inner_join(QEspr, CHASEQE, 'Waterbody')
    
    
    for(j in 1:nextra){
      if(extracols[j]=='Waterbody' & okextra[j]==0){
        #assessmentdata[[extracols[j]]]<-NULL
        #QEdata[[extracols[j]]]<-NULL
      }
    }
    
    #return(n)
    if(summarylevel==2){
      return(QEspr)
    }else if(summarylevel==3){
      return(QEdata)
    }else if(summarylevel==4){
      return(CHASEQE)
    }else{
      return(assessmentdata)
    }
    #
  }
   }
  
}

#===============================================================================
# function ContaminationRatio
ContaminationRatio<- function(threshold, status, response=1){
  # If response is not specified, it will be assumed to be positive
  # i.e. ContaminationRatio increases (worsens) with increasing status value
  if (missing(response)){
    response=1
  }
  response<-ifelse(is.na(response), 1, response)
  
  # ContaminationRatio calculated depending on Response direction
  cr<-ifelse(response>0, status/threshold, threshold/status)
    return(cr)
}

#===============================================================================
#Function CHASEStatus
CHASEStatus<-function(CRsum){
  status<-ifelse(CRsum>0.5, "Good", "High")
  status<-ifelse(CRsum>1, "Moderate", status)
  status<-ifelse(CRsum>5, "Poor", status)
  status<-ifelse(CRsum>10, "Bad",status )
  return(status)
}

AddColours<-function(CRsum){
  co<-ifelse(CRsum>0.5, '#66FF66', '#3399FF')
  co<-ifelse(CRsum>1, '#FFFF66', co)
  co<-ifelse(CRsum>5, '#FF9933', co)
  co<-ifelse(CRsum>10, '#FF6600',co)
  return(co)
}


#===============================================================================
# function Threshold
Threshold <- function(substance){
  substances<- read.csv('./data/substances.csv', header = TRUE,  sep=";")
  return(value)
}


#===============================================================================
#Function Contamination sum
ContaminationSum <- function(ratiolist){
  
}



