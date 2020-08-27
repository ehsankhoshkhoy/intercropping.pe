



#' is a function for general output of intercropping
#'
#' @param beanDatPath is path of input csv file.
#' this file has six column that are Zbs,Zsb,B,R,SYb,SYs
#' Zbs is for example ratio of Beans to sunflower Zsb vise versa
#' B is value Ratio and R is Replication
#' in that example Yb is Yield of Beans
#' and Ys is Yield of Sunflower
#' It is noteworthy, mensioned description is for all function of input path
#'
#' @param outputAddress is path for general csv file of intercropping
#' @export
#' @examples
#' do.basic.intercrop("d:\\input.csv","d:\\output.csv")
do.basic.intercrop <- function(beanDatPath,outputAddress)
{
  print("please wait .....")
  gendf <- dogenintercrop(beanDatPath)
  utils::write.csv(gendf,file=outputAddress,row.names = FALSE)
  print(gendf)
  print("finished")

}


#' is a function for summary output of intercropping
#' @param beanDatPath is path of input csv file.
#' this file has six column that are Zbs,Zsb,B,R,SYb,SYs
#' Zbs is for example ratio of Beans to sunflower Zsb vise versa
#' B is value Ratio and R is Replication
#' in that example Yb is Yield of Beans
#' and Ys is Yield of Sunflower
#' It is noteworthy, mensioned description is for all function of input path
#' @param outputAddress is path of summery output of intercropping
#' @export
#' @examples
#' do.summary.intercrop("d:\\input.csv","d:\\output.csv")
do.summary.intercrop <- function( beanDatPath,outputAddress)
{
  print("please wait .....")
  gendf <- dogenintercrop(beanDatPath)
  firstfinalData <- gendf[,c("B","LERb","LERs","LER","Pb","Ps","CRs",
                             "CRb","Kb","Ks","KbMKs")]
  firstfinalData <- subset(firstfinalData, firstfinalData$LERb != 1 )
  firstfinalData <- subset(firstfinalData,firstfinalData$LERs != 1 )
  firstfinalData <- unique(firstfinalData)
  print(firstfinalData)
  utils::write.csv(firstfinalData,outputAddress,row.names = FALSE)
  print("finished")

}

#' this function is for calculate other indices namely Aggressivity, Actual yield loss, relative yield, RVT and ...
#' @param beanDatPath path for input csv file
#' this file has six column that are Zbs,Zsb,B,R,SYb,SYs
#' Zbs is for example ratio of Beans to sunflower Zsb vise versa
#' B is value Ratio and R is Replication
#' in that example Yb is Yield of Beans
#' and Ys is Yield of Sunflower
#' It is noteworthy, mensioned description is for all function of input path
#' @param outputAddress  path and file name for output csv file
#' @param priceofbean  is number and is price of bean
#' @param priceofsunflower is number and is price of sunflower
#' @return return a dataframe
#' @export
#' @examples
#' do.otherindices.intercrop("d:\\input.csv","d:\\otherindicesoutput.csv")
#' do.otherindices.intercrop("d:\\input.csv","d:\\otherindicesoutput.csv",100000,20000)
do.otherindices.intercrop <- function(beanDatPath,outputAddress,priceofbean=NULL,priceofsunflower=NULL)
{
  print("please wait ......")
  inputdf <- dogenintercrop(beanDatPath)
  maxb <- max(inputdf$Mb)
  maxs <- max(inputdf$Ms)
  inputdf <- subset(inputdf,inputdf$Zbs != 100  )
  inputdf <- subset(inputdf,inputdf$Zbs != 0  )

  inputdf$Ebs <- maxb * ( inputdf$Zbs /100 )
  inputdf$Esb <- maxs * ( inputdf$Zsb /100 )

  inputdf$YbsDEbs <- inputdf$Mb / ( inputdf$Ebs)
  inputdf$YsbDEsb <- inputdf$Ms / ( inputdf$Esb)

  inputdf$Ab <- inputdf$YbsDEbs - ( inputdf$YsbDEsb)
  inputdf$As <- inputdf$YsbDEsb - ( inputdf$YbsDEbs)

  inputdf$YbsDZbs <- inputdf$Mb / ( inputdf$Zbs / 100)
  inputdf$YsbDZsb <- inputdf$Ms / ( inputdf$Zsb / 100)

  inputdf$YbsDZbsDmaxb <- inputdf$YbsDZbs / maxb
  inputdf$YsbDZsbDmaxs <- inputdf$YsbDZsb / maxs

  inputdf$AYLb <- inputdf$YbsDZbsDmaxb - 1
  inputdf$AYLs <- inputdf$YsbDZsbDmaxs - 1
  inputdf$AYL <- inputdf$AYLb + inputdf$AYLs

  inputdf$maxbSMbMmaxsSMs <- (maxb-inputdf$Mb)*(maxs-inputdf$Ms)
  inputdf$YbsMYsb <- inputdf$Mb * inputdf$Ms

  inputdf$CI <- inputdf$maxbSMbMmaxsSMs / inputdf$YbsMYsb
  if(is.null(priceofbean) )
    priceofbean = 100000
  if(is.null(priceofsunflower) )
    priceofsunflower = 20000

  bM1 <- maxb * priceofbean
  sM2 <- maxs * priceofsunflower

  inputdf$MbMPbPMsMPs <- ( inputdf$Mb * priceofbean ) + (inputdf$Ms*priceofsunflower)
  inputdf$RVTb <- inputdf$MbMPbPMsMPs / bM1
  inputdf$RVTs <- inputdf$MbMPbPMsMPs / sM2

  inputdf$NE <- (inputdf$Mb+inputdf$Ms) - (inputdf$Ebs +inputdf$Esb)
  inputdf$MbDmaxb <- inputdf$Mb / maxb
  inputdf$MsDmaxs <- inputdf$Ms / maxs

  inputdf$RYb <- inputdf$MbDmaxb / (inputdf$Zbs / 100)
  inputdf$RYs <- inputdf$MsDmaxs / (inputdf$Zsb / 100)

  inputdf$CEb <- (inputdf$LER - 1) * maxb
  inputdf$CEs <- (inputdf$LER - 1) * maxs
  print(inputdf)

  otherindicesdf <- inputdf[,c("Zbs","Zsb","B","Mb","Ms","Ebs","Esb","YbsDEbs","YsbDEsb","Ab"
  ,"As","YbsDZbs","YsbDZsb","YbsDZbsDmaxb","YsbDZsbDmaxs","AYLb","AYLs","AYL",
  "maxbSMbMmaxsSMs","YbsMYsb","CI","MbMPbPMsMPs","RVTb","RVTs","NE",
  "MbDmaxb","MsDmaxs","RYb","RYs","CEb","CEs")]

  otherindicesdf <- unique(otherindicesdf)

  utils::write.csv(otherindicesdf,outputAddress,row.names = FALSE)
  print(unique(inputdf))
  print("finished.....")
  return(inputdf)

}

#' this fucnction is for ANOVA on beans
#' @param inputDataPath is path for csv input file
#' this file has six column that are Zbs,Zsb,B,R,SYb,SYs
#' Zbs is for example ratio of Beans to sunflower Zsb vise versa
#' B is value Ratio and R is Replication
#' in that example Yb is Yield of Beans
#' and Ys is Yield of Sunflower
#' It is noteworthy, mensioned description is for all function of input path
#' @export
#' @examples
#' do.anova.beans("d:\\input.csv")
do.anova.beans <- function(inputDataPath)
{
  df <- utils::read.csv(file=inputDataPath,sep = ",")
  df <- adjustcolumns(df)
  maxSYb <- max(df$SYb)
  ######### ANOVA for beans #################
  beandata <- subset(df,df$Zsb != 100)
  beandata <- beandata[,c("B","R","SYb")]
  beandata <- unique(beandata)
  beandata$B <- factor(beandata$B)
  beandata$R <- factor(beandata$R)
  ANOVA <- stats::aov (SYb ~ B + R, data=beandata)
  print( summary(ANOVA))
  out.LSD <- agricolae::LSD.test (ANOVA,"B", p.adj="bonferroni")
  out.duncan <- agricolae::duncan.test(ANOVA,"B")

  # # # # # #
  # draw a plot for output of LSD or duncan test
  # 2500 is a optional argument dependent to max value of trait#
  #stargraph
  agricolae::bar.group(out.LSD$groups,ylim=c(0,maxSYb),density=4,border="blue")
  #endgraph
}

#' this fucnction is for ANOVA on sunflower
#' @param inputDataPath is path for csv input file
#' this file has six column that are Zbs,Zsb,B,R,SYb,SYs
#' Zbs is for example ratio of Beans to sunflower Zsb vise versa
#' B is value Ratio and R is Replication
#' in that example Yb is Yield of Beans
#' and Ys is Yield of Sunflower
#' It is noteworthy, mensioned description is for all function of input path
#' @export
#' @examples
#' do.anova.sunflower("d:\\input.csv")
do.anova.sunflower <- function(inputDataPath)
{
  df <- utils::read.csv(file=inputDataPath,sep = ",")
  df <- adjustcolumns(df)
  maxSYs <- max(df$SYs)
  ######### ANOVA for beans #################
  sundata <- subset(df,df$Zbs != 100)
  sundata <- sundata[,c("B","R","SYs")]
  sundata <- unique(sundata)
  sundata$B <- factor(sundata$B)
  sundata$R <- factor(sundata$R)
  ANOVA <- stats::aov (SYs ~ B + R, data=sundata)
  print( summary(ANOVA))
  out.LSD <- agricolae::LSD.test (ANOVA,"B", p.adj="bonferroni")
  out.duncan <- agricolae::duncan.test(ANOVA,"B")

  # # # # # #
  # draw a plot for output of LSD or duncan test
  # 2500 is a optional argument dependent to max value of trait#
  #stargraph
  agricolae::bar.group(out.LSD$groups,ylim=c(0,maxSYs),density=4,border="blue")
  #endgraph
}

dogenintercrop <- function( beanDatPath )
{
  inputdf <- utils::read.csv(beanDatPath,  sep = ',') ### sheet 1
  inputdf <- adjustcolumns( inputdf )
  meanList <- c()
  LERbList <- c()
  j <- 1
  for(i in inputdf[,"B"] )
  {
    if(inputdf$Zsb[j] != 100  )
    {
      beans_meandata <- subset(inputdf,inputdf$B == i)
      meanList[j] <- mean( beans_meandata[,"SYb"] )
    }
    else
      meanList[j] <- 0
    j <- j+1
  }
  inputdf$Mb <- meanList

  maximum <- max(meanList)
  j <- 1
  for(i in inputdf[,"Mb"] )
  {
    if( inputdf$Zsb[j] != 100 )
      LERbList[j] <-  inputdf[j,"Mb"]/maximum
    else
      LERbList[j] <- 0
    j <- j+1
  }

  inputdf$LERb <- LERbList

  meansList <- c()
  LERsList <- c()
  j <- 1
  for(i in inputdf[,"B"] )
  {
    if(inputdf$Zbs[j] != 100  )
    {
      beans_meandata <- subset(inputdf,inputdf$B == i)
      meansList[j] <- mean( beans_meandata[,"SYs"] )
    }
    else
      meansList[j] <- 0
    j <- j+1
  }
  inputdf$Ms <- meansList

  maximum <- max(meansList)
  j <- 1
  for(i in inputdf[,"Ms"] )
  {
    if( inputdf$Zbs[j] != 100 )
      LERsList[j] <-  inputdf[j,"Ms"]/maximum
    else
      LERsList[j] <- 0
    j <- j+1
  }

  inputdf$LERs <- LERsList
  j <- 1
  inputdf$LER <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zbs"] != 100  &&  inputdf[j,"Zbs"] != 0 )
      inputdf$LER[j] <- inputdf[j,"LERs"] + inputdf[j,"LERb"]
    else
      inputdf$LER[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$Pb <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zbs"] != 100  &&  inputdf[j,"Zbs"] != 0 )
      inputdf$Pb[j] <- inputdf[j,"LERb"] / inputdf[j,"LER"]
    else
      inputdf$Pb[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$Ps <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zbs"] != 100  &&  inputdf[j,"Zbs"] != 0 )
      inputdf$Ps[j] <- inputdf[j,"LERs"] / inputdf[j,"LER"]
    else
      inputdf$Ps[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$LERsDLERb <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zbs"] != 100  &&  inputdf[j,"Zbs"] != 0 )
      inputdf$LERsDLERb[j] <- inputdf[j,"LERs"] / inputdf[j,"LERb"]
    else
      inputdf$LERsDLERb[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$ZbsDZsb <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zbs"] != 100  &&  inputdf[j,"Zbs"] != 0 )
      inputdf$ZbsDZsb[j] <- inputdf[j,"Zbs"] / inputdf[j,"Zsb"]
    else
      inputdf$ZbsDZsb[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$CRs <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zbs"] != 100  &&  inputdf[j,"Zbs"] != 0 )
      inputdf$CRs[j] <- inputdf[j,"LERsDLERb"] * inputdf[j,"ZbsDZsb"]
    else
      inputdf$CRs[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$LERbDLERs <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zbs"] != 100  &&  inputdf[j,"Zbs"] != 0 )
      inputdf$LERbDLERs[j] <- inputdf[j,"LERb"] / inputdf[j,"LERs"]
    else
      inputdf$LERbDLERs[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$ZsbDZbs <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zbs"] != 100  &&  inputdf[j,"Zbs"] != 0 )
      inputdf$ZsbDZbs[j] <- inputdf[j,"Zsb"] / inputdf[j,"Zbs"]
    else
      inputdf$ZsbDZbs[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$CRb <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zbs"] != 100  &&  inputdf[j,"Zbs"] != 0 )
      inputdf$CRb[j] <- inputdf[j,"LERbDLERs"] * inputdf[j,"ZsbDZbs"]
    else
      inputdf$CRb[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$YabMZsb <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zbs"] != 100  &&  inputdf[j,"Zbs"] != 0 )
      inputdf$YabMZsb[j] <- inputdf[j,"Mb"] * ( inputdf[j,"Zsb"]/100 )
    else
      inputdf$YabMZsb[j] <- 0
    j <- j+1

  }
  Yaa <- max(inputdf$Mb) #max(Mb)
  j <- 1
  inputdf$YaaSYabMZbs <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zbs"] != 100  &&  inputdf[j,"Zbs"] != 0 )
      inputdf$YaaSYabMZbs[j] <- ( Yaa - inputdf[j,"Mb"] ) * (inputdf[j,"Zbs"]/100)
    else
      inputdf$YaaSYabMZbs[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$Kb <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zbs"] != 100  &&  inputdf[j,"Zbs"] != 0 )
      inputdf$Kb[j] <- ( inputdf[j,"YabMZsb"] ) / (inputdf[j,"YaaSYabMZbs"])
    else
      inputdf$Kb[j] <- 0
    j <- j+1

  }

  ### Sun

  j <- 1
  inputdf$YbaMZbs <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zbs"] != 100  &&  inputdf[j,"Zbs"] != 0 )
      inputdf$YbaMZbs[j] <- inputdf[j,"Ms"] * ( inputdf[j,"Zbs"]/100 )
    else
      inputdf$YbaMZbs[j] <- 0
    j <- j+1

  }
  Ybb <- max(inputdf$Ms) #max(Ms)
  j <- 1
  inputdf$YbbSYbaMZsb <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zbs"] != 100  &&  inputdf[j,"Zbs"] != 0 )
      inputdf$YbbSYbaMZsb[j] <- ( Ybb - inputdf[j,"Ms"] ) * (inputdf[j,"Zsb"]/100)
    else
      inputdf$YbbSYbaMZsb[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$Ks <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zbs"] != 100  &&  inputdf[j,"Zbs"] != 0 )
      inputdf$Ks[j] <- ( inputdf[j,"YbaMZbs"] ) / (inputdf[j,"YbbSYbaMZsb"])
    else
      inputdf$Ks[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$KbMKs <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zbs"] != 100  &&  inputdf[j,"Zbs"] != 0 )
      inputdf$KbMKs[j] <- ( inputdf[j,"Kb"] ) * (inputdf[j,"Ks"])
    else
      inputdf$KbMKs[j] <- 0
    j <- j+1

  }

  return(inputdf)

} # end of function


adjustcolumns <- function( df )
{
  names(df)[1] <- "Zbs"
  names(df)[2] <- "Zsb"
  names(df)[4] <- "R"
  names(df)[5] <- "SYb"
  names(df)[6] <- "SYs"
  return(df)
}





