
# filepath <- system.file("extdata", "input.csv", package = "intercropping.pe")
# inputdata <-  utils::read.csv(file=filepath,sep = ",")
# use_data(inputdata)


#' this function is for intercropping and  calculate total relative value, namely Aggressivity, Actual yield loss, relative yield, RVT and ...
#' @param InputPath is path of input csv file.
#' this file has six columns that are Zab,Zba,B,R,SYa,SYb
#' Zab is  ratio of first species  to second species; Zba vise versa.
#' B is value Ratio.
#' R is Replication.
#' SYa is Yield of first species.
#' SYb is Yield of second species.
#' It is noteworthy, mensioned description is for all function of input path
#' @param OutputPath  is path of output of total relative value
#' @param inputdf is a dataframe for input data
#' @param PriceOfFirstSpeies  is a number namely  is price of First Speies
#' @param PriceOfSecondSpeies is a number namely  is price of Second Speies
#' @return return a dataframe
#' @export
#' @examples
#' do.intercropping.indies(InputPath="d:\\input.csv",OutputPath="d:\\output.csv")
do.intercropping.indies <- function(InputPath = NULL,OutputPath,inputdf = NULL,PriceOfFirstSpeies=NULL,PriceOfSecondSpeies=NULL)
{
  print("please wait ......")
  if(is.null(inputdf))
    inputdf <- return.df(path = InputPath )

  inputdf <- do.intercrop(df = inputdf )

  #print(inputdf)
  #utils::write.csv(inputdf,"d:\\out.csv",row.names = FALSE)

  maxa <- max(inputdf$Ma)
  maxb <- max(inputdf$Mb)
  inputdf <- subset(inputdf,inputdf$Zab != 0  ) #new filter
  inputdf <- subset(inputdf,inputdf$Zba != 0  ) #new filter

  inputdf$Eab <- maxa * ( inputdf$Zab /100 )
  inputdf$Eba <- maxb * ( inputdf$Zba /100 )

  inputdf$YabDEab <- inputdf$Ma / ( inputdf$Eab)
  inputdf$YbaDEba <- inputdf$Mb / ( inputdf$Eba)

  inputdf$Aa <- inputdf$YabDEab - ( inputdf$YbaDEba)
  inputdf$Ab <- inputdf$YbaDEba - ( inputdf$YabDEab)

  inputdf$YabDZab <- inputdf$Ma / ( inputdf$Zab / 100)
  inputdf$YbaDZba <- inputdf$Mb / ( inputdf$Zba / 100)

  inputdf$YbaDZabDmaxa <- inputdf$YabDZab / maxa
  inputdf$YbaDZbaDmaxb <- inputdf$YbaDZba / maxb

  inputdf$AYLa <- inputdf$YbaDZabDmaxa - 1
  inputdf$AYLb <- inputdf$YbaDZbaDmaxb - 1
  inputdf$AYL <- inputdf$AYLa + inputdf$AYLb

  inputdf$maxaSMaMmaxbSMb <- (maxa-inputdf$Ma)*(maxb-inputdf$Mb)
  inputdf$YabMYba <- inputdf$Ma * inputdf$Mb

  inputdf$CI <- inputdf$maxaSMaMmaxbSMb / inputdf$YabMYba
  if(is.null(PriceOfFirstSpeies) )
    PriceOfFirstSpeies = 100000
  if(is.null(PriceOfSecondSpeies) )
    PriceOfSecondSpeies = 20000

  aM1 <- maxa * PriceOfFirstSpeies
  bM2 <- maxb * PriceOfSecondSpeies

  inputdf$MaMPaPMbMPb <- ( inputdf$Ma * PriceOfFirstSpeies ) + (inputdf$Mb*PriceOfSecondSpeies)
  inputdf$RVTa <- inputdf$MaMPaPMbMPb / aM1
  inputdf$RVTb <- inputdf$MaMPaPMbMPb / bM2

  inputdf$NE <- (inputdf$Ma+inputdf$Mb) - (inputdf$Eab +inputdf$Eba)
  inputdf$MaDmaxa <- inputdf$Ma / maxa
  inputdf$MbDmaxb <- inputdf$Mb / maxb

  inputdf$DELTARYa <- inputdf$MaDmaxa / (inputdf$Zab / 100)
  inputdf$DELTARYb <- inputdf$MbDmaxb / (inputdf$Zba / 100)

  inputdf$CEa <- (inputdf$LER - 1) * maxa
  inputdf$CEb <- (inputdf$LER - 1) * maxb

  p1 <- (PriceOfFirstSpeies /(PriceOfFirstSpeies+PriceOfSecondSpeies)) *inputdf$AYLb
  p2 <- (PriceOfSecondSpeies /(PriceOfFirstSpeies+PriceOfSecondSpeies)) *inputdf$AYLa
  inputdf$AI <- p1+p2
  #RCC = Ka M Kb

  outdf <- inputdf[,c("Zab","Zba","B","Ma","Mb","LERa","LERb","LER","Pa"
  ,"Pb","CRa","CRb","Ka",  "Kb","RCC","Aa"  ,"Ab","AYLa","AYLb","AYL"
  ,"CI","RVTa","RVTb","NE","DELTARYa","DELTARYb","CEa","CEb","AI")]
  outdf <- unique(outdf)
  print(outdf)

   utils::write.csv(outdf,OutputPath,row.names = FALSE)
  print("finished")
  return(inputdf)

}
#' this fucnction is for ANOVA on two species.
#' @param InputPath is path of input csv file.
#' @param alphavalue value of alpha for LSD.test and duncan
#' this file has six columns that are Zab,Zba,B,R,SYa,SYb
#' Zab is  ratio of first species  to second species; Zba vise versa.
#' B is value Ratio.
#' R is Replication.
#' SYa is Yield of first species.
#' SYb is Yield of second species.
#' It is noteworthy, mensioned description is for all function of input path
#' @param inputdf is a dataframe
#' @export
#' @examples
#' do.anova(InputPath="d:\\input.csv")
do.anova<- function(InputPath = NULL,inputdf = NULL,alphavalue = NULL)
{
  if( is.null( inputdf ) )
  {
   df <- return.df(InputPath)
  }
  else
    df <- inputdf

  if(is.null(alphavalue))
  {
    alphavalue <- .05
    print(alphavalue)
  }

  df <- adjustcolumns(df)

  maxSYa <- max(df$SYa)
  maxSYa <- maxSYa + (0.15 * maxSYa)
  maxSYb <- max(df$SYb)
  maxSYb <- maxSYb +  (0.15 * maxSYb)
  print(maxSYa)
  print(maxSYb)

  ######### ANOVA 2-way for first species. #################

  firstdata <- subset(df,df$Zab != 0)
  firstdata <- firstdata[,c("B","R","SYa")]

  firstdata$B <- factor(firstdata$B)
  firstdata$R <- factor(firstdata$R)
  ANOVAa <- stats::aov (SYa ~ B + R, data=firstdata)
  print(" summary of first species ANOVA:")
  print( summary(ANOVAa))
  out.LSD.a <- agricolae::LSD.test (ANOVAa,"B", p.adj="bonferroni",alpha =alphavalue )
  print(" output of first species LSD.test:")
  print(out.LSD.a)
  out.duncan.a <- agricolae::duncan.test(ANOVAa,"B",alpha =alphavalue )
  print(" output of first species out.duncan:")
  print(out.duncan.a)
  # # # # # #
  # draw a plot for output of LSD or duncan test
  # 2500 is a optional argument dependent to max value of trait#
  #stargraph

  #endgraph

  ######### ANOVA 2-way for beans #################

  seconddata <- subset(df,df$Zba != 0 )
  seconddata <- seconddata[,c("B","R","SYb")]
  seconddata$B <- factor(seconddata$B)
  seconddata$R <- factor(seconddata$R)

  ANOVAb <- stats::aov (SYb ~ B + R , data=seconddata)
  print(" summary of second species ANOVA:")
  print( summary(ANOVAb))
  out.LSD.b <- agricolae::LSD.test (ANOVAb,"B", p.adj="bonferroni",alpha =alphavalue )
  print(" output of second species LSD.test:")
  print( out.LSD.b)
  out.duncan.b <- agricolae::duncan.test(ANOVAb,"B",alpha =alphavalue )
  print(" output of second species out.duncan:")
  print(out.duncan.b)
  # # # # # #
  # draw a plot for output of LSD or duncan test
  # 2500 is a optional argument dependent to max value of trait#
  #stargraph
  agricolae::bar.group(out.LSD.a$groups,ylim=c(0,maxSYa),density=4,border="blue")
  agricolae::bar.group(out.LSD.b$groups,ylim=c(0,maxSYb),density=4,border="blue")

  agricolae::bar.group(out.duncan.a$groups,ylim=c(0,maxSYa),density=4,border="red")
  agricolae::bar.group(out.duncan.b$groups,ylim=c(0,maxSYb),density=4,border="red")
  #endgraph
}

do.intercrop <- function( df )
{
  #inputdf <- utils::read.csv(beanDatPath,  sep = ',') ### sheet 1
  inputdf <- adjustcolumns( df )
  meanList <- c()
  LERaList <- c()
  j <- 1
  for(i in inputdf[,"B"] )
  {
    if(inputdf$Zab[j] != 0  ) #remove net
    {
      first_meandata <- subset(inputdf,inputdf$B == i)
      meanList[j] <- mean( first_meandata[,"SYa"] )
    }
    else
      meanList[j] <- 0
    j <- j+1
  }
  inputdf$Ma <- meanList

  maximum <- max(meanList)
  j <- 1
  for(i in inputdf[,"Ma"] )
  {
    if( inputdf$Zab[j] != 0 )
      LERaList[j] <-  inputdf[j,"Ma"]/maximum
    else
      LERaList[j] <- 0
    j <- j+1
  }

  inputdf$LERa <- LERaList

  meansList <- c()
  LERbList <- c()
  j <- 1
  for(i in inputdf[,"B"] )
  {
    if(inputdf$Zba[j] != 0  )
    {
      second_meandata <- subset(inputdf,inputdf$B == i)
      meansList[j] <- mean( second_meandata[,"SYb"] )
    }
    else
      meansList[j] <- 0
    j <- j+1
  }
  inputdf$Mb <- meansList

  maximum <- max(meansList)
  j <- 1
  for(i in inputdf[,"Mb"] )
  {
    if( inputdf$Zba[j] != 0 )
      LERbList[j] <-  inputdf[j,"Mb"]/maximum
    else
      LERbList[j] <- 0
    j <- j+1
  }

  inputdf$LERb <- LERbList
  j <- 1
  inputdf$LER <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zab"] != 0 || inputdf[j,"Zba"] != 0 ) ### && <- ||
      inputdf$LER[j] <- inputdf[j,"LERb"] + inputdf[j,"LERa"]
    else
      inputdf$LER[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$Pa <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zab"] != 0 && inputdf[j,"Zba"] != 0 )
      inputdf$Pa[j] <- inputdf[j,"LERa"] / inputdf[j,"LER"]
    else
      inputdf$Pa[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$Pb <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zab"] != 0 && inputdf[j,"Zba"] != 0 )
      inputdf$Pb[j] <- inputdf[j,"LERb"] / inputdf[j,"LER"]
    else
      inputdf$Pb[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$LERbDLERa <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zab"] != 0 && inputdf[j,"Zba"] != 0 )
      inputdf$LERbDLERa[j] <- inputdf[j,"LERb"] / inputdf[j,"LERa"]
    else
      inputdf$LERbDLERa[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$ZabDZba <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zab"] != 0 && inputdf[j,"Zba"] != 0 )
      inputdf$ZabDZba[j] <- inputdf[j,"Zab"] / inputdf[j,"Zba"]
    else
      inputdf$ZabDZba[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$CRb <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zab"] != 0 && inputdf[j,"Zba"] != 0 )
      inputdf$CRb[j] <- inputdf[j,"LERbDLERa"] * inputdf[j,"ZabDZba"]
    else
      inputdf$CRb[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$LERaDLERb <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zab"] != 0 && inputdf[j,"Zba"] != 0 )
      inputdf$LERaDLERb[j] <- inputdf[j,"LERa"] / inputdf[j,"LERb"]
    else
      inputdf$LERaDLERb[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$ZbaDZab <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zab"] != 0 &&  inputdf[j,"Zba"] != 0 )
      inputdf$ZbaDZab[j] <- inputdf[j,"Zba"] / inputdf[j,"Zab"]
    else
      inputdf$ZbaDZab[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$CRa <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zab"] != 0 && inputdf[j,"Zba"] != 0 )
      inputdf$CRa[j] <- inputdf[j,"LERaDLERb"] * inputdf[j,"ZbaDZab"]
    else
      inputdf$CRa[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$YabMZba <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zab"] != 0 && inputdf[j,"Zba"] != 0 )
      inputdf$YabMZba[j] <- inputdf[j,"Ma"] * ( inputdf[j,"Zba"]/100 )
    else
      inputdf$YabMZba[j] <- 0
    j <- j+1

  }
  Yaa <- max(inputdf$Ma) #max(Ma)
  j <- 1
  inputdf$YaaSYabMZab <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zab"] != 0 && inputdf[j,"Zba"] != 0 )
      inputdf$YaaSYabMZab[j] <- ( Yaa - inputdf[j,"Ma"] ) * (inputdf[j,"Zab"]/100)
    else
      inputdf$YaaSYabMZab[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$Ka <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zab"] != 0  && inputdf[j,"Zba"] != 0 )
      inputdf$Ka[j] <- ( inputdf[j,"YabMZba"] ) / (inputdf[j,"YaaSYabMZab"])
    else
      inputdf$Ka[j] <- 0
    j <- j+1

  }

  ### Sun

  j <- 1
  inputdf$YbaMZab <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zab"] != 0 && inputdf[j,"Zba"] != 0 )
      inputdf$YbaMZab[j] <- inputdf[j,"Mb"] * ( inputdf[j,"Zab"]/100 )
    else
      inputdf$YbaMZab[j] <- 0
    j <- j+1

  }
  Ybb <- max(inputdf$Mb) #max(Mb)
  j <- 1
  inputdf$YbbSYbaMZba <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zab"] != 0 && inputdf[j,"Zba"] != 0 )
      inputdf$YbbSYbaMZba[j] <- ( Ybb - inputdf[j,"Mb"] ) * (inputdf[j,"Zba"]/100)
    else
      inputdf$YbbSYbaMZba[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$Kb <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zab"] != 0 && inputdf[j,"Zba"] != 0 )
      inputdf$Kb[j] <- ( inputdf[j,"YbaMZab"] ) / (inputdf[j,"YbbSYbaMZba"])
    else
      inputdf$Kb[j] <- 0
    j <- j+1

  }

  j <- 1
  inputdf$RCC <- 0
  for(i in inputdf[,"B"])

  {
    if(  inputdf[j,"Zab"] != 0 && inputdf[j,"Zba"] != 0 )
      inputdf$RCC[j] <- ( inputdf[j,"Ka"] ) * (inputdf[j,"Kb"])
    else
      inputdf$RCC[j] <- 0
    j <- j+1

  }

  return(inputdf)

} # end of function


return.df <- function(path = NULL)
{
  if( is.null(path))
  {
    filepath <- system.file("extdata", "input.csv", package = "intercropping.pe")
    df <-  utils::read.csv(file=filepath,sep = ",")
  }
  else
    df <- utils::read.csv(file=path,sep = ",")
  return(df)
}

adjustcolumns <- function( df )
{
  names(df)[1] <- "Zab"
  names(df)[2] <- "Zba"
  names(df)[4] <- "R"
  names(df)[5] <- "SYa"
  names(df)[6] <- "SYb"
  return(df)
}







