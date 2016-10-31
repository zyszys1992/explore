
#for this assignment, I have some unfixed problems. So I used parts of my group member yichen zhang's code.
#I wrote down my code at the very end. 
equire(ggplot2)
require(grid)

#Q1
nonum.table <- function(dafra){
  #the function nonum.table() is to return a frequency table for every categorical and logical variable 
  
  #parameter:
  #dafra- a dataframe
  
  #return:
  # a frequent table in list 
  tb1 <- lapply(dafra[,sapply(dafra,is.logical)],table) #draw the table for logical variables
  tb2 <- lapply(dafra[,sapply(dafra,is.factor)],table) #draw the table for factor variables
  return(list(tb1,tb2))
}

#Q2
#Q2-1
summary.table <- function(dafra){
  #the function summary.table() is to return a summary statistics table for each numerical variable
  
  # parameter:
  #dafra: a dataframe
  
  #return:
  #a summary statistics table
  num <- Filter(is.numeric,dafra) #filter all numeric variables
  return(summary(num)) #return their summary table
}

#Q2-2a
rsquare <- function(num){
  # the function rsqure() is going to return a dataframe that contains each pair of column names in
  #the first column (name the column ???Variable Pairs???) and the associated r-square value in the second column (name the column ???R-Square???)
  
  #parameter:
  #num - a dataframe that all are numeric variables
  
  #return:
  #a dataframe that contains each pair of column names and corresponding r-square values
  colna <- colnames(num) # take out all the variables' names
  com_num <- combn(colna, 2) #combine the names pairwise
  VP <- c()
  R2 <- c()
  for(i in 1:ncol(com_num)){
    temp1 <- paste(com_num[1,i],com_num[2,i],sep = '-') 
    VP <- c(VP,temp1)                                   #write the pairwise names and add them in vector
    a<- num[,com_num[1,i]]
    b<- num[,com_num[2,i]]
    model <- lm(a~1+b)
    temp2 <- summary(model)["r.squared"]                #use linear regression to count and take out corresponding r-square
    R2 <- c(R2,as.numeric(temp2))
  }
  RR <- data.frame(VP,R2)                       #create a dataframe and change its name
  colnames(RR) <- c("Variable Pairs","R-square")
  return(RR)
}

#Q2-2b
Cor_pearson <- function(num,threshold){
  # the function Cor_pearson() is going to return A data frame that contains each pair of column names in the first column 
  #(name the column ???Variable Pairs???) and correlation coefficient (Pearson) for all coefficients whose absolute value 
  #is greater than the correlation threshold (do not repeat any pairs) in the second column
  
  #parameter:
  #num - a dataframe that all are numeric variables
  #threshold - a value being coefficient threshold
  
  #return:
  #a dataframe that contains each pair of column names and corresponding correlation coefficient
  colna <- colnames(num) # take out all the variables' names
  com_num <- combn(colna, 2) #combine the names pairwise
  VP2 <- c()
  Cor <- c()  #create two empty vectors
  for(i in 1:ncol(com_num)){
    temp1 <- paste(com_num[1,i],com_num[2,i],sep = '-')  #write pairwise names
    corr <- cor(num[,com_num[1,i]],num[,com_num[2,i]],method = "pearson") # count corresponding Pearson correlation coefficient
    if(corr >= threshold){
      VP2 <- c(VP2,temp1) 
      Cor <- c(Cor,corr)  #if correlation coefficient is greater than threshold then add them to vectors
    }
  }
  CRR <- data.frame(VP2,Cor) #create dataframe for pairwise names and correlation coefficients 
  colnames(CRR) <- c("Variable Pairs","Pearson Exceeds Threshold")# rename the dataframe then return the result
  return(CRR)
}

# this function for Question3
#this function is used to plot several graphs in one picture
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


#Q3
plot_density_count <- function(num,switch="on",vector=NULL){
  # the function plot_density_count() is thatIf the plot switch parameter is ???on???,then plot a pair of
  #blue histograms with a vertical red line at the mean (one using counts and the other density) for 
  #every numerical variable at each number of bins integer specified in the bin vector parameter. 
  #If the plot switch is set to ???grid???, there should be a grid for each count-bin combination and a 
  #separate grid for each density-bin size combination. 
  
  #parameter:
  #num - a dataframe that all are numeric variables
  #switch - a character that decide the plot
  #vector - a vector that decide the bins of histograms
  
  #return:
  #NULL
  if(switch == "on"){
    if(!is.null(vector)){ # if vector is NULL then
      for(j in 1:length(vector)){ 
        for(i in 1:ncol(num)){
          
          mean <- mean(num[,i]) #count the mean of one numeric variable
          
          p1 <- ggplot(num,aes(x=num[i]),color = "blue")+  
            geom_histogram(fill="blue",bins=vector[j])+
            ggtitle(paste(colnames(num[i]),vector[j],sep=" bins="))+
            xlab(colnames(num[i]))+
            geom_vline(xintercept = mean,col="red")  #draw the histogram of count with ggplot and add a red line on graph
          
          p2 <- ggplot(num,aes(x=num[i],..density..))+
            geom_histogram(fill="blue",bins=vector[j])+
            ggtitle(paste(colnames(num[i]),vector[j],sep=" bins="))+
            xlab(colnames(num[i]))+
            geom_vline(xintercept = mean,col="red") #draw the histogram of density with ggplot and add a red line on graph
          
          grid.newpage()
          pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
          title <- paste(colnames(num[i]),vector[j],sep=" bin=")
          grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
          print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
          print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2)) #use grid package to separate the picture and plot two histograms
          
        }
      }
    }else{ #
      for(i in 1:ncol(num)){
        
        mean <- mean(num[,i]) #count the mean of one numeric variable
        
        p1 <- ggplot(num,aes(x=num[i]),color = "blue")+  
          geom_histogram(fill="blue")+
          ggtitle(paste(colnames(num[i]),"default bins",sep=" bins="))+
          xlab(colnames(num[i]))+
          geom_vline(xintercept = mean,col="red")  #draw the histogram of count with ggplot and add a red line on graph
        
        p2 <- ggplot(num,aes(x=num[i],..density..))+
          geom_histogram(fill="blue")+
          ggtitle(paste(colnames(num[i]),"default bins",sep=" bins="))+
          xlab(colnames(num[i]))+
          geom_vline(xintercept = mean,col="red") #draw the histogram of density with ggplot and add a red line on graph
        
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
        title <- paste(colnames(num[i]),"default bins",sep=" bins=")
        grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
        print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
        print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2)) #use grid package to separate the picture and plot two histograms
        
      }
      
    }
    
  }else{
    if(switch == "grid"){
      if(!is.null(vector)){
        for(j in 1:length(vector)){
          grid.newpage()
          his_count <-list()   
          his_density <- list()  #create two empty list for store picture later
          for(i in 1:ncol(num)){
            his_count[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") + 
              geom_histogram(fill="blue", bins = vector[j])+ 
              labs(title= paste(vector[j], "bins")) #draw histograms of count and store them in list 
          }
          multiplot(plotlist = his_count, cols = 2)  #draw all histogram of count with same bins in one picture
          
          for(i in 1:ncol(num)){
            his_density[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") + 
              geom_histogram(aes(y= ..density..), fill="blue", bins = vector[j])+ 
              labs(title= paste(vector[j], "bins")) #draw histograms of density and store them in list 
          }
          multiplot(plotlist = his_density, cols = 2)  #draw all histogram of density with same bins in one picture
        }
      }else{
        grid.newpage()
        his_count <-list()   
        his_density <- list()  #create two empty list for store picture later
        for(i in 1:ncol(num)){
          his_count[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") + 
            geom_histogram(fill="blue")+ 
            labs(title= 'default bins') #draw histograms of count and store them in list 
        }
        multiplot(plotlist = his_count, cols = 2)  #draw all histogram of count with same bins in one picture
        
        for(i in 1:ncol(num)){
          his_density[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") + 
            geom_histogram(aes(y= ..density..), fill="blue")+ 
            labs(title= 'default bins') #draw histograms of density and store them in list 
        }
        multiplot(plotlist = his_density, cols = 2) 
      }
      
    }
  }
}


is.binary <- function(v) {
  #this function is to decide whether the vector is a binary vector
  
  #parameter:
  #v: a vector
  
  #return:
  #TRUE or FALSE
  x <- unique(v)                    #check all the distinct and put those in a vector x
  length(x) - sum(is.na(x)) == 2L         #check to see if x only contains 2 distinct values
}


plot_categ_binary <- function(data_frame,switch){
  #this function is to plot a gray bar graph for every categorical and binary variable if the plot switch parameter is ???on??? or ???grid???.
  
  #parameter:
  #data_frame- a dataframe
  #switch- a character 
  
  #return:
  #NULL
  data_frame1 <- data_frame[,sapply(data_frame,is.factor)]
  data_frame2 <- data_frame[,sapply(data_frame,is.logical)] 
  data_frame3 <- data_frame[,sapply(data_frame,is.binary)]
  data_frame <-data.frame(data_frame1,data_frame2,data_frame3)
  if(switch=="on"|| switch=="grid"){
    for(i in 1:ncol(data_frame)){
      p <- ggplot(data_frame,aes(x=data_frame[,i]))+
        geom_bar(fill='gray')+
        xlab(colnames(data_frame)[i])
      print(p)
    }
  }
}

explore <- function(dafra,switch = "on",threshold = 0,vector = NULL){
  #This function is a main function 
  
  #parameters:
  # dafra- A data frame
  # switch - A plot switch that can accept three values :off,on,or grid
  # threshold-A threshold cut -of fvalue between 0 and 1 for correlations
  # vector- An optional vector that contains one or more integers that
  # represent the numbers of bins to use for a histogram. If the vector is not provided, then let ggplot use it???s default.
  
  nonnum_table <- nonum.table(dafra) #create a frequency table for every categorical and logical variable
  
  summary_table <-summary.table(dafra)#create a summary statistics table for each numerical variable
  
  num <- Filter(is.numeric,dafra) # filter the dataframe and take out all numeric
  
  RR <- rsquare(num) #create a dataframe that contains pairwise names and their r-square values
  
  CRR <- Cor_pearson(num,threshold) #create a dataframe that contains pairwise names and their pearson correlation coefficient 
  
  plot_density_count(num,switch,vector) # plot their density and count histograms
  
  plot_categ_binary(dafra,switch) #plot their gray bar graphs
  
  out <- list(nonnum_table,summary_table,RR,CRR) #create a list that contain all the results then return 
  
  return(out)
}
explore(diamonds,'on',0.2,c(10,20))

improve_explore <- function(dafra,switch = 'on', threshold = 0, vector = NULL){
  # this function is the improvement of explore()
  
  data_frame <- na.omit(dafra) #cancel the whole line if there is NA exist
  
  if(!is.data.frame(data_frame)){                 
    data_frame <- as.data.frame(data_frame)  #if it is not dataframe, transfer it into dataframe
  }
  
  
  while(switch != "off" && switch != "on" && switch != "grid"){   #Check to see if switch is valid input, if not, input until valid switch
    print("invalid input for switch")
    switch <- readline(prompt="Enter your option(off / on / grid): ")  #re-enter the input
  }
  
  while(!is.numeric(threshold) || threshold < 0 || threshold >1 ){    #check to see if threshold is a valid input,if not, input until valid threshold
    print("correlation threshold must be numeric and in range [0,1]")
    threshold <- as.numeric(readline(prompt="Enter your correlation threshold: "))   #re-enter the threshold
  }
  
  if(!is.null(vector)){
    if(!is.numeric(vector)||(is.numeric(vector) && (TRUE %in% (vector <= 0)))){ #check if bin vector is all numeric and all not less than 0
      print("the bins vector must be numeric vector and not less than 0, please enter new bins one by one and press 'return' at last to finish input")
      vector <- c()
      bin <- 1
      while(bin != ""){  #input "return"  to finish loop
        bin <- readline(prompt="Enter the number of bins: ")->bin1
        bin1 <- as.numeric(bin1)
        vector <- c(vector, bin1)
      }#re-enter the bin vector
      vector <- na.omit(vector) #cancel the NA
    }
    
    
    if (!is.integer(vector)) {            #Check to see if bins are all integer, if not, round it 
      vector <- round(vector)
    }
  }
  return(explore(data_frame,switch,threshold,vector))
}
improve_explore(diamonds,1,-1,10)#this is wrong input so you need to re-enter later
#improve_explore(diamonds,1,2,c('s'))

#below is what I did, which cannot be fixed when considering the max and min of data on the plots
#I tried my best to fix it but I failed in some parts. it works but with a small error. 
dataexpl<-function(da,plot_switch='on',threshold=c(0.1,0.5),binvector=c(30,80)) 
{#define a data explore function that can loop thorugh all data, show hisstograms, density plots, and some 
  #specied values such as rs-square values and pearson correlation coefficients 
  #paratmeter:data,plot_switch that you can choose on, off, or grid 
  #binvector that is for bins size 
  #threshold for pearson correlation exceeds the range that you set
  
  #define a function6 that has to be here which if I put at the last, it won't work
  #anything binary will show a barplot
  #parameter data, plot_switch
  function6<-function(da,plot_switch="off",newpage=F)
  { #for loop through data and is factor and is logical function choose data
    if (plot_switch=="on" || plot_switch=="grid"){
      for ( t in 1:(ncol(da))){
        if (length(unique(da[,t]))<=2 ){
          barplot(table(da[,t]),col='grey',main=paste(colnames(da)[t],"barplot",sep="-"))
        }
      }
    }
  }
  
  #test
  function6(da,plot_switch)   
  
  grid.newpage() 
  
  
  
  
  
  #define a function1 that gives the table of factored, logical, numeric variables' table 
  #including frequency or summary
  #parameter is da that you enter
  function1<-function(da)
  {for (i in 1:ncol(da))
  {if( is.factor(da[,i]))
  {print(colnames(da)[i]) 
    print(table(da[,i]))}
    if(is.logical(da[,i]))
    {print(colnames(da)[i]) 
      print(table(da[,i]))}   
    if (is.numeric(da[,i]))
    {print(colnames(da)[i]) 
      print(summary(da[,i]))}}
  }
  
  #test 
  function1(da)
  
  
  #define a function2 that gives variable pairs in the first column and r-squre in the 
  #second column
  #parameter is data you enter
  function2<-function(da)
  {colvar<-c()
  rsquare<-c()
  pcor<-c()
  w=1
  #for loop through all data to determine whether they numeric or not 
  #if yes calculate the corresponding r-squre values
  for( j in 1:(ncol(da)-1))
    for (k in (j+1):ncol(da))
      if (is.numeric(da[,j] ) & is.numeric(da[,k]))
      {colvar[w]<-paste(colnames(da)[j],colnames(da)[k],sep="-")#use paste function to paste
      rsquare[w]<-summary(lm(da[,j]~da[,k]))$r.square#extract the r square value from summary
      w=w+1}
  newdata<-data.frame(colvar,rsquare)
  
  #rename the column names
  names(newdata)[1]<-"Variable Pairs"
  names(newdata)[2]<-"R-Square"
  print(newdata)}
  #test
  function2(da)
  
  
  #define a function3 that give the variables pairs in the first column and pearson correlation value
  #which exceeds the threshold in the second column in a data frame format
  #parameter is data you enter
  function3<-function(da)
  {w=1
  colvar2<-c()
  pcorex<-c()
  #for loop through all data to find out the numeric class data 
  for( j in 1:(ncol(da)-1))#for loop through data
    for (k in (j+1):ncol(da))#for loop through data
      if (is.numeric(da[,j] ) & is.numeric(da[,k]))#use is numeric function to determine 
        if (abs(cor(da[,j],da[,k]))>threshold[2] || abs(cor(da[,j],da[,k]))<threshold[1])
        {colvar2[w]<-paste(colnames(da)[j],colnames(da)[k],sep="-")#put all data which satisfied the requisites in cache
        pcorex[w]<-cor(da[,j],da[,k])
        w=w+1}
  
  newdata2<-data.frame(colvar2,pcorex)
  #rename the columns names
  names(newdata2)[1]<-"Variable Pairs"
  names(newdata2)[2]<-"Pearson Exceeds	Threshold"
  
  print(newdata2)
  }
  
  #test 
  function3(da)
  
  
  
  
  grid.newpage()
  
  #define a function4 that gives histograms and density histograms for each numerical variables
  #with mean line on the graph 
  #parameter is da, plot_switch
  function4<-function(da,plot_switch) 
  { binvector<-c(30,50)
  #use ggplot2 draw graphs 
  if(plot_switch=='on')
    for( i in 1:ncol(da))#for loop through all data
      if (is.numeric(da[,i] ) )
        for(j in 1:length(binvector))##for loop through data
        {print(ggplot(da,aes_string(x=da[,i]))+geom_histogram(bins=binvector[j],colour="blue",stat="bin")+
                 geom_vline(xintercept=mean(da[,i]),color="red")+xlab(colnames(da)[i]))#ggplot2 to show histogram
          
          
          print(ggplot(da,aes_string(x=da[,i]))+geom_histogram(aes(y=..density..),bins=binvector[j],colour="blue",stat="bin")+
                  geom_vline(xintercept=mean(da[,i]),color="red")+xlab(colnames(da)[i]))
        }#ggplot2 to show histogram for density
  }
  
  #test 
  function4(da,plot_switch)
  
  
  
  #define a function that can give a grid plots contains all plots which have same bins size
  #in one graph and you can turn it on,off,and grid
  #parameter data, plot_switch
  function5<-function(da,plot_switch)
  {
    histplot<-c()
    densityplot<-c()
    w=1
    k=1
    z=1
    if(plot_switch=='grid')
      for(j in 1:length(binvector)){#for llop through data columns
        for( i in 1:ncol(da))
        {if (is.numeric(da[,i]))#is numeric function to determine
          
        {
          histplot[w]<-list(ggplot(da,aes_string(x=da[,i]))+geom_histogram(bins=binvector[j],colour="blue",stat="bin")+
                              geom_vline(xintercept=mean(da[,i]),color="red")+xlab(colnames(da)[i]))
          #ggplot2 to show histogram
          densityplot[w]<-list(ggplot(da,aes_string(x=da[,i]))+geom_histogram(aes(y=..density..),bins=binvector[j],colour="blue",stat="bin")+
                                 geom_vline(xintercept=mean(da[,i]),color="red")+xlab(colnames(da)[i]))
          w=w+1}#ggplot2 to show histogram
        }
      }
    for (k in seq( (w-1)/length(binvector),(w-1), by=(w-1)/length(binvector) ) )
      #set an another for loop give all same attributed data based on bin size 
    {do.call(grid.arrange, c(histplot[z:k], list(ncol=2)))#use grid arrange to put multiple plots on a graph
      do.call(grid.arrange, c(densityplot[z:k], list(ncol=2)))
      z=z+(w-1)/length(binvector) }
  }
  
  #test 
  function5(da,plot_switch)
  
  
  
  
  
  
}




dataexpl(diamonds,"grid")






