library(OECD)
library(tseries)
library(stats)
library(forecast)
library(ggplot2)
library(ggfortify)
library(plm)
library(urca)
library(xtable)

dataset <- "STLABOUR"
data_str <- get_data_structure(dataset)
filter <-  "BEL+FRA+DEU+IRL+ITA+LUX+NLD+ESP+PRT+AUT+FIN.LRHUTTFE+LRHUTTMA+LRHUTTTT.ST+STSA.Q"
STLABOUR <- get_dataset(dataset,filter = filter, start_time = 1998)
country_list <- c("BEL","FRA","DEU","IRL","ITA","LUX","NLD","ESP","PRT","AUT","FIN")
country_names <- c("Belgium","France","Germany","Ireland","Italy","Luxemburg","Netherlands","Spain","Portugal","Austria","Finland")
subsets <- c("LRHUTTMA", "LRHUTTFE", "LRHUTTTT")
measures <- c("STSA","ST")
Dataset <- STLABOUR


#################################################################

##This section prints all the graphs for the descriptives section

#################################################################

pathnameGRAPHS <- gsub("/R","/LaTeX/Graphs/",getwd())

STSA_FE_MA_TT_facet_plot <- ggplot(data = Dataset[Dataset$MEASURE=="STSA",], aes(x = as.numeric(as.yearqtr(obsTime, format="%Y-Q%q")), y = obsValue, color = SUBJECT)
) + geom_line() + facet_wrap(~LOCATION) + ylab("Unemployment rate %") + xlab("")+ scale_colour_manual(
  labels = c("Female","Male","Total"), values = c("red","green", "blue"), name="Sex") + labs(
    caption = "(Source: STLABOUR database. OECD (2016))") + theme(legend.position = c(0.855,0.12) )

ggsave(STSA_FE_MA_TT_facet_plot,filename = "STSA_FE_MA_TT_facet.pdf", path = pathnameGRAPHS , height = 4, width = 7,units = "in")

ST_FE_MA_TT_facet_plot <- ggplot(data = Dataset[Dataset$MEASURE=="ST",], aes(x = as.numeric(as.yearqtr(obsTime, format="%Y-Q%q")), y = obsValue, color = SUBJECT)
) + geom_line() + facet_wrap(~LOCATION) + ylab("Unemployment rate %") + xlab("")+ scale_colour_manual(
  labels = c("Female","Male","Total"), values = c("red","green", "blue"), name="Sex") + labs(
    caption = "(Source: STLABOUR database. OECD (2016))") + theme(legend.position = c(0.855,0.12) )

ggsave(STSA_FE_MA_TT_facet_plot,filename = "ST_FE_MA_TT_facet.pdf", path = pathnameGRAPHS, height = 4, width = 7,units = "in")




#This loop goes through the subsets and measures and complies and saves all the necessary plot for each combination

for(subject in subsets) {
  
  #This is the faceted plot of seasonally adjusted and unadjusted data
  STSA_SA_facet_plot <- ggplot(data = Dataset[Dataset$SUBJECT==subject,], aes(x = as.yearqtr(obsTime, format="%Y-Q%q"), y = obsValue, color = MEASURE)
  ) + geom_line() + facet_wrap(~LOCATION) + ylab("Unemployment rate %") + xlab("") + scale_colour_manual(
    labels = c("Non-adjusted", "Adjusted"),values=c("red","black"),name="") + theme(legend.position = c(0.855,0.12) )
  
  ggsave(STSA_SA_facet_plot, filename = paste("ST_STSA_",sub("LRHUTT","",subject),"_facet.pdf",sep = ""), path = pathnameGRAPHS,
         height = 4, width = 7,units = "in")
  
  for(measure in measures) {
    
    #This is the autocovariance-plot for each subject group for both adjusted and unadjusted data
    acf_table <- data.frame()
    for(i in country_list) {
      a <- acf(Dataset$obsValue[Dataset$LOCATION==i & Dataset$SUBJECT==subject & Dataset$MEASURE==measure],plot=FALSE,lag.max = 50)
      acf_table <- rbind(acf_table, data.frame(lag=a$lag, acf=a$acf, rep(i,length(a$acf))))
    }
    colnames(acf_table) <- c("lag","acf","Country")
    acfplot <- ggplot(acf_table, aes(lag, acf)) + geom_area(fill="grey") +
      theme_bw() + facet_wrap(~Country)
    
    ggsave(acfplot,filename = paste(measure,"_", sub("LRHUTT","",subject),"_ACF_facet.pdf",sep = ""), path = pathnameGRAPHS
           ,height = 4, width = 7,units = "in")
    
    #This is the first difference plot for each subject group for both adjusted and unadjusted data
    diffset <- Dataset[Dataset$MEASURE==measure & Dataset$SUBJECT==subject,]
    diffset$obsValue <- c(0,diff(diffset$obsValue))
    diffset <- diffset[!(diffset$obsTime=="1998-Q1"),]
    
    diffplot <- ggplot(data = diffset[diffset$SUBJECT==subject & diffset$MEASURE==measure,], 
                       aes(x = as.yearqtr(obsTime, format="%Y-Q%q"), y = obsValue)) +
      geom_line() + facet_wrap(~LOCATION) + ylab(" Change in Unemployment rate %") + xlab("")
    
    ggsave(diffplot,filename = paste(measure,"_", sub("LRHUTT","",subject),"_diff_facet.pdf",sep = ""), path = pathnameGRAPHS,
           height = 4, width = 7,units = "in") 
  }
}

####################################################################################################

## This section does the econometric tests and prits all the result tables into the Tables directory
## and lists the countries that produce statistically significant statistic values

####################################################################################################

pathnameTABLES <- gsub("/R","/LaTeX/Tables2/",getwd())

measure <- "STSA"

critical_countries <- character(0)
IPS_table <- numeric(0)
MW_table <- numeric(0)
diagtable <- numeric(0)
for(subject in subsets) {

  Dataset <- STLABOUR[STLABOUR$SUBJECT== subject & STLABOUR$MEASURE==measure,]
  order_table <- data.frame(matrix(nrow=length(country_list),ncol = 2))
  
  rownames(order_table) <- country_names
  colnames(order_table) <- c("$p_i$","T")
  for(i in 1:length(country_list)) {
    a <- ts(Dataset$obsValue[Dataset$LOCATION==country_list[i]])
    a <- (a/100)
    a <- log(a/(1-a)) 
    model <- ar.ols(a,aic=TRUE,order.max = 8,intercept = TRUE)
    order_table[i,1] <- model$order
    order_table[i,2] <- length(a)
    
    
    
  
  }

  u_table <- data.frame(matrix(nrow = 75,ncol = length(country_list)))
  res_table <- data.frame(matrix(nrow = 75,ncol = length(country_list)))
  adf_table <- data.frame(matrix(nrow=length(country_list),ncol = 2))
  colnames(u_table) <- country_list
  colnames(res_table) <- country_list
  adf_table[,2] <- rep("",length(adf_table[,2]))
  for(i in 1:length(country_list)) {
  a <- ts(Dataset$obsValue[Dataset$LOCATION==country_list[i]])
  a <- (a/100)
  a <- log(a/(1-a)) 
  b <- ur.df(a,lags = order_table[i,1],type = "drift")
  adf_table[i,1] <- b@teststat[1]
  u_table[,i] <- a
  res_table[,i] <- c(rep(0,75-length(b@res)),b@res)
  
  if (b@teststat[1] < (-2.58)){
    adf_table[i,2] <- "*"
    critical_countries <- rbind(critical_countries, c(country_list[i],subject,order_table[i,1]))
  } 
  if (b@teststat[1] < (-2.89)) adf_table[i,2] <- "**"
  if (b@teststat[1] < (-3.51)) adf_table[i,2] <- "***"
  
  }

  results <- cbind(order_table[,c(1,2)],paste(format(adf_table[,1],digits = 4),adf_table[,2]))
  colnames(results)[3] <- "ADF"
  rownames(results) <- country_names


  printout <- xtable(results)
  res_correlations <- xtable(cor(res_table[10:75,],method = "pearson"))

  print.xtable(res_correlations, file = paste(pathnameTABLES,measure,"_",sub("LRHUTT","",subject),"_CORRtable.txt",sep = ""),
             floating = FALSE, sanitize.text.function=function(x){x})

  
  comm <- paste0("\\hline \n \\multicolumn{4}{l}",
               "{\\scriptsize{Signifigant at level 0.1(*),0.05(**),0.01(***)}} \n")

  print.xtable(x = printout,
             floating = FALSE,
             hline.after = c(-1, 0),
             add.to.row = list(pos = list(nrow(results)),command=comm),
             sanitize.text.function=function(x){x},
             file = paste(pathnameTABLES,measure,"_",sub("LRHUTT","",subject),"_ADFtable.txt",sep = ""))

  IPStest <- purtest(u_table,lags = order_table[,1],test = "ips",exo = "intercept",dfcor = TRUE)
  MWtest <- purtest(u_table,lags = order_table[,1], test = "madwu",exo = "intercept", dfcor = TRUE)
  IPS_table <- rbind(IPS_table, c(IPStest$statistic$statistic, IPStest$statistic$p.value))
  MW_table <- rbind(MW_table, c(MWtest$statistic$statistic, MWtest$statistic$p.value))
}

rownames(IPS_table) <- c("Male","Female","Total")
colnames(IPS_table) <- c("$\\hat{t}$", "p-value")

rownames(MW_table) <- c("Male","Female","Total")
colnames(MW_table) <- c("$p_\\lambda$", "p-value")

print.xtable(xtable(IPS_table), file = paste(pathnameTABLES,"IPS_table.txt",sep=""),floating = FALSE,
             sanitize.text.function=function(x){x})

print.xtable(xtable(MW_table), file = paste(pathnameTABLES,"MW_table.txt",sep=""),floating = FALSE,
             sanitize.text.function=function(x){x})


#############################################################################################

## This section does additional diagnostics for the countries that reject the null hypothesis

#############################################################################################
Dataset <- Dataset <- STLABOUR[STLABOUR$MEASURE=="STSA",]
max_lag <- 50
IRFframe <- data.frame()

for(i in 1:length(critical_countries[,1])) {
  country <- critical_countries[i,1]
  subject <- critical_countries[i,2]
  order <- as.numeric(critical_countries[i,3])
  a <- ts(Dataset$obsValue[Dataset$LOCATION==country & Dataset$SUBJECT==subject])
  a <- (a/100)
  a <- log(a/(1-a)) 
  b <- ar.ols(a,aic = FALSE,order.max = order,intercept = TRUE)
  frame <- data.frame( cbind( seq(0,max_lag,1), c(1,ARMAtoMA(ar = b$ar, lag.max = max_lag))))
  frame <- cbind(rep(paste(country,sub("LRHUTT","",subject ),sep = ", "),max_lag+1 ),frame )
  colnames(frame) <- c("LOCATION", "lag", "IRF")
  IRFframe <- rbind(IRFframe, frame)
  
  plot <- ggplot(data = frame, aes(x = lag,y = IRF)) + geom_line() + abline(yintercept=0.5) + 
            geom_hline(yintercept = c(0.5,-0.5), linetype = "dashed",size=0.3) + ggtitle(paste("Impulse Respose Function for ", country,", ",sub("LRHUTT","",subject )))
  
  ggsave(plot, filename = paste("STSA_",sub("LRHUTT","",subject ), "_",country,"_IRF.pdf",sep=""), path = pathnameGRAPHS)
}
STSA_IRF_facet <-  ggplot(data=IRFframe, aes(x = lag, y = IRF)) + geom_line() + facet_wrap(~LOCATION) + geom_hline(yintercept = c(0.5,-0.5), linetype = "dashed",size=0.3) 
ggsave(STSA_IRF_facet, filename = "STSA_IRF_facet.pdf", path = pathnameGRAPHS)
  
 
plot(seq(0,2000,1),c(1,ARMAtoMA(ar = model$ar,lag.max = 2000)),type="h")




################################################################################################

# In this section I download data concerning additional descriptives and print graphs and tables

################################################################################################



# Here the data on labour force participation rate is downloaded from the OECD's LFS database and saved into a data frame
dataset <- "LFS_SEXAGE_I_R"
filter3 <-  "AUT+BEL+FIN+FRA+DEU+IRL+ITA+LUX+NLD+PRT+ESP+OECD+EUR+USA+UK.MEN+WOMEN+MW.1564.LFPR.A"
LFSPAR <- get_dataset(dataset, filter = filter3, start_time = 1992) 


Dataset <- LFSPAR
Dataset$PARDIFF <- Dataset$obsValue[Dataset$SEX=="MEN",] - Dataset$obsValue[Dataset$SEX=="WOMEN",]

LFPR_2000 <- ggplot(data = Dataset[Dataset$obsTime==1998 & Dataset$SEX=="WOMEN",], aes(x = reorder(COUNTRY,obsValue), y = obsValue)) + geom_bar(stat = "identity",width = 0.5)

LFPR_SEX_facet <- ggplot(data = Dataset, aes(x = as.numeric(obsTime), y = obsValue , color =SEX) ) + geom_line() + facet_wrap(~COUNTRY) + ylab("Labour Market Participation Rate %") + xlab("")+ scale_colour_manual(
  labels = c("Male","Total","Female"), values = c("blue","green", "red"), name="Sex") + labs(caption = "(Source: LFS_SEXAGE_I_R database. OECD (2016))") + theme(legend.position = c(0.855,0.12) )

ggsave(LFPR_SEX_facet,filename = "LFPR_FE_MA_TT_facet.pdf", path = pathnameGRAPHS , height = 4, width = 7,units = "in")



