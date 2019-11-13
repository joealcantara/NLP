###--- Function to remove features ---###
# Input - Dataset
# Output - Dataset cleaned 
removeBadFeatures = function(df){
  df$JDate = NULL
  df$X = NULL
  df$index = NULL
  df$Julian = NULL
  df$Filename = NULL
  df$Date = NULL
  return(df)
}

###--- Function to create a dataframe ---###
# Input - Dataframe Name
# Output - Dataframe
createDataFrame = function(){
  x = data.frame(feature = character(),
                        r2      = numeric(),
                        pvalue  = numeric(),
                        stringsAsFactors = FALSE)
  return(x)
}

###--- Function to run correlations ---###
# Input - Data and Dataframe to put results in
# Output - Dataframe with results
correlations = function(data, df){
  for(i in seq_along(names(data))){
    if(is.numeric(data[, i])){
      pearson = cor.test(data$Days, data[, i], method = "pearson")
      df = rbind(df, data.frame(feature = names(data)[i],
                                            r2      = unname(pearson$estimate),
                                            pvalue  = pearson$p.value,
                                            stringsAsFactors = FALSE))
    }
  }
  return(df)
}

###--- Function to calculate Familywise Error Rates and False Discovery rates---###
# Input - dataframe with correlations
# Output - dataframe with added results for Familywise Error Rates and False Discovery rates
fdr = function(df){
  df$Bonferroni = p.adjust(df$pvalue, method = "bonferroni")
  df$BH = p.adjust(df$pvalue, method = "BH")
  df$holm = p.adjust(df$pvalue, method = "holm")
  df$hochberg = p.adjust(df$pvalue, method = "hochberg")
  df$hommel = p.adjust(df$pvalue, method = "hommel")
  df$BY = p.adjust(df$pvalue, method = "BY")
  return(df)
}

cleanData = function(data){
  data = removeBadFeatures(data)
  return(data)
}

preprocess = function(data){
  df = createDataFrame()
  df = correlations(data, df)
  df = df[order(df$pvalue),]
  df = fdr(df)
  return(df)
}