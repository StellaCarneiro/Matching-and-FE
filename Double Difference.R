
## Com .25 e .75 da certo. change size definition, ok tb.

#Did exporting have an effect on firms cash-flow during the crisis?

rm(list = ls())

library(lfe)

library(MASS)

library(weights)

library(reshape2)

library(MatchIt)

library(optmatch)

library(lsr)

library(RItools)

library(Hmisc)

library(plm)

library(survey)

library(WriteXLS)

library(stargazer)

library(dplyr)

library(tidyr)

library(broom)

library(cobalt)

library(splitstackshape)

library(plyr)

library(Hmisc)

library(ggplot2)

library(scales)

library(ggthemes)

library(gtable)

library(ggrepel)

library(sandwich)

library(lmtest)

### Difference-in-difference in R ###

setwd("/Users/stellacarneiro/Desktop")

df <- read.csv("Data.Long Panel_Quali.txt",header = TRUE,sep = " ")

df[8:32] <- lapply(df[8:32], function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})

sapply(df, class)

df <- subset(df, df$Export.Rate == 0 | Export.Rate >= 0.5)

#Removing outliars

# Panel de long para wide#

constant.variables <- c("Company.ID","Sector","Size.GFIP","Size.by.Assets.and.Sector","Size.by.Assets","Ownership","Overall.Sector")

df.wide <- reshape(df, idvar = constant.variables, timevar = "Year", direction = "wide",sep = "_")



# Transforming Size into a dummie:

df.wide$Sector.Dummy <- as.numeric(factor(df.wide$Sector))

df.wide$Size.Assets.Dummy <- as.numeric(factor(df.wide$Size.GFIP))
 
df.wide$Size.GFIP.Dummy <- as.numeric(factor(df.wide$Size.by.Assets))



######################################################################################

# Definindo variaveis - Treatment

df.wide$treated <- ifelse(df.wide$Export.Rate_2009 >= 0.5, 1, 0)

cash.asset <- sort(colnames(df.wide[ ,grep("Real.CF.to.K.in.t.1",colnames(df.wide))])[-c(1)])

cov.columns <- c("Size.Assets.Dummy",
                 "Size.GFIP.Dummy",
                 "Sector.Dummy")

selected.columns <- c("Company.ID","treated",cov.columns)

df.wide.sel <- df.wide[ ,selected.columns]

df.wide.sel <- na.omit(df.wide.sel)


#Calculates the propensity score


ps <- glm(formula = treated ~ Size.Assets.Dummy + Size.GFIP.Dummy + Sector.Dummy, family = binomial(), data = df.wide.sel)

summary(ps)

#Some authors suggest that the final model should include not only statistically significant variables, 
#but also variables known to be associated with selection.

# Attach the predicted propensity score to the datafile

df.wide.sel$psvalue <- predict(ps, type = "response")

# Match using nearneighbor

m.nn <- matchit(treated ~ Size.Assets.Dummy + Size.GFIP.Dummy + Sector.Dummy, data = df.wide.sel, replace=TRUE, method = "nearest", ratio = 4)

# index function contains the Company.ID of treated and the others the matched controls

match.data <- match.data(m.nn)

###### Reshape again the matched data #######

id <- colnames(match.data)

match.data.merge <- cbind(match.data[ ,id],rep("2009",length(match.data$Company.ID)))

colnames(match.data.merge)[9] <-c("Year") 

df$Company.ID <- as.numeric(df$Company.ID)

df <- merge(df,match.data.merge,by=c("Company.ID","Year"),all=T)

Treated <- NULL

for (i in 1:length(df$Company.ID)){
  if (df$Company.ID[i] %in% match.data$Company.ID == T) {
    Treated[i]  <- match.data$treated[which(match.data$Company.ID == df$Company.ID[i], arr.ind=TRUE)] 
  } else{ Treated[i] <- NA
  }
}

df["Treated"] <- Treated

#########

Weights.Long <- NULL

for (i in 1:length(df$Company.ID)){
  if (df$Company.ID[i] %in% match.data$Company.ID == T) {
    Weights.Long[i]  <- match.data$weights[which(match.data$Company.ID == df$Company.ID[i], arr.ind=TRUE)] 
  } else{ Weights.Long[i] <- NA
  }
}

df["Weights.Long"] <- Weights.Long


#####################################################################

# Creating interactions

df$Treated_PreYear2008 <- ifelse((df$Year == 2008) & df$Treated == 1, 1, 0) 

df$Treated_PostYear2009 <- ifelse((df$Year == 2009) & df$Treated == 1, 1, 0) 

df$Treated_PostYear2010 <- ifelse((df$Year == 2010) & df$Treated == 1, 1, 0) 

df$Treated_PostYear2011 <- ifelse((df$Year == 2011) & df$Treated == 1, 1, 0) 

df$Treated_PostYear2012 <- ifelse((df$Year == 2012) & df$Treated == 1, 1, 0) 

df$Treated_PostYear2013 <- ifelse((df$Year == 2013) & df$Treated == 1, 1, 0) 


df$Treated_Cash2008 <- df$Treated_PreYear2008*df$Real.CF.to.K.in.t.1

df$Treated_Cash2009 <- df$Treated_PostYear2009*df$Real.CF.to.K.in.t.1

df$Treated_Cash2010 <- df$Treated_PostYear2010*df$Real.CF.to.K.in.t.1

df$Treated_Cash2011 <- df$Treated_PostYear2011*df$Real.CF.to.K.in.t.1

df$Treated_Cash2012 <- df$Treated_PostYear2012*df$Real.CF.to.K.in.t.1

df$Treated_Cash2013 <- df$Treated_PostYear2013*df$Real.CF.to.K.in.t.1

dtt <- data.table(df)

df <- dtt[,lagSales := lag(Real.Sales.Growth,1), by = c("Company.ID")]



#### Exclude the non-matched ####

write.csv(df,"/Users/stellacarneiro/Desktop/Data.Long.Final.csv")

df.matched <- subset(df, !(is.na(df$Weights.Long)))

write.csv(df.matched,"/Users/stellacarneiro/Desktop/Matched.Sample.csv")


#### Create histograms ####


est.inv1 <- felm(formula = Real.Investment ~ Treated_Cash2009 + Treated_Cash2010
                 + Treated_Cash2011 + Treated_Cash2012  
                 + Treated_PostYear2009 + Treated_PostYear2010 + Treated_PostYear2011 + Treated_PostYear2012
                 + Real.CF.to.K.in.t.1 | Company.ID + Year, df.matched, exactDOF=TRUE, weights = df.matched$Weights.Long)

est.inv2 <- felm(formula = Real.Investment ~ Treated_Cash2009 + Treated_Cash2010
                + Treated_Cash2011 + Treated_Cash2012
                + Treated_PostYear2009 + Treated_PostYear2010 + Treated_PostYear2011 + Treated_PostYear2012
                + Real.CF.to.K.in.t.1 + lagSales  | Company.ID + Year, df.matched, exactDOF=TRUE, weights = df.matched$Weights.Long)




### To Latex

est.inv1er <- summary(est.inv1, robust = T)$coefficients[ ,4]

est.inv2er <- summary(est.inv2, robust = T)$coefficients[ ,4]


stargazer(est.inv1,est.inv2,title="Fixed Effects Estimation ",dep.var.labels=c("Investment","Investment"),
          covariate.labels = c( "Treatment*Cash-Flow*Year2009", "Treatment*Cash-Flow*Year2010", 
                                "Treatment*Cash-Flow*Year2011", "Treatment*Cash-Flow*Year2012", 
                                "Treatment*Year2009", "Treatment*Year2010","Treatment*Year2011",
                                "Treatment*Year2012","Cash Flow /Fixed Assets",   
                                "Lag Sales"), 
          se = list(est.inv1er, est.inv2er),
          align=TRUE,
          omit.table.layout = "n")

############# Split the Sample BY SIZE GFIP ##############

df.Small <- df.matched[df.matched$Size.GFIP == "Small", ] 

df.Medium <- df.matched[df.matched$Size.GFIP == "Medium", ] 

df.Large <- df.matched[df.matched$Size.GFIP == "Large", ] 

est.small <- felm(formula = Real.Investment ~ Treated_Cash2009 + Treated_Cash2010
                 + Treated_Cash2011 + Treated_Cash2012 
                 + Treated_PostYear2009 + Treated_PostYear2010 + Treated_PostYear2011 + Treated_PostYear2012
                 + Real.CF.to.K.in.t.1 + lagSales  | Company.ID + Year, df.Small, exactDOF=TRUE, weights = df.Small$Weights.Long)

est.medium <- felm(formula = Real.Investment ~ Treated_Cash2009 + Treated_Cash2010
                  + Treated_Cash2011 + Treated_Cash2012 
                  + Treated_PostYear2009 + Treated_PostYear2010 + Treated_PostYear2011 + Treated_PostYear2012
                  + Real.CF.to.K.in.t.1 + lagSales  | Company.ID + Year, df.Medium, exactDOF=TRUE, weights = df.Medium$Weights.Long)

est.large <- felm(formula = Real.Investment ~ Treated_Cash2009 + Treated_Cash2010
                   + Treated_Cash2011 + Treated_Cash2012 
                   + Treated_PostYear2009 + Treated_PostYear2010 + Treated_PostYear2011 + Treated_PostYear2012
                   + Real.CF.to.K.in.t.1 + lagSales  | Company.ID + Year, df.Large, exactDOF=TRUE, weights = df.Large$Weights.Long)


est.invser <- summary(est.small,robust = T)$coefficients[ ,4]

est.invmer <- summary(est.medium,robust = T)$coefficients[ ,4]

est.invler <- summary(est.large,robust = T)$coefficients[ ,4]


stargazer(est.small, est.medium, est.large,title="Fixed Effects Estimation: Dividing the Sample by Size (Number of Employees)",dep.var.labels=c("Investment","Investment"),
          covariate.labels = c( "Treatment*Cash-Flow*Year2009", "Treatment*Cash-Flow*Year2010", 
                                "Treatment*Cash-Flow*Year2011", "Treatment*Cash-Flow*Year2012", 
                                "Treatment*Year2009", "Treatment*Year2010","Treatment*Year2011",
                                "Treatment*Year2012","Cash Flow /Fixed Assets",   
                                "Lag Sales"), 
          se = list(est.invser,est.invmer,est.invler),
          align=TRUE,
          omit.table.layout = "n")




