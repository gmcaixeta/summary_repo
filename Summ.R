# Packeges - Data Manipulation --------------------

library(reshape2)
library(ggthemes)
library(Rcpp)
library(tibble)
library(tidyverse)
library(lubridate)
library(modelr)
library(data.table)
library(sjPlot)
library(plyr)
library(ggplot2)


# First Contact ---------------------------------------------------------------------

df <- Hao.casa.certo.5
as_tibble(df) #convert dataframe to tibble 
attach(df)
summary(df)

a1 <- sjt.frq(TD)
a2 <- sjt.frq(A)


a1

a <- as.numeric(b)
binnedSamples <- cut(PTD, breaks = c(0.0,  400.0, 600.0, 800.0, 1000.0, 1300, 10^6))
b <- sjt.frq(binnedSamples)
b  
c <- as.numeric(binnedSamples)

p1 <- ggplot(df, aes(PTD)) +
  geom_freqpoly(binwidth = 100)

p2 <- ggplot(df, aes(A)) +
  geom_freqpoly(binwidth = 100)

p3 <- ggplot(df, aes(TD)) +
  geom_density(alpha = 0.7)

p4 <- ggplot(df, aes(G)) +
  geom_histogram(binwidth = 1)

p5 <- ggplot(df, aes(Q)) +
  geom_histogram(binwidth = 1)



ggplot(df, aes(pred )) +
  geom_histogram(binwidth = 100)

ggplot() +
  geom_histogram(data = b, aes(PTD), binwidth = 2) +
  scale_fill_brewer(palette = "Spectral")



boxplot(b, notch=TRUE, varwidth=TRUE, col="red", main = "Resumo", xlab = "Dados", ylab = "Valores", boxwex=.6, frame.plot=FALSE)


pred <- cbind(PTD, A, TD, G, Q, D)

multiplot(a1, a2, cols=2)

# Which predictors are associated with the response?
# What is the relationship (positive/negative) between the response and each predictor?
# Can the relationship between Y and each predictor be adequately summarized using a linear equation, or is the relationship more complicated?

# Data Manipulation------------------------

b <- select(df, PTD, A, TD, G, Q, D, Y12, Y13, Y14, Y15, Y16) #Pick/remove columns (variables)


df1 <- df %>% drop_na() #Remove nas


select() #Pick/remove columns (variables)
filter() #Pick/remove rows (observations) 

fix(df1)


arrange() #Reorder the rows
mutate() #Create new columns (variables) with functions of existing variables
summarise() #Collapse many values down to a single summary

#--- Charts ----------------------------------

# Statistics-----------------------------------------------------------------------------------
library("car")
library(stargazer)

#Normalidade
#Plotagem dos residuos 


# Definir variaveis

pred <- cbind(narr86,pcnv,ptime86,qemp86)

# Definir regressoes 

reg1<-lm(Y~X)
summary(reg1)

# Exportar Latex 

stargazer(a, title="Results", align=TRUE)

# Pressupostos ----

library(gvlma)
gvmodel <- gvlma(reg1) 
summary(gvmodel)

#lineariedade
spm(pred)

#Correlacao
mat <- cor(pred)
print(mat)

# Multicolineariedade 
vif(reg1)

# Homocedasticity

yhatsq<-(fitted(reg1) ^ 2)
uhatsq<-(resid(reg1) ^ 2)
uhat<-(resid(reg1))
reg7 <-lm(uhatsq~yhatsq)
summary(reg7)
plot(X,uhat)


bptest(X~Y,data=data1)
ncvTest(reg1)
spreadLevelPlot(reg1)

# Robust Test

robust <-coeftest(reg1, vcovHC(reg1, type = "HC0"))
robust

#-----------------------------------------------------


# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
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



