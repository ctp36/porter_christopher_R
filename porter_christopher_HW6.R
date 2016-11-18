#Chris Porter
#Math 510
# Homework 6 ggplot

#Overall Comments: Good Try! I have indicate the potential problem in the code and you have refer to. You can study the example
#                   I have given you and once you correcte your own and those codes really work, I am glad to grade your assignment
#                   second chance.

#First load ggplot2 and set the df to the diamonds dataframe
library(ggplot2)
df = diamonds

#2.) Create a simple scatter plot of Weight ("Carat") and Price using Color (the "Color" column in the diamonds 
#dataframe) as a facet. This might be the precursor for developing a model to predict price given some characteristic 
#like weight. Notice that the  relationship is non-linear. 


# To create the scatter plot I use ggplot, passing
# the carat, price and color through the aes argument.
# I use geom_point to create the scatter plot, 
# and color the dots based on their color variable.
# I edit the title using ggtitle
# I edit the text in the title using theme.
ggplot(df, aes(carat,price, color)) + 
  geom_point(stat="identity", aes(colour= factor(color))) + 
  ggtitle('Diamonds - Weight to Price by Color') +
  theme(plot.title = element_text(color = "Blue", size = 14))

#Comments: I tried the code and it seems doesn't work for me. I think maybe the problem is on "aes(carat, price, color)".
#          Basically speaking I think the ggplot is ploting with layers. You first set up one basic layer with the data of carat
#          and price. So it is not reasonable to see 3 dimensions in basic layer. And I have one good format you may have a look
ggplot(diamonds,aes(carat,price))+geom_point(aes(color=factor(color)))+labs(title="Diamonds-Weight to Price by Color",x='Weight',y='Price')


#3.)
#first I take the natural log, the default of the log function,
# of the price and carat fields.

#I then use the same pattern as above to plot this new scatter plot.
df$logPrice <- log(diamonds$price)
df$logCarat <- log(diamonds$carat)
ggplot(df, aes(logCarat,logPrice, color)) + 
  geom_point(stat="identity", aes(colour= factor(color))) + 
  labs(x="Carat", y="Price") +
  ggtitle('Diamonds - Weight to Price (Linear)') +
  theme(plot.title = element_text(color = "Blue", size = 14))


#4.)
# I use the lm function to create a linear model of
# the log of the Price as a function of the log of the Carat
# amount. And I add this column to the dataframe.
m1 <- lm(df$logPrice~df$logCarat)
df$priceResids <- resid(m1)

# I then use plot techniques as problems 2 and 3.
ggplot(df, aes(logCarat, priceResids, color)) + 
  geom_point(stat="identity", aes(colour= factor(color))) + 
  labs(x="Carat", y="Price") +
  ggtitle('Diamonds - Weight to Price By Color') +
  theme(plot.title = element_text(color = "Blue", size = 14))

#5.)
# First I load the grid package, from which I will use viewports
library(grid)

# I store the residual plot as above to the variable residPlot.
residPlot <- ggplot(df, aes(logCarat, priceResids, color)) + 
                geom_point(stat="identity", aes(colour= factor(color))) + 
                labs(x="Carat", y="Price") +
                ggtitle('Diamonds - Weight to Price By Color') +
                theme(plot.title = element_text(color = "Blue", size = 14))

# Next I create a density histogram of price, where I pass ..density.. to the y argument
# and set my binwidth to 30.
priceHist <- ggplot(df, aes(price)) + 
                geom_histogram(aes(y=..density.., colour= factor(color)), binwidth = 30) +
                theme(legend.position="none", axis.title.x=element_blank(),axis.title.y=element_blank())
# Next I create a density histogram of carat, where I pass ..density.. to the y argument
# and set my binwidth to .02555
caratHist <- ggplot(df, aes(carat)) + 
                geom_histogram(aes(y=..density.., colour= factor(color)), binwidth = 0.02555) +
                theme(legend.position="none", axis.title.x=element_blank(),axis.title.y=element_blank())

#display the residual plot.
residPlot

#Create the viewport
vp <-  viewport()

#define my sub viewport for the price histogram
# and print it
subvp<-viewport(width=.4,height=.3,x=.28,y=.25)
print(priceHist,vp=subvp)


#define my sub viewport for the carat histogram
# and print it
subvp<-viewport(width=.37,height=.25,x=.63,y=.8)
print(caratHist,vp=subvp)
