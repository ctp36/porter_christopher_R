#Overall Comment: I can tell you benefit something from the assignment and this is what we are trying to get! You are really
#making great progress although things are not so perfect currently. There are some problems that I have observed from the code
#and you can refer to my comment.


#1.) Print to the console all methods and attributes associates with a dataframe. 
#   Write code to determine the number of columns in a dataframe


# Use the library function to load the package ggplot2 which contains the diamonds dataset
library(ggplot2)

#Use the attributes function to get all of the attributes of the diamonds dataset
attributes(diamonds)

#Use the class function to get the class of the diamonds dataset
class(diamonds)
#Diamonds has three classes, the tbl_df, tbl, and data.frame class
# use the methods function on the third class, data.frame, to get the methods associated with 
# a data.frame class
methods(class=class(diamonds)[3])


# To get the number of columns in diamonds I'll use the ncol function
ncol(diamonds)
#An alternative way to do this, could be to use the attr function with 'names' as the which argument 
#on the diamonds dataset to get the column names, in a character list.
colNames <- attr(diamonds, 'names')
# then get the length of the list
length(colNames)
# Both get the correct answer of 10.




#2.) Write code to determine how many rows are in a dataframe


#nrow is used to count the number of rows in diamonds.
nrow(diamonds)




#3.) Write code to extract the column names from a dataframe and print the  names 
# of the columns (one per line) to the console.


#Use the attr function as above to get the list of column names from the dataframe.
colNames <- attr(diamonds, 'names')

#Then I'll use a for loop and print statement to loop through this list and print each name to 
#the console.
for (name in colNames) print(name)

#Comments: This way do work but I want to point out that you can actually do this with simple names(diamonds). But it is good to solve 
  #a questions with what you already know.



#4.) Write code to determine the type of each column (numeric, factor, logical, etc.). 
# Print the type of each column to the console.


#I'll use sapply to apply the functions class and type of to get a resulting list showing the class
# and type of each column in the diamonds data frame. class will provide whether it is numeric, integer, ordered factor, etc. 
# and typeof will tell me the specific type of each class.
sapply(diamonds, class)
sapply(diamonds, typeof)


#5.) Write code that will loop through any dataframe and calculate the mean of 
# every numeric column. Label the output with the name of the column.


#for this question I will use sapply again to apply the mean function to each column in diamonds
# I will wrap the na.omit function to remove the logical columns from the resulting vector. Doing it 
# this way creates a labeled vector with the mean of each column and prints the omitted columns to the console.
x <- na.omit(sapply(diamonds, mean))
x
#Comment: Maybe you have some misunderstanding about na.omit() function. na.omit returns the object with incomplete cases removed.
#         so it will not filter out the numeric columns

#6.)Write code that will loop through any dataframe and create a frequency table 
# for every factor column. Label the output with the name of the column


# First I'll use a for loop to loop through each column in the data frame.
# Then I'll use an if statement with is.factor to test if a column is a factor.
# Whtn this is true, the table function is passed the argument col to create 
# frequency tables.  I wrap this inside a print table to print to the console.
for (col in diamonds){
  if (is.factor(col)) {
    print(table(col, dnn=attr(col, 'names')))    
  }
}


#7.) Write code that will loop through any dataframe and determine the number 
# of rows containing NA (missing value) in each column and the percentage of rows containing 
# an NA in any of the columns, HINT: In a single row, zero or more columns may contain an NA. 
# For the percentage of rows containing NA in any column , do not double count NA in rows 
# that contain more than one column with an NA. Print the results to the console. 

for (col in diamonds){
  rowCount  <- length(col)
  naCount   <- sum(is.na(col))
  naPercent <- naCount / rowCount 
  print(names(col))
  print(naCount)
  print(rowCount)  
  print(naPercent)
}
#Comments: You don't need to define so many variables like rowCount, naPercent...just use the expression directly this can
#make you code clear. Define process variables is suitable for quite long loop or function. And one more thing even more
#important is the design of the loop. If you want to loop through the columns you should have something like followings:
  for (i in dim(diamonds)[2]){
    rowCount = length(diamonds[,i])
    naCount = sum(is.na(diamonds[,i]))
    naPercent = naCount/rowCount
    print(names(diamonds)[i])
    print(naCount)
    print(rowCount)
    print(naPercent)
    }

#8.) Create an R function that can accept any dataframe as a parameter and returns a data
# frame that contains each pair of column names in the first column in a single 
# string separated by a -, e.g. for the variables x and y, you should form the string “x - y”
# and their corresponding Pearson correlation coefficient in the second column. 
# Do not repeat any pairs.

colCorr <- function (df){
  # This function takes a dataframe, creates a sub dataframe consisting of the numeric
  # columns in the input df. Then calculates the correlation matrix of each column in the
  # numeric dataframe. It then extracts the unique correlations and columns names to format
  # as an n X 2 data frame, where n is the number of unique combinations of the numeric 
  # columns.  The first column is the correlated columns, and the second column is the 
  # correlation values.
  
  # Remove non-Numeric columns from the dataframe
  nums <- sapply(df, is.numeric)  
  numericDf <- diamonds[ ,nums]
  
  # Create a vector of the columns names used later
  colNames <-attr(numericDf, 'names')  
  
  # Calculate area of the upper triangle to allocate space in for loop 
  colLen <- ncol(numericDf)
  outputLen <- (colLen^2 - colLen) / 2
  
  # Create the pearson correlation matrix of the numeric columns, 
  corMat <- cor(numericDf, method = "pearson")
  
  # Create a logical matrix of the upper triangle of the correlation matrix, no diagonal
  corTriLog <- upper.tri(corMat)
  
  # Name the dimenstions according to the column names of numericDf
  dimnames(corTriLog) <- list(colNames,colNames)
  
  # Create a vector of the unique correlation values, this is the second column of our output DF
  corVec <- corMat[corTriLog]
    
  # Create the Column Name Vector for the output DF
  nameVec <- vector(mode="character", length = outputLen)  
  
  # initialize a counter variable
  n = 1
    
  # Loop through the Logical Matrix going down each column.
  # If the entry in the matrix is true, overwrite each sequential
  # position in the nameVec with the defined format in the paste function ('x-y')
  # nameVec <- paste(colNames,colNames,sep="-")
  # nameVec = nameVec[corTriLog]
  for (y in colNames) {
    for (x in colNames){      
      if (corTriLog[x,y]){         
         nameVec[n] <- paste(x,y,sep="-")
         n = n + 1                
      }      
    }
  }
  
  
  # Create the output dataframe
  corrDf <- data.frame(nameVec,corVec)
  
  #name the columns
  colnames(corrDf) <- c('CorrelatedColumns','Correlation')
  
  #return dataframe
  return(corrDf)
 
}   
  
diamondCorr <- colCorr(diamonds)


# This problem was great! It took me a few hours, but I would have to say it helped me
# Really grasp a lot about how to program in R, and it was an interesting transition from
# R to Python.  I am very interested in how to simplify it just a step further by removing the nested
# for loops.

# Further as an experienced SAS programmer this helped connect a lot of dots between data frames
# and matrices in R, that weren't connected previously. I believe this is a proc with in SAS, so
# it is nice to get under the hood so to speak and figure out how to do a lot of little
# things I take for granted when I am writing SAS, i.e. creating the column names, and designing,
# the output format more explicitly, as opposed to selecting a built-in SAS option.

# This was how i formed the outline of my solution.
#SUDO CODE (Pythonic)
# x = dfName0 as an empty list
# y = dfName1_n as a list of the dataFrames Names
# n = 1
# add y to x in a way that creates the series [ab], [ac, bc], [ad, bd, cd], [ae, be, ce, de], ...
# write this as a df with a string 'x-y[1]' as a columne and
#           the corTri[n] val as a column
# loop through y
#        x = append(y[1]) to x
#        remove y[1] from y         
#        loop through x
#            column1[n] = paste(x, y[1], sep = "-")
#            column2[n] = corTri(n)
#            n += 1
#            if n > corTriLen:break
#            Now Just need to 

