#Apriori Algorithm Implementation Attempt 3


#Data Preprocessing Block
#install.packages('arulesViz')

#Importing All Required Libraries
library(arules)
library(RColorBrewer)
library(ggplot2)
library("arulesViz")

#Importing the Dataset
olddataset = read.csv('Salesstore_Edited.csv')
#Dropping Unnecessary Columns
dataset = subset(olddataset, select = -c(Order_ID, Sales))


#Segregating Dataset
#Calculations for Average and Standard Deviation
avg_orderquantity = mean(dataset$Order_Quantity)
avg_Profit = mean(dataset$Profit)

sd_orderquantity = sd(dataset$Order_Quantity)
sd_Profit = sd(dataset$Profit)

#Segregating Order Quantity based on Average and Standard Deviation
dataset$Order_Quantity = ifelse (dataset$Order_Quantity <= (avg_orderquantity - sd_orderquantity), 
                                 0, dataset$Order_Quantity)
dataset$Order_Quantity = ifelse (dataset$Order_Quantity > (avg_orderquantity - sd_orderquantity) & dataset$Order_Quantity <= avg_orderquantity ,
                                 1 ,dataset$Order_Quantity)
dataset$Order_Quantity = ifelse (dataset$Order_Quantity <= (avg_orderquantity + sd_orderquantity) & dataset$Order_Quantity > avg_orderquantity ,
                                 2, dataset$Order_Quantity)
dataset$Order_Quantity = ifelse (dataset$Order_Quantity >= (avg_orderquantity + sd_orderquantity) , 
                                 3, dataset$Order_Quantity)

#Segregating Profit based on Requuired Conditions
dataset$Profit = ifelse (dataset$Profit <= 0,
                         0,dataset$Profit)
dataset$Profit = ifelse (dataset$Profit <= avg_Profit & dataset$Profit > 0 ,
                         1,dataset$Profit)
dataset$Profit = ifelse (dataset$Profit <= (avg_Profit + sd_Profit) & dataset$Profit > avg_Profit ,
                         2,dataset$Profit)
dataset$Profit = ifelse (dataset$Profit >= (avg_Profit + sd_Profit) ,
                         3,dataset$Profit)

dataset$Order_Quantity = factor(dataset$Order_Quantity, 
                                levels = c(0,1,2,3), 
                                labels = c("Small_OrderQuantity","Medium_OrderQuantity","Large_OrderQuantity","VLarge_OrderQuantity"))

dataset$Profit = factor(dataset$Profit, 
                       levels = c(0,1,2,3), 
                       labels = c("Loss","Low_Profit","Large_Profit","VLarge_Profit"))


#Data Preprocessing
#Converting new Dataset to CSV file
write.table(dataset,"SalesApriori.csv",quote = FALSE, row.names=FALSE, col.names = FALSE, sep = ",")

#Reading newly created Dataset
dataset = read.csv("SalesApriori.csv", header = FALSE)

#Apriori Library expects in Transaction Format
dataset = read.transactions("SalesApriori.csv", sep = ',', rm.duplicates = TRUE)
summary(dataset)

#Displaying 50 most Frequent Itemsets
itemFrequencyPlot(dataset, topN = 50)


#Training Apriori on the Dataset
rules = apriori(data = dataset, parameter = list(support = 0.5, confidence = 0.5))

#Plotting Graphs corresponding to Rules
args(getS3method("plot", "rules"))
plot(rules)

subrules = rules[quality(rules)$confidence > 0.8]
plot(subrules, method = "matrix", measure = "lift")

subrules2 = head(rules, n = 10, by = "lift")
plot(subrules2, method = "graph")

saveAsGraph(head(rules, n = 1000, by = "lift"), file = "rules.graphml")
plot(subrules2, method = "paracoord")

#Inspecting Rules Created
inspect(sort(rules, by = 'lift')[1:10])

