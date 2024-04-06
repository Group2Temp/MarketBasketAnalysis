##### Load in Packages
library(ggplot2) 
library(lubridate)
library(zoo)
library(tidyverse)
library(arules)
library(arulesViz)
library(plotly)

##### Read in Dataset

######
groceries_basket <- tibble(read.csv("basket.csv"), header = FALSE)

transactions <- as(groceries_basket, "transactions")

transactions <- read.transactions('basket.csv', sep = ',', rm.duplicates = TRUE)

#####

itemFrequencyPlot(transactions, topN = 60)


#Apriori Algorithm of Agrawal and Srikant
rules <- apriori(transactions, parameter = list(support = 0.003, confidence = 0.1))

print(rules)
#set of 324 rules found

# Plot the rules
plot(rules, method = "scatter")

# Extracting association rules
association_df <- as(rules, "data.frame")

#extract antecedent and consequent itmes
antecedents <- lhs(rules)
consequents <- rhs(rules)

# Extract antecedents, consequents, support, confidence, and lift
antecedents <- labels(lhs(rules))
consequents <- labels(rhs(rules))
support <- quality(rules)$support
confidence <- quality(rules)$confidence
lift <- quality(rules)$lift

# Create a dataframe
association_df <- data.frame(Antecedents = I(antecedents), 
                             Consequents = I(consequents),
                             Support = support,
                             Confidence = confidence,
                             Lift = lift)

# Order the dataframe by lift (descending)
association_df <- association_df[order(association_df$Lift, decreasing = TRUE), ]


# Print the dataframe
print(association_df)

