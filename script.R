#reading in the data
chaff <- read.delim("~/Documents/Uni/Data Science/workshop 1/workshop 1/Raw_data/chaff.txt")
View(chaff)

#tidying data
library(tidyverse)
chaff2<- chaff%>% 
  pivot_longer(names_to = "sex", 
               values_to = "mass",
               cols = everything())

# saving processed data
file <-  "Processed_data/chaff2.txt"
write.table(chaff2, 
            file, 
            quote = FALSE,
            row.names = FALSE)

#statistical analysis
t.test(chaff2$mass ~ chaff2$sex)
