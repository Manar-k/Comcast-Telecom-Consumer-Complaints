setwd("C:/Users/loka1/Desktop")
getwd()

#Import data into R environment
Complaints_data <- read.csv("Comcast Telecom Complaints data.csv")
View(Complaints_data)
str(Complaints_data)
summary(Complaints_data)

#libraries
library(lubridate)
library(ggplot2)
library(plyr)
library(dplyr)

#compute number of tickets daily
Complaints_data$Date <- dmy(Complaints_data$Date)
tb1 <- Complaints_data %>% group_by(Date) %>% summarise(counts = n())
tb1
#the trend chart for the number of complaints at daily levels
ggplot(tb1, aes(Date, counts)) + 
  geom_point() + 
  geom_line(aes(group=1)) + geom_point() + theme_bw()+
  xlab("Days") + 
  ylab("No. of Tickets") +
  theme(axis.text.x = element_text(angle = 90))

# compute number of tickets monthly
tb2 <- Complaints_data %>% mutate(month = months(Date)) %>% group_by(month) %>% summarize(counts = n())
tb2

#the trend chart for the number of complaints at monthly levels
ggplot(tb2, aes(month, counts)) + 
  geom_point() + 
  geom_line(aes(group=1)) + geom_point() + theme_bw()+
  xlab("Months") + 
  ylab("No. of Tickets") +
  theme(axis.text.x = element_text(angle = 90))

#Provide a table with the frequency of complaint types.
table(Complaints_data$Customer.Complaint)
#Which complaint types are maximum i.e., around internet, network issues, 
#or across any other domains.
internet <- contains(Complaints_data$Customer.Complaint,match ="internet",ignore.case = T)
table(internet)

charge <- contains(Complaints_data$Customer.Complaint,match ="charge",ignore.case = T)
table(charge)

bill <- contains(Complaints_data$Customer.Complaint,match ="bill",ignore.case = T)
table(bill)

network <- contains(Complaints_data$Customer.Complaint,match ="network",ignore.case = T)
table(network)

email <- contains(Complaints_data$Customer.Complaint,match ="email",ignore.case = T)
table(email)

Complaints_data$complaint_types[internet] <- "Internet"
Complaints_data$complaint_types[network] <- "Network"
Complaints_data$complaint_types[charge] <- "Charge"
Complaints_data$complaint_types[bill] <- "Bill"
Complaints_data$complaint_types[email] <- "Email"
Complaints_data$complaint_types[-c(internet,network,charge,bill,email)] <- "Others"

table(Complaints_data$complaint_types)

ggplot( Complaints_data, aes(x=complaint_types)) +
geom_histogram(stat="count", binwidth=3, fill="#72bcd4", color="#ffb733", alpha=0.9) +
ggtitle("Number of Complaints in each month") +
xlab("complaint types")+
ylab("No. of complaints")+
theme(
  plot.title = element_text(size=15)
  )

#Create a new categorical variable with value as Open and Closed. 
#Open & Pending is to be categorized as Open and Closed 
#& Solved is to be categorized as Closed.
Complaints_status_Categories <- ifelse(Complaints_data$Status == "Open" | 
                                         Complaints_data$Status == "Pending","Open", "Close")
table(Complaints_status_Categories)
table(Complaints_data$Status)

#Provide state wise status of complaints in a stacked bar chart. 
#Use the categorized variable from Q3. Provide insights on:
agg_tbl <- Complaints_data %>% group_by(State) %>% summarise(Categories_states=ifelse(Status == "Open" | Status == "Pending","Open", "Close"))
table(agg_tbl)

# stacked bar chart
ggplot(agg_tbl, aes(fill=Categories_states, y=Categories_states, x=State)) + 
  geom_bar(position="stack", stat="identity") + 
  theme(axis.text.x = element_text(angle = 90))

#Which state has the maximum complaints
table(agg_tbl)
max_complaints <- agg_tbl %>% group_by(State) %>% summarise(complaints=n())
max_complaints$State[max_complaints$complaints>=288]

#Which state has the highest percentage of unresolved complaints
agg_tbl %>% filter(Categories_states=="Open") %>% group_by(State) %>% summarize(complaints=n()) %>% print(n = 34)

#Provide the percentage of complaints resolved till date, which were 
#received through the Internet and customer care calls.
percentage <-agg_tbl %>% group_by(Categories_states) %>% summarize(NumOfComplaints=n())
percentage
lbls<-paste(percentage$Categories_states," ",round((percentage$NumOfComplaints/sum(percentage$NumOfComplaints)*100),2),"%",sep="")

#pie chart
pie(percentage$NumOfComplaints,labels=lbls)

#which were received through the Internet and customer care calls.
#Internet
internet_per <- Complaints_data %>% filter(Received.Via=="Internet") %>% filter( Status=="Closed") %>% group_by(Received.Via) %>% summarize(perc=n())
internet_per

internet_result<-round(internet_per$perc/sum( percentage$NumOfComplaints)*100,2)
internet_result

# customer care calls
customer_per <- Complaints_data %>% filter(Received.Via=="Customer Care Call") %>% filter( Status=="Closed") %>% group_by(Received.Via) %>% summarize(perc=n())
customer_per

customer_result<-round(customer_per$perc/sum( percentage$NumOfComplaints)*100,2)
customer_result

