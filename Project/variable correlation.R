#Julia 

#Correlation Coefficients betwteen Status & other Viariables
Data$Status = as.integer(Data$Status) #1 = Failed, 2 = Success

#TopCompanyExp CATEGORICAL
#1 = No, 2 = Yes
table(Status = Data$Status, TopCompanyExp = Data$TopCompanyExp)
cor(as.integer(Data$TopCompanyExp), Data$Status)
# 0.09679584

#StartupExp CATEGORICAL
#1 = No, 2 = Yes
table(Status = Data$Status, StartupExp = Data$StartupExp)
cor(as.integer(Data$StartupExp), Data$Status)
# 0.0649647

#SuccessfulStartupExp CATEGORICAL
table(Status = Data$Status, SuccessfulStartupExp = Data$SuccessfulStartupExp)
cor(as.integer(Data$SuccessfulStartupExp), Data$Status)
# 0.02047792

#Big5Partner CATEGORICAL
#1 = No, 2 = Yes
table(Status = Data$Status, Big5Partner = Data$Big5Partner)
cor(as.integer(Data$Big5Partner), Data$Status)
#0.08710295

#ConsultingExp CATEGORICAL
#1 = No, 2 = Yes
table(Status = Data$Status, ConsultingExp = Data$ConsultingExp)
cor(as.integer(Data$ConsultingExp), Data$Status)
# -0.191745

#ProductorService CATEGORICAL
# 1=Both, 2=Product, 3=Service
table(Status = Data$Status, ProductorService = Data$ProductorService)
cor(as.integer(Data$ProductorService), Data$Status)
# -0.09075112

#DataFocus CATEGORICAL
# 1=Both, 2=No, 3=Private, 4=Public
table(Status = Data$Status, DataFocus = Data$DataFocus)
cor(as.integer(Data$DataFocus), Data$Status)
# 0.08567326

#ConsumerDataFocus CATEGORICAL
#1 = No, 2 = Yes
table(Status = Data$Status, ConsumerDataFocus = Data$ConsumerDataFocus)
cor(as.integer(Data$ConsumerDataFocus), Data$Status)
#0.169483


#DataStructureFocus CATEGORICAL
# 1=Both, 2=No, 3=Structured, 4=Unstructured
table(Status = Data$Status, DataStructureFocus = Data$DataStructureFocus)
cor(as.integer(Data$DataStructureFocus), Data$Status)
#0.007592873

#SubscriptionBased CATEGORICAL
#1 = No, 2 = Yes
table(Status = Data$Status, SubscriptionBased = Data$SubscriptionBased)
cor(as.integer(Data$SubscriptionBased), Data$Status)
#0.1010671
