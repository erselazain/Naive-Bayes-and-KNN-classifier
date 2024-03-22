library(e1071)
library(caret)

# Masukkan data
dataset <- read.csv("healthcare_dataset.csv")
data <- select(dataset, Age, Gender, Blood.Type, Medical.Condition,
               Insurance.Provider, Billing.Amount, Admission.Type, 
               Medication, Test.Results)
data <- data[1:100,1:9]
print(data)

data$Age <- ifelse(data$Age <= 18, 1,
                   ifelse(data$Age <= 25, 2,
                          ifelse(data$Age <= 45, 3,
                                 ifelse(data$Age <= 65, 4, 5))))

data$Gender <- ifelse(data$Gender == "Male", 1,
                      ifelse(data$Gender == "Female", 2, 0))

data$Blood.Type <- ifelse(data$Blood.Type == "O-", 1,
                          ifelse(data$Blood.Type == "O+", 2,
                                 ifelse(data$Blood.Type == "A-", 3,
                                        ifelse(data$Blood.Type == "A+", 4, 
                                               ifelse(data$Blood.Type == "B-", 5,
                                                      ifelse(data$Blood.Type == "B+", 6,
                                                             ifelse(data$Blood.Type == "AB-", 7,
                                                                    ifelse(data$Blood.Type == "AB+", 8, 0))))))))

data$Medical.Condition <- ifelse(data$Medical.Condition == "Diabetes", 1,
                                 ifelse(data$Medical.Condition == "Asthma", 2,
                                        ifelse(data$Medical.Condition == "Obesity", 3,
                                               ifelse(data$Medical.Condition == "Arthritis", 4, 
                                                      ifelse(data$Medical.Condition == "Hypertension", 5,
                                                             ifelse(data$Medical.Condition == "Cancer", 6, 0 ))))))

data$Insurance.Provider <- ifelse(data$Insurance.Provider == "Medicare", 1,
                                  ifelse(data$Insurance.Provider == "UnitedHealthcare", 2,
                                         ifelse(data$Insurance.Provider == "Cigna", 3,
                                                ifelse(data$Insurance.Provider == "Blue Cross", 4, 
                                                       ifelse(data$Insurance.Provider == "Aetna", 5, 0 )))))

data$Billing.Amount <- ifelse(data$Billing.Amount <= 10000, 1,
                              ifelse(data$Billing.Amount <= 20000, 2,
                                     ifelse(data$Billing.Amount <= 30000, 3,
                                            ifelse(data$Billing.Amount <= 40000, 4, 5))))

data$Admission.Type <- ifelse(data$Admission.Type == "Elective", 1,
                              ifelse(data$Admission.Type == "Urgent", 2,
                                     ifelse(data$Admission.Type == "Emergency", 3, 0)))

data$Medication <- ifelse(data$Medication == "Penicillin", 1,
                          ifelse(data$Medication == "Paracetamol", 2,
                                 ifelse(data$Medication == "Aspirin", 3,
                                        ifelse(data$Medication == "Lipitor", 4, 
                                               ifelse(data$Medication == "Ibuprofen", 5, 0 )))))

data$Test.Results <- ifelse(data$Test.Results == "Normal", 1,
                            ifelse(data$Test.Results == "Abnormal", 2,
                                   ifelse(data$Test.Results == "Inconclusive", 3, 0)))
print(data)

set.seed(124)

indexes = createDataPartition(data$Test.Results, p = .8, list = F)
train = data[indexes, ]
test = data[-indexes, ]
dim(train)
dim(test)

model=naiveBayes(Test.Results~., data = train)
print(model)

pred = predict(model,  test, type="class")

cm = table(test$Test.Results, pred)
print(cm) 

akurasi = sum(diag(cm))/sum(cm)
print(akurasi)
