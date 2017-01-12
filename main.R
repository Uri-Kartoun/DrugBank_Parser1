library(Hmisc);library(ROCR);library(parcor);
rm(list=ls());cat("\014")

setwd('E:\\MGH\\sleep\\Manuscript3\\code')

mydata = read.csv(file="data.csv", sep=",", header=TRUE)
colnames = data.frame(colnames(mydata))

mydata$label<-factor(mydata$label)
mydata$Gender<-factor(mydata$Gender)
mydata$Ethnicity<-impute(factor(mydata$Ethnicity))
mydata$Marital.Status<-impute(factor(mydata$Marital.Status))
mydata$Smoking.Status<-impute(factor(mydata$Smoking.Status))
mydata$BMI<-impute(mydata$BMI)

mydata$is_medicare<-factor(mydata$is_medicare)
mydata$is_medicaid<-factor(mydata$is_medicaid)
mydata$is_other_insurance<-factor(mydata$is_other_insurance)

mydata$Ethnicity = relevel(mydata$Ethnicity, ref = "WHITE")
mydata$Smoking.Status = relevel(mydata$Smoking.Status, ref = "Never")

#select <- sample(nrow(as.matrix(mydata)), nrow(mydata) * 0.6666667[])
#Training_Set <- mydata[select,]
#Validation_Set <- mydata[-select,]

#write.table(Training_Set, file="Training_Set.txt", sep="\t")
#write.table(Validation_Set, file="Validation_Set.txt", sep="\t")

Training_Set = read.table(file="Training_Set.txt", sep="\t")
Validation_Set = read.table(file="Validation_Set.txt", sep="\t")

colnames = data.frame(colnames(Training_Set))

Training_Set_factored <- model.matrix(Training_Set$label~
##Structured
+log10(1 + Training_Set$X..Insomnia..S...life.)+log10(1 + Training_Set$X..Disorders.of.lipid.metabolism..S...life.)+log10(1 + Training_Set$X..Obesity..S...life.)+log10(1 + Training_Set$X..Anxiety.and.Depression..S...life.)+log10(1 + Training_Set$X..Asthma..S...life.)+log10(1 + Training_Set$X..Afib..S...life.)
+log10(1 + Training_Set$X..CHF..S...life.)+log10(1 + Training_Set$X..Chronic.kidney.disease..S...life.)+log10(1 + Training_Set$X..COPD..S...life.)+log10(1 + Training_Set$X..CVD..S...life.)+log10(1 + Training_Set$X..Diabetes..S...life.)+log10(1 + Training_Set$X..Gastrointestinal.Disorder..S...life.)
+log10(1 + Training_Set$X..Hypertension..S...life.)+log10(1 + Training_Set$X..Joint.Disorder..S...life.)+log10(1 + Training_Set$X..Coronary.Heart.Disease..S...life.)+log10(1 + Training_Set$X..Peripheral.vascular.disease..S...life.)+log10(1 + Training_Set$X..Pneumonia..S...life.)+log10(1 + Training_Set$X..Psychiatric.Disorders..S...life.)
+log10(1 + Training_Set$X..Renal.Failure..S...life.)+log10(1 + Training_Set$X..Sleep.apnea..S...life.)+log10(1 + Training_Set$X..Cancer..S...life.)+log10(1 + Training_Set$X..Viral.hepatitis..S...life.)+log10(1 + Training_Set$X..Non.Viral.hepatitis..S...life.)+log10(1 + Training_Set$X..NAFLD..S...life.)
+log10(1 + Training_Set$X..Cirrhosis..S...life.)+log10(1 + Training_Set$X..Alzheimers.or.dementia..S...life.)+log10(1 + Training_Set$X..Stroke..S...life.)+log10(1 + Training_Set$X..Osteoporosis..S...life.)+log10(1 + Training_Set$X..Facts..S...life.)+Training_Set$Gender
+Training_Set$Ethnicity+Training_Set$Marital.Status+Training_Set$Age+Training_Set$BMI+log10(1 + Training_Set$X..all.sleep.meds..life.)+Training_Set$is_medicare+Training_Set$is_medicaid+Training_Set$is_other_insurance

##Unstructured
+log10(1 + Training_Set$X..Sleep.disorder..U...life.)+Training_Set$Smoking.Status+log10(1 + Training_Set$X..Alcohol.abuse..U...life.)+log10(1 + Training_Set$X..Psychiatric.disorder..U...life.)
)[,-1]

Validation_Set_factored <- model.matrix(Validation_Set$label~
##Structured
+log10(1 + Validation_Set$X..Insomnia..S...life.)+log10(1 + Validation_Set$X..Disorders.of.lipid.metabolism..S...life.)+log10(1 + Validation_Set$X..Obesity..S...life.)+log10(1 + Validation_Set$X..Anxiety.and.Depression..S...life.)+log10(1 + Validation_Set$X..Asthma..S...life.)+log10(1 + Validation_Set$X..Afib..S...life.)
+log10(1 + Validation_Set$X..CHF..S...life.)+log10(1 + Validation_Set$X..Chronic.kidney.disease..S...life.)+log10(1 + Validation_Set$X..COPD..S...life.)+log10(1 + Validation_Set$X..CVD..S...life.)+log10(1 + Validation_Set$X..Diabetes..S...life.)+log10(1 + Validation_Set$X..Gastrointestinal.Disorder..S...life.)
+log10(1 + Validation_Set$X..Hypertension..S...life.)+log10(1 + Validation_Set$X..Joint.Disorder..S...life.)+log10(1 + Validation_Set$X..Coronary.Heart.Disease..S...life.)+log10(1 + Validation_Set$X..Peripheral.vascular.disease..S...life.)+log10(1 + Validation_Set$X..Pneumonia..S...life.)+log10(1 + Validation_Set$X..Psychiatric.Disorders..S...life.)
+log10(1 + Validation_Set$X..Renal.Failure..S...life.)+log10(1 + Validation_Set$X..Sleep.apnea..S...life.)+log10(1 + Validation_Set$X..Cancer..S...life.)+log10(1 + Validation_Set$X..Viral.hepatitis..S...life.)+log10(1 + Validation_Set$X..Non.Viral.hepatitis..S...life.)+log10(1 + Validation_Set$X..NAFLD..S...life.)
+log10(1 + Validation_Set$X..Cirrhosis..S...life.)+log10(1 + Validation_Set$X..Alzheimers.or.dementia..S...life.)+log10(1 + Validation_Set$X..Stroke..S...life.)+log10(1 + Validation_Set$X..Osteoporosis..S...life.)+log10(1 + Validation_Set$X..Facts..S...life.)+Validation_Set$Gender
+Validation_Set$Ethnicity+Validation_Set$Marital.Status+Validation_Set$Age+Validation_Set$BMI  +log10(1 + Validation_Set$X..all.sleep.meds..life.)+Validation_Set$is_medicare+Validation_Set$is_medicaid+Validation_Set$is_other_insurance
  
##Unstructured
+log10(1 + Validation_Set$X..Sleep.disorder..U...life.)+Validation_Set$Smoking.Status+log10(1 + Validation_Set$X..Alcohol.abuse..U...life.)+log10(1 + Validation_Set$X..Psychiatric.disorder..U...life.)                                 
)[,-1]

prev_Training_Set = nrow(Training_Set[which(Training_Set$label=="Y"),]) / nrow(Training_Set); prev_Training_Set
prev_Validation_Set = nrow(Validation_Set[which(Validation_Set$label=="Y"),]) / nrow(Validation_Set); prev_Validation_Set

df_coefficients_focused_cummulative<-NULL
# run alasso 1000 times and then manually pick only varaibles showed in >80% of the runs
for (i in 1:1000 ) {
alasso.model=adalasso(Training_Set_factored, as.numeric(Training_Set$label), k=4)
lasso_coefficient = exp(as.matrix(alasso.model$coefficients.lasso))
adalasso_coefficient = exp(as.matrix(alasso.model$coefficients.adalasso))
coefficient_name = colnames(Training_Set_factored)
df_coefficients = data.frame(lasso_coefficient, adalasso_coefficient, coefficient_name)
write.csv(df_coefficients, file="coefficients.csv")

#df_coefficients
df_coefficients_focused = df_coefficients[which(df_coefficients$adalasso_coefficient != 1),]; df_coefficients_focused
df_coefficients_focused_cummulative <- rbind(df_coefficients_focused_cummulative, df_coefficients_focused)
}

write.csv(data.frame(df_coefficients_focused_cummulative), file="df_coefficients_focused_cummulative.csv")

#AUC

fit <- glm(label ~        
           +X..Insomnia..S...life.
           +X..Anxiety.and.Depression..S...life.
           +X..Joint.Disorder..S...life.
           +X..Facts..S...life.
           +X..all.sleep.meds..life.     
           +X..Sleep.disorder..U...life.
           +X..Psychiatric.disorder..U...life.
           ,data=Training_Set, family=binomial())
summary(fit)

write.csv(summary(fit)$coefficients, file = "results.csv")

library(ROCR)
p <- predict(fit, newdata=Validation_Set, type="response")
pr <- prediction(p, Validation_Set$label)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

auc_structure = as.data.frame.matrix(cbind(prf@x.values[[1]], prf@y.values[[1]]))
write.table(auc_structure, file="auc_structure.txt", sep="\t")

### Test no-over fitting
avg_auc=0; auc_acumulated = 0;
file.remove("lg results.txt")
for (i in 1:100 ) {
  select2 <- sample(nrow(as.matrix(Training_Set)), nrow(Training_Set) * 0.75[])
  T2 <- Training_Set[select2,]
  V2 <- Training_Set[-select2,]
  
#   T2_focused <- model.matrix(T2$label ~
#                              +log10(1 + T2$X..Insomnia..S...life.)+log10(1 + T2$X..Disorders.of.lipid.metabolism..S...life.)+log10(1 + T2$X..Obesity..S...life.)+log10(1 + T2$X..Anxiety.and.Depression..S...life.)+log10(1 + T2$X..Asthma..S...life.)+log10(1 + T2$X..Afib..S...life.)
#                              +log10(1 + T2$X..CHF..S...life.)+log10(1 + T2$X..Chronic.kidney.disease..S...life.)+log10(1 + T2$X..COPD..S...life.)+log10(1 + T2$X..CVD..S...life.)+log10(1 + T2$X..Diabetes..S...life.)+log10(1 + T2$X..Gastrointestinal.Disorder..S...life.)
#                              +log10(1 + T2$X..Hypertension..S...life.)+log10(1 + T2$X..Joint.Disorder..S...life.)+log10(1 + T2$X..Coronary.Heart.Disease..S...life.)+log10(1 + T2$X..Peripheral.vascular.disease..S...life.)+log10(1 + T2$X..Pneumonia..S...life.)+log10(1 + T2$X..Psychiatric.Disorders..S...life.)
#                              +log10(1 + T2$X..Renal.Failure..S...life.)+log10(1 + T2$X..Sleep.apnea..S...life.)+log10(1 + T2$X..Cancer..S...life.)+log10(1 + T2$X..Viral.hepatitis..S...life.)+log10(1 + T2$X..Non.Viral.hepatitis..S...life.)+log10(1 + T2$X..NAFLD..S...life.)
#                              +log10(1 + T2$X..Cirrhosis..S...life.)+log10(1 + T2$X..Alzheimers.or.dementia..S...life.)+log10(1 + T2$X..Stroke..S...life.)+log10(1 + T2$X..Osteoporosis..S...life.)+log10(1 + T2$X..Facts..S...life.)+T2$Gender
#                              +T2$Ethnicity+T2$Marital.Status+T2$Age+T2$BMI  +log10(1 + T2$X..all.sleep.meds..life.)
#                              
#                              ##Unstructured
#                              +log10(1 + T2$X..Sleep.disorder..U...life.)+T2$Smoking.Status+log10(1 + T2$X..Alcohol.abuse..U...life.)+log10(1 + T2$X..Psychiatric.disorder..U...life.)
#   )[,-1]
#   
#   V2_focused <- model.matrix(V2$label ~
#                              +log10(1 + V2$X..Insomnia..S...life.)+log10(1 + V2$X..Disorders.of.lipid.metabolism..S...life.)+log10(1 + V2$X..Obesity..S...life.)+log10(1 + V2$X..Anxiety.and.Depression..S...life.)+log10(1 + V2$X..Asthma..S...life.)+log10(1 + V2$X..Afib..S...life.)
#                              +log10(1 + V2$X..CHF..S...life.)+log10(1 + V2$X..Chronic.kidney.disease..S...life.)+log10(1 + V2$X..COPD..S...life.)+log10(1 + V2$X..CVD..S...life.)+log10(1 + V2$X..Diabetes..S...life.)+log10(1 + V2$X..Gastrointestinal.Disorder..S...life.)
#                              +log10(1 + V2$X..Hypertension..S...life.)+log10(1 + V2$X..Joint.Disorder..S...life.)+log10(1 + V2$X..Coronary.Heart.Disease..S...life.)+log10(1 + V2$X..Peripheral.vascular.disease..S...life.)+log10(1 + V2$X..Pneumonia..S...life.)+log10(1 + V2$X..Psychiatric.Disorders..S...life.)
#                              +log10(1 + V2$X..Renal.Failure..S...life.)+log10(1 + V2$X..Sleep.apnea..S...life.)+log10(1 + V2$X..Cancer..S...life.)+log10(1 + V2$X..Viral.hepatitis..S...life.)+log10(1 + V2$X..Non.Viral.hepatitis..S...life.)+log10(1 + V2$X..NAFLD..S...life.)
#                              +log10(1 + V2$X..Cirrhosis..S...life.)+log10(1 + V2$X..Alzheimers.or.dementia..S...life.)+log10(1 + V2$X..Stroke..S...life.)+log10(1 + V2$X..Osteoporosis..S...life.)+log10(1 + V2$X..Facts..S...life.)+V2$Gender
#                              +V2$Ethnicity+V2$Marital.Status+V2$Age+V2$BMI  +log10(1 + V2$X..all.sleep.meds..life.)
#                              
#                              ##Unstructured
#                              +log10(1 + V2$X..Sleep.disorder..U...life.)+V2$Smoking.Status+log10(1 + V2$X..Alcohol.abuse..U...life.)+log10(1 + V2$X..Psychiatric.disorder..U...life.)
#   )[,-1]  
#   
#   fit <- glmnet(x=T2_focused, y=as.numeric(T2$label), lambda=alasso.model$lambda.lasso)
#   lasso.prob <- predict(fit, type="response", newx = as.matrix(V2_focused));
#   pred <- prediction(lasso.prob, as.matrix(V2$label));
#   perf <- performance(pred, "tpr", "fpr");
#   performance(pred,"auc");auc=attributes(performance(pred, 'auc'))$y.values[[1]]; auc
  
  
  fit <- glm(label ~        
              +X..Insomnia..S...life.
             +X..Anxiety.and.Depression..S...life.
             +X..Joint.Disorder..S...life.
             +X..Facts..S...life.
             +X..all.sleep.meds..life.     
             +X..Sleep.disorder..U...life.
             +X..Psychiatric.disorder..U...life.
             ,data=T2, family=binomial())
  
  p <- predict(fit, newdata=V2, type="response")
  pr <- prediction(p, V2$label)
  prf <- performance(pr, measure = "tpr", x.measure = "fpr")  
  
  auc <- performance(pr, measure = "auc")
  auc <- auc@y.values[[1]]
  
  avg_auc = avg_auc + auc;
  auc_acumulated = avg_auc / i;
  
  write(auc, file="lg results.txt",append=TRUE)
}
avg_auc / 100

#################################################

cutoffs <- data.frame(cut=exp(perf@alpha.values[[1]])/(1+exp(perf@alpha.values[[1]])), Specificity = 1 - perf@x.values[[1]], Sensitivity_TPR=perf@y.values[[1]]); cutoffs
write.table(cutoffs, file="cutoffs.txt", sep="\t")

#auc for one variable
mylogit <- glm(label~X..Insomnia..S...life., data=Training_Set, family=binomial)
pp <- predict.glm(mylogit,newdata = Validation_Set, type="response")
pred <- prediction(pp, as.data.frame(Validation_Set$label))
auc <- performance(pred,"auc"); auc
perf <- performance(pred, "tpr", "fpr");
auc_structure = as.data.frame.matrix(cbind(perf@x.values[[1]], perf@y.values[[1]]))
write.table(auc_structure, file="auc_structure.txt", sep="\t")

#########################################

#GLM

glm_model2 <- glm(mydata$label~
                  mydata$X..Insomnia..S...life.                                    
                 +mydata$X..Anxiety.and.depression..U...life.                  
                  ,family=binomial)

summary(glm_model2)

PARAMETER_X..Insomnia..S...life. = 3
PARAMETER_X..Anxiety.and.depression..U...life. = 5

linear_part = -0.895489 + 0.933873 * PARAMETER_X..Insomnia..S...life. + 0.034113 * PARAMETER_X..Anxiety.and.depression..U...life.

Prediction_probability = ((exp(linear_part) /  (1 + exp(linear_part))));Prediction_probability

