pa = read.csv("C:/Users/My Lappie/Downloads/Datasets (2)/Property_Price_Train.csv")

col_to_be_dropped <-  c("Id","Lane_Type" , "Fireplace_Quality" , "Pool_Quality"  , 
                        "Fence_Quality"  , "Miscellaneous_Feature")

pa  <-  pa[ ,      !(names(pa)      %in%     col_to_be_dropped)]

dim(pa)

pa$Lot_Extent[is.na(pa$Lot_Extent)]                     <- 70 
pa$Brick_Veneer_Type[is.na(pa$Brick_Veneer_Type)]       <- 'None'
pa$Brick_Veneer_Area[is.na(pa$Brick_Veneer_Area)]              <- 103
pa$Basement_Height[is.na(pa$Basement_Height)]                  <- 'TA'   
pa$Basement_Condition[is.na(pa$Basement_Condition)]            <- 'TA'
pa$Exposure_Level[is.na(pa$Exposure_Level)]                    <- 'No'
pa$BsmtFinType1[is.na(pa$BsmtFinType1)]                        <- 'Unf'
pa$BsmtFinType2[is.na(pa$BsmtFinType2)]                        <- 'Unf'
pa$Electrical_System[is.na(pa$Electrical_System)]              <- 'SBrkr'
pa$Garage[is.na(pa$Garage)]                                    <-  'Attchd'
pa$Garage_Built_Year[is.na(pa$Garage_Built_Year)]              <-  1979
pa$Garage_Finish_Year[is.na(pa$Garage_Finish_Year)]            <- 'Unf' 
pa$Garage_Quality[is.na(pa$Garage_Quality)]                    <- 'TA'
pa$Garage_Condition[is.na(pa$Garage_Condition)]                <- 'TA'

colnames(pa[colSums(is.na(pa))*100/dim(pa)[1] > 0])   # answer is zero implies that there are no nulls

pa$Lot_Extent           <-as.numeric(as.factor(pa$Lot_Extent))
pa$Property_Shape       <-as.numeric(as.factor(pa$Property_Shape))
pa$Land_Outline         <-as.numeric(as.factor(pa$Land_Outline))
pa$Utility_Type         <-as.numeric(as.factor(pa$Utility_Type))
pa$Lot_Configuration    <-as.numeric(as.factor(pa$Lot_Configuration))
pa$Neighborhood         <-as.numeric(as.factor(pa$Neighborhood))
pa$Condition1           <-as.numeric(as.factor(pa$Condition1))
pa$Condition2           <-as.numeric(as.factor(pa$Condition2))
pa$House_Type           <-as.numeric(as.factor(pa$House_Type))
pa$House_Design         <-as.numeric(as.factor(pa$House_Design))
pa$Roof_Design          <-as.numeric(as.factor(pa$Roof_Design))
pa$Roof_Quality         <-as.numeric(as.factor(pa$Roof_Quality))
pa$Exterior1st          <-as.numeric(as.factor(pa$Exterior1st))
pa$Exterior2nd          <-as.numeric(as.factor(pa$Exterior2nd))
pa$Brick_Veneer_Type    <-as.numeric(as.factor(pa$Brick_Veneer_Type))
pa$Exterior_Material    <-as.numeric(as.factor(pa$Exterior_Material))
pa$Exterior_Condition   <-as.numeric(as.factor(pa$Exterior_Condition))

pa$Foundation_Type      <-as.numeric(as.factor(pa$Foundation_Type))
pa$Basement_Height      <-as.numeric(as.factor(pa$Basement_Height))
pa$Basement_Condition   <-as.numeric(as.factor(pa$Basement_Condition))
pa$Exposure_Level       <-as.numeric(as.factor(pa$Exposure_Level))
pa$BsmtFinType1         <-as.numeric(as.factor(pa$BsmtFinType1))
pa$BsmtFinType2         <-as.numeric(as.factor(pa$BsmtFinType2))
pa$Heating_Type         <-as.numeric(as.factor(pa$Heating_Type))
pa$Heating_Quality      <-as.numeric(as.factor(pa$Heating_Quality))
pa$Air_Conditioning     <-as.numeric(as.factor(pa$Air_Conditioning))
pa$Electrical_System    <-as.numeric(as.factor(pa$Electrical_System))
pa$Kitchen_Quality      <-as.numeric(as.factor(pa$Kitchen_Quality))
pa$Functional_Rate      <-as.numeric(as.factor(pa$Functional_Rate))
pa$Garage               <-as.numeric(as.factor(pa$Garage))
pa$Garage_Finish_Year   <-as.numeric(as.factor(pa$Garage_Finish_Year))
pa$Garage_Quality       <-as.numeric(as.factor(pa$Garage_Quality))
pa$Garage_Condition     <-as.numeric(as.factor(pa$Garage_Condition))

pa$Pavedd_Drive         <-as.numeric(as.factor(pa$Pavedd_Drive))
pa$Sale_Type            <-as.numeric(as.factor(pa$Sale_Type))
pa$Sale_Condition       <-as.numeric(as.factor(pa$Sale_Condition))

colnames(pa)[grepl('factor|logical|character', sapply(pa, class))] # if any column is still non numeric

pa$Zoning_Class    <-as.numeric(as.factor(pa$Zoning_Class))
pa$Road_Type    <-as.numeric(as.factor(pa$Road_Type))
pa$Property_Slope    <-as.numeric(as.factor(pa$Property_Slope))

colnames(pa)[grepl('factor|logical|character', sapply(pa, class))] # if any column is still non numeric

pa$Utility_Type        = NULL
pa$Total_Basement_Area = NULL
pa$Grade_Living_Area   = NULL


library(dplyr)
sam_pa    = sample(2 , nrow(pa) , replace = TRUE, prob = c(.8 , .2))
pa_train  = pa[pa_sample == 1 , ]
pa_test   = pa[pa_sample == 2 , ]

dim(pa_train)
dim(pa_test)

lm_pa = lm(Sale_Price ~ . , data= pa_train)
summary(lm_pa)


# pa$Utility_Type        = NULL    these 3 columns have singular data hence while 
# pa$Total_Basement_Area = NULL    building the model R did not considered these
# pa$Grade_Living_Area   = NULL    columns

lm_pa$coefficients

mean(lm_pa$residuals)

hist(lm_pa$residuals)
boxplot(lm_pa$residuals)
plot(lm_pa$residuals)


pred_test = predict(lm_pa, pa_test)
err_test = pa_test$Sale_Price - pred_test
mape = mean(abs(err_test*100/pa_test$Sale_Price))
mape

aa = boxplot(pa$Sale_Price)
aa
dim(pa)
pa = pa[pa$Sale_Price <= 340000 , ] # removing Outliers which is above 340000
dim(pa)


#------again built model
###########################################################




###########################################################
sam_pa    = sample(2 , nrow(pa) , replace = TRUE, prob = c(.8 , .2))
pa_train  = pa[pa_sample == 1 , ]
pa_test   = pa[pa_sample == 2 , ]

dim(pa_train)
dim(pa_test)

lm_pa = lm(Sale_Price ~ . , data= pa_train)
summary(lm_pa)

hist(lm_pa$residuals)
boxplot(lm_pa$residuals)
plot(lm_pa$residuals)


