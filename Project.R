rm(list=ls())
setwd('C:/Users/vishwanath/Desktop/Skill set/EdWisor/Project 1/Data')
getwd()
macro = read.csv("macro.csv", header = T)
#aftermacro = read.csv("set1_macro.csv", header = T)

train = read.csv("train.csv", header = T)

library(lubridate)
macro$day = day(macro$timestamp)
train$room_area = train$num_room / train$floor

#macro cleaning
#replace comma by decimal in child var
macro$child_on_acc_pre_school<-as.numeric(gsub(",", ".", macro$child_on_acc_pre_school))
macro$old_education_build_share = as.numeric(macro$old_education_build_share)
macro$modern_education_share = as.numeric(macro$modern_education_share)


#missing value imputation - missForest
library(missForest)
imp <- missForest(macro[2:100],maxiter = 1)
macro[2:100] =imp$ximp

#missing value imputation - Hmisc
library(Hmisc)
a=colnames(macro)
#a=colnames(macro)[ apply(macro, 2, (anyNA)) ]
write.csv(a,"macrocols.csv")
f = ~oil_urals+gdp_quart+gdp_quart_growth+cpi+ppi+gdp_deflator+balance_trade+balance_trade_growth+usdrub+eurrub+brent+net_capital_export+gdp_annual+gdp_annual_growth+average_provision_of_build_contract+average_provision_of_build_contract_moscow+rts+micex+micex_rgbi_tr+micex_cbi_tr+deposits_value+deposits_growth+deposits_rate+mortgage_value+mortgage_growth+mortgage_rate+grp+grp_growth+income_per_cap+real_dispos_income_per_cap_growth+salary+salary_growth+fixed_basket+retail_trade_turnover+retail_trade_turnover_per_cap+retail_trade_turnover_growth+labor_force+unemployment+employment+invest_fixed_capital_per_cap+invest_fixed_assets+profitable_enterpr_share+unprofitable_enterpr_share+share_own_revenues+overdue_wages_per_cap+fin_res_per_cap+marriages_per_1000_cap+divorce_rate+construction_value+invest_fixed_assets_phys+pop_natural_increase+pop_migration+pop_total_inc+childbirth+mortality+housing_fund_sqm+lodging_sqm_per_cap+water_pipes_share+baths_share+sewerage_share+gas_share+hot_water_share+electric_stove_share+heating_share+old_house_share+average_life_exp+infant_mortarity_per_1000_cap+perinatal_mort_per_1000_cap+incidence_population+rent_price_4.room_bus+rent_price_3room_bus+rent_price_2room_bus+rent_price_1room_bus+rent_price_3room_eco+rent_price_2room_eco+rent_price_1room_eco+load_of_teachers_preschool_per_teacher+child_on_acc_pre_school+load_of_teachers_school_per_teacher+students_state_oneshift+modern_education_share+old_education_build_share+provision_doctors+provision_nurse+load_on_doctors+power_clinics+hospital_beds_available_per_cap+hospital_bed_occupancy_per_year+provision_retail_space_sqm+provision_retail_space_modern_sqm+turnover_catering_per_cap+theaters_viewers_per_1000_cap+seats_theather_rfmin_per_100000_cap+museum_visitis_per_100_cap+bandwidth_sports+population_reg_sports_share+students_reg_sports_share+apartment_build+apartment_fund_sqm

g <- aregImpute(f, n.impute=5, data=macro[2:100],nk=0)

imputed_results <- as.data.frame(impute.transcan(g, imputation=1, data=macro, list.out=TRUE, pr=FALSE, check=FALSE))
macro[2:100] = imputed_results

#clean R
rm(imputed_results,f,g)

#missing value imputation - mean
for(i in 2:ncol(macro)){
  macro[is.na(macro[,i]), i] <- mean(macro[,i], na.rm = TRUE)
}

#feature selection by vif in usdm
library(usdm)
#convert factor to numeric
#macro$modern_education_share<-as.numeric(macro$modern_education_share)
#macro$old_education_build_share<-as.numeric(macro$old_education_build_share)
#run vifcor to analyse VIF, copy excluded variables to res1 
#and remove the vars from macro
#copy back date to macro
res=(vifcor(macro[2:100],th=0.9))
res1 = res@excluded
`%ni%` <- Negate(`%in%`)
macro=subset(macro,select = names(macro) %ni% res1)

#clean R
rm(res1,res,res1,'%ni%')

#clean data - build year inconsistency
#train$build_year <- as.character(train$build_year)

train$build_year[train$build_year == "20052009"] <- "2005"
train$build_year[train$build_year == "0"] <- NA
train$build_year[train$build_year == "1"] <- NA
train$build_year[train$build_year == "20"] <- "2000"
train$build_year[train$build_year == "215"] <- "2015"
train$build_year[train$build_year == "3"] <- NA
train$build_year[train$build_year == "4965"] <- NA
train$build_year[train$build_year == "71"] <- NA

train$build_year <- as.numeric(train$build_year)

#clean data - state inconsistency
#train$state <- as.character(train$state)

train$state[train$state == "33"] <- "3"

train$state <- as.numeric(train$state)

#clean data - full_sq inconsistency
#train$full_sq <- as.character(train$full_sq)

train$full_sq[train$full_sq == "5326"] <- "26"

train$full_sq <- as.numeric(train$full_sq)

#separate categoric and numeric data
num = train[ ,sapply(train, is.numeric)]
cat = train[ ,!sapply(train, is.numeric)]

#missing value imputation - Hmisc
library(Hmisc)
b=colnames(num)[ apply(num, 2, Negate(anyNA)) ]
write(b,"traincols.txt")
b=colnames(num)
write.csv(b,"traincols.csv")
f = ~id+full_sq+life_sq+floor+max_floor+material+build_year+num_room+kitch_sq+state+area_m+raion_popul+green_zone_part+indust_part+children_preschool+preschool_quota+preschool_education_centers_raion+children_school+school_quota+school_education_centers_raion+school_education_centers_top_20_raion+hospital_beds_raion+healthcare_centers_raion+university_top_20_raion+sport_objects_raion+additional_education_raion+culture_objects_top_25_raion+shopping_centers_raion+office_raion+full_all+male_f+female_f+young_all+young_male+young_female+work_all+work_male+work_female+ekder_all+ekder_male+ekder_female+X0_6_all+X0_6_male+X0_6_female+X7_14_all+X7_14_male+X7_14_female+X0_17_all+X0_17_male+X0_17_female+X16_29_all+X16_29_male+X16_29_female+X0_13_all+X0_13_male+X0_13_female+raion_build_count_with_material_info+build_count_block+build_count_wood+build_count_frame+build_count_brick+build_count_monolith+build_count_panel+build_count_foam+build_count_slag+build_count_mix+raion_build_count_with_builddate_info+build_count_before_1920+build_count_1921.1945+build_count_1946.1970+build_count_1971.1995+build_count_after_1995+ID_metro+metro_min_avto+metro_km_avto+metro_min_walk+metro_km_walk+kindergarten_km+school_km+park_km+green_zone_km+industrial_km+water_treatment_km+cemetery_km+incineration_km+railroad_station_walk_km+railroad_station_walk_min+ID_railroad_station_walk+railroad_station_avto_km+railroad_station_avto_min+ID_railroad_station_avto+public_transport_station_km+public_transport_station_min_walk+water_km+mkad_km+ttk_km+sadovoe_km+bulvar_ring_km+kremlin_km+big_road1_km+ID_big_road1+big_road2_km+ID_big_road2+railroad_km+zd_vokzaly_avto_km+ID_railroad_terminal+bus_terminal_avto_km+ID_bus_terminal+oil_chemistry_km+nuclear_reactor_km+radiation_km+power_transmission_line_km+thermal_power_plant_km+ts_km+big_market_km+market_shop_km+fitness_km+swim_pool_km+ice_rink_km+stadium_km+basketball_km+hospice_morgue_km+detention_facility_km+public_healthcare_km+university_km+workplaces_km+shopping_centers_km+office_km+additional_education_km+preschool_km+big_church_km+church_synagogue_km+mosque_km+theater_km+museum_km+exhibition_km+catering_km+green_part_500+prom_part_500+office_count_500+office_sqm_500+trc_count_500+trc_sqm_500+cafe_count_500+cafe_sum_500_min_price_avg+cafe_sum_500_max_price_avg+cafe_avg_price_500+cafe_count_500_na_price+cafe_count_500_price_500+cafe_count_500_price_1000+cafe_count_500_price_1500+cafe_count_500_price_2500+cafe_count_500_price_4000+cafe_count_500_price_high+big_church_count_500+church_count_500+mosque_count_500+leisure_count_500+sport_count_500+market_count_500+green_part_1000+prom_part_1000+office_count_1000+office_sqm_1000+trc_count_1000+trc_sqm_1000+cafe_count_1000+cafe_sum_1000_min_price_avg+cafe_sum_1000_max_price_avg+cafe_avg_price_1000+cafe_count_1000_na_price+cafe_count_1000_price_500+cafe_count_1000_price_1000+cafe_count_1000_price_1500+cafe_count_1000_price_2500+cafe_count_1000_price_4000+cafe_count_1000_price_high+big_church_count_1000+church_count_1000+mosque_count_1000+leisure_count_1000+sport_count_1000+market_count_1000+green_part_1500+prom_part_1500+office_count_1500+office_sqm_1500+trc_count_1500+trc_sqm_1500+cafe_count_1500+cafe_sum_1500_min_price_avg+cafe_sum_1500_max_price_avg+cafe_avg_price_1500+cafe_count_1500_na_price+cafe_count_1500_price_500+cafe_count_1500_price_1000+cafe_count_1500_price_1500+cafe_count_1500_price_2500+cafe_count_1500_price_4000+cafe_count_1500_price_high+big_church_count_1500+church_count_1500+mosque_count_1500+leisure_count_1500+sport_count_1500+market_count_1500+green_part_2000+prom_part_2000+office_count_2000+office_sqm_2000+trc_count_2000+trc_sqm_2000+cafe_count_2000+cafe_sum_2000_min_price_avg+cafe_sum_2000_max_price_avg+cafe_avg_price_2000+cafe_count_2000_na_price+cafe_count_2000_price_500+cafe_count_2000_price_1000+cafe_count_2000_price_1500+cafe_count_2000_price_2500+cafe_count_2000_price_4000+cafe_count_2000_price_high+big_church_count_2000+church_count_2000+mosque_count_2000+leisure_count_2000+sport_count_2000+market_count_2000+green_part_3000+prom_part_3000+office_count_3000+office_sqm_3000+trc_count_3000+trc_sqm_3000+cafe_count_3000+cafe_sum_3000_min_price_avg+cafe_sum_3000_max_price_avg+cafe_avg_price_3000+cafe_count_3000_na_price+cafe_count_3000_price_500+cafe_count_3000_price_1000+cafe_count_3000_price_1500+cafe_count_3000_price_2500+cafe_count_3000_price_4000+cafe_count_3000_price_high+big_church_count_3000+church_count_3000+mosque_count_3000+leisure_count_3000+sport_count_3000+market_count_3000+green_part_5000+prom_part_5000+office_count_5000+office_sqm_5000+trc_count_5000+trc_sqm_5000+cafe_count_5000+cafe_sum_5000_min_price_avg+cafe_sum_5000_max_price_avg+cafe_avg_price_5000+cafe_count_5000_na_price+cafe_count_5000_price_500+cafe_count_5000_price_1000+cafe_count_5000_price_1500+cafe_count_5000_price_2500+cafe_count_5000_price_4000+cafe_count_5000_price_high+big_church_count_5000+church_count_5000+mosque_count_5000+leisure_count_5000+sport_count_5000+market_count_5000+price_doc

g <- aregImpute(f, n.impute=1, data=num,nk=0)
imputed_results <- as.data.frame(impute.transcan(g, imputation=1, data=num, list.out=TRUE, pr=FALSE, check=FALSE))
imp = read.csv("impres.csv",header = T)
imp$X=NULL
safe3 = (num)[colnames(imp)] 
num[colnames(imp)] <- imp


#missing value imputation - missForest
library(missForest)
imp <- missForest(num,maxiter = 1)
num =imp$xmp

#missing value imputation - mean
for(i in 1:ncol(num)){
  num[is.na(num[,i]), i] <- mean(num[,i], na.rm = TRUE)
}

#combine numeric and categoric data back to train
train = cbind(cat,num)

#CLean R
rm(cat,imputed_results,num,safe,cat,a,b,f,g,res,res1,'%ni%')

#till no more NA present
sum(is.na(train))
#if any columns contain NA
colnames(train)[colSums(is.na(train)) > 0]

#convert factors to numeric
temp1 = train$timestamp
train$timestamp = NULL
indx <- sapply(train, is.factor)
train[indx] <- lapply(train[indx], function(x) as.numeric(x))
#run vifcor to analyse VIF, copy excluded variables to res1 
#and remove the vars from macro
#copy back date to macro
res=(vifcor(train,th=0.9))
res1 = res@excluded
`%ni%` <- Negate(`%in%`)
train=subset(train,select = names(train) %ni% res1)
train$timestamp=temp1
train=train[,c(ncol(train),1:(ncol(train)-1))]

#clean R
rm(res1,temp1,i,j,res,indx,num,cat,'%ni%')

#alternative - removing var with <0.1 variance
#library(caret)
#library(mlbench)
#correlationMatrix <- cor(train)

#nzv <- nearZeroVar(train, saveMetrics = TRUE)
#df_nzv <- train[c(rownames(nzv[nzv$percentUnique > 0.1,])) ]
#train = df_nzv
#train$timestamp = temp2
#train=train[,c(ncol(train),1:(ncol(train)-1))]

#clean R
#rm(cat,correlationMatrix,df_nzv,nzv,num,safedf,temp2,i,indx)


#merge train and macro by timestamp var
df=read.csv("set1_df.csv",header = T)
df = merge(macro, train, by = "timestamp")
dft = merge(macro, test, by = "timestamp")

df_num = df[ ,sapply(df, is.numeric)]
df$timestamp=NULL
#check for outliers
attach(L)
label=data.frame(colnames(df))
plot(df$price_doc, df$salary,main="Outliers", xlab="price", ylab="salary", pch=18, col="blue")
text(df$price_doc, label, cex=0.6, pos=4, col="red")
abline(lm(~ df$price_doc))
#library(outliers)
#mod <- lm(price_doc ~ ., data=df_num)
#cooksd <- cooks.distance(mod)
#plot(cooksd, pch="*", cex=1, main="Influential Obs by Cooks distance",type='l',col="dark red")
#var_name <- eval(substitute(var),eval(df_num))
boxplot(df$deposits_growth,col = "pink",border = "darkblue")
#rm.outlier(safe1, fill = FALSE, median = FALSE,border opposite = FALSE)
outs=boxplot.stats(safe1)$out

#collinearity
library("mctest")
x<-df_num[ ,-1]  # X variables from Hald data
y<-df_num[ ,1]   # y variable from Hald data
mctest(x, y)   # default collinearity diagnostics
mctest(x, y, type = "o") # overall collinearity diagnostics
mc.plot(x, y, Inter = FALSE, vif = 10, ev = 0.01)

#vars in train not in test
#remove them
df$preschool_quota=NULL
df$female_f=NULL
df$build_count_wood=NULL
df$kindergarten_km=NULL
df$railroad_station_walk_km=NULL
df$zd_vokzaly_avto_km=NULL
df$swim_pool_km=NULL
df$shopping_centers_km=NULL
df$museum_km=NULL
df$cafe_sum_1000_max_price_avg=NULL
df$workplaces_km=NULL
df$office_sqm_1500=NULL

#now reverse
dft$school_quota=NULL
dft$build_count_slag=NULL
dft$cafe_sum_1000_min_price_avg=NULL

#delete id and timestamp which doesn't help our model
df$id=NULL
dft$id=NULL
df$timestamp=NULL
dft$timestamp=NULL
dft$green_part_3000=NULL

#df=data.frame(scale(df))
#LINEAR REGRESSION
lm_model = lm(log(price_doc) ~ ., data = df[1:7662,])
summary(lm_model)

pred = predict(lm_model,dft)
par(mfrow = c(2,2))
plot(lm_model)
#calculate MAPE
mape = function(y, yhat)
  mean(abs((y - yhat)/y))

x =data.frame(df$price_doc[1:7662])
pred2=data.frame(exp(pred2))
mape(x, pred2)

#extract coefficients from lm
coeff = data.frame(Coeff = lm_model$coefficients, 
                   StdError = summary(lm_model$coefficients[2,]),
                   tValue = summary(lm_model$coefficients[3,]),
                   Pval = summary(lm_model$coefficients[4,]))
coeff$variables = row.names(coeff)
coeff = coeff[,c(5,1:4)]
rownames(coeff) = NULL
write.csv(coeff, "Coefficients_stats.csv", row.names = F)

#rpart
library(rpart)
rpart = rpart(log(price_doc) ~ . ,data=df , method = "anova")
pred2 = data.frame(predict(rpart,dft))
write.csv(pred2,"res_rpart_log_2.csv",row.names =T) 

#random forest
library(randomForest)
rf = randomForest(price_doc ~ ., df,importance=TRUE,ntree=500)


yash = read.csv("res_LR_2_withlog.csv")
yash = yash$price_doc
