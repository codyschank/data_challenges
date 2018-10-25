library(lme4)

redcard_1 = read.csv("./data/redcard_1.csv", stringsAsFactors = F)
redcard_2 = read.csv("./data/redcard_2.csv", stringsAsFactors = F)

# combine dataframes
redcard = rbind(redcard_1, redcard_2)

# remove unnecessary columns
redcard = redcard[,-which(colnames(redcard) %in% c("X","photoID"))]

# take average of two skin tone ratings
# higher skintone value = darker skin
redcard$skintone = rowMeans(redcard[,c('rater1','rater2')])

# get rid of rows where redCards > 1
redcard = redcard[redcard$redCards <= 1,]

# remove rows with missing position data
redcard = redcard[redcard$position != "",]

# impute height and weight
redcard[is.na(redcard$weight),"weight"] = mean(redcard$weight,na.rm=T)
redcard[is.na(redcard$height),"height"] = mean(redcard$height,na.rm=T)

# keep only rows with no NAs 
redcard = redcard[complete.cases(redcard),]

# this might not be necessary, but just making sure
redcard$refNum = as.factor(redcard$refNum)

# scale continuous variables
redcard$skintone = scale(redcard$skintone)
redcard$weight = scale(redcard$weight)
redcard$height = scale(redcard$height)

# not using height and weight, model is unidentifiable with them
m1 = glmer(redCards ~ skintone + (1|refNum) + (1|position), data=redcard, family="binomial")

summary(m1)

plot(density(coef(m1)$refNum[,1]))
plot(density(coef(m1)$position[,1]))

# should we do cross-validation to investigate accuracy?
# should we compare accuracy of smote vs non-smote?
# should that determine which we use to investigate the coefficient estimate for skin tone

# do refs from countries with higher bias scores give out more red cards to darker skin players?
