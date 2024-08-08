library(readr)
# Load the training data
train_data <- read.csv("E:/Friends/Prakash/housing_train.csv")  

# Convert Categorical variable to a factor with specified levels
train_data$Type <- factor(train_data$Type, levels = c("h","u","t"))

train_data$Method <- factor(train_data$Method, levels = c("S", "SP",	"VB",	"PI",	"SA"))

train_data$Postcode <- factor(train_data$Postcode, levels = c("3056",	"3073",	"3015",	"3187",	"3123",	"3165",	"3127",	"3033",	"3163",	"3141",	"3046",	"3071",	"3012",	"3124",	"3081",	"3103",	"3072",	"3186",	"3065",	"3101",	"3031",	"3042",	"3020",	"3142",	"3144",	"3044",	"3107",	"3058",	"3108",	"3182",	"3204",	"3068",	"3102",	"3206",	"3002",	"3016",	"3052",	"3148",	"3122",	"3079",	"3125",	"3034",	"3185",	"3032",	"3011",	"3189",	"3018",	"3055",	"3040",	"3121",	"3146",	"3039",	"3188",	"3104",	"3145",	"3051",	"3181",	"3019",	"3041",	"3143",	"3087",	"3054",	"3105",	"3013",	"3184",	"3167",	"3060",	"3066",	"3207",	"3084",	"3161",	"3003",	"3147",	"3057",	"3166",	"3021",	"3000",	"3083",	"3183",	"3043",	"3205",	"3078",	"3070",	"3126",	"3025",	"3162",	"3067",	"3053",	"3006",	"3085",	"3128",	"3008",	"3047",	"3061"))

train_data$SellerG <- factor(train_data$SellerG, levels = c("Nelson",	"Ray",	"RT",	"Buxton",	"Hooper",	"hockingstuart",	"Fletchers",	"RW",	"Jellis",	"Marshall",	"Biggin",	"YPA",	"Barry",	"Miles",	"Noel",	"Stockdale",	"Chambers",	"Caine",	"Rendina",	"Sweeney",	"Morleys",	"Whiting",	"Greg",	"Brad",	"GL",	"Wilson",	"Woodards",	"Eview",	"Walshe",	"Harcourts",	"Gary",	"Maddison",	"David",	"Buxton/Find",	"Nicholson",	"Barlow",	"McGrath",	"Thomson",	"Grantham",	"hockingstuart/Advantage",	"William",	"Williams",	"O'Brien",	"Douglas",	"Paul",	"Ken",	"Chisholm",	"Beller",	"Professionals",	"Ross",	"Zahn",	"C21",	"Re",	"Jas",	"Cayzer",	"O'Donoghues",	"Melbourne",	"Village",	"Nick",	"Hodges",	"McDonald",	"Garvey",	"Burnham",	"Tim",	"Kay",	"Brace",	"Edward",	"Castran",	"Alexkarbon",	"Abercromby's",	"LITTLE",	"Australian",	"Philip",	"Love",	"Dingle",	"Appleby",	"Purplebricks",	"Pride",	"Morrison",	"New",	"Gunn&Co",	"Lindellas",	"Weda",	"R&H",	"Bayside",	"Collins",	"Sotheby's",	"Bells",	"MICM",	"Owen",	"Moonee",	"Raine",	"Parkes",	"Charlton",	"Bekdon",	"Haughton",	"Buckingham",	"Galldon",	"Leased",	"Rodney",	"Assisi",	"J",	"FN",	"Trimson",	"Airport",	"Considine",	"Thomas",	"Harrington",	"Christopher",	"hockingstuart/Buxton",	"Peter",	"Ham",	"Pagan",	"JMRE",	"Lucas",	"ASL",	"Frank",	"Hamilton",	"HAR",	"Nguyen",	"Besser",	"Blue",	"Wood",	"W.B.",	"Changing",	"Luxe",	"Darren",	"Allens",	"iTRAK",	"S&L",	"Karen",	"Holland",	"Ascend",	"Oak",	"Iconek",	"Calder",	"Crane",	"Buxton/Advantage",	"Anderson",	"Kelly",	"D'Aprano",	"Scott",	"Naison",	"Hunter",	"VICPROP",	"Johnston",	"Weast",	"Direct",	"Coventry",	"Domain",	"Luxton",	"Prof.",	"Propertyau",	"Joseph",	"Red",	"Vic",	"Fletchers/One",	"Dixon",	"Raine&Horne",	"Win",	"One",	"Jason",	"Walsh",	"Parkinson",	"Del",	"Matthew",	"Prowse",	"First",	"Meadows",	"hockingstuart/Village",	"iOne",	"Compton",	"Redina",	"Century",	"Homes",	"Allan",	"Geoff",	"Nardella",	"Elite",	"LJ",	"AIME",	"North",	"Mandy",	"Batty",	"Tiernan's",	"Property",	"Private/Tiernan's",	"Bustin",	"Joe",	"Sweeney/Advantage",	"Reach",	"Fletchers/Fletchers",	"Clairmont",	"hockingstuart/Barry",	"Steveway",	
                                                            "Mason"))
                                                            

# Check for missing values in 'Age', Rooms, Bedrooms, Bathrooms, Landsize
missing_age <- sum(is.na(train_data$Age))
if (missing_age > 0) {
  # Impute missing values in 'Age' with the mean
  mean_age <- mean(train_data$Age, na.rm = TRUE)
  train_data$Age[is.na(train_data$Age)] <- mean_age
}

missing_bedrooms <- sum(is.na(train_data$Bedroom2))
if (missing_bedrooms > 0) {
  # Impute missing values in bedroom with the zero
  train_data$Bedroom2[is.na(train_data$Bedroom2)] <- 0
}

missing_bathrooms <- sum(is.na(train_data$Bathroom))
if (missing_bathrooms > 0) {
  # Impute missing values in bathroom with the zero
  train_data$Bathroom[is.na(train_data$Bathroom)] <- 0
}

missing_landsize <- sum(is.na(train_data$Landsize))
if (missing_landsize > 0) {
  # Impute missing values in landsize with the zero or mean
  #mean_landsize <- mean(train_data$Landsize, na.rm = TRUE)
  train_data$Landsize[is.na(train_data$Landsize)] <- 0
}

missing_car <- sum(is.na(train_data$Car))
if (missing_car > 0) {
  # Impute missing values in car with the zero
  train_data$Car[is.na(train_data$Car)] <- 0
}

#Rooms	Type	Price	Method	SellerG	Distance	Postcode	Bedroom2	Bathroom	Car	Landsize	BuildingArea	YearBuilt	CouncilArea	Age

# Fit a linear regression model
model1 <- lm(Price ~ Rooms + Distance + Landsize, data = train_data)
model2 <- lm(Price ~ Rooms + Distance + Landsize + Age, data = train_data)
model3 <- lm(Price ~ Rooms + Distance + Landsize + Age + Bedroom2 + Bathroom + Car, data = train_data)
model4 <- lm(Price ~ Rooms + Distance + Landsize + Age + Bedroom2 + Bathroom + Car + Type + Method + Postcode, data = train_data)


# Summary of the model
summary(model1)
summary(model2)
summary(model3)
summary(model4)

# Load the test data
test_data <- read.csv("E:/Friends/Prakash/housing_test.csv")  # Replace file path with your actual file name

# Convert Categorical variable to a factor with specified levels
test_data$Type <- factor(test_data$Type, levels = c("h","u","t"))

test_data$Method <- factor(test_data$Method, levels = c("S", "SP",	"VB",	"PI",	"SA"))

test_data$Postcode <- factor(test_data$Postcode, levels = c("3056",	"3073",	"3015",	"3187",	"3123",	"3165",	"3127",	"3033",	"3163",	"3141",	"3046",	"3071",	"3012",	"3124",	"3081",	"3103",	"3072",	"3186",	"3065",	"3101",	"3031",	"3042",	"3020",	"3142",	"3144",	"3044",	"3107",	"3058",	"3108",	"3182",	"3204",	"3068",	"3102",	"3206",	"3002",	"3016",	"3052",	"3148",	"3122",	"3079",	"3125",	"3034",	"3185",	"3032",	"3011",	"3189",	"3018",	"3055",	"3040",	"3121",	"3146",	"3039",	"3188",	"3104",	"3145",	"3051",	"3181",	"3019",	"3041",	"3143",	"3087",	"3054",	"3105",	"3013",	"3184",	"3167",	"3060",	"3066",	"3207",	"3084",	"3161",	"3003",	"3147",	"3057",	"3166",	"3021",	"3000",	"3083",	"3183",	"3043",	"3205",	"3078",	"3070",	"3126",	"3025",	"3162",	"3067",	"3053",	"3006",	"3085",	"3128",	"3008",	"3047",	"3061"))

test_data$SellerG <- factor(test_data$SellerG, levels = c("Nelson",	"Ray",	"RT",	"Buxton",	"Hooper",	"hockingstuart",	"Fletchers",	"RW",	"Jellis",	"Marshall",	"Biggin",	"YPA",	"Barry",	"Miles",	"Noel",	"Stockdale",	"Chambers",	"Caine",	"Rendina",	"Sweeney",	"Morleys",	"Whiting",	"Greg",	"Brad",	"GL",	"Wilson",	"Woodards",	"Eview",	"Walshe",	"Harcourts",	"Gary",	"Maddison",	"David",	"Buxton/Find",	"Nicholson",	"Barlow",	"McGrath",	"Thomson",	"Grantham",	"hockingstuart/Advantage",	"William",	"Williams",	"O'Brien",	"Douglas",	"Paul",	"Ken",	"Chisholm",	"Beller",	"Professionals",	"Ross",	"Zahn",	"C21",	"Re",	"Jas",	"Cayzer",	"O'Donoghues",	"Melbourne",	"Village",	"Nick",	"Hodges",	"McDonald",	"Garvey",	"Burnham",	"Tim",	"Kay",	"Brace",	"Edward",	"Castran",	"Alexkarbon",	"Abercromby's",	"LITTLE",	"Australian",	"Philip",	"Love",	"Dingle",	"Appleby",	"Purplebricks",	"Pride",	"Morrison",	"New",	"Gunn&Co",	"Lindellas",	"Weda",	"R&H",	"Bayside",	"Collins",	"Sotheby's",	"Bells",	"MICM",	"Owen",	"Moonee",	"Raine",	"Parkes",	"Charlton",	"Bekdon",	"Haughton",	"Buckingham",	"Galldon",	"Leased",	"Rodney",	"Assisi",	"J",	"FN",	"Trimson",	"Airport",	"Considine",	"Thomas",	"Harrington",	"Christopher",	"hockingstuart/Buxton",	"Peter",	"Ham",	"Pagan",	"JMRE",	"Lucas",	"ASL",	"Frank",	"Hamilton",	"HAR",	"Nguyen",	"Besser",	"Blue",	"Wood",	"W.B.",	"Changing",	"Luxe",	"Darren",	"Allens",	"iTRAK",	"S&L",	"Karen",	"Holland",	"Ascend",	"Oak",	"Iconek",	"Calder",	"Crane",	"Buxton/Advantage",	"Anderson",	"Kelly",	"D'Aprano",	"Scott",	"Naison",	"Hunter",	"VICPROP",	"Johnston",	"Weast",	"Direct",	"Coventry",	"Domain",	"Luxton",	"Prof.",	"Propertyau",	"Joseph",	"Red",	"Vic",	"Fletchers/One",	"Dixon",	"Raine&Horne",	"Win",	"One",	"Jason",	"Walsh",	"Parkinson",	"Del",	"Matthew",	"Prowse",	"First",	"Meadows",	"hockingstuart/Village",	"iOne",	"Compton",	"Redina",	"Century",	"Homes",	"Allan",	"Geoff",	"Nardella",	"Elite",	"LJ",	"AIME",	"North",	"Mandy",	"Batty",	"Tiernan's",	"Property",	"Private/Tiernan's",	"Bustin",	"Joe",	"Sweeney/Advantage",	"Reach",	"Fletchers/Fletchers",	"Clairmont",	"hockingstuart/Barry",	"Steveway",	
                                                            "Mason"))


# Check for missing values in 'Age', Rooms, Bedrooms, Bathrooms, Landsize
missing_age <- sum(is.na(test_data$Age))
if (missing_age > 0) {
  # Impute missing values in 'Age' with the mean
  mean_age <- mean(test_data$Age, na.rm = TRUE)
  test_data$Age[is.na(test_data$Age)] <- mean_age
}

missing_bedrooms <- sum(is.na(test_data$Bedroom2))
if (missing_bedrooms > 0) {
  # Impute missing values in bedroom with the zero
  test_data$Bedroom2[is.na(test_data$Bedroom2)] <- 0
}

missing_bathrooms <- sum(is.na(test_data$Bathroom))
if (missing_bathrooms > 0) {
  # Impute missing values in bathroom with the zero
  test_data$Bathroom[is.na(test_data$Bathroom)] <- 0
}

missing_landsize <- sum(is.na(test_data$Landsize))
if (missing_landsize > 0) {
  # Impute missing values in landsize with the zero or mean
  #mean_landsize <- mean(test_data$Landsize, na.rm = TRUE)
  test_data$Landsize[is.na(test_data$Landsize)] <- 0
}

missing_car <- sum(is.na(test_data$Car))
if (missing_car > 0) {
  # Impute missing values in car with the zero
  test_data$Car[is.na(test_data$Car)] <- 0
}

# Make predictions on the test data
predictions1 <- predict(model1, newdata = test_data)
predictions2 <- predict(model2, newdata = test_data)
predictions3 <- predict(model3, newdata = test_data)
predictions4 <- predict(model4, newdata = test_data)

# Add the predictions to the test data
test_data$PredictedPrice1 <- predictions1
test_data$PredictedPrice2 <- predictions2
test_data$PredictedPrice3 <- predictions3
test_data$PredictedPrice4 <- predictions4

# Save the test data with predictions
write.csv(test_data, file = "E:/Friends/Prakash/housing_out.csv", row.names = FALSE)

