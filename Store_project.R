library(readr)
#Load Training Data
train_data <- read_csv("E:/Friends/Prakash/Project2/store_train.csv")

# Convert Categorical variable to a factor with specified levels
train_data$store_Type <- factor(train_data$store_Type, levels = c("Supermarket Type1",	"Supermarket Type3",	"Grocery Store",	"Supermarket Type2"))

train_data$country <- factor(train_data$country, levels = c("9",	"1",	"13",	"35",	"27",	"103",	"183",	"89",	"57",	"3",	"109",	"73",	"11",	"115",	"113",	"5",	"99",	"107",	"81",	"19",	"71",	"31",	"127",	"23",	"7",	"25",	"67",	"139",	"75",	"45",	"15",	"47",	"29",	"61",	"111",	"117",	"33",	"77",	"149",	"630",	"21",	"41",	"69",	"167",	"55",	"30",	"505",	"413",	"147",	"85",	"720",	"63",	"175",	"199",	"169",	"125",	"91",	"153",	"137",	"43",	"17",	"265",	"39",	"161",	"191",	"65",	"145",	"87",	"83",	"119",	"133",	"287",	"273",	"159",	"221",	"399",	"187",	"540",	"49",	"363",	"59",	"121",	"53",	"510",	"177",	"189",	"299",	"257",	"163",	"790",	"51",	"157",	"101",	"211",	"95",	"240",	"143",	"123",	"230",	"155",	"37",	"79",	"195",	"131",	"235",	"237",	"223",	"387",	"205",	"678",	"100",	"307",	"600",	"469",	"165",	"431",	"377",	"441",	"129",	"281",	"267",	"135",	"93",	"171",	"810",	"373",	"423",	"197",	"830",	"122",	"16",	"493",	"415",	"97",	"270",	"233",	"397",	"239",	"317",	"241",	"660",	"401",	"225",	"245",	"193",	"275",	"213",	"489",	"105",	"164",	"359",	"283",	"203",	"201",	"343",	"345",	"357",	"730",	"683",	"173",	"60",	"151",	"185",	"760",	"12",	"141",	"179",	"263",	"6",	"289",	"409",	"181",	"433",	"271",	"735",	"170",	"295",	"293",	"425",	"279",	"207",	"261",	"349",	"391",	"291",	"347",	"259",	"570",	"321",	"443",	"331",	"54",	"449",	"186",	"198",	"285",	"437",	"395",	"227",	"457",	"351",	"180",	"453",	"365",	"305",	"435",	"209",	"303",	"110",	"471",	"90",	"217",	"311",	"670",	"231",	"640",	"315",	"403",	"325",	"220",	"455",	"68",	"333",	"337",	"229",	"451",	"247",	"465",	"445",	"840",	"253",	"740",	"219",	"251",	"427",	"580",	"383",	"10",	"411",	"20",	"479",	"560",	"475",	"491",	"329",	"477",	"249",	"355",	"610",	"361",	"405",	"503",	"70",	"710",	"150",	"309",	"319",	"56",	"327",	"463",	"487",	"407",	"277",	"297",	"820",	"417",	"130",	"459",	"369",	"375",	"595",	"243",	"86",	"690",	"188",	"282",	"290",	"14",	"215",	"255",	"269",	"301",	"313",	"28",	"323",	"335",	"339",	"341",	"353",	"367",	"371",	"379",	"381",	"385",	"389",	"393",	"419",	"421",	"429",	"439",	"447",	"461",	"467",	"473",	"481",	"483",	"485",	"495",	"497",	"499",	"501",	"507",	"520",	"530",	"550",	"590",	"620",	"685",	"700",	"750",	"770",	"775",	"800",	"78",	"680",	"36",	"NA",	"50",	"650",	
                                                            "515"))

train_data$State <- factor(train_data$State, levels = c("23",	"50",	"25",	"6",	"26",	"37",	"12",	"5",	"53",	"28",	"31",	"21",	"33",	"19",	"30",	"13",	"54",	"49",	"42",	"55",	"44",	"29",	"17",	"45",	"46",	"39",	"22",	"18",	"32",	"40",	"38",	"51",	"78",	"48",	"72",	"41",	"27",	"24",	"11",	"8",	"9",	"20",	"56",	"16",	"47",	"34",	"36",	"1",	"10",	"35",	"2",	"15",	"4",	
                                                        "66"))

library(aod)
library(ggplot2)

#Logit models
logit1 <- glm(store ~ sales0 + sales1 + sales2 + sales3 + sales4, 
               family = binomial(link = "logit"), 
               data = train_data)
logit2 <- glm(store ~ sales0 + sales1 + sales2 + sales3 + sales4 + population, 
               family = binomial(link = "logit"), 
               data = train_data)
logit3 <- glm(store ~ sales0 + sales1 + sales2 + sales3 + sales4 + population + store_Type, 
               family = binomial(link = "logit"), 
               data = train_data)
logit4 <- glm(store ~ sales0 + sales1 + sales2 + sales3 + sales4 + population + store_Type + State + country, 
               family = binomial(link = "logit"), 
               data = train_data)


#Probit models
probit1 <- glm(store ~ sales0 + sales1 + sales2 + sales3 + sales4, 
               family = binomial(link = "probit"), 
               data = train_data)
probit2 <- glm(store ~ sales0 + sales1 + sales2 + sales3 + sales4 + population, 
               family = binomial(link = "probit"), 
               data = train_data)
probit3 <- glm(store ~ sales0 + sales1 + sales2 + sales3 + sales4 + population + store_Type, 
               family = binomial(link = "probit"), 
               data = train_data)
probit4 <- glm(store ~ sales0 + sales1 + sales2 + sales3 + sales4 + population + store_Type + State + country, 
               family = binomial(link = "probit"), 
               data = train_data)

#Load test data
test_data <- read_csv("E:/Friends/Prakash/Project2/store_test.csv")

# Convert Categorical variable to a factor with specified levels
test_data$store_Type <- factor(test_data$store_Type, levels = c("Supermarket Type1",	"Supermarket Type3",	"Grocery Store",	"Supermarket Type2"))

test_data$country <- factor(test_data$country, levels = c("9",	"1",	"13",	"35",	"27",	"103",	"183",	"89",	"57",	"3",	"109",	"73",	"11",	"115",	"113",	"5",	"99",	"107",	"81",	"19",	"71",	"31",	"127",	"23",	"7",	"25",	"67",	"139",	"75",	"45",	"15",	"47",	"29",	"61",	"111",	"117",	"33",	"77",	"149",	"630",	"21",	"41",	"69",	"167",	"55",	"30",	"505",	"413",	"147",	"85",	"720",	"63",	"175",	"199",	"169",	"125",	"91",	"153",	"137",	"43",	"17",	"265",	"39",	"161",	"191",	"65",	"145",	"87",	"83",	"119",	"133",	"287",	"273",	"159",	"221",	"399",	"187",	"540",	"49",	"363",	"59",	"121",	"53",	"510",	"177",	"189",	"299",	"257",	"163",	"790",	"51",	"157",	"101",	"211",	"95",	"240",	"143",	"123",	"230",	"155",	"37",	"79",	"195",	"131",	"235",	"237",	"223",	"387",	"205",	"678",	"100",	"307",	"600",	"469",	"165",	"431",	"377",	"441",	"129",	"281",	"267",	"135",	"93",	"171",	"810",	"373",	"423",	"197",	"830",	"122",	"16",	"493",	"415",	"97",	"270",	"233",	"397",	"239",	"317",	"241",	"660",	"401",	"225",	"245",	"193",	"275",	"213",	"489",	"105",	"164",	"359",	"283",	"203",	"201",	"343",	"345",	"357",	"730",	"683",	"173",	"60",	"151",	"185",	"760",	"12",	"141",	"179",	"263",	"6",	"289",	"409",	"181",	"433",	"271",	"735",	"170",	"295",	"293",	"425",	"279",	"207",	"261",	"349",	"391",	"291",	"347",	"259",	"570",	"321",	"443",	"331",	"54",	"449",	"186",	"198",	"285",	"437",	"395",	"227",	"457",	"351",	"180",	"453",	"365",	"305",	"435",	"209",	"303",	"110",	"471",	"90",	"217",	"311",	"670",	"231",	"640",	"315",	"403",	"325",	"220",	"455",	"68",	"333",	"337",	"229",	"451",	"247",	"465",	"445",	"840",	"253",	"740",	"219",	"251",	"427",	"580",	"383",	"10",	"411",	"20",	"479",	"560",	"475",	"491",	"329",	"477",	"249",	"355",	"610",	"361",	"405",	"503",	"70",	"710",	"150",	"309",	"319",	"56",	"327",	"463",	"487",	"407",	"277",	"297",	"820",	"417",	"130",	"459",	"369",	"375",	"595",	"243",	"86",	"690",	"188",	"282",	"290",	"14",	"215",	"255",	"269",	"301",	"313",	"28",	"323",	"335",	"339",	"341",	"353",	"367",	"371",	"379",	"381",	"385",	"389",	"393",	"419",	"421",	"429",	"439",	"447",	"461",	"467",	"473",	"481",	"483",	"485",	"495",	"497",	"499",	"501",	"507",	"520",	"530",	"550",	"590",	"620",	"685",	"700",	"750",	"770",	"775",	"800",	"78",	"680",	"36",	"NA",	"50",	"650",	
                                                            "515"))

test_data$State <- factor(test_data$State, levels = c("23",	"50",	"25",	"6",	"26",	"37",	"12",	"5",	"53",	"28",	"31",	"21",	"33",	"19",	"30",	"13",	"54",	"49",	"42",	"55",	"44",	"29",	"17",	"45",	"46",	"39",	"22",	"18",	"32",	"40",	"38",	"51",	"78",	"48",	"72",	"41",	"27",	"24",	"11",	"8",	"9",	"20",	"56",	"16",	"47",	"34",	"36",	"1",	"10",	"35",	"2",	"15",	"4",	
                                                        "66"))

#Predictions
prediction1 <- predict(probit1, 
                       newdata = test_data,
                       type = "response")
prediction2 <- predict(probit2, 
                       newdata = test_data,
                       type = "response")
prediction3 <- predict(probit3, 
                       newdata = test_data,
                       type = "response")
prediction4 <- predict(probit4, 
                       newdata = test_data,
                       type = "response")
prediction5 <- predict(logit1, 
                       newdata = test_data,
                       type = "response")
prediction6 <- predict(logit2, 
                       newdata = test_data,
                       type = "response")
prediction7 <- predict(logit3, 
                       newdata = test_data,
                       type = "response")
prediction8 <- predict(logit4, 
                       newdata = test_data,
                       type = "response")

#Add predicted probabilities to test data
test_data$prob1 <- prediction1
test_data$prob2 <- prediction2
test_data$prob3 <- prediction3
test_data$prob4 <- prediction4
test_data$prob5 <- prediction5
test_data$prob6 <- prediction6
test_data$prob7 <- prediction7
test_data$prob8 <- prediction8

# Save the test data with predictions
write.csv(test_data, file = "E:/Friends/Prakash/Project2/store_out.csv", row.names = FALSE)

