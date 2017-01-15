# reading the data files 
require(readxl)
#libraries for places extract (geo maps)
library(RCurl)
library(RJSONIO)

# setting working directory

#reading properties Data 
#propertiesdata <- read.csv("PropertiesData_500_300_neighbour_score.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,na.strings = c(""," ","[]"),strip.white=TRUE)
#buyerdata <- read.csv("BuyerData.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,na.strings = c(""," ","[]"),strip.white=TRUE)

# Function for posting new property - append (New Seller)--- Submit Home 
# arg1 is a row vector consist of values for all headers in Properties data file
submithome <- function(arg1){
  propertiesdata <- read.csv("PropertiesData_500_300_neighbour_score.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,na.strings = c(""," ","[]"),strip.white=TRUE)
  newrow <- arg1 # All the details are given in a data row vector except neighbourhood score
  newrow$house_id <- nrow(propertiesdata) + 1
  newrow_neighborscore <- df_neighborhoodscore(newrow)
  
  data_updated <- rbind(propertiesdata,newrow_neighborscore)
  write.csv(data_updated, file="PropertiesData_500_300_neighbour_score.csv",row.names=FALSE)
  return()
}
#testing whether function working or not 
#testproperty <- read.csv("PropoertiesData.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,na.strings = c(""," ","[]"),strip.white=TRUE)
#newrow <- testproperty[1:2,]
#submithome(newrow)

###############################################
# Function for storing the search - Buyer input 
# arg1 is a row vector consist of values for all headers in buyer data file
# if user selects multiple options under any header ... Assign NA for that label, so that subset does not happen in search function
buyerdata_append <- function(arg1){
  buyerdata <- read.csv("BuyerData.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,na.strings = c(""," ","[]"),strip.white=TRUE)
  newrow <- arg1
  newrow$user_id <- user_details$id
  data_updated <- rbind(buyerdata,newrow)
  write.csv(data_updated, file="BuyerData.csv",row.names=FALSE)
  return()
}
#testing whether function working or not 
# testbuyer <- read.csv("BuyerData.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,na.strings = c(""," ","[]"),strip.white=TRUE)
# newrow <- testbuyer[1:2,]
# buyerdata_append(newrow)

################################################
# Function for calculating approx rental value 
rentcalculation <- function(arg1){
  rentsinfo <- read_excel("RentInfo_AreaScore.xlsx",1,na="")
  propdata <- arg1 #propertiesdata
  propdata$approxrent <- NULL
  for(i in 1:dim(propdata)[1]){
    rowindex <- which(rentsinfo$area == propdata$area[i])
    if(propdata$property_type[i]=="apartment"){
      propdata$approxrent[i] <- rentsinfo[rowindex,(dim(rentsinfo)[2]+as.numeric(propdata$bedrooms[i])-3)] 
    }
    else{
      one_bhkrent <- 0
      two_bhkrent <- 0
      three_bhkrent <- 0 
      
      if(propdata$X1bhk[i]!="NA")
        one_bhkrent <- as.numeric(propdata$X1bhk[i])* rentsinfo[rowindex,which(colnames(rentsinfo)=="1BHK_House")]  
      if(propdata$X2bhk[i]!="NA")
        two_bhkrent <- as.numeric(propdata$X2bhk[i])* rentsinfo[rowindex,which(colnames(rentsinfo)=="2BHK_House")]  
      if(propdata$X3bhk[i]!="NA")
        three_bhkrent <- as.numeric(propdata$X3bhk[i])* rentsinfo[rowindex,which(colnames(rentsinfo)=="3BHK_House")]  
      
      propdata$approxrent[i] <- one_bhkrent + two_bhkrent + three_bhkrent 
    }
  }
  return(propdata)  
}

###########################################################
# Neighbourhood score calculation 
# A. make url using "lat, long, place type, radius" 
url <- function(lat,long,radius,placetype){
  root1 <- "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location="
  root1 <- paste(root1,lat,sep="")
  root1 <- paste(root1,long,sep=",")
  root1 <- paste(root1,"&radius=",sep="")
  root1 <- paste(root1,radius,"&type=",placetype,"&key=","AIzaSyCOy3XaNOmug9z1GvhWqLgtw-McbFsKNVc",sep="") #Sai's Key
}
# B. Extract Places info as JSON File 
nearbyplace_json <- function(lat, long,radius,placetype) {
  u <- url(lat, long,radius,placetype)
  doc <- getURL(u)
  x <- fromJSON(doc,simplify = FALSE)
}
# C. count placetype 
placescount <- function(lat, long,radius,placetype){
  jsonfile <- nearbyplace_json(lat,long,radius,placetype)
  x <- jsonfile
  if(x$status=="OK") {
    countofschools <- length(x$results)
    return(countofschools)
  }else {
    return(c(0))
  }
}
# D. Neighborhood score for given lat long 
neighborhoodscore <- function(lat,long){
  
  schools_count <- placescount(lat,long,1000,"school")
  hospital_count <- placescount(lat,long,1000,"hospital")
  park_count <- placescount(lat,long,500,"amusement_park")
  atm_count <- placescount(lat,long,500,"atm")
  pharmacy_count <- placescount(lat,long,500,"pharmacy")
  store_count <- placescount(lat,long,500,"grocery_or_supermarket")
  restaurant_count <- placescount(lat,long,500,"restaurant")
  
  school_score <- if(schools_count>=2){10}else if(schools_count>=1){5} else 0
  hospital_score <- if(hospital_count>=1){10}else if(hospital_count>=1){5} else 0
  park_score <- if(park_count>=1){10}else 0
  atm_score <- if(atm_count>=2){10}else if(atm_count>=1){5} else 0
  pharmacy_score <- if(pharmacy_count>=2){10}else if(pharmacy_count>=1){5} else 0
  store_score <- if(store_count>=2){10}else if(store_count>=1){5} else 0
  restaurant_score <- if(restaurant_count>=2){10}else if(restaurant_count>=1){5} else 0
  
  final_neighborhoodscore <- sum(school_score,hospital_score,park_score,atm_score,
                                 pharmacy_score,store_score,restaurant_score)/length(c(school_score,hospital_score,park_score,atm_score,
                                                                                       pharmacy_score,store_score,restaurant_score))
}
# E. Add the score column and send dataframe with neighborhood score added 
df_neighborhoodscore <- function(arg1){
  propdata <- arg1 #propertiesdata
  propdata$score_neighborhood <- NULL
  for(i in 1:dim(propdata)[1]){
    lat <- as.numeric(propdata$latitude[i])
    long <- as.numeric(propdata$longitude[i])
    if((is.na(lat)==FALSE)&(long!="NA")&(is.na(lat)==FALSE)&(long!="NA")){
      propdata$score_neighborhood[i] <- neighborhoodscore(lat,long) 
    }else{
      propdata$score_neighborhood[i] <- c(0)
    }
  }
  return(propdata)
}

###########################################################
# property recommendation score calculation  
recommend_score <- function(arg1){
  propdata <- arg1 #propertiesdata_sub_rent_neighborbood
  rentsinfo <- read_excel("RentInfo_AreaScore.xlsx",1,na="")
  propdata$proprecomendscore <- NULL
  propdata$plotprice_sqft <- NULL
  
  for(i in 1:dim(propdata)[1]){
    rowindex <- which(rentsinfo$area == propdata$area[i])
    sqftprice <- rentsinfo$Sqft_Price[rowindex] 
    propdata$plotprice_sqft[i] <- propdata$plot_area[i]*sqftprice
  }
  
  score_proptype <- 10 
  score_area <- 10
  score_plot_price <- 10
  score_floors <- 10
  score_price <- 10
  score_readytomove <- 10
  score_transaction <- 10 
  score_onloan <- 10 
  score_eastfacing <- 10 
  score_rent <- 10
  
  for(i in 1:dim(propdata)[1]){
    # prop type 
    if(length(unique(propdata$property_type))==2){
      if(propdata$property_type[i]=="apartment")
        score_proptype = 5 
      else 
        score_proptype = 10
    }
    # area/location
    rowindex <- which(rentsinfo$area == propdata$area[i])
    score_area = rentsinfo$Areascore[rowindex]
    
    # plot area - plotprice_sqft
    score_plot_price = (10*propdata$plotprice_sqft[i])/max(propdata$plotprice_sqft,na.rm=FALSE)
    
    # floors 
    if(length(unique(propdata$property_type))==2){
      if(propdata$property_type[i]=="apartment")
        score_floors = 0
      else 
        score_floors = (10*as.numeric(propdata$total_floors[i]))/as.numeric(max(subset(propdata,propdata$property_type=="independent_house")$total_floors))
    }
    
    # price 
    score_price = 10 - ((10*propdata$price[i])/max(propdata$price,na.rm=FALSE))
    
    # ready to move 
    if(propdata$availability[i]=="ready_to_move")
      score_readytomove = 10
    else 
      score_readytomove = 8 
    
    #transaction 
    if(propdata$transaction_type[i]=="new")
      score_transaction = 10
    else 
      score_transaction = 8 
    
    # loan 
    if(propdata$on_loan[i]=="yes")
      score_onloan = 10
    else 
      score_onloan = 7 
    
    # site facing  
    if(propdata$site_facing[i]=="east")
      score_eastfacing = 10
    else 
      score_eastfacing = 8 
    
    # rent 
    score_rent = (10*propdata$approxrent[i])/max(propdata$approxrent,na.rm=FALSE)
    
    # neighborhood score 
    score_neighborhood = propdata$score_neighborhood[i]
    
    finalscore <- sum(score_proptype,  
                      score_area,
                      score_plot_price,
                      score_floors,
                      score_price,
                      score_readytomove,
                      score_transaction, 
                      score_onloan, 
                      score_eastfacing, 
                      score_rent,
                      score_neighborhood)/length(c(score_proptype,  
                                                   score_area,
                                                   score_plot_price,
                                                   score_floors,
                                                   score_price,
                                                   score_readytomove,
                                                   score_transaction, 
                                                   score_onloan, 
                                                   score_eastfacing, 
                                                   score_rent,
                                                   score_neighborhood))
    propdata$proprecomendscore[i] <- round(finalscore,2)
  }
  return(propdata)
}

#################################################
# Function for searching the relevant properties 
property_search <- function(search_city , search_housetype , search_zone , search_minprice ,
                            search_maxprice , search_location , search_minplot , search_maxplot ,
                            search_bhk , search_nooffloors , search_transaction , search_siteface ,
                            search_doorface , search_onloan , search_furnishing , search_available ,
                            search_parking , search_flooring , search_water , search_age , search_rentexp ){
  
  propertiesdata <- read.csv("PropertiesData_500_300_neighbour_score.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,na.strings = c(""," ","[]"),strip.white=TRUE)
  
  propertiesdata_sub <- as.data.frame(rep(0,1))
  propertiesdata_sub_rent <- as.data.frame(rep(0,1))
  propertiesdata_sub_rent_neighborbood <- as.data.frame(rep(0,1))
  propertiesdata_sub_final <- as.data.frame(rep(0,1))
  
  # search filter starts here 
  # location subset 
  propertiesdata_sub <- propertiesdata
  originalsize <- dim(propertiesdata_sub)[1]
  if((is.na(search_city)==FALSE)&(search_city!="NA"))
    propertiesdata_sub <- subset(propertiesdata, tolower(propertiesdata$city)==search_city)
  # Property type (apartment/independent house)....OR....Both could be selected 
  if((is.na(search_housetype[1])==FALSE)&(search_housetype[1]!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1))
    propertiesdata_sub <- subset(propertiesdata_sub, propertiesdata_sub$property_type %in% search_housetype) 
  # ****input is just zone, which is concatenated with city name ***converted to lower and subset is done
  if((is.na(search_zone[1])==FALSE)&(search_zone[1]!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1))
    propertiesdata_sub <- subset(propertiesdata_sub, tolower(propertiesdata_sub$zone) %in% paste(search_zone,search_city))
  # Price Filter - subset for minimum price 
  if((is.na(search_minprice)==FALSE)&(search_minprice!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1))
    propertiesdata_sub <- subset(propertiesdata_sub, propertiesdata_sub$price>=as.numeric(search_minprice))
  # subset for maximum price 
  if((is.na(search_maxprice)==FALSE)&(search_maxprice!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1))
    propertiesdata_sub <- subset(propertiesdata_sub, propertiesdata_sub$price<=as.numeric(search_maxprice))
  # subset for location # catch here subset only if search location is available 
  if((is.na(search_location[1])==FALSE)&(search_location!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1))
  { if(sum(propertiesdata_sub$area==search_location)>=1)
    propertiesdata_sub <- subset(propertiesdata_sub, tolower(propertiesdata_sub$area) %in% search_location)
  }
  if((is.na(search_minplot)==FALSE)&(search_minplot!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1))
    propertiesdata_sub <- subset(propertiesdata_sub, propertiesdata_sub$plot_area>=as.numeric(search_minplot))
  # subset for maximum price 
  if((is.na(search_maxplot)==FALSE)&(search_maxplot!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1))
    propertiesdata_sub <- subset(propertiesdata_sub, propertiesdata_sub$plot_area<=as.numeric(search_maxplot))
  # subset for apartment/independent house (2bhk/2floor house) 
  if((length(search_housetype)>=2)&(search_housetype[1]!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1))
  { apartments <- subset(propertiesdata_sub, propertiesdata_sub$property_type=="apartment")
  houses <- subset(propertiesdata_sub, propertiesdata_sub$property_type=="independent_house")
  apartment_sub <- subset(apartments,apartments$bedrooms>=min(as.numeric(search_bhk)))
  houses_sub <- subset(houses, houses$total_floors>=min(as.numeric(search_nooffloors)))
  propertiesdata_sub <- rbind(apartment_sub,houses_sub)
  }else if((propertiesdata_sub$property_type[1]=="apartment")&(is.na(search_bhk)==FALSE)&(search_bhk!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1)){
    propertiesdata_sub <- subset(propertiesdata_sub,propertiesdata_sub$bedrooms>=min(as.numeric(search_bhk)))
  }else{
    if((is.na(search_nooffloors)==FALSE)&(search_nooffloors!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1)){
      propertiesdata_sub <- subset(propertiesdata_sub,propertiesdata_sub$total_floors>=min(as.numeric(search_nooffloors)))
    }
  }
  #resale or new (transaction_type)
  if((is.na(search_transaction[1])==FALSE)&(search_transaction!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1))
    propertiesdata_sub <- subset(propertiesdata_sub, propertiesdata_sub$transaction_type %in% search_transaction)
  # site facing 
  if((is.na(search_siteface[1])==FALSE)&(search_siteface!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1))
    propertiesdata_sub <- subset(propertiesdata_sub, propertiesdata_sub$site_facing %in% search_siteface)
  # door facing 
  if((is.na(search_doorface[1])==FALSE)&(search_doorface!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1))
    propertiesdata_sub <- subset(propertiesdata_sub, propertiesdata_sub$door_facing %in% search_doorface)
  # on loan
  if((is.na(search_onloan[1])==FALSE)&(search_onloan!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1))
    propertiesdata_sub <- subset(propertiesdata_sub, propertiesdata_sub$on_loan %in% search_onloan)
  # furnishing 
  if((is.na(search_furnishing[1])==FALSE)&(search_furnishing!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1))
    propertiesdata_sub <- subset(propertiesdata_sub, propertiesdata_sub$furnishing %in% search_furnishing)
  # available 
  if((is.na(search_available[1])==FALSE)&(search_available!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1))
    propertiesdata_sub <- subset(propertiesdata_sub, propertiesdata_sub$availability %in% search_available)
  # parking
  if((is.na(search_parking[1])==FALSE)&(search_parking!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1))
    propertiesdata_sub <- subset(propertiesdata_sub, propertiesdata_sub$parking %in% search_parking)
  # flooring 
  if((is.na(search_flooring[1])==FALSE)&(search_flooring!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1))
    propertiesdata_sub <- subset(propertiesdata_sub, propertiesdata_sub$flooring_type %in% search_flooring)
  # water 
  if((is.na(search_water[1])==FALSE)&(search_water!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1))
    propertiesdata_sub <- subset(propertiesdata_sub, propertiesdata_sub$water_source %in% search_water)
  # age 
  if((is.na(search_age[1])==FALSE)&(search_age!="NA")&(dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1))
    propertiesdata_sub <- subset(propertiesdata_sub, (lubridate::year(Sys.Date())- propertiesdata_sub$year_of_construction)>= min(as.numeric(search_age)))
  
  # rent expected
  if((dim(propertiesdata_sub)[1]>=1)&(dim(propertiesdata_sub)[2]>1)){
    propertiesdata_sub_rent <- rentcalculation(propertiesdata_sub)
    if((is.na(search_rentexp)==FALSE)&(search_rentexp!="NA")){
      propertiesdata_sub_rent <- subset(propertiesdata_sub_rent, propertiesdata_sub_rent$approxrent >= as.numeric(search_rentexp))
    }
  }
  
  # Neighbourhood score added - this function is not in use as it is creating latency. For moment these values are calculated by running the programming 
  # independently and stored the values. Now this function is only called once when every time new home is stored to database, the neighbourhood score is 
  # calculated and stored with entered data. 
  # if(dim(propertiesdata_sub_rent)[1]>=1){
  # propertiesdata_sub_rent_neighborbood <- df_neighborhoodscore(propertiesdata_sub_rent) 
  # }
  if(is.null(propertiesdata_sub_rent)==FALSE)
    propertiesdata_sub_rent_neighborbood <- propertiesdata_sub_rent
  
  # Final with property recommendation score added 
  if(is.null(propertiesdata_sub_rent_neighborbood)==FALSE){
    if((dim(propertiesdata_sub_rent_neighborbood)[1]>=1)&(dim(propertiesdata_sub_rent_neighborbood)[2]>1)){
      propertiesdata_sub_final <- recommend_score(propertiesdata_sub_rent_neighborbood)
    }
  }
  
  # No subset happens due to no selection
  if(dim(propertiesdata_sub)[1]==originalsize){
    propertiesdata_sub_final <- propertiesdata_sub
    propertiesdata_sub_final <- recommend_score(propertiesdata_sub)
  }
  
  if((dim(propertiesdata_sub_final)[1]>=1)&(dim(propertiesdata_sub_final)[2]>1)){
    propertiesdata_sub_final <- arrange(propertiesdata_sub_final,desc(proprecomendscore))
  }
  return(propertiesdata_sub_final)
}

###############################################################
# Function for showing the recommendations on login - For Buyer (Right Properties Details)
buyerlogin <- function(id){
  # id is user id # retrive the search for buyer 
  buyerdata <- read.csv("BuyerData.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,na.strings = c(""," ","[]"),strip.white=TRUE)
  buyerdata <- subset(buyerdata,as.numeric(buyerdata$user_id)==as.numeric(id))
  buyerdata <- buyerdata[nrow(buyerdata),]
  #calling search function 
  prop_buyer_match <- property_search(buyerdata$city, buyerdata$property_type, buyerdata$zone, buyerdata$min_price,
                                      buyerdata$max_price , buyerdata$area , buyerdata$min_plot_area, buyerdata$max_plot_area,
                                      buyerdata$bhk, buyerdata$total_floors, buyerdata$transaction_type, buyerdata$site_facing,
                                      buyerdata$door_facing, buyerdata$on_loan,buyerdata$furnishing,buyerdata$availability,
                                      buyerdata$parking, buyerdata$flooring_type,buyerdata$water_source, buyerdata$property_age, 
                                      buyerdata$expected_rental_income )
  return(prop_buyer_match)
}

##################################################################################
# id here will be house id - For which Buyers has to be recommended 
# Function for showing the recommendations on login - For Seller (Potential Buyers)
sellerlogin <- function(id){
  # id is house id # retrive the recommendation for seller 
  houseid <- id
  buyerdata_all <- read.csv("BuyerData.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,na.strings = c(""," ","[]"),strip.white=TRUE)
  #buyerdata_all <- buyerdata_all[unique(buyerdata_all[,1]),] 
  Propdata <- read.csv("PropertiesData_500_300_neighbour_score.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,na.strings = c(""," ","[]"),strip.white=TRUE)
  sellerdata <- subset(Propdata,as.numeric(Propdata$house_id)==as.numeric(houseid))
  buyerdata_all$zone_combined <- paste(buyerdata_all$zone, buyerdata_all$city)
  
  # potentail buyers algorithm starts here 
  # city subset 
  if((is.na(sellerdata$city)==FALSE)&(sellerdata$city!="NA"))
    buyerdata_sub <- subset(buyerdata_all, tolower(buyerdata_all$city)==sellerdata$city)
  # Property type (apartment/independent house)....
  if((is.na(sellerdata$property_type==FALSE)&(sellerdata$property_type!="NA")&(dim(buyerdata_sub)[1]>=1)&(dim(buyerdata_sub)[2]>1)))
    buyerdata_sub <- subset(buyerdata_sub, buyerdata_sub$property_type==sellerdata$property_type) 
  # ****input is just zone, which is concatenated with city name ***converted to lower and subset is done
  if((is.na(sellerdata$zone)==FALSE)&(sellerdata$zone!="NA")&(dim(buyerdata_sub)[1]>=1)&(dim(buyerdata_sub)[2]>1))
    buyerdata_sub <- subset(buyerdata_sub, tolower(buyerdata_sub$zone_combined)== tolower(sellerdata$zone))
  # for remaining buyers...The search property function is called 
  # for each buyers, few house ids are retrieved 
  # later for this seller the buyer ids will be sorted 
  reco_buyers <- NULL
  for(i in 1:dim(buyerdata_sub)[1]){
    
    buyerdata <- buyerdata_sub[i,]
    
    search_city <- buyerdata$city #tolower(c('Bangalore'))
    search_housetype <- buyerdata$property_type #tolower(c("apartment","independent_house"))
    search_zone <- buyerdata$zone # tolower(c('south','north'))
    search_minprice <- buyerdata$min_price #3000000
    search_maxprice <- buyerdata$max_price#8000000
    search_location <- buyerdata$area#c('electronic_city','hebbal')
    search_minplot <- buyerdata$min_plot_area#500
    search_maxplot <- buyerdata$max_plot_area#1500
    search_bhk <- buyerdata$bhk#c(2,3)
    search_nooffloors <- buyerdata$total_floors#c(2,3)
    search_transaction <- buyerdata$transaction_type#c('resale','new')
    search_siteface <- buyerdata$site_facing#c('south','north','east','west')
    search_doorface <- buyerdata$door_facing#c('south','north','east','west')
    search_onloan <- buyerdata$on_loan#c('yes','no')
    search_furnishing <- buyerdata$furnishing#c('semi','fully')
    search_available <- buyerdata$availability#c('ready_to_move','under_construction')
    search_parking <- buyerdata$parking#c('open','covered')
    search_flooring <- buyerdata$flooring_type#c('vitrified_tiles','marble')
    search_water <- buyerdata$water_source#c('bore_well','both')
    search_age <- buyerdata$property_age#c(2,3,5)
    search_rentexp <- buyerdata$expected_rental_income#10000
    
    sellerhouses <-  property_search(search_city , search_housetype , search_zone , search_minprice ,
                                     search_maxprice , search_location , search_minplot , search_maxplot ,
                                     search_bhk , search_nooffloors , search_transaction , search_siteface ,
                                     search_doorface , search_onloan , search_furnishing , search_available ,
                                     search_parking , search_flooring , search_water , search_age , search_rentexp)
    
    seller_ids <- sellerhouses$house_id
    if(sum(seller_ids==sellerdata$house_id)==1)
    {
      reco_buyers <- cbind(reco_buyers,buyerdata$user_id)
    }
  }
  return(reco_buyers)
}

#######################################
# testing for search function 
# receive the inputs required for property search --- These should come from UI
id=2
buyerdata <- read.csv("BuyerData.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,na.strings = c(""," ","[]"),strip.white=TRUE)
buyerdata <- subset(buyerdata,as.numeric(buyerdata$user_id)==as.numeric(id))

search_city <- buyerdata$city #tolower(c('Bangalore'))
search_housetype <- buyerdata$property_type #tolower(c("apartment","independent_house"))
search_zone <- buyerdata$zone # tolower(c('south','north'))
search_minprice <- buyerdata$min_price #3000000
search_maxprice <- buyerdata$max_price#8000000
search_location <- buyerdata$area#c('electronic_city','hebbal')
search_minplot <- buyerdata$min_plot_area#500
search_maxplot <- buyerdata$max_plot_area#1500
search_bhk <- buyerdata$bhk#c(2,3)
search_nooffloors <- buyerdata$total_floors#c(2,3)
search_transaction <- buyerdata$transaction_type#c('resale','new')
search_siteface <- buyerdata$site_facing#c('south','north','east','west')
search_doorface <- buyerdata$door_facing#c('south','north','east','west')
search_onloan <- buyerdata$on_loan#c('yes','no')
search_furnishing <- buyerdata$furnishing#c('semi','fully')
search_available <- buyerdata$availability#c('ready_to_move','under_construction')
search_parking <- buyerdata$parking#c('open','covered')
search_flooring <- buyerdata$flooring_type#c('vitrified_tiles','marble')
search_water <- buyerdata$water_source#c('bore_well','both')
search_age <- buyerdata$property_age#c(2,3,5)
search_rentexp <- buyerdata$expected_rental_income#10000

prop_final_output <- property_search(search_city , search_housetype , search_zone , search_minprice ,
                                     search_maxprice , search_location , search_minplot , search_maxplot ,
                                     search_bhk , search_nooffloors , search_transaction , search_siteface ,
                                     search_doorface , search_onloan , search_furnishing , search_available ,
                                     search_parking , search_flooring , search_water , search_age , search_rentexp)

#testing buyer login
prop_final_housesmatch <- buyerlogin(id)

#testing seller login
houseid=22 #10, 11, 12, 19, 22, 31, 36, 48, 81, 83, 91 and 201
final_users_match <- sellerlogin(houseid)
