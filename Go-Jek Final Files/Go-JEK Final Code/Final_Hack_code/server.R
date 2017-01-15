library(shiny)
library(Rook)
library(plyr)
#source("Property_Recommendation_11Aug16.R")
source("Property_Recommendation_13Aug16_V2_neighbourscore_fixed.R")
source("Text_Features.R")
shinyServer(function(input,output,session){
  ### SIGNUP FUNCTIONLAITY ###
  ############################
  observeEvent(input$signupBtn, {
    data <- read.csv("users.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,na.strings = c(""," ","[]"),strip.white=TRUE)
    
    id <- nrow(data) + 1
    name <- input$name
    password <- input$password
    email <- input$email
    phone <- input$phone
    newrow <- data.frame(id,name,email,password,phone)
    
    data_updated <- rbind(data,newrow)
    
    write.csv(data_updated, file="users.csv", row.names = FALSE)
    output$successMsg <- renderText({
      HTML(paste0("successfully signed up. you can proceed to login"))
    })
  })
  ##### LOGIN FUNCTIONALITY ######
  ###############################
  observeEvent(input$loginBtn, {
    data <- read.csv("users.csv",sep=",",header=TRUE,stringsAsFactors = FALSE,na.strings = c(""," ","[]"),strip.white=TRUE)
    
    password <- input$password
    email <- input$email
    flag <- 0
    for(i in 1:nrow(data)) {
      row <- data[i,]
      # do stuff with row
      if((row$email == email) && (row$password == password)){
        flag <- 1
        assign("user_details", row, envir = .GlobalEnv)
        updateSelectInput(session, "loginSuccess", selected = sprintf("1"))
        break
      }
    }
    if(flag == 0){
      output$errorMsg <- renderText({
        HTML(paste0("Your email/password is wrong please try again"))
      })
    }
  })
  ###username showing #####
  #########################
  output$user_name <- renderText({
    HTML(paste0(user_details$name))
  })
  
  ### Recommendations showing ##########
  #####################################
  output$recomendProps <- renderUI({
    prevVals <- buyerlogin(user_details$id)
    w=""
    for (i in 1:nrow(prevVals)){
      x <- sample(1:4,1)
      w=paste0(w,'<div class="col-md-3"><div class="project-fur"><a href="single.html" ><img class="img-responsive" src="images/homes/',x,'.jpg" alt="" /></a><div class="fur"><div class="fur1"><span class="fur-money">Price: Rs.',prevVals$price[i],'</span><h6 class="fur-name"><a href="single.html">House Id: ',prevVals$house_id[i],'</a></h6><span>',prevVals$area[i],'</span></div><div class="fur2"><span>Recomm Score:',prevVals$proprecomendscore[i],'</span></div></div></div></div>')
    }
    HTML(w)
  })
  
  ###### house search button clicked ######
  #########################################
  
  observeEvent(input$searchBtn, {
    #print(input$variable)
    # print(c(input$city))
    # print(c(input$housetype))
    # print(c(input$zone))
    # print(c(input$minprice))
    # print(c(input$maxprice))
    # print(c(input$location))
    # print(c(input$minplot))
    # print(c(input$maxplot))
    # print(input$transaction)
    # print(input$site_facing)
    # print(input$door_facing)
    # print(input$onloan)

    search_city <- c("NA")
    search_housetype <- c("NA")
    search_zone <- c("NA")
    search_minprice <- c("NA")
    search_maxprice <- c("NA")
    search_location <- c("NA")
    search_minplot <- c("NA")
    search_maxplot <- c("NA")
    search_bhk <- c("NA")
    search_nooffloors <- c("NA")
    search_transaction <- c("NA")
    search_siteface <- c("NA")
    search_doorface <- c("NA")
    search_onloan <- c("NA")
    search_furnishing <- c("NA")
    search_available <- c("NA")
    search_parking <- c("NA")
    search_flooring <- c("NA")
    search_water <- c("NA")
    search_age <- c("NA")
    search_rentexp <- c("NA")
    
    if(!is.null(input$city))
      search_city <- c(as.character(input$city))
    if(!is.null(input$housetype))
      search_housetype <- c(as.character(input$housetype))
    if(!is.null(input$zone))
      search_zone <- c(as.character((input$zone)))
    if(!is.null(input$minprice))
      search_minprice <- c(as.numeric(input$minprice))
    if(!is.null(input$maxprice))
      search_maxprice <- c(as.numeric(input$maxprice))
    
    if(!is.null(input$location))
      search_location <- c(as.character(input$location))
    if(!is.null(input$minplot))
      search_minplot <- c(as.numeric(input$minplot))
    if(!is.null(input$maxplot))
      search_maxplot <- c(as.numeric(input$maxplot))
    if(!is.null(input$bhk))
      search_bhk <- c(as.numeric(input$bhk))
    if(!is.null(input$nooffloors))
      search_nooffloors <- c(as.numeric(input$nooffloors))
    if(!is.null(input$transaction))
      search_transaction <- c(as.character(input$transaction))
    if(!is.null(input$siteface))
      search_siteface <- c(as.character(input$siteface))
    if(!is.null(input$doorface))
      search_doorface <- c(as.character(input$doorface))
    if(!is.null(input$onloan))
      search_onloan <- c(as.character(input$onloan))
    if(!is.null(input$furnishing))
      search_furnishing <- c(as.character(input$furnishing))
    if(!is.null(input$available))
      search_available <- c(as.character(input$available))
    if(!is.null(input$parking))
      search_parking <- c(as.character(input$parking))
    if(!is.null(input$flooring))
      search_flooring <- c(as.character(input$flooring))
    if(!is.null(input$water))
      search_water <- c(as.character(input$water))
    if(!is.null(input$age))
      search_age <- c(as.numeric(input$age))
    if(!is.null(input$rentexp))
      search_rentexp <- c(as.numeric(input$rentexp))
    
    print(search_city)
    print(search_housetype)
    print(search_zone)
    print(search_minprice)
    print(search_maxprice)
    print(search_bhk)
    print(search_nooffloors)
    print(search_transaction)
    print(search_siteface)
    print(search_doorface)
    print(search_onloan)
    print(search_furnishing)
    print(search_available)
    print(search_parking)
    print(search_flooring)
    print(search_water)
    print(search_age)
    print(search_rentexp)
    
    # search_city <- c("bangalore")
    # search_housetype <- c("independent_house")
    # search_zone <- c("south")
    # search_minprice <- c(2000000)
    # search_maxprice <- c(12000000)
    # search_location <- c("NA")
    # search_minplot <- c(500)
    # search_maxplot <- c(1500)
    # search_bhk <- c("NA")
    # search_nooffloors <- c(1)
    # search_transaction <- c("resale")
    # search_siteface <- c("NA")
    # search_doorface <- c("NA")
    # search_onloan <- c("NA")
    # search_furnishing <- c("semi")
    # search_available <- c("ready_to_move")
    # search_parking <- c("NA")
    # search_flooring <- c("vitrified_tiles")
    # search_water <- c("NA")
    # search_age <- c(2)
    # search_rentexp <- c(11000)

    final <- property_search(search_city , search_housetype , search_zone , search_minprice ,
                             search_maxprice , search_location , search_minplot , search_maxplot ,
                             search_bhk , search_nooffloors , search_transaction , search_siteface ,
                             search_doorface , search_onloan , search_furnishing , search_available ,
                             search_parking , search_flooring , search_water , search_age , search_rentexp )
    View(final)
    
    output$temptest=renderUI({
      if(ncol(final)>1){
        w=""
        for (i in 1:nrow(final)){
          x <- sample(1:4,1)
          w=paste0(w,'<div class="box-col"><div class=" col-sm-7 left-side"><a href="single.html"><img class="img-responsive" src="images/homes/',x,'.jpg" alt=""></a></div><div class="  col-sm-5 middle-side"><h4>House Id: ',final$house_id[i],'</h4><p><span class="bath">Property Type</span>: <span class="two">',final$property_type[i],'</span></p><p>  <span class="bath1">Area </span>: <span class="two">',final$area[i],'</span></p><p><span class="bath2">Plot Area</span>: <span class="two">',final$plot_area[i],'Sq.Yrds</span></p><p><span class="bath3">Price</span>: Rs.<span class="two">',final$price[i], '</span></p><p><span class="bath4">On Loan</span> : <span class="two">',final$on_loan[i],'</span></p><p><span class="bath5">Recommendation Score </span>:<span class="two">',final$proprecomendscore[i],'</span></p><p><span class="bath5">Approx. Rent Expectation</span>:<span class="two">',final$approxrent[i],'</span></p><div class="   right-side"><a href="contact.html" class="hvr-sweep-to-right more">Contact Builder</a></div></div><div class="clearfix"></div></div>')
          
        }
        HTML(w)
      }
      else{
        w=""
        w=paste0(w,"<h1>No Properties Found</h1>")
        HTML(w)
      }
    })
    
    property_type <- search_housetype
    city <- search_city
    zone <- search_zone
    area <- search_location
    min_plot_area <- search_minplot
    max_plot_area <- search_maxplot
    bhk <- search_bhk
    total_floors <- search_nooffloors
    expected_rental_income <- search_rentexp
    parking <- search_parking
    min_price <- search_minprice
    max_price <- search_maxprice
    availability <- search_available
    transaction_type <- search_transaction
    on_loan <- search_onloan
    water_source <- search_water
    site_facing <- search_siteface
    door_facing <- search_doorface
    flooring_type <- search_flooring
    furnishing<- search_furnishing
    property_age<- search_age
    searchDF <- data.frame(property_type , city , zone , area ,
                           min_plot_area , max_plot_area , bhk , total_floors ,
                           expected_rental_income , parking , min_price , max_price ,
                           availability , transaction_type , on_loan , water_source ,
                           site_facing , door_facing , flooring_type , furnishing , property_age )
    buyerdata_append(searchDF)
  })
  
  ####submit home values new ad #####
  ###################################
  
  observeEvent(input$submitHomeBtn,{
      listing <- "sell"
      property_type <- as.character(input$property_type)
      city <- as.character(input$city)
      zone <- as.character(input$zone)
      area <- as.character(input$area)
      latitude <- as.numeric(input$latitude)
      longitude <- as.numeric(input$longitude)
      plot_area <- as.numeric(input$plot_area)
      built_up_area <- as.numeric(input$built_up_area)
      bedrooms <- "NA"
      balconies <- "NA"
      bathrooms <- "NA"
      total_floors <- "NA"
      X1bhk <- "NA"
      X2bhk <- "NA"
      X3bhk <- "NA"
      parking <- as.character(input$parking)
      price <- as.numeric(input$price)
      availability <- as.character(input$availability)
      transaction_type <- as.character(input$transaction)
      on_loan <- as.character(input$onloan)
      negotiable <- as.character(input$negotiable)
      water_source <- as.character(input$water)
      site_facing <- as.character(input$site_facing)
      door_facing <- as.character(input$door_facing)
      flooring_type <- as.character(input$flooring_type)
      furnishing <- as.character(input$furnishing)
      year_of_construction <- as.numeric(input$year_of_construction)
      
      if(as.character(input$property_type) == "apartment"){
        bedrooms <- as.numeric(input$bedroom)
        balconies <- as.numeric(input$balconies)
        bathrooms <- as.numeric(input$bathrooms)
      }
      if(as.character(input$property_type) == "independent_house"){
        total_floors <- as.numeric(input$total_floors)
        X1bhk <- as.numeric(input$one_bhk)
        X2bhk <- as.numeric(input$two_bhk)
        X3bhk <- as.numeric(input$three_bhk)
      }
      
      print(listing)
      print(property_type)
      print(city)
      print(zone)
      print(area)
      print(latitude)
      print(longitude)
      print(plot_area)
      print(built_up_area)
      print(bedrooms)
      print(balconies)
      print(bathrooms)
      print(total_floors)
      print(X1bhk)
      print(X2bhk)
      print(X3bhk)
      print(parking)
      print(price)
      print(availability)
      print(transaction_type)
      print(on_loan)
      print(negotiable)
      print(water_source)
      print(site_facing)
      print(door_facing)
      print(flooring_type)
      print(furnishing)
      print(year_of_construction)
      
      finalDF <- data.frame(listing,property_type,city,zone,area,latitude,longitude,plot_area,built_up_area,bedrooms,
                            balconies,bathrooms,total_floors,X1bhk,X2bhk,X3bhk,parking,price,availability,
                            transaction_type,on_loan,negotiable,water_source,site_facing,door_facing,flooring_type,furnishing,
                            year_of_construction)

      submithome(finalDF)
      output$homeSuccess <- renderText({
        HTML(paste0("Your home details are successfully posted :) "))
      })
    })
    
   ## jayanagarBtn Quick search##
   ###########################
   observeEvent(input$quickJayaBtn,{
     print("jaya here")
     search_city <- c("bangalore")
     search_housetype <- c("NA")
     search_zone <- c("NA")
     search_minprice <- c("NA")
     search_maxprice <- c("NA")
     search_location <- c("jayanagar")
     search_minplot <- c("NA")
     search_maxplot <- c("NA")
     search_bhk <- c("NA")
     search_nooffloors <- c("NA")
     search_transaction <- c("NA")
     search_siteface <- c("NA")
     search_doorface <- c("NA")
     search_onloan <- c("NA")
     search_furnishing <- c("NA")
     search_available <- c("NA")
     search_parking <- c("NA")
     search_flooring <- c("NA")
     search_water <- c("NA")
     search_age <- c("NA")
     search_rentexp <- c("NA")
     
     final <- property_search(search_city , search_housetype , search_zone , search_minprice ,
                              search_maxprice , search_location , search_minplot , search_maxplot ,
                              search_bhk , search_nooffloors , search_transaction , search_siteface ,
                              search_doorface , search_onloan , search_furnishing , search_available ,
                              search_parking , search_flooring , search_water , search_age , search_rentexp )
     #View(final)
     
     output$temptest=renderUI({
       if(ncol(final)>1){
         w=""
         for (i in 1:nrow(final)){
           x <- sample(1:4,1)
           w=paste0(w,'<div class="box-col"><div class=" col-sm-7 left-side"><a href="single.html"><img class="img-responsive" src="images/homes/',x,'.jpg" alt=""></a></div><div class="  col-sm-5 middle-side"><h4>House Id: ',final$house_id[i],'</h4><p><span class="bath">Property Type</span>: <span class="two">',final$property_type[i],'</span></p><p>  <span class="bath1">Area </span>: <span class="two">',final$area[i],'</span></p><p><span class="bath2">Plot Area</span>: <span class="two">',final$plot_area[i],'Sq.Yrds</span></p><p><span class="bath3">Price</span>: Rs.<span class="two">',final$price[i], '</span></p><p><span class="bath4">On Loan</span> : <span class="two">',final$on_loan[i],'</span></p><p><span class="bath5">Recommendation Score </span>:<span class="two">',final$proprecomendscore[i],'</span></p><p><span class="bath5">Approx. Rent Expectation</span>:<span class="two">',final$approxrent[i],'</span></p><div class="   right-side"><a href="contact.html" class="hvr-sweep-to-right more">Contact Builder</a></div></div><div class="clearfix"></div></div>')
           
         }
         HTML(w)
       }
       else{
         w=""
         w=paste0(w,"<h1>No Properties Found</h1>")
         HTML(w)
       }
     })
     
     property_type <- search_housetype
     city <- search_city
     zone <- search_zone
     area <- search_location
     min_plot_area <- search_minplot
     max_plot_area <- search_maxplot
     bhk <- search_bhk
     total_floors <- search_nooffloors
     expected_rental_income <- search_rentexp
     parking <- search_parking
     min_price <- search_minprice
     max_price <- search_maxprice
     availability <- search_available
     transaction_type <- search_transaction
     on_loan <- search_onloan
     water_source <- search_water
     site_facing <- search_siteface
     door_facing <- search_doorface
     flooring_type <- search_flooring
     furnishing<- search_furnishing
     property_age<- search_age
     searchDF <- data.frame(property_type , city , zone , area ,
                            min_plot_area , max_plot_area , bhk , total_floors ,
                            expected_rental_income , parking , min_price , max_price ,
                            availability , transaction_type , on_loan , water_source ,
                            site_facing , door_facing , flooring_type , furnishing , property_age )
     buyerdata_append(searchDF)
   })
   
   ## indiraBtn Quick search##
   ###########################
   observeEvent(input$indiraBtn,{
     print("indiraBtn here")
     search_city <- c("bangalore")
     search_housetype <- c("NA")
     search_zone <- c("NA")
     search_minprice <- c("NA")
     search_maxprice <- c("NA")
     search_location <- c("indiranagar")
     search_minplot <- c("NA")
     search_maxplot <- c("NA")
     search_bhk <- c("NA")
     search_nooffloors <- c("NA")
     search_transaction <- c("NA")
     search_siteface <- c("NA")
     search_doorface <- c("NA")
     search_onloan <- c("NA")
     search_furnishing <- c("NA")
     search_available <- c("NA")
     search_parking <- c("NA")
     search_flooring <- c("NA")
     search_water <- c("NA")
     search_age <- c("NA")
     search_rentexp <- c("NA")
     
     final <- property_search(search_city , search_housetype , search_zone , search_minprice ,
                              search_maxprice , search_location , search_minplot , search_maxplot ,
                              search_bhk , search_nooffloors , search_transaction , search_siteface ,
                              search_doorface , search_onloan , search_furnishing , search_available ,
                              search_parking , search_flooring , search_water , search_age , search_rentexp )
     #View(final)
     
     output$temptest=renderUI({
       if(ncol(final)>1){
         w=""
         for (i in 1:nrow(final)){
           x <- sample(1:4,1)
           w=paste0(w,'<div class="box-col"><div class=" col-sm-7 left-side"><a href="single.html"><img class="img-responsive" src="images/homes/',x,'.jpg" alt=""></a></div><div class="  col-sm-5 middle-side"><h4>House Id: ',final$house_id[i],'</h4><p><span class="bath">Property Type</span>: <span class="two">',final$property_type[i],'</span></p><p>  <span class="bath1">Area </span>: <span class="two">',final$area[i],'</span></p><p><span class="bath2">Plot Area</span>: <span class="two">',final$plot_area[i],'Sq.Yrds</span></p><p><span class="bath3">Price</span>: Rs.<span class="two">',final$price[i], '</span></p><p><span class="bath4">On Loan</span> : <span class="two">',final$on_loan[i],'</span></p><p><span class="bath5">Recommendation Score </span>:<span class="two">',final$proprecomendscore[i],'</span></p><p><span class="bath5">Approx. Rent Expectation</span>:<span class="two">',final$approxrent[i],'</span></p><div class="   right-side"><a href="contact.html" class="hvr-sweep-to-right more">Contact Builder</a></div></div><div class="clearfix"></div></div>')
           
         }
         HTML(w)
       }
       else{
         w=""
         w=paste0(w,"<h1>No Properties Found</h1>")
         HTML(w)
       }
     })
     
     property_type <- search_housetype
     city <- search_city
     zone <- search_zone
     area <- search_location
     min_plot_area <- search_minplot
     max_plot_area <- search_maxplot
     bhk <- search_bhk
     total_floors <- search_nooffloors
     expected_rental_income <- search_rentexp
     parking <- search_parking
     min_price <- search_minprice
     max_price <- search_maxprice
     availability <- search_available
     transaction_type <- search_transaction
     on_loan <- search_onloan
     water_source <- search_water
     site_facing <- search_siteface
     door_facing <- search_doorface
     flooring_type <- search_flooring
     furnishing<- search_furnishing
     property_age<- search_age
     searchDF <- data.frame(property_type , city , zone , area ,
                            min_plot_area , max_plot_area , bhk , total_floors ,
                            expected_rental_income , parking , min_price , max_price ,
                            availability , transaction_type , on_loan , water_source ,
                            site_facing , door_facing , flooring_type , furnishing , property_age )
     buyerdata_append(searchDF)
   })
   
   ## kormangalaBtn Quick search##
   ###########################
   observeEvent(input$kormangalaBtn,{
     print("kormangalaBtn here")
     search_city <- c("bangalore")
     search_housetype <- c("NA")
     search_zone <- c("NA")
     search_minprice <- c("NA")
     search_maxprice <- c("NA")
     search_location <- c("kormangala")
     search_minplot <- c("NA")
     search_maxplot <- c("NA")
     search_bhk <- c("NA")
     search_nooffloors <- c("NA")
     search_transaction <- c("NA")
     search_siteface <- c("NA")
     search_doorface <- c("NA")
     search_onloan <- c("NA")
     search_furnishing <- c("NA")
     search_available <- c("NA")
     search_parking <- c("NA")
     search_flooring <- c("NA")
     search_water <- c("NA")
     search_age <- c("NA")
     search_rentexp <- c("NA")
     
     final <- property_search(search_city , search_housetype , search_zone , search_minprice ,
                              search_maxprice , search_location , search_minplot , search_maxplot ,
                              search_bhk , search_nooffloors , search_transaction , search_siteface ,
                              search_doorface , search_onloan , search_furnishing , search_available ,
                              search_parking , search_flooring , search_water , search_age , search_rentexp )
     #View(final)
     
     output$temptest=renderUI({
       if(ncol(final)>1){
         w=""
         for (i in 1:nrow(final)){
           x <- sample(1:4,1)
           w=paste0(w,'<div class="box-col"><div class=" col-sm-7 left-side"><a href="single.html"><img class="img-responsive" src="images/homes/',x,'.jpg" alt=""></a></div><div class="  col-sm-5 middle-side"><h4>House Id: ',final$house_id[i],'</h4><p><span class="bath">Property Type</span>: <span class="two">',final$property_type[i],'</span></p><p>  <span class="bath1">Area </span>: <span class="two">',final$area[i],'</span></p><p><span class="bath2">Plot Area</span>: <span class="two">',final$plot_area[i],'Sq.Yrds</span></p><p><span class="bath3">Price</span>: Rs.<span class="two">',final$price[i], '</span></p><p><span class="bath4">On Loan</span> : <span class="two">',final$on_loan[i],'</span></p><p><span class="bath5">Recommendation Score </span>:<span class="two">',final$proprecomendscore[i],'</span></p><p><span class="bath5">Approx. Rent Expectation</span>:<span class="two">',final$approxrent[i],'</span></p><div class="   right-side"><a href="contact.html" class="hvr-sweep-to-right more">Contact Builder</a></div></div><div class="clearfix"></div></div>')
           
         }
         HTML(w)
       }
       else{
         w=""
         w=paste0(w,"<h1>No Properties Found</h1>")
         HTML(w)
       }
     })
     
     property_type <- search_housetype
     city <- search_city
     zone <- search_zone
     area <- search_location
     min_plot_area <- search_minplot
     max_plot_area <- search_maxplot
     bhk <- search_bhk
     total_floors <- search_nooffloors
     expected_rental_income <- search_rentexp
     parking <- search_parking
     min_price <- search_minprice
     max_price <- search_maxprice
     availability <- search_available
     transaction_type <- search_transaction
     on_loan <- search_onloan
     water_source <- search_water
     site_facing <- search_siteface
     door_facing <- search_doorface
     flooring_type <- search_flooring
     furnishing<- search_furnishing
     property_age<- search_age
     searchDF <- data.frame(property_type , city , zone , area ,
                            min_plot_area , max_plot_area , bhk , total_floors ,
                            expected_rental_income , parking , min_price , max_price ,
                            availability , transaction_type , on_loan , water_source ,
                            site_facing , door_facing , flooring_type , furnishing , property_age )
     buyerdata_append(searchDF)
   })
   
   ## whitefieldBtn Quick search##
   ###########################
   observeEvent(input$whitefieldBtn,{
     print("whitefieldBtn here")
     search_city <- c("bangalore")
     search_housetype <- c("NA")
     search_zone <- c("NA")
     search_minprice <- c("NA")
     search_maxprice <- c("NA")
     search_location <- c("whitefield")
     search_minplot <- c("NA")
     search_maxplot <- c("NA")
     search_bhk <- c("NA")
     search_nooffloors <- c("NA")
     search_transaction <- c("NA")
     search_siteface <- c("NA")
     search_doorface <- c("NA")
     search_onloan <- c("NA")
     search_furnishing <- c("NA")
     search_available <- c("NA")
     search_parking <- c("NA")
     search_flooring <- c("NA")
     search_water <- c("NA")
     search_age <- c("NA")
     search_rentexp <- c("NA")
     
     final <- property_search(search_city , search_housetype , search_zone , search_minprice ,
                              search_maxprice , search_location , search_minplot , search_maxplot ,
                              search_bhk , search_nooffloors , search_transaction , search_siteface ,
                              search_doorface , search_onloan , search_furnishing , search_available ,
                              search_parking , search_flooring , search_water , search_age , search_rentexp )
     #View(final)
     
     output$temptest=renderUI({
       if(ncol(final)>1){
         w=""
         for (i in 1:nrow(final)){
           x <- sample(1:4,1)
           w=paste0(w,'<div class="box-col"><div class=" col-sm-7 left-side"><a href="single.html"><img class="img-responsive" src="images/homes/',x,'.jpg" alt=""></a></div><div class="  col-sm-5 middle-side"><h4>House Id: ',final$house_id[i],'</h4><p><span class="bath">Property Type</span>: <span class="two">',final$property_type[i],'</span></p><p>  <span class="bath1">Area </span>: <span class="two">',final$area[i],'</span></p><p><span class="bath2">Plot Area</span>: <span class="two">',final$plot_area[i],'Sq.Yrds</span></p><p><span class="bath3">Price</span>: Rs.<span class="two">',final$price[i], '</span></p><p><span class="bath4">On Loan</span> : <span class="two">',final$on_loan[i],'</span></p><p><span class="bath5">Recommendation Score </span>:<span class="two">',final$proprecomendscore[i],'</span></p><p><span class="bath5">Approx. Rent Expectation</span>:<span class="two">',final$approxrent[i],'</span></p><div class="   right-side"><a href="contact.html" class="hvr-sweep-to-right more">Contact Builder</a></div></div><div class="clearfix"></div></div>')
           
         }
         HTML(w)
       }
       else{
         w=""
         w=paste0(w,"<h1>No Properties Found</h1>")
         HTML(w)
       }
     })
     
     property_type <- search_housetype
     city <- search_city
     zone <- search_zone
     area <- search_location
     min_plot_area <- search_minplot
     max_plot_area <- search_maxplot
     bhk <- search_bhk
     total_floors <- search_nooffloors
     expected_rental_income <- search_rentexp
     parking <- search_parking
     min_price <- search_minprice
     max_price <- search_maxprice
     availability <- search_available
     transaction_type <- search_transaction
     on_loan <- search_onloan
     water_source <- search_water
     site_facing <- search_siteface
     door_facing <- search_doorface
     flooring_type <- search_flooring
     furnishing<- search_furnishing
     property_age<- search_age
     searchDF <- data.frame(property_type , city , zone , area ,
                            min_plot_area , max_plot_area , bhk , total_floors ,
                            expected_rental_income , parking , min_price , max_price ,
                            availability , transaction_type , on_loan , water_source ,
                            site_facing , door_facing , flooring_type , furnishing , property_age )
     buyerdata_append(searchDF)
   })
   
   ## marathalliBtn Quick search##
   ###########################
   observeEvent(input$marathalliBtn,{
     print("marathalliBtn here")
     search_city <- c("bangalore")
     search_housetype <- c("NA")
     search_zone <- c("NA")
     search_minprice <- c("NA")
     search_maxprice <- c("NA")
     search_location <- c("marathalli")
     search_minplot <- c("NA")
     search_maxplot <- c("NA")
     search_bhk <- c("NA")
     search_nooffloors <- c("NA")
     search_transaction <- c("NA")
     search_siteface <- c("NA")
     search_doorface <- c("NA")
     search_onloan <- c("NA")
     search_furnishing <- c("NA")
     search_available <- c("NA")
     search_parking <- c("NA")
     search_flooring <- c("NA")
     search_water <- c("NA")
     search_age <- c("NA")
     search_rentexp <- c("NA")
     
     final <- property_search(search_city , search_housetype , search_zone , search_minprice ,
                              search_maxprice , search_location , search_minplot , search_maxplot ,
                              search_bhk , search_nooffloors , search_transaction , search_siteface ,
                              search_doorface , search_onloan , search_furnishing , search_available ,
                              search_parking , search_flooring , search_water , search_age , search_rentexp )
     #View(final)
     
     output$temptest=renderUI({
       if(ncol(final)>1){
         w=""
         for (i in 1:nrow(final)){
           x <- sample(1:4,1)
           w=paste0(w,'<div class="box-col"><div class=" col-sm-7 left-side"><a href="single.html"><img class="img-responsive" src="images/homes/',x,'.jpg" alt=""></a></div><div class="  col-sm-5 middle-side"><h4>House Id: ',final$house_id[i],'</h4><p><span class="bath">Property Type</span>: <span class="two">',final$property_type[i],'</span></p><p>  <span class="bath1">Area </span>: <span class="two">',final$area[i],'</span></p><p><span class="bath2">Plot Area</span>: <span class="two">',final$plot_area[i],'Sq.Yrds</span></p><p><span class="bath3">Price</span>: Rs.<span class="two">',final$price[i], '</span></p><p><span class="bath4">On Loan</span> : <span class="two">',final$on_loan[i],'</span></p><p><span class="bath5">Recommendation Score </span>:<span class="two">',final$proprecomendscore[i],'</span></p><p><span class="bath5">Approx. Rent Expectation</span>:<span class="two">',final$approxrent[i],'</span></p><div class="   right-side"><a href="contact.html" class="hvr-sweep-to-right more">Contact Builder</a></div></div><div class="clearfix"></div></div>')
           
         }
         HTML(w)
       }
       else{
         w=""
         w=paste0(w,"<h1>No Properties Found</h1>")
         HTML(w)
       }
     })
     
     property_type <- search_housetype
     city <- search_city
     zone <- search_zone
     area <- search_location
     min_plot_area <- search_minplot
     max_plot_area <- search_maxplot
     bhk <- search_bhk
     total_floors <- search_nooffloors
     expected_rental_income <- search_rentexp
     parking <- search_parking
     min_price <- search_minprice
     max_price <- search_maxprice
     availability <- search_available
     transaction_type <- search_transaction
     on_loan <- search_onloan
     water_source <- search_water
     site_facing <- search_siteface
     door_facing <- search_doorface
     flooring_type <- search_flooring
     furnishing<- search_furnishing
     property_age<- search_age
     searchDF <- data.frame(property_type , city , zone , area ,
                            min_plot_area , max_plot_area , bhk , total_floors ,
                            expected_rental_income , parking , min_price , max_price ,
                            availability , transaction_type , on_loan , water_source ,
                            site_facing , door_facing , flooring_type , furnishing , property_age )
     buyerdata_append(searchDF)
   })
   
   ## btmBtn Quick search##
   ###########################
   observeEvent(input$btmBtn,{
     print("btmBtn here")
     search_city <- c("bangalore")
     search_housetype <- c("NA")
     search_zone <- c("NA")
     search_minprice <- c("NA")
     search_maxprice <- c("NA")
     search_location <- c("btmlayout")
     search_minplot <- c("NA")
     search_maxplot <- c("NA")
     search_bhk <- c("NA")
     search_nooffloors <- c("NA")
     search_transaction <- c("NA")
     search_siteface <- c("NA")
     search_doorface <- c("NA")
     search_onloan <- c("NA")
     search_furnishing <- c("NA")
     search_available <- c("NA")
     search_parking <- c("NA")
     search_flooring <- c("NA")
     search_water <- c("NA")
     search_age <- c("NA")
     search_rentexp <- c("NA")
     
     final <- property_search(search_city , search_housetype , search_zone , search_minprice ,
                              search_maxprice , search_location , search_minplot , search_maxplot ,
                              search_bhk , search_nooffloors , search_transaction , search_siteface ,
                              search_doorface , search_onloan , search_furnishing , search_available ,
                              search_parking , search_flooring , search_water , search_age , search_rentexp )
     #View(final)
     
     output$temptest=renderUI({
       if(ncol(final)>1){
         w=""
         for (i in 1:nrow(final)){
           x <- sample(1:4,1)
           w=paste0(w,'<div class="box-col"><div class=" col-sm-7 left-side"><a href="single.html"><img class="img-responsive" src="images/homes/',x,'.jpg" alt=""></a></div><div class="  col-sm-5 middle-side"><h4>House Id: ',final$house_id[i],'</h4><p><span class="bath">Property Type</span>: <span class="two">',final$property_type[i],'</span></p><p>  <span class="bath1">Area </span>: <span class="two">',final$area[i],'</span></p><p><span class="bath2">Plot Area</span>: <span class="two">',final$plot_area[i],'Sq.Yrds</span></p><p><span class="bath3">Price</span>: Rs.<span class="two">',final$price[i], '</span></p><p><span class="bath4">On Loan</span> : <span class="two">',final$on_loan[i],'</span></p><p><span class="bath5">Recommendation Score </span>:<span class="two">',final$proprecomendscore[i],'</span></p><p><span class="bath5">Approx. Rent Expectation</span>:<span class="two">',final$approxrent[i],'</span></p><div class="   right-side"><a href="contact.html" class="hvr-sweep-to-right more">Contact Builder</a></div></div><div class="clearfix"></div></div>')
           
         }
         HTML(w)
       }
       else{
         w=""
         w=paste0(w,"<h1>No Properties Found</h1>")
         HTML(w)
       }
     })
     
     property_type <- search_housetype
     city <- search_city
     zone <- search_zone
     area <- search_location
     min_plot_area <- search_minplot
     max_plot_area <- search_maxplot
     bhk <- search_bhk
     total_floors <- search_nooffloors
     expected_rental_income <- search_rentexp
     parking <- search_parking
     min_price <- search_minprice
     max_price <- search_maxprice
     availability <- search_available
     transaction_type <- search_transaction
     on_loan <- search_onloan
     water_source <- search_water
     site_facing <- search_siteface
     door_facing <- search_doorface
     flooring_type <- search_flooring
     furnishing<- search_furnishing
     property_age<- search_age
     searchDF <- data.frame(property_type , city , zone , area ,
                            min_plot_area , max_plot_area , bhk , total_floors ,
                            expected_rental_income , parking , min_price , max_price ,
                            availability , transaction_type , on_loan , water_source ,
                            site_facing , door_facing , flooring_type , furnishing , property_age )
     buyerdata_append(searchDF)
   })
   
   ###Featured Communities###
   ##########################
   output$featuredCommunities <- renderUI({
     prevVals <- buyerlogin(user_details$id)
     w=""
     for (i in 1:5){
       w=paste0(w,'<div class="single-box-img"><div class="box-img"><a href="single.html"><img class="img-responsive" src="images/sl4.jpg" alt=""></a></div><div class="box-text"><p><a href="single.html">House Id:',prevVals$house_id[i],'</a></p><a href="single.html" class="in-box">',prevVals$area[i],'</a></div><div class="clearfix"></div></div>')
     }
     HTML(w)
   })
   
   observeEvent(input$sendMsgBtn,{
     
     msg_entered <- input$message
     
     #### User message ######
     ########################
     output$messaging <- renderUI({
       print(msg_entered)
       w = '<div class="row msg_container base_sent">
       <div class="col-md-10 col-xs-10 ">
       <div class="messages msg_sent">
       <p>'
       r = '</p>
       <time datetime="2009-11-13T20:00">Aravind . just now</time>
       </div>
       </div>
       <div class="col-md-2 col-xs-2 avatar">
       <img src="https://media.licdn.com/mpr/mpr/shrink_100_100/AAEAAQAAAAAAAAiGAAAAJGRmZDBhMDE0LWRiODUtNDk3Zi1hMTYxLWNiMDgzYjg2ZjY4NA.jpg" class=" img-responsive ">
       </div>
       </div>'
       final_val = paste0(w,msg_entered,r)
       HTML(final_val)
     })
     updateSelectInput(session,"setDiv",selected="1")
     
     ##Pre-defined Locations##
     #########################
     #reference lists
     Location_all <- tolower(c("jayanagar","indiranagar","kormangala","whitefield","marathalli",
                               "btmlayout","jpnagar","hsrlayout","bellandur","brookefield",
                               "sarjapur_road","kr_puram","hebbal","ramamurthy_nagar","mahadevapura",
                               "kanakapura_road","horamavu","electronic_city"))
     Direction <- tolower(c("north", "east", "west", "south"))
     Property_type <- tolower(c("House", "Apartment"))
     
     #translate all letters to lower case
     text <- msg_entered
     clean_corpus <- tolower(text)
     # remove punctuation
     clean_corpus <- removePunctuation(clean_corpus)
     # remove common non-content words
     clean_corpus <- removeWords(clean_corpus, stopwords())
     
     clean_corpus <- str_trim(clean_corpus)
     clean_corpus <- gsub("  ", " ", clean_corpus, fixed = TRUE)
     
     words <- strsplit(clean_corpus, " ")
     words1 <- matrix(unlist(words), byrow = TRUE)
     words1
     
     # All outputs
     #Extractig budget entered
     Key_budget <- sum(as.numeric(words1), na.rm = TRUE)
     
     #Logic for extracting property type
     Key_PropertyType <- mapfunction(words1,Property_type)
     if(is.null(Key_PropertyType))
     {
       Key_PropertyType <- "independent_house"
     } else if(Key_PropertyType=="apartment")
     {
       Key_PropertyType <- Key_PropertyType
     } else {
       Key_PropertyType <- "independent_house"
     }
     
     #Logic for extracting location
     Key_location <- mapfunction(words1,Location_all)
     
     # Logic for zone & place extraction
     Key_zone <- mapfunction(words1, Direction)
     
     if(is.null(Key_zone))
     {
       Key_location <- Key_location
     } else{
       Key_location <- NULL
     }
     
     ##BOT REPLY#############
     ########################
     output$messaging1 <- renderUI({
       botVal <- paste0('<div class="row msg_container base_receive">
                        <div class="col-md-2 col-xs-2 avatar">
                        <img src="http://www.bitrebels.com/wp-content/uploads/2011/02/Original-Facebook-Geek-Profile-Avatar-1.jpg" class=" img-responsive ">
                        </div>
                        <div class="col-md-10 col-xs-10">
                        <div class="messages msg_receive">
                        <p>Information Extracted: Key_location - ',Key_location, ' Key_zone - ', Key_zone, ' Key_PropertyType - ', Key_PropertyType, ' Key_budget - ',Key_budget,' Check the results on the left :)</p>
                        <time datetime="2009-11-13T20:00">Bot . just now</time>
                        </div>
                        </div>
                        </div>')
       HTML(botVal)
     })
     updateSelectInput(session,"setDiv1",selected="1")
     
     ###Build the dataframe with the query###
     ########################################
     search_city <- c("bangalore")
     search_housetype <- c("NA")
     search_zone <- c("NA")
     search_minprice <- c("NA")
     search_maxprice <- c("NA")
     search_location <- c("NA")
     search_minplot <- c("NA")
     search_maxplot <- c("NA")
     search_bhk <- c("NA")
     search_nooffloors <- c("NA")
     search_transaction <- c("NA")
     search_siteface <- c("NA")
     search_doorface <- c("NA")
     search_onloan <- c("NA")
     search_furnishing <- c("NA")
     search_available <- c("NA")
     search_parking <- c("NA")
     search_flooring <- c("NA")
     search_water <- c("NA")
     search_age <- c("NA")
     search_rentexp <- c("NA")
     
     if(!is.null(Key_budget)){
       search_minprice <- c(as.numeric(Key_budget))
     }
     if(!is.null(Key_zone)){
       search_zone <- c(as.character(Key_zone))
     }
     if(!is.null(Key_location)){
       search_location <- c(as.character(Key_location))
     }
     if(!is.null(Key_PropertyType)){
       search_housetype <- c(as.character(Key_PropertyType))
     }
     
     final <- property_search(search_city , search_housetype , search_zone , search_minprice ,
                              search_maxprice , search_location , search_minplot , search_maxplot ,
                              search_bhk , search_nooffloors , search_transaction , search_siteface ,
                              search_doorface , search_onloan , search_furnishing , search_available ,
                              search_parking , search_flooring , search_water , search_age , search_rentexp )
     #View(final)
     
     output$temptest=renderUI({
       if(ncol(final)>1){
         w=""
         for (i in 1:nrow(final)){
           x <- sample(1:4,1)
           w=paste0(w,'<div class="box-col"><div class=" col-sm-7 left-side"><a href="single.html"><img class="img-responsive" src="images/homes/',x,'.jpg" alt=""></a></div><div class="  col-sm-5 middle-side"><h4>House Id: ',final$house_id[i],'</h4><p><span class="bath">Property Type</span>: <span class="two">',final$property_type[i],'</span></p><p>  <span class="bath1">Area </span>: <span class="two">',final$area[i],'</span></p><p><span class="bath2">Plot Area</span>: <span class="two">',final$plot_area[i],'Sq.Yrds</span></p><p><span class="bath3">Price</span>: Rs.<span class="two">',final$price[i], '</span></p><p><span class="bath4">On Loan</span> : <span class="two">',final$on_loan[i],'</span></p><p><span class="bath5">Recommendation Score </span>:<span class="two">',final$proprecomendscore[i],'</span></p><p><span class="bath5">Approx. Rent Expectation</span>:<span class="two">',final$approxrent[i],'</span></p><div class="   right-side"><a href="contact.html" class="hvr-sweep-to-right more">Contact Builder</a></div></div><div class="clearfix"></div></div>')
           
         }
         HTML(w)
       }
       else{
         w=""
         w=paste0(w,"<h1>No Properties Found</h1>")
         HTML(w)
       }
     })
     
     
     property_type <- search_housetype
     city <- search_city
     zone <- search_zone
     area <- search_location
     min_plot_area <- search_minplot
     max_plot_area <- search_maxplot
     bhk <- search_bhk
     total_floors <- search_nooffloors
     expected_rental_income <- search_rentexp
     parking <- search_parking
     min_price <- search_minprice
     max_price <- search_maxprice
     availability <- search_available
     transaction_type <- search_transaction
     on_loan <- search_onloan
     water_source <- search_water
     site_facing <- search_siteface
     door_facing <- search_doorface
     flooring_type <- search_flooring
     furnishing<- search_furnishing
     property_age<- search_age
     searchDF <- data.frame(property_type , city , zone , area ,
                            min_plot_area , max_plot_area , bhk , total_floors ,
                            expected_rental_income , parking , min_price , max_price ,
                            availability , transaction_type , on_loan , water_source ,
                            site_facing , door_facing , flooring_type , furnishing , property_age )
     buyerdata_append(searchDF)
     
   })
})