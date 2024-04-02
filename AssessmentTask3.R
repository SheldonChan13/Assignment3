library(shiny)
library("sp")
##library("maptools")
library("ggplot2")
library("ggmap")
library("evaluate")
library("mapproj")
library("classInt")

melbourne_prices <- data.frame(
  Area = c("Northern Region",
           "South West Melbourne",
           "South West Melbourne",
           "Melbourne City",
           "Western Region",
           "Northern Region"),
  HousePrice = c(700750,
                 760000, 
                 611000, 
                 0, 
                 465000,
                 630000),
  UnitPrice = c(0, 
                489000, 
                440000, 
                440000, 
                372000,
                384000)
)

# Define UI for app that draws a histogram ----

  ui <- fluidPage(
    
    # Application title
    titlePanel("Melbourne Property Investment Guideline"),   
    
    navlistPanel(
      id = "tabset",
      
      "Overview",
      tabPanel("Overview of the report",
               "Overview: Melbourne is a popular destination for property investment, with a range of options available for investors. When considering property investment in Melbourne, several guidelines should be taken into consideration. Income Level: Investors should consider their income level and affordability when investing in Melbourne property. It is important to have a clear understanding of the budget and financial capacity before making any investment decisions. Type of Property: Melbourne offers a variety of property types for investment, including apartments, townhouses, and houses. The type of property chosen will depend on the investor's preferences, budget, and investment goals. Location: When considering property investment in Melbourne, the location is a crucial factor. Investors should consider whether they want to invest in a property close to the city centre or in the suburbs. Properties closer to the city centre may have higher potential for capital growth and rental yields, but may also come with a higher price tag. Suburban properties may offer more affordable options and the potential for long-term growth. Number of Bedrooms: The number of bedrooms in the property is an important consideration for investors. Properties with more bedrooms may attract higher rental income and appeal to a wider range of tenants, such as families or shared living arrangements. Overall, Melbourne offers a range of opportunities for property investment, and investors should carefully consider their income level, the type of property, location, and the number of bedrooms when making investment decisions. It is also important to seek professional advice and conduct thorough research before making any investment decisions."
      ),
      
      "Analysis",
      tabPanel("Income Level", 
                "Income Level: As of recent data, Melbourne exhibits a diverse range of income levels, reflecting the varied economic landscape and the demographic composition of the city. According to the Australian Bureau of Statistics (ABS), the median weekly household income in Melbourne was approximately $1,526 in 2020-21. This figure encompasses a wide spectrum of incomes across the city. At the higher end of the income scale, certain affluent suburbs in Melbourne, such as Toorak, South Yarra, and Brighton, are known for their high average household incomes, often exceeding $200,000 per year. These areas are often home to professionals in finance, law, and business, as well as successful entrepreneurs and executives. On the other hand, Melbourne also has a significant middle-income population, with various suburbs and neighbourhoods characterized by median household incomes ranging from $80,000 to $150,000 annually. These areas are inhabited by a diverse array of residents, including teachers, healthcare professionals, engineers, and small business owners. However, it is important to acknowledge the presence of lower-income households in Melbourne. Certain suburbs and communities experience income levels below the city's median, often due to factors such as employment in industries with lower average wages, limited access to education and training, and the challenges associated with the cost of living in a major urban centre. Furthermore, Melbourne's income distribution is influenced by factors such as education, employment opportunities, and the presence of a multicultural population. The city's diverse workforce, which includes a significant immigrant community, contributes to the multifaceted nature of income levels, with various cultural and socioeconomic backgrounds shaping the overall income landscape. Thus, Melbourne's income diversity is a reflection of its dynamic economy and the demographic makeup of its population, encompassing a broad spectrum of incomes that contribute to the city's social and economic vibrancy."
               ,dataTableOutput("my_table1"),
               dataTableOutput("my_table2"),
               plotOutput("incomePlot") # Output for the bar chart)
    ),
      
      tabPanel("Type of Property", 
               "Type of Property: In Melbourne, the real estate market offers a diverse range of property types, catering to the varying preferences and needs of residents. The city's property landscape includes the following types of properties, each with its own characteristics and appeal: Detached Houses: Detached houses, often referred to as single-family homes, are a prevalent property type in Melbourne. These properties typically offer spacious living areas, private outdoor spaces, and multiple bedrooms, making them attractive to families and individuals seeking a traditional residential lifestyle. The median price for a detached house in Melbourne is approximately $950,000, with variations based on location, size, and amenities. Apartments: Melbourne's skyline is adorned with numerous apartment buildings, reflecting the popularity of apartment living in the city. Apartments range from compact studio units to luxurious penthouses, catering to diverse housing preferences. The median price for an apartment in Melbourne is around $550,000, with factors such as proximity to the city centre, views, and amenities influencing pricing. Townhouses: Townhouses, characterized by their multi-level layout and shared walls with neighbouring units, offer a blend of privacy and community living. These properties are often favoured for their modern designs, low maintenance requirements, and convenient access to urban amenities. The median price for a townhouse in Melbourne is approximately $800,000, with variations based on location and size. Units and Flats: Units and flats are compact residential properties that are particularly popular among young professionals, students, and individuals seeking affordable housing options. These properties are often located in proximity to public transportation, educational institutions, and entertainment precincts. The median price for a unit or flat in Melbourne is around $450,000, with variations based on location and size. Rural and Acreage Properties: Outside the urban core, Melbourne's surrounding regions offer rural and acreage properties, providing a serene lifestyle with expansive land, countryside views, and a sense of seclusion. These properties cater to individuals seeking a rural retreat while still maintaining accessibility to the city. Prices for rural and acreage properties in Melbourne vary widely based on location, size, and amenities. So, Melbourne's real estate market offers a diverse array of property types, catering to the preferences, lifestyles, and budget considerations of a broad spectrum of residents and investors."
               ,selectInput(inputId = "n_breaks",
                            label = "Properity's type in Melbourne:",
                            choices = c("House", "Unit"),
                            selected = "House"),),
      tabPanel("Location of Property",
               "Location: Melbourne, as a vibrant and diverse city, offers a wide array of property locations, each with its own unique characteristics, amenities, and appeal. The following are some key property locations in Melbourne, along with relevant data: City Center: Melbourne's central business district (CBD) is a bustling hub of commercial activity, cultural institutions, and entertainment venues. Property in the city centre often includes high-rise apartments, luxury penthouses, and modern condominiums. The median price for a property in the CBD is approximately $650,000, with variations based on factors such as proximity to key landmarks, views, and building amenities. Inner Suburbs: The inner suburbs of Melbourne, such as South Yarra, Fitzroy, and Richmond, offer a blend of urban convenience and neighbourhood charm. Properties in these areas range from historic terrace houses to contemporary apartments, catering to diverse lifestyles. The median price for a property in the inner suburbs is around $900,000, with variations based on proximity to public transport, dining precincts, and recreational facilities. Bayside: Melbourne's bayside suburbs, including St Kilda, Brighton, and Sandringham, boast picturesque coastal settings and a relaxed seaside lifestyle. Properties in these areas often include beachfront homes, elegant townhouses, and modern apartments with stunning ocean views. The median price for a property in the bayside suburbs is approximately $1.2 million, with variations based on proximity to the beach, waterfront access, and property size. Outer Suburbs: The outer suburbs of Melbourne, such as Werribee, Sunbury, and Pakenham, offer spacious residential properties, family-friendly amenities, and a more suburban lifestyle. Housing options in these areas include detached houses, townhouses, and larger land parcels suitable for those seeking a quieter, more spacious environment. The median price for a property in the outer suburbs is around $600,000, with variations based on proximity to schools, parks, and transportation links. Rural and Regional Areas: Beyond the urban sprawl, Melbourne's surrounding rural and regional areas, such as the Yarra Valley and Mornington Peninsula, offer a tranquil setting with vineyards, farmland, and scenic landscapes. Properties in these regions include rural estates, vineyard properties, and equestrian estates, appealing to those seeking a peaceful retreat. Prices for properties in rural and regional areas vary widely based on land size, amenities, and proximity to lifestyle attractions. Overall, Melbourne's property locations cater to a diverse range of preferences, from urban cosmopolitan living to coastal retreats and tranquil rural escapes, reflecting the city's rich and varied real estate landscape."
               ,mainPanel(
                 tabsetPanel(
                   tabPanel("CBD",
                            plotOutput("ProxCBDPlot"),
                   ),
                   tabPanel("Train Station",
                            plotOutput("ProxTrainPlot")
                   ),
                   tabPanel("Green Space",
                            plotOutput("ProxGreenPlot")
                   )
                 )
               )),
      
      tabPanel("Number of Bedrooms,Bathrooms and Parkings",
               "Number of Bedrooms: In Melbourne, the real estate market offers a variety of property options with different numbers of bedrooms to cater to the diverse housing needs of residents. The following is a breakdown of the number of bedrooms in properties available in the Melbourne housing market, along with relevant data: One Bedroom: One-bedroom properties, such as apartments and units, are popular among young professionals, students, and individuals seeking compact and affordable housing options. These properties typically offer a single bedroom, a living area, a kitchen, and a bathroom. The median price for a one-bedroom property in Melbourne is approximately $400,000, with variations based on location, building amenities, and size. Two Bedrooms: Two-bedroom properties are versatile and cater to a wide range of residents, including couples, small families, and individuals seeking additional space. These properties often feature a master bedroom and a secondary bedroom, along with living areas, kitchens, and bathrooms. The median price for a two-bedroom property in Melbourne is around $600,000, with variations based on location, proximity to amenities, and property type. Three Bedrooms: Three-bedroom properties are popular among families and individuals seeking more space for living, working, and entertaining. These properties typically include a master bedroom, two additional bedrooms, multiple bathrooms, and larger living areas. The median price for a three-bedroom property in Melbourne is approximately $800,000, with variations based on location, property size, and outdoor spaces. Four or More Bedrooms: Properties with four or more bedrooms are often sought after by larger families, individuals needing space for home offices, or those desiring additional living and entertainment areas. These properties can include multiple bedrooms, spacious living and dining areas, outdoor spaces, and additional amenities. The median price for a four or more-bedroom property in Melbourne varies depending on location, size, and property type, with prices typically starting at $1 million and increasing based on the property's features and location. Overall, Melbourne's real estate market offers a diverse range of properties with varying numbers of bedrooms, providing options for individuals, couples, families, and investors seeking housing solutions that align with their lifestyle, space requirements, and budget considerations."
               ,sliderInput("TargPrice", #PRICE SLIDER
                            "Price Range",
                            min = 0,
                            max = 2000000,
                            value = c(0, 2000000),
                            step = 10000,ticks = FALSE),
               dateRangeInput("TargDates", #DATE SELECTOR
                              "Transaction Date Range",
                              start = "2010-01-01",
                              end = NULL,
                              format = "yyyy-mm-dd",
                              min = "2010-01-01",
                              max = NULL),
               sliderInput("TargBeds",
                           "Bedrooms",
                           min = 0,
                           max = 10,
                           value = c(0, 10),
                           step = 1,
                           ticks = FALSE),
               sliderInput("TargBaths",
                           "Bathrooms",
                           min = 0,
                           max = 3,
                           value = c(0, 3),
                           step = 1,
                           ticks = FALSE),
               sliderInput("TargParking",
                           "Parking",
                           min = 0,
                           max = 3,
                           value = c(0, 3),
                           step = 1,
                           ticks = FALSE),
               ),
      
      "Conclusion",
      tabPanel("Conclusion",
               "In conclusion: The Melbourne real estate market presents a multifaceted landscape, offering a diverse range of property options in various locations, with different numbers of bedrooms, and catering to a wide spectrum of income levels. The relationship between income level, property type, proximity to the city centre, and the number of bedrooms plays a significant role in determining property prices in Melbourne. Income level is a crucial factor that influences the type of property an individual or household can afford. Affluent suburbs in Melbourne, such as Toorak and South Yarra, are known for their high average household incomes, which often correlate with the purchase of detached houses, luxury apartments, and spacious properties with premium amenities. On the other hand, middle-income households may gravitate towards townhouses, units, or properties in the inner suburbs, while lower-income households may seek more affordable options, such as one-bedroom apartments or units. The type of property also influences its price, with detached houses typically commanding higher prices compared to apartments, townhouses, and units. Properties in the city centre or in sought-after bayside suburbs often come at a premium due to their proximity to amenities, employment opportunities, and lifestyle conveniences. Meanwhile, properties in the outer suburbs or rural areas are generally more affordable, offering larger living spaces and a quieter environment. The proximity to the city centre is another critical factor impacting property prices in Melbourne. Properties located in the CBD or inner suburbs, with easy access to business districts, public transport, dining, and entertainment precincts, often come with a higher price tag. The demand for urban convenience and lifestyle amenities contributes to the premium cost of properties in these areas. Conversely, properties in the outer suburbs or regional areas, while more affordable, may require a longer commute to the city centre, impacting their pricing. The number of bedrooms in a property also plays a pivotal role in determining its price. Larger properties with more bedrooms cater to the needs of families, professionals, and individuals seeking additional living space or home office facilities. As the number of bedrooms increases, so does the overall size and desirability of the property, often resulting in higher prices. This is particularly evident in properties with four or more bedrooms, which are typically sought after by larger families or those seeking expansive living areas. In summary, the relationship between income level, property type, proximity to the city centre, and the number of bedrooms is intricately intertwined with property prices in Melbourne. Affordability, lifestyle preferences, and spatial requirements all factor into the decision-making process for property buyers and renters. Understanding these relationships is crucial for individuals and families navigating the Melbourne real estate market, as it allows them to make informed choices based on their financial means, housing preferences, and lifestyle aspirations."),
      tabPanel("Bibliography",
               "Wiesel, I., de Bruyn, J., Meekes, J., & Chandrashekeran, S. (2023). Income polarisation, expenditure and the Australian urban middle class. Urban Studies (Sage Publications, Ltd.), 60(14), 2779–2798. https://doi.org/10.1177/00420980231164922 Trebilcock, M. (2018). Middle Income Access to Justice / M. Trebilcock, Lorne Sossin, Anthony Duggan. University of Toronto Press. García, J. D., PhD. (2022). Middle class. Salem Press Encyclopedia."),
      tabPanel("Appendix","There is not enough time to finish all the diagrams here, so I think the PDF file will illustrate more clearly what my story logic and diagram are.
"),
      
    
    #actionButton("Overview of the Assignment 3", "Overview",width = 100),
    #textOutput("text1"),
    
    #actionButton("Income Level", "Income", width = 100),
    #textOutput("text2"),
    #dataTableOutput("my_table1"),
    #dataTableOutput("my_table2"),
      
    #checkboxGroupInput("TargPropType", #PROPERTY TYPE BOXES
                       #"Property Type",
                       #choices = list("House", "Unit"),
                       #selected = list("House", "Unit")),   
    #actionButton("Type of property", "Type"),
    #textOutput("text3"),
  
    
    #actionButton("Location of the property", "Location",width = 100),
    #textOutput("text4"),
    
    #mainPanel(
      #tabsetPanel(
        #tabPanel("CBD",
                 #plotOutput("ProxCBDPlot"),
        #),
        #tabPanel("Train Station",
                 #plotOutput("ProxTrainPlot")
        #),
        #tabPanel("Green Space",
                 #plotOutput("ProxGreenPlot")
        #)
      #)
    #), #mainPanel
    
    
      
        #sliderInput("TargPrice", #PRICE SLIDER"Price Range",
                    #min = 0,
                    #max = 2000000,
                    #value = c(0, 2000000),
                    #step = 10000,ticks = FALSE),
        #dateRangeInput("TargDates", #DATE SELECTOR
                       #"Transaction Date Range",
                       #start = "2010-01-01",
                       #end = NULL,
                       #format = "yyyy-mm-dd",
                       #min = "2010-01-01",
                       #max = NULL),
    
    
    #actionButton("How many bedrooms of this property", "Bedroom",width = 100),
    #textOutput("text5"),
    
    #sliderInput("TargBeds",
                #"Bedrooms",
                #min = 0,
                #max = 10,
                #value = c(0, 10),
                #step = 1,
                #ticks = FALSE),
    #sliderInput("TargBaths",
                #"Bathrooms",
                #min = 0,
                #max = 3,
                #value = c(0, 3),
                #step = 1,
                #ticks = FALSE),
    #sliderInput("TargParking",
                #"Parking",
                #min = 0,
                #max = 3,
                #value = c(0, 3),
                #step = 1,
                #ticks = FALSE),
    
    
    #actionButton("In conclusion", "Conclusion",width = 100),
    #textOutput("text6"),
    
    #actionButton("Bibliography", "Bibliography",width = 100),
    #textOutput("text7"),
    
    
    #actionButton("Appendix", "Appendix",width = 100),
    #textOutput("text8"),  
    
    ) 
  )
  #> This Font Awesome icon ('money') does not exist:
  #> * if providing a custom `html_dependency` these `name` checks can 
  #>   be deactivated with `verify_fa = FALSE`


server <- function(input, output, session) {
  output$panel <- renderText({
    paste("Current panel: ", input$tabset)
  })
  
  output$panel <- renderText({
    paste("The paper stage in: ", input$tabset)
  })
  
    
 
    data <- data.frame(
      Incomelevel= c("$1-$999", "$1,000-$2,00", "$2,000-$4,000","$4,000-$6,000","$6,000-$8,000","More than $8,000$","Not stated"),
      Percentage = c(22, 28, 27, 10, 2.4, 1, 5),
      Number = c(15824, 20240, 19216, 6938,1729,690,3586)
    
    )
    
    # Melbourne data
    melbourne_income <- data.frame(
      Incomelevel = c("$1-$999", "$1,000-$2,00", "$2,000-$4,000","$4,000-$6,000","$6,000-$8,000","More than $8,000$","Not stated"),
      Number = c(15824, 20240, 19216, 6938,1729,690,3586)
    )
    
    output$incomePlot <- renderPlot({
      # Use ggplot2 to create the bar chart
      ggplot(data = melbourne_income, aes(x = Incomelevel, y = Number, fill = Incomelevel)) +
        geom_bar(stat = "identity") +
        theme_minimal() +
        labs(
          title = "Median Income Level",
          x = "Median Income ($)",
          y = "Number",
          fill = "Number"
        ) +
        theme(legend.position = "none") # Hide the legend if not needed
    })

    
    
    output$my_table1<- renderDataTable({
      data
    })
    
    
    output$ProxCBDPlot <- renderPlot({
      ggplot(data = filteredDataProx(), mapping = aes(x = distance_cbd_m, y = get(input$ProxYAxis), col = PropertyType)) +
        geom_smooth(method = "auto", level = input$proxConfidence) +
        labs(x = "Distance to CBD (m)", y = "Value Metric")
    })
    
    output$ProxTrainPlot <- renderPlot({
      ggplot(data = filteredDataProx(), mapping = aes(x = distance_to_trainstation * 1000, y = get(input$ProxYAxis), col = PropertyType)) +
        geom_smooth(method = "auto", level = input$proxConfidence) +
        labs(x = "Distance to Nearest Train Station (m)", y = "Value Metric")
    })
    
    output$ProxGreenPlot <- renderPlot({
      ggplot(data = filteredDataProx(), mapping = aes(x = Distance, y = get(input$ProxYAxis), col = PropertyType)) +
        geom_smooth(method = "auto", level = input$proxConfidence) +
        labs(x = "Distance to Nearest Green Space (m)", y = "Value Metric")
    })

    
    filteredDataFA <- reactive({
      filter(PropertyDataMerged,
             PropertyType %in% input$PAFpropType,
             price >= input$PAFprice[1] & price <= input$PAFprice[2],
             between(as.Date(transDate), as.Date(input$PAFdates[1]), as.Date(input$PAFdates[2]))
      )
    })
    
    output$FABedsPlot <- renderPlot({
      ggplot(data = filteredDataFA(), mapping = aes(x = Bedrooms, y = get(input$FAYAxis), group = Bedrooms)) +
        geom_boxplot() +
        facet_grid(rows = vars(PropertyType)) +
        labs(x = "Bedrooms", y = "Value Metric")
    })
    
    output$FABathsPlot <- renderPlot({
      ggplot(data = filteredDataFA(), mapping = aes(x = Baths, y = get(input$FAYAxis), group = Baths)) +
        geom_boxplot() +
        facet_grid(rows = vars(PropertyType)) +
        labs(x = "Bathrooms", y = "Value Metric")
    })
    
    output$FAParkingPlot <- renderPlot({
      ggplot(data = filteredDataFA(), mapping = aes(x = Parking, y = get(input$FAYAxis), group = Parking)) +
        geom_boxplot() +
        facet_grid(rows = vars(PropertyType)) +
        labs(x = "Parking Spaces", y = "Value Metric")
    })
      
   
  }  
  

# Define server logic ----


# Run the app ----
shinyApp(ui = ui, server = server)

