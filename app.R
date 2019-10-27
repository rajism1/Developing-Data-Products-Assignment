
#List of available brands in a showroom
brand_list <- c("Mazda","Datsun","Hornet","Valiant","Duster","Merc","Cadillac","Lincoln","Chrysler","Fiat","Honda","Toyota","Dodge","AMC","Camaro","Pontiac","Porsche","Lotus","Ford","Ferrari","Maserati","Volvo")

##Building the app
shinyApp(
    #Writing the UI functions
    ui = fluidPage(
        #Adding Title for the report
        titlePanel("Car Shopping"),
        #Defining filter layout, type , values and default selections
        sidebarLayout(
            sidebarPanel(
                helpText("Select Specification of Cars"),
                selectInput("Brand", "Brand",brand_list,multiple = TRUE,selected = brand_list),
                checkboxGroupInput('am', 'Transmission:', c("Automatic"=0, "Manual"=1), selected = c(0,1)),
                numericInput('mpg', 'Minimum Mileage of car:', 15, min=10, max=35),
                numericInput('hp', 'Minimum Horsepower:', 60, min=52, max=335),
                checkboxGroupInput('cyl', 'Number of cylinders:', c("Four"=4, "Six"=6, "Eight"=8), selected = c(4,6,8)),
                checkboxGroupInput('gear', 'Number of Gears:', c("Three"=3, "Four"=4, "Five"=5), selected = c(3,4,5))
                        ),

        #Displaying output
        mainPanel(
                tableOutput("data")
                 )
                    )
    ),
    #Writing sever function
    server = function(input, output) {
            library(dplyr)
            library(markdown)

            #Bulding the required table
            output$data <- renderTable({
            data <- mtcars
            data[,"Brand"] <- substr(rownames(data), 1, ifelse(regexpr(' ', rownames(data)) == -1,length(rownames(data)),regexpr(' ', rownames(data))-1) )
            data[,"Cars"] <- rownames(mtcars)

            ##Filtering out the required data
            data <- filter(data,
                            Brand %in% input$Brand,
                            cyl %in% input$cyl,
                            am %in% input$am,
                            mpg > input$mpg,
                            gear %in% input$gear,
                            hp > input$hp

            )
            ##Rearranging data and renaming the columns
            data <- data[,c(12,13,9,1,2,4,10,11,3)]
            colnames(data) <- c("Brand","Model","Transmission","Miles/gallon","Cylinder","Gross Horsepower","Gear","No. Of Carburetors","Displacement")
            data <- mutate(data, Transmission = ifelse(Transmission==0, "Automatic", "Manual"))
            data
        })
    }
)


