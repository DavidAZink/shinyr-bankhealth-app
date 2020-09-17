library(shinyr)
library(ggplot2)
library(plotly)


fip_bs=readRDS('bs-app/data/fip_bs.rds')
counties=readRDS('bs-app/data/counties_json.rds')
labs=c("Equity Ratio (%)"='equity_ratio',
         "Funding Cost (%)" = 'avg_interest_expense', 
         "Liquidity Risk"='liquidity_risk')
dates=c("03/31/2019"=20190331, "06/30/2019"=20190630, 
        "09/30/2019"=20190930, 
        "03/31/2020"=20200331, "06/30/2020"=20200630, 
        "09/30/2020"=20200930)

# User interface ----
ui <- fluidPage(
  titlePanel("Bank Health by County"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Plots the geographical distribution of key bank balance sheet metrics by quarter"),
      
      selectInput("metric", 
                  label = "Choose a metric to display",
                  choices = c("equity ratio (%)"='equity_ratio',
                              "funding cost (%)" = 'avg_interest_expense', 
                              "liquidity risk"='liquidity_risk'),
                  selected = "liquidity risk"),
      
      selectInput("qtr", 
                  label = "Choose a quarter",
                  choices=c("03/31/2019"=20190331, "06/30/2019"=20190630, 
                            "09/30/2019"=20190930, 
                            "03/31/2020"=20200331, "06/30/2020"=20200630), 
                  selected=20200630)
    ),
    
    mainPanel(plotlyOutput("map"),
              textOutput("text2"))
  )
)

server <- function(input, output) {
  
  g <- list(
    scope = 'usa',
    projection = list(type = 'albers usa'),
    showlakes = TRUE,
    lakecolor = toRGB('white')
  )
  output$text2 <- renderText({
    if (input$metric=="equity_ratio"){
      "Equity Ratio: The weighted average equity ratio (equity/assets) of banks active in each county. Weights are given by 
      2018 mortgage origination market share within each county."
    }
    else if(input$metric=="liquidity_risk"){
      "Liquidity Risk = The weighted average liquidity risk of banks active in each county. 
      Weights are given by 2018 mortgage orignation market shares within each county. Liquidity risk is defined
      as: (unused commitments + wholesale funding - liquidity)/assets"}
    else if (input$metric=="avg_interest_expense") {
      "Average Interest Expense = The weighted average interest expense of banks active in each county. Weights 
      are given by 2018 mortgage origination market shares within each county. Interest expense is defined
      as: interest expense/liabilities"} 
    }
  )
  
  output$map <- renderPlotly({
    
    plot_ly() %>% add_trace(
      type="choropleth",
      geojson=counties,
      locations=filter(fip_bs, DATE==input$qtr)$fips,
      z=filter(fip_bs, DATE==input$qtr)[, input$metric],
      text=filter(fip_bs, DATE==input$qtr)$county,
      colorscale="Viridis",
      zmin=round(quantile(filter(fip_bs, DATE==input$qtr)[, input$metric], 0), 0),
      zmax=round(quantile(filter(fip_bs, DATE==input$qtr)[, input$metric], 0.90), 0),
      marker=list(line=list(
        width=0)
      )
    ) %>% 
      colorbar(title = labels(labs)[which(labs==input$metric)]) %>% 
      layout(
      title = paste0("County Average Bank ", labels(labs)[which(labs==input$metric)], ': ', labels(dates)[which(dates==input$qtr)])
    ) %>% layout(
      geo = g
    )->fig1
    fig1

  })
}

shinyApp(ui, server)














