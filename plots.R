library(readr)
library(ggplot2)
library(shiny)
library(plotly)
library(dplyr)

setwd("C:\\Users\\think\\OneDrive\\Desktop\\coding\\R")

ev_data <- read.csv("mydata_project.csv")
first <- read.csv("Cheapestelectriccars-EVDatabase.csv")
ev_data <- ev_data %>%
  mutate(Brand = gsub(" .*", "", Name))

ev_data <- ev_data[!is.na(ev_data$PriceinGermany_euro), ]

# Group by Brand and calculate the mean price
brand_summary <- ev_data %>%
  group_by(Brand) %>%
  summarise(AvgPrice = mean(PriceinGermany_euro, na.rm = TRUE))

brand_summary <- brand_summary %>%
  mutate(Quantile = ntile(AvgPrice, 10))

### Visualization - Acceleration vs. Top Speed
# Scatter plot for Acceleration vs. Range, color-coded by Drive type
pl <- ggplot(ev_data, aes(x = Acceleration_sec, y = Range_km, color = Drive)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "lightgray", size = 1, formula = y ~ poly(x, 2), n = 100) +
  labs(title = "Acceleration vs. Range for Electric Cars\n",
       x = "\nAcceleration (seconds)",
       y = "Range (km)\n",
       color = "Drive Type") +
  theme_light() +
  theme(legend.position = c(0.922, 0.89)) +  # Adjust the title position
  scale_y_continuous(breaks = seq(0, 700, by = 50)) +
  scale_x_continuous(breaks = seq(0, 20, by = 2),limits = c(1,20))
pl

# Save the plot
ggsave("Acceleration vs. Range, color-coded by Drive type.png", plot = pl, width = 10, height = 6, units = "in", dpi = 300)


# Scatter plot for Efficiency vs Range with a custom legend and correlation line
scatter_plot <- ggplot(ev_data, aes(x = Efficiency_whkm, y = Range_km, color = factor(NumberofSeats), size = NumberofSeats)) +
  geom_point(alpha = 0.7) +
  geom_point(shape = 21, color = "black") +  # Add points with transparency
  scale_color_manual(values = c("4" = "lightgray", "5" = "lightgray", "7" = "red")) +  # Define colors for each number of seats
  labs(title = "Efficiency vs Range of Electric Cars\n",
       x = "\nEfficiency (Wh/km)",
       y = "Range (km)\n",
       caption = "Size and color represent the number of seats") +
  theme_light() +
  theme(legend.position = c(0.94, 0.93)) +  # Adjust the legend position (values are between 0 and 1)
  guides(
    color = "none",
    size = guide_legend(override.aes = list(color = c("lightgray", "lightgray", "red"), shape = c(16, 16, 16)),
                        title = "Number of Seats",
                        keywidth = 1, keyheight = 1)
  ) +  # Combine color and size in a single legend
  geom_smooth(aes(color = factor(NumberofSeats)), method = "lm", se = FALSE, size = 1) +  # Add thin correlation lines with seat color
  annotate("text", x = max(ev_data$Efficiency_whkm), y = min(ev_data$Range_km),
           label = paste("Correlation: ", round(cor(ev_data$Efficiency_whkm, ev_data$Range_km), 3)),
           hjust = 1, vjust = 0, color = "black")  # Add text annotation for correlation value

# Show the updated plot
print(scatter_plot)
ggsave("Efficiency vs Range (7 seats).png", plot = scatter_plot, width = 12, height = 8, units = "in", dpi = 300)




#### To create a bar chart for price comparison between electric cars in Germany and the UK
# Create a grouped bar chart for average price comparison by brand
# Remove rows with missing values in the 'PriceinGermany_euro' column


library(RColorBrewer)
mcolor <- c(brewer.pal(9,"YlOrRd"),"#3B0B0b")

# Modify the theme to remove the frame from the bar chart
bar_chart <- ggplot(brand_summary, aes(x = reorder(Brand, -AvgPrice), y = AvgPrice, fill = as.factor(Quantile))) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  labs(title = "Average Price Comparison of Electric Cars by Brand in Germany\n",
       x = "Brand",
       y = "Average Price in Euros\n") +
  scale_fill_manual(values = mcolor) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Make x-axis labels horizontal
        legend.position = c(0.95, 0.95),  # Adjust the legend position to the right top corner
        legend.justification = c("right", "top"),
        panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),  # Remove panel border (frame)
        legend.title = element_blank()) +
  scale_y_continuous(breaks = seq(0, 420000, by = 20000), 
                     labels = scales::number_format(scale = 0.001, suffix = "k"))

# Show the plot
print(bar_chart)
ggsave("bar_chart.png", plot = bar_chart, width = 10, height = 6, units = "in", dpi = 300)




library(RColorBrewer)

# Choose a color palette from RColorBrewer
chosen_palette <- brewer.pal(9, "YlOrRd")

# Add an additional color to your palette
mcolor <- c(chosen_palette, "#3B0B0b")

#### Interactive doshboard

# Install and load required packages
#install.packages(c("shiny", "plotly", "dplyr"))


# Load your dataset
# Assuming your dataset is stored in a CSV file named 'electric_cars.csv'
# Change the path accordingly
electric_cars <- read.csv("mydata_project.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Electric Cars Dashboard"),
  sidebarLayout(
    sidebarPanel(
      # Add interactive elements here (e.g., filters, sliders)
      selectInput("drive_type", "Select Drive Type:", choices = unique(electric_cars$Drive), selected = NULL),
      sliderInput("price_range", "Select Price Range:", min = 0, max = 300000, value = c(0, 300000), step = 1000)
    ),
    mainPanel(
      # Output plotly chart
      plotlyOutput("scatter_plot")
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Create reactive data subset based on user inputs
  filtered_data <- reactive({
    electric_cars %>%
      filter(Drive %in% input$drive_type,
             PriceinGermany_euro >= input$price_range[1],
             PriceinGermany_euro <= input$price_range[2])
  })
  
  # Create scatter plot
  output$scatter_plot <- renderPlotly({
    # Use plotly for interactive plots
    plot_ly(data = filtered_data(),
            x = ~Efficiency_whkm,
            y = ~Range_km,
            type = "scatter",
            mode = "markers",
            text = ~Name,
            marker = list(size = 10, opacity = 0.8, color = ~PriceinGermany_euro, colors = mcolor))
  })
}

# Run the application
shinyApp(ui, server)
 




