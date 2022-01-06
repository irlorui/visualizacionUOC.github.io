# Load packages ----------------------------------------------------------------

library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)
library(wordcloud2)

# code to run just once (e.g. loading data) ------------------------------------
movies_df <- read.csv("MoviesOnStreamingPlatforms_updated.csv", na.strings = "")
movies_df <- movies_df %>% select(-c(1:2)) %>%
  mutate_at(vars("IMDb", "Rotten.Tomatoes"), funs(gsub("/10{1}0*", "", .))) %>%
  mutate_at(vars("Type"), funs(gsub("0", "Movies", .))) %>%
  mutate_at(vars("Age"), funs(gsub("all", "General Audiences", .))) %>%
  mutate_at(vars("Age"), funs(factor(., levels = c("General Audiences", "7+", "13+", "16+", "18+")))) %>%
  mutate_at(vars("IMDb", "Rotten.Tomatoes"), funs(as.numeric(.))) %>%
  filter(complete.cases(select(movies_df, c(2:10))))

series_df <- read.csv("tv_shows.csv", na.strings = "")
series_df <- series_df %>% select(-c(1:2)) %>%
  mutate_at(vars("IMDb", "Rotten.Tomatoes"), funs(gsub("/10{1}0*", "", .))) %>%
  mutate_at(vars("Type"), funs(gsub("1", "Series", .))) %>%
  mutate_at(vars("Age"), funs(gsub("all", "General audiences", .))) %>%
  mutate_at(vars("Age"), funs(factor(., levels = c("General Audiences", "7+", "13+", "16+", "18+")))) %>%
  mutate_at(vars("IMDb", "Rotten.Tomatoes"), funs(as.numeric(.))) %>%
  filter(complete.cases(select(series_df, c(2:10))))

# combine both tables
contents_df <- bind_rows(movies_df, series_df)
contents_df <- contents_df %>% 
  mutate_at(vars("Type"), funs(as.factor(.))) %>%
  rename("Prime" = "Prime.Video", "Disney" = "Disney.", "Rotten Tomatoes" = "Rotten.Tomatoes") %>%
  gather(platform, present, Netflix:Disney, factor_key=TRUE) %>%
  filter(present == 1, Year > 1944) %>%
  select(-c(present)) 
  

# Different values of genre, countries and languages
genres <- unique(unlist(strsplit(as.character(contents_df$Genres), ",")))
genres <- genres[!genres %in% NA]
countries <- unique(unlist(strsplit(as.character(contents_df$Country), ",")))
countries <- countries[!countries %in% NA]
languages <- unique(unlist(strsplit(as.character(contents_df$Language), ",")))
languages <- languages[!languages %in% NA]


unique_genres <- unique(unlist(strsplit(as.character(contents_df$Genres), ",")))
unique_genres <- genres[!genres %in% NA]
unique_platforms <- levels(contents_df$platform)
genre_count <- matrix(1:108, nrow = length(unique_genres), ncol = length(unique_platforms))
colnames(genre_count) <- unique_platforms
rownames(genre_count) <- unique_genres
for (i in c(1:length(unique_genres))) {
  for(j in c(1:length(unique_platforms))) {
    genre_count[i,j] <- nrow(filter(contents_df[contents_df$platform == unique_platforms[j], ],
                                    grepl(unique_genres[i],
                                          contents_df[contents_df$platform == unique_platforms[j], "Genres"])))
  }
}

genre_count <- as.data.frame(genre_count)

platform_colors <- c("#EA2B1F", "#45F0DF", "#0077B6", "#F0C808")

# Define UI --------------------------------------------------------------------

ui <- navbarPage(inverse = TRUE, "Best streaming platform",
  
  # First Page - Intro        
  tabPanel("Intro", includeCSS("styles.css"),
      
     fluidPage(
          
          h1("Choose the perfect streaming platform for you"),
          br(),
          p(paste0("What is the best streaming service out there? Short answer",
          " would be: it depends on what you are looking for. To save you some ",
          "time going into deeply philosophical discussion, this visualization ", 
          "tool will help you analyse the content across four streaming services",
          " in an easy and interactive way.")),
          br(),
          p("Navigate to the next tab and interact with the graphs using the ",
            "different data filters to check", 
            "which streaming platform suits better your preferences ", 
            "within Movies and TV shows. "),
          br(),
          p("You can compare the following streaming platforms:"),
          br(),
          br(),
          fluidRow(
            column(img(src = "netflix.png", height = 100, width = 100), width = 2, offset=2), 
            column(img(src = "hulu.png", height = 100, width = 100), width = 2), 
            column(img(src = "prime.png", height = 100, width = 100), width = 2), 
            column(img(src = "disney.jpg", height = 100, width = 100), width = 2)
          ),
          
          br(),
          br(),
          br(),
          div(p(strong("Author:"), "Irene Lopez Ruiz, using Rstudio and Shiny."), 
              p(strong("Code:"), "All R files and CSV datasets files are available at my", a("GitHub", href="https://github.com/irlorui")),
              p(strong("R Packages:"), "shiny, tidyr, dplyr, wordcloud2, ggplot2."),
              p(strong("Data Sources:"), "The datasets used are available in ", 
                a("Kaggle", href = "https://www.kaggle.com")),
              style="text-align: right;"
          )                 
      )
  ),
  
  tabPanel("Streaming platform analysis",
      fluidPage(
        sidebarLayout(
          
          sidebarPanel(style = "background: #343E3D",
            
            wellPanel(style = "background: white",
                      h2("General comparison"),
                      br(),
                      p(paste0("Here you can configure different parameters such",
                               " as the type of contents, the audience rating, ",
                               "the year of release or the score scale to compare",
                               " the four streamimng platforms.")),
                      br(),
                      p(paste0("You would get a general idea of each platform",
                               " quantity, quality and audience rating distribution."))),
                       
                       
            wellPanel(style = "background: white",
              
            
              checkboxGroupInput(inputId="type_to_check",
                                 label="Type of content",
                                 choices = levels(contents_df$Type),
                                 selected = levels(contents_df$Type)
              ),
              radioButtons(inputId = "score",
                           label = "Score scale",
                           choices = c("IMDb", "Rotten Tomatoes"),
                           selected = "IMDb"),
              
              sliderInput(inputId="years",
                          label="Year of release",
                          min=min(contents_df$Year),
                          max=max(contents_df$Year),
                          value=c(min(contents_df$Year), max(contents_df$Year)),
                          sep=""),
              checkboxGroupInput(inputId = "age",
                          label = "Select audience rate", 
                          choices = levels(contents_df$Age),
                          selected = levels(contents_df$Age))
              
           ),
           wellPanel(style = "background: white",
                     h2("Specific platform genres"),
                     br(),
                     p(paste0("Choose a specific platform to see",
                              " the more common movie genres in it."))),
           
           
           wellPanel(style = "background: white",
                     
                     selectInput(inputId = "platform_selection",
                                 label = "Select a streaming platform",
                                 choices = levels(contents_df$platform),
                                 selected = levels(contents_df$platform)[1],
                                 multiple = FALSE)), 
           width = 3),
          
          mainPanel(
            
            fluidRow(
              column(h2("Amount of titles Across the Streaming Services"), width = 12, offset= 2)
            ),
            
            fluidRow(
              plotOutput(outputId = "stackedBarPlot")
            ),
            
            fluidRow(
             column(div(h3("Rating Distribution of each platform"), style="text-align: center;"), 
                    plotOutput(outputId = "boxplot"), width = 6),
             column(div(h3("Target audience on each platform titles"), style="text-align: center;"),
                    plotOutput(outputId = "agePlot"), width = 6)
            ),
            
            fluidRow(
              column(div(h2(textOutput(outputId="title_wordcloud")), style="text-align: center;"),
                     wordcloud2Output(outputId = "wordcloud", width = "100%", height = "565px"), width=12)
            )
          )
        ) 
                            
      )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
   
  type_per_platform <- reactive({
    contents_df %>% 
      select(platform, Type, Year, Age, Genres) %>%
      filter(Type %in% input$type_to_check, 
             Year %in% input$years,
             Age %in% input$age) %>%
      group_by(Type, platform) %>%
      summarise(Count = n())
  })
  
  filtered_title_type_score <- reactive({
    contents_df %>% 
      filter(Type %in% input$type_to_check, 
             Year %in% input$years,
             Age %in% input$age) %>%
      select(Type, platform, input$score, Year, Age, Genres) %>%
      rename(Score = input$score)
  })
  
  audience_rates <- reactive({
    filtered_title_type_score() %>%
      group_by(Age, platform) %>%
      summarise(Count =n())
  })
  
  platform_wordcloud <- reactive({
    genre_count %>%
    select(input$platform_selection) %>%
    rename(word_count = input$platform_selection) %>%
    mutate(word = row.names(genre_count)) %>%
    relocate(word_count, .after=word)
  })
  
  platform_data <- reactive({
    contents_df %>%
    filter(platform %in% input$platform_selection)
  })
  
  output$stackedBarPlot <- renderPlot({
    ggplot(type_per_platform(), aes(fill=Type, x = platform, y = Count)) + 
      geom_bar(position='stack', stat="identity") +
      labs(y="Total", x="") + 
      scale_fill_manual(values = c(platform_colors[1], platform_colors[2])) +
      theme_classic() +
      theme(text = element_text(size=15), panel.grid.minor.x = element_blank(),
            axis.line.x = element_line(colour = "black", size = 1),
            axis.text = element_text(size = 12),
            panel.grid.major.x = element_blank(),
            axis.ticks.x = element_line(),
            plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
            plot.title = element_text(hjust = 0.5),
            legend.title = element_blank())
  })
  
  output$boxplot <- renderPlot({
    ggplot(filtered_title_type_score(), aes(x=Type, y=Score, fill=platform))+
      geom_boxplot() +
      labs(y=input$score, x="Type") + 
      scale_fill_manual(values = platform_colors) +
      theme_classic() +
      theme(text = element_text(size=15), panel.grid.minor.x = element_blank(),
            axis.line.x = element_line(colour = "black", size = 1),
            axis.text = element_text(size = 12),
            panel.grid.major.x = element_blank(),
            axis.ticks.x = element_line(),
            plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
            plot.title = element_text(hjust = 0.5),
            legend.title = element_blank(), legend.position = "top")
    
  })
  
  
  output$agePlot <- renderPlot({
    ggplot(audience_rates(), aes(x=Age, fill=platform, y=Count)) + 
      geom_bar(position='dodge', stat="identity") +
      labs(y="Total", x="Audience rating") + 
      scale_fill_manual(values = platform_colors) +
      theme_classic() +
      theme(text = element_text(size=15), panel.grid.minor.x = element_blank(),
            axis.line.x = element_line(colour = "black", size = 1),
            axis.text = element_text(size = 12),
            panel.grid.major.x = element_blank(),
            axis.ticks.x = element_line(),
            plot.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"),
            plot.title = element_text(hjust = 0.5),
            legend.title = element_blank(), legend.position = "top")
  })
  
  output$title_wordcloud <- renderText({
    paste("More common movie genres in ", input$platform_selection)
  })
  
  output$wordcloud <- renderWordcloud2({
    wordcloud2(platform_wordcloud(), size=1.6, color=rep(platform_colors, length.out=nrow(demoFreq)))#, nrow(word_counts()))
  })  

}
# Create the Shiny app object --------------------------------------------------

shinyApp(ui, server)
