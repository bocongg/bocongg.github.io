library(shiny)
library(plotly)
library(gridlayout)
library(bslib)
library(DT)
library(tidyverse)
library(ggplot2)
library(plyr)
library(lubridate)
library(zoo)
library(shinyBS)
library(bsicons)

### Tidy the dataset
wait_times <- read_csv("test_waiting_time.csv")

bed_occ <- read_csv("test_BOR.csv")

EMD_attendance <- read_csv("test_EMD_attendance.csv")


tidy_wait_times <- wait_times %>%
  separate(col = Date, into = c("Day_wk", "Date"), sep = ", ") %>%
  mutate(new_Date_format = 
           parse_date_time(Date, orders = c("mdy", "dmy", "ymd"))) %>%
  mutate(Dates = format(new_Date_format, "%d/%m/%Y")) %>%
  separate(col = Date, into = c("Day", "Month", "Year"), sep = "/") %>%
  gather(key = hospital, value = wait_duration, 5:12 ) %>%
  select(Day_wk, Dates, Day, Month, Year, hospital, wait_duration)

tidy_bed_occ <- bed_occ %>%
  filter(Years == 2023) %>%
  mutate(new_Date_format = 
           parse_date_time(Date, orders = c("mdy", "dmy", "ymd"))) %>%
  mutate(Dates = format(new_Date_format, "%d/%m/%Y")) %>%
  gather(key = hospital, value = occ_rate, 3:10) %>%
  separate(col = occ_rate, into = c("occ_rate"), sep = "%") %>%
  mutate(occ_rate_num = as.numeric(occ_rate)) %>%
  select(occ_rate_num)

tidy_EMD_attendance <- EMD_attendance %>%
  separate(col = Date, into = c("Day_wk", "Date"), sep = ", ") %>%
  mutate(new_Date_format = 
           parse_date_time(Date, orders = c("mdy", "dmy", "ymd"))) %>%
  mutate(Dates = format(new_Date_format, "%d/%m/%Y")) %>%
  separate(col = Date, into = c("Day", "Month", "Year"), sep = "/") %>%
  gather(key = hospital, value = attendance, 5:12) %>%
  select(attendance)

final_dataset <- cbind(tidy_wait_times,tidy_bed_occ,tidy_EMD_attendance)

final_dataset <- 
  final_dataset %>%
  transform(Day = as.numeric(Day)) %>%
  mutate(new_Date_format = 
           parse_date_time(Dates, orders = c("mdy", "dmy", "ymd"))) %>%
  mutate(week_num = as.numeric(format(new_Date_format, "%U"))) %>%
  mutate(fullmonth = format(new_Date_format, "%b")) %>%
  mutate(yearmonth = as.yearmon(new_Date_format)) %>%
  mutate(yearmonthf = factor(yearmonth)) %>%
  ddply(.(yearmonthf), transform, monthweek=5-week_num+min(week_num)) %>%
  select(-new_Date_format,-yearmonth,-yearmonthf)

###############################################################################

### Define shiny modular functions
sliderInputdate <- function(id, setvalue){
  sliderInput(id, label = NULL, min = 1, max = 31, value = setvalue)
}

selectInputdate <- function(id, setlabel, choices = list(
  "January" = "01",
  "February" = "02",
  "March" = "03",
  "April" = "04",
  "May" = "05",
  "June" = "06",
  "July" = "07",
  "August" = "08",
  "September" = "09")){
  selectInput(id, label = setlabel, choices = choices)
}

checkboxGroupInputhosp <- function(id, choices, selected = c("CGH", "SGH", "SKH")){
  choices <- switch(
    id,
    "SH" = list("CGH" = "CGH", "SGH" = "SGH", "SKH" = "SKH"),
    "NUHS" = list("AH" = "AH", "NTFGH" = "NTFGH", "NUH" = "NUH(A)"),
    "NHG" = list("KTPH" = "KTPH", "TTSH" = "TTSH")
  )
  hosplabel <- switch(
    id,
    "SH" = "Singhealth:",
    "NUHS" = "National University Health System:",
    "NHG" = "National Healthcare Group:"
  )
  
  checkboxGroupInput(id, label = hosplabel, choices = choices, selected = selected)
}

plotlyOutputgrp <- function(datagrp, plotname, grpid = " "){
  grpid <- switch(
    datagrp,
    "Waiting Time" = "WT",
    "Bed Occupancy Rate" = "BOR",
    "EMD Attendance" = "EMD"
  )
  
  plottype = paste(grpid, plotname, sep = "")
  plotlyOutput(outputId = plottype)
}

nav_panelplot <- function(datagrp, tabicon){
  nav_panel(
    title = datagrp,
    grid_container(
      layout = c("linechart","boxplot"),
      gap_size = "0px",
      col_sizes = c("1fr"),
      row_sizes = c("1fr","1fr"),
      grid_card(
        area = "linechart",
        full_screen = TRUE,
        card_body(plotlyOutputgrp(datagrp, plotname = "linechart"))
      ),
      grid_card(
        area = "boxplot",
        full_screen = TRUE,
        card_body(plotlyOutputgrp(datagrp, plotname = "boxplot"))
      )
    ),
    icon = icon(tabicon)
  )
}

plotlinedata <- function(mydata, x, startmthInput, startdateId, endmthInput, enddateId, SH, NUHS, NHG) {
  
  labelunit <- switch(
    x,
    "Waiting Time" = "hrs",
    "Bed Occupancy Rate" = "%",
    "EMD Attendance" = "patients"
  )
  ylabel <- switch(
    x,
    "Waiting Time" = "Hours (h)",
    "Bed Occupancy Rate" = "BOR (%)",
    "EMD Attendance" = "No. of patients"
  )
  
  line <- mydata %>%
    filter(Month >= startmthInput &
             Day >= startdateId &
             Month <= endmthInput &
             Day <= enddateId &
             hospital %in% c(SH, NUHS, NHG)) %>%
    ggplot(aes(x = Dates,
               y = switch(x,"Waiting Time" = wait_duration,"Bed Occupancy Rate" = occ_rate_num,"EMD Attendance" = attendance),
               text = paste(switch(x,"Waiting Time" = wait_duration,"Bed Occupancy Rate" = occ_rate_num,"EMD Attendance" = attendance),
                            labelunit))) + 
    geom_point(size = 0.75) + 
    geom_line(aes(group = hospital, color = hospital)) +
    labs(x = "Date",
         y = ylabel,
         colour = "Hospital") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  ggplotly(line, tooltip = "text") %>%
    config(displayModeBar = FALSE) %>%
    layout(dragmode = FALSE)
}

plotboxdata <- function(mydata, x, startmthInput, startdateId, endmthInput, enddateId, SH, NUHS, NHG) {
  
  ylabel <- switch(
    x,
    "Waiting Time" = "Hours (h)",
    "Bed Occupancy Rate" = "BOR (%)",
    "EMD Attendance" = "No. of patients"
  )
  
  box <- mydata %>% 
    filter(Month >= startmthInput &
             Day >= startdateId &
             Month <= endmthInput &
             Day <= enddateId &
             hospital %in% c(SH, NUHS, NHG)) %>%
    transform(hospital = as.factor(hospital)) %>%
    ggplot(aes(x = fct_reorder(hospital, desc(hospital)),
               y = switch(x,"Waiting Time" = wait_duration,"Bed Occupancy Rate" = occ_rate_num,"EMD Attendance" = attendance))) + 
    geom_boxplot(aes(fill = hospital)) + 
    labs(x = "Hospital",
         y = ylabel) +
    theme(legend.position = "none") +
    coord_flip()
  
  ggplotly(box, tooltip = "text") %>%
    config(displayModeBar = FALSE) %>%
    layout(dragmode = FALSE)
}

radioButtonshosp <- function(id, choice, selected = c("CGH")){
  choices <- switch(
    id,
    "SHradio" = list("CGH" = "CGH", "SGH" = "SGH", "SKH" = "SKH"),
    "NUHSradio" = list("AH" = "AH", "NTFGH" = "NTFGH", "NUH" = "NUH(A)"),
    "NHGradio" = list("KTPH" = "KTPH", "TTSH" = "TTSH")
  )
  
  radioButtons(id, label = NULL, choices = choices, selected = selected)
}

nav_panelplota <- function(datagrp, tabicon){
  nav_panel(
    title = datagrp,
    grid_container(
      layout = c("calendar"),
      gap_size = "0px",
      col_sizes = c("1fr"),
      row_sizes = c("1fr"),
      grid_card(
        area = "calendar",
        full_screen = TRUE,
        card_body(plotlyOutputgrp(datagrp, plotname = "calendar"))
      )
    ),
    icon = icon(tabicon)
  )
}

plotcaldata <- function(mydata, x, choosehosp){
  labelunit <- switch(
    x,
    "Waiting Time" = " hrs",
    "Bed Occupancy Rate" = "%",
    "EMD Attendance" = " patients"
  )
  xlabel <- switch(
    choosehosp,
    "CGH" = "Changi General Hospital",
    "SGH" = "Singapore General Hospital",
    "SKH" = "Sengkang General Hospital",
    "AH" = "Alexandra Hospital",
    "NTFGH" = "Ng Teng Fong General Hospital",
    "NUH(A)" = "National University Hospital",
    "KTPH" = "Khoo Teck Puat Hospital",
    "TTSH" = "Tan Tock Seng Hospital"
  )
  ylabel <- switch(
    x,
    "Waiting Time" = "h",
    "Bed Occupancy Rate" = "%",
    "EMD Attendance" = "Pt #"
  )
  
  calendar_plot <- mydata %>%
    filter(hospital == choosehosp) %>%
    ggplot(aes(x = fct_inorder(Day_wk),
               y = monthweek,
               fill = switch(x,"Waiting Time" = wait_duration,"Bed Occupancy Rate" = occ_rate_num,"EMD Attendance" = attendance),
               text = paste(x, ": ",
                            switch(x,"Waiting Time" = wait_duration,"Bed Occupancy Rate" = occ_rate_num,"EMD Attendance" = attendance),
                            labelunit, sep = ""))) +
    geom_tile(colour = "white") +
    geom_text(aes(label = Day),
              color = "black",
              size = 2) +
    facet_wrap( ~ fct_inorder(fullmonth),
                ncol = 3,
                scales = "fixed") +
    scale_fill_gradient(low="green", high = "red") +
    labs(x = xlabel,
         y = "",
         fill = ylabel) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.spacing.y = unit(0.5, "cm")) +
    scale_x_discrete(guide = guide_axis(angle = 90))
  
  ggplotly(calendar_plot, tooltip = "text") %>%
    config(displayModeBar = FALSE) %>%
    layout(dragmode = FALSE)
}

###############################################################################

### Main UI
ui <- page_navbar(
  title = "Data Dashboard",
  selected = "Multi-hospital analysis",
  collapsible = TRUE,

  ##################################### Tab 1 ###################################,
  nav_panel(
    title = "Multi-hospital analysis",
    icon = icon("map"),
    grid_page(
      layout = c("settings tabset"),
      gap_size = "10px",
      col_sizes = c("250px","1fr"),
      row_sizes = c("1fr"),
      grid_card(
        area = "settings",
        card_header("Settings:"),
        card_body(
          selectInputdate("startmthInput", setlabel = "Start Date"),
          sliderInputdate("startdateId", 1),
          selectInputdate("endmthInput", setlabel = "End Date"),
          sliderInputdate("enddateId", 31),
          checkboxGroupInputhosp("SH"),
          checkboxGroupInputhosp("NUHS"),
          checkboxGroupInputhosp("NHG")
        )
      ),

      grid_card(
        area = "tabset",
        card_body(
          tabsetPanel(
            nav_panelplot("Waiting Time", "clock"),
            nav_panelplot("Bed Occupancy Rate", "bed"),
            nav_panelplot("EMD Attendance", "person")
          )
        )
      )
    )
  ),
  
################################### Tab 2 ####################################
  nav_panel(
    title = "Single-hospital analysis",
    icon = icon("hospital"),
    grid_page(
      layout = c("settings tabset"),
      gap_size = "10px",
      col_sizes = c("250px","1fr"),
      row_sizes = c("1fr"),
      grid_card(
        area = "settings",
        card_header("Settings:"),
        card_body(
          selectInputdate("startmthInput", setlabel = "Start Date"),
          sliderInputdate("startdateId", 1),
          selectInputdate("endmthInput", setlabel = "End Date"),
          sliderInputdate("enddateId", 31),
          radioButtons("choosehosp", label = "Choose hospital",
                       choices = list("CGH" = "CGH", "SGH" = "SGH", "SKH" = "SKH",
                                      "AH" = "AH", "NTFGH" = "NTFGH", "NUH" = "NUH(A)",
                                      "KTPH" = "KTPH", "TTSH" = "TTSH")
                       )
        )
      ),

      grid_card(
        area = "tabset",
        card_body(
          tabsetPanel(
            nav_panelplota("Waiting Time", "clock"),
            nav_panelplota("Bed Occupancy Rate", "bed"),
            nav_panelplota("EMD Attendance", "person")
          ),
          grid_container(
            layout = c("header",
                       "linebarchart1",
                       "scatterplot1",
                       "linebarchart2",
                       "scatterplot2"),
            gap_size = "0px",
            col_sizes = c("1fr"),
            row_sizes = c("50px","500px", "500px", "500px", "500px"),
            grid_card_text(
              content = "Multi-variable analysis",
              alignment = "start",
              area = "header"
            ),
            grid_card(
              area = "linebarchart1",
              full_screen = TRUE,
              card_body(plotlyOutput(outputId = "linebarchart1"))
            ),
            grid_card(
              area = "scatterplot1",
              full_screen = TRUE,
              card_body(plotlyOutput(outputId = "scatterplot1"))
            ),
            grid_card(
              area = "linebarchart2",
              full_screen = TRUE,
              card_body(plotlyOutput(outputId = "linebarchart2"))
            ),
            grid_card(
              area = "scatterplot2",
              full_screen = TRUE,
              card_body(plotlyOutput(outputId = "scatterplot2"))
            )
          )
        )
      )
    )
  )
)

### Main server
server <- function(input, output) {
  
  output$WTlinechart <- renderPlotly({
    plotlinedata(final_dataset, x = "Waiting Time", input$startmthInput, input$startdateId, input$endmthInput, input$enddateId, input$SH, input$NUHS, input$NHG)
  })
  output$WTboxplot <- renderPlotly({
    plotboxdata(final_dataset, x = "Waiting Time", input$startmthInput, input$startdateId, input$endmthInput, input$enddateId, input$SH, input$NUHS, input$NHG)
  })
  
  output$BORlinechart <- renderPlotly({
    plotlinedata(final_dataset, x = "Bed Occupancy Rate", input$startmthInput, input$startdateId, input$endmthInput, input$enddateId, input$SH, input$NUHS, input$NHG)
  })
  output$BORboxplot <- renderPlotly({
    plotboxdata(final_dataset, x = "Bed Occupancy Rate", input$startmthInput, input$startdateId, input$endmthInput, input$enddateId, input$SH, input$NUHS, input$NHG)
  })
  
  output$EMDlinechart <- renderPlotly({
    plotlinedata(final_dataset, x = "EMD Attendance", input$startmthInput, input$startdateId, input$endmthInput, input$enddateId, input$SH, input$NUHS, input$NHG)
  })
  output$EMDboxplot <- renderPlotly({
    plotboxdata(final_dataset, x = "EMD Attendance", input$startmthInput, input$startdateId, input$endmthInput, input$enddateId, input$SH, input$NUHS, input$NHG)
  })
  

  output$WTcalendar <- renderPlotly({
    plotcaldata(final_dataset, x = "Waiting Time", input$choosehosp)
  })
  output$BORcalendar <- renderPlotly({
    plotcaldata(final_dataset, x = "Bed Occupancy Rate", input$choosehosp)
  })
  output$EMDcalendar <- renderPlotly({
    plotcaldata(final_dataset, x = "EMD Attendance", input$choosehosp)
  })
  
  output$linebarchart1 <- renderPlotly({
    linebar1 <- final_dataset %>% 
      filter(Month >= input$startmthInput &
             Day >= input$startdateId &
             Month <= input$endmthInput &
             Day <= input$enddateId &
             hospital == input$choosehosp) %>%
      ggplot(aes(x = Dates)) + 
      geom_col(aes(y = occ_rate_num/10), fill = "lightgrey") +
      geom_point(aes(y = wait_duration), size = 2) + 
      geom_line(aes(y = wait_duration, group = hospital, color = hospital), size = 0.75) + 
      labs(x = "Date",
           y = "Hours (h)") +
      theme(legend.position = "none") +
      scale_y_continuous(sec.axis = sec_axis(~.*10, name = "BOR (%)")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(linebar1, tooltip = "text") %>%
      config(displayModeBar = FALSE) %>%
      layout(dragmode = FALSE)
  })
  
}

shinyApp(ui, server)
