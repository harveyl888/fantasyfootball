##
## Fantasy football 2016/17
##

library(shiny)
library(DT)
library(formattable)
library(fplR)
library(dplyr)

leagueID <- 388914


## return position name from ID
.posID <- function(id) {
  positions <- c('GLK', 'DEF', 'MID', 'FWD')
  return(sapply(id, function(x) positions[x]))
}

## return team name from ID
.teamID <- function(fpl, id) {
  return(sapply(id, function(x) fpl$teams[fpl$teams$id == x, 'short_name']))
}



server <- function(input, output) {
  
  fpl <- reactiveValues()
  
  observe({
    withProgress(message = 'Downloading Data', value = 0, {
      incProgress(0.5, message = 'Grabbing Player Details')
      fpl$all <- fplR::getFPLData()
      fpl$currentWeek <- which(fpl$all$events$is_current)
      incProgress(1.0, message = 'Grabbing League Table')
      fpl$league <- fplR::getLeagueTable(leagueID, fpl$currentWeek)
      entries <- fpl$league$standings$results$entry
      setProgress(0, message = 'Grabbing Teams')
      l.teams <- list()
      for (i in 1:length(entries)) {
        setProgress(i / length(entries), message = paste0('Grabbing Team: ', fpl$league$standings$results$entry_name[i]))
        l.teams[[paste0('T_', entries[[i]])]] <- fplR::getTeam(entries[[i]] , fpl$currentWeek)
        Sys.sleep(0.1)
      }
      fpl$teamsAll <- l.teams
      fpl$teamsTable <- lapply(l.teams, function(x) x$picks %>% left_join(fpl$all$elements %>%
                                                                      select(id, first_name, second_name, element_type, team, event_points, total_points), c(element = 'id')) %>%
                                 arrange(element_type, team) %>%
                                 mutate(element_type = .posID(element_type)) %>%
                                 mutate(team = .teamID(fpl$all, team)) %>%
                                 rename(pos = element_type) %>%
                                 rename(pts = event_points) %>%
                                 rename(total = total_points)
                                                                      )
    })
  })
  

  output$header <- renderUI({
    h3(fpl$league$league$name)
  })
  
  output$leagueTable <- DT::renderDataTable({
    df.league <- fpl$league$standings$results[, c('rank', 'entry_name', 'player_name', 'event_total', 'total')]
    names(df.league) <- c('Position', 'Team', 'Manager', 'Week', 'Total')
    as.datatable(formattable(df.league, list()), 
                 options = list(dom = 't', 
                                paging = FALSE,
                                ordering = FALSE,
                                info = FALSE),
                 rownames = FALSE,
                 selection = 'single')
  })
  
  output$teamTable <- DT::renderDataTable({
    req(input$leagueTable_row_last_clicked)
    rowID <- input$leagueTable_row_last_clicked
    teamID <- fpl$league$standings$results[rowID, 'entry']
    team_table <- fpl$teamsTable[[paste0('T_', teamID)]]
    team_table$Captaincy <- NA
    team_table[which(team_table$is_captain), 'Captaincy'] <- 'C'
    team_table[which(team_table$is_vice_captain), 'Captaincy'] <- 'V'
    team_table$name <- paste(team_table$first_name, team_table$second_name)
    team_table <- team_table[, c(1:7, 13, 8:12)]
    
    as.datatable(
      formattable(team_table, 
                  list(first_name = formatter('span', style = ~ style(color=ifelse(is_captain == TRUE, 'green', 
                                                                                   ifelse(is_vice_captain == TRUE, 'red', 'black')))),
                       second_name = formatter('span', style = ~ style(color=ifelse(is_captain == TRUE, 'green', 
                                                                                         ifelse(is_vice_captain == TRUE, 'red', 'black')))),
                       Captaincy = formatter('span', style = x ~ style(color = ifelse(x == 'C', "green", "blue")),
                                             x ~ icontext(ifelse(x == 'C', 'user', 'user')))
                       )
                  ), 
      options = list(columnDefs = list(list(visible = FALSE, targets = c(0:6))),
                     dom = 't',
                     paging = FALSE,
                     ordering = FALSE,
                     info = FALSE), 
      selection = 'none',
      rownames = FALSE)
  })
  
}

ui <- fluidPage(
  uiOutput('header'),
  tabsetPanel(
    tabPanel('Table',
             column(7, DT::dataTableOutput('leagueTable')),
             column(5, div(DT::dataTableOutput('teamTable'), style = "font-size:80%"))
    ),
    tabPanel('Available Players')
  )
)

shinyApp(server = server, ui = ui)
