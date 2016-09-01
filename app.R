##
## Fantasy football 2016/17
##

library(shiny)
library(DT)
library(formattable)
library(fplR)
library(dplyr)
library(curl)
library(RMySQL)

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

## open database connection
con <- file("pw.txt", "r")
pw <- readLines(con)
close(con)

drv = dbDriver('MySQL')
mydb = dbConnect(drv, 
                 host = 'mysql3.gear.host', 
                 dbname = 'mysqlgearhost', 
                 user = 'fantasyfooty', 
                 pass = pw)

server <- function(input, output) {
  
  fpl <- reactiveValues()
  
  observe({
    withProgress(message = 'Downloading Data', value = 0, {
      incProgress(0.5, message = 'Grabbing Player Details')
      fpl$all <- fplR::getFPLData()
      fpl$currentWeek <- which(fpl$all$events$is_current)
      incProgress(1.0, message = 'Grabbing League Table')
      fpl$league <- fplR::getLeagueTable(leagueID, NULL)
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
                                 rename(P = position) %>%
                                 rename(pos = element_type) %>%
                                 rename(pts = event_points) %>%
                                 rename(total = total_points)
                               )
      fpl$availablePlayers <- fplR::playerCount(fpl$teamsTable, fpl$all)
      setProgress(1, message = 'Loading Transfer Table')
      fpl$transfers <- fetch(dbSendQuery(mydb, 'select * from transferlist'))
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
    team_table$C <- NA
    team_table[which(team_table$is_captain), 'C'] <- 'C'
    team_table[which(team_table$is_vice_captain), 'C'] <- 'V'
    team_table$name <- paste(team_table$first_name, team_table$second_name)
    team_table <- team_table[, c(1, 3:7, 2, 13, 8:12)]
    
    as.datatable(
      formattable(team_table, 
                       list(C = formatter('span', style = x ~ style(color = ifelse(x == 'C', "green", "orange")),
                                             x ~ icontext(ifelse(x == 'C', 'user', 'user'))),
                       P = formatter('span', style = x ~ style(color = ifelse(x <= 11, "green", "red")),
                                             x ~ icontext(ifelse(x <= 11, "ok", "remove")))
                    )
                  ), 
      options = list(columnDefs = list(list(visible = FALSE, targets = c(0:5))),
                     dom = 't',
                     paging = FALSE,
                     ordering = FALSE,
                     info = FALSE), 
      selection = 'none',
      rownames = FALSE)
  })
  
  output$playerList <- DT::renderDataTable({
    req(fpl$availablePlayers)
    df <- fpl$availablePlayers %>% 
      mutate(player = paste(first_name, second_name)) %>%
      filter(element_type %in% input$selPosition) %>%
      rename(available = count) %>%
      rename(cost = now_cost) %>%
      rename(total = total_points) %>%
      rename(pos = element_type) %>% 
      select(available, player, pos, team, cost, total)
    
    df$avail <- apply(df, 1, function(x) ifelse(x['available'] > 0,
                                             as.character(tags$span(icon('remove', lib = 'glyphicon'), style = 'color:red')),
                                             as.character(tags$span(icon('ok', lib = 'glyphicon'), style = 'color:green'))))
    
    if(input$chkAvailable) df <- df[df$available == 0, ]
    
    df <- df[, c(7, 2:6)]
    
    DT::datatable(df,
                        extensions = 'Scroller',
                        options = list(paging = TRUE,
                                       deferRender = TRUE,
                                       scrollY = 400,
                                       scroller = TRUE
                                       ),
                        selection = 'none',
                        rownames = FALSE,
                  escape = FALSE)
  })
  
  output$uiTransferTeamName <- renderUI({
    req(fpl$league)
    selectInput('selTransferTeam', 'Team', fpl$league$standings$results$entry_name)
  })
  
  transferTeam <- reactive({
    req(input$selTransferTeam)
    teamID <- fpl$league$standings$results[fpl$league$standings$results$entry_name == input$selTransferTeam, 'entry']
    fpl$teamsTable[[paste0('T_', teamID)]]
  })
  
  output$uiTransferPlayerOut <- renderUI({
    req(input$selTransferTeam)
    teamPlayers <- setNames(transferTeam()[['element']], transferTeam()[['second_name']])
    selectInput('selTransferOut', 'Player Out', teamPlayers)
  })
  
  output$uiTransferPlayerIn <- renderUI({
     req(input$selTransferTeam)
     playerOutPos <- transferTeam()[transferTeam()['element'] == input$selTransferOut, 'pos']
     df.filter <- fpl$availablePlayers[fpl$availablePlayers$element_type == playerOutPos, ]
     playersIn <- setNames(df.filter[['element']], paste0(df.filter[['second_name']], ' (', df.filter[['team']], ')'))
     selectInput('selTransferIn', 'Player In', playersIn)
  })
  
  output$tableTransfers <- DT::renderDataTable({
    
    df <- fpl$transfers %>%
      left_join(fpl$all$elements, by = c('OutRef' = 'id')) %>% 
      mutate(Out = paste0(second_name, ' (', .teamID(fpl$all, team), ')')) %>% 
      left_join(fpl$all$elements, by = c('InRef' = 'id')) %>% 
      mutate(In = paste0(second_name.y, ' (', .teamID(fpl$all, team.y), ')')) %>% 
      select(Date, Team, Out, In) %>%
      arrange(desc(Date)) %>%
      mutate(Date = as.Date(Date))
    
    DT::datatable(df,
                  extensions = 'Scroller',
                  options = list(dom = 't',
                                 paging = TRUE,
                                 deferRender = TRUE,
                                 scrollY = 200,
                                 scroller = TRUE
                  ),
                  selection = 'none',
                  rownames = FALSE,
                  escape = FALSE) 
  })
  
  observeEvent(input$butTransfer, {
    pIn <- fpl$availablePlayers[fpl$availablePlayers$element == input$selTransferIn, ]
    pOut <- fpl$availablePlayers[fpl$availablePlayers$element == input$selTransferOut, ]
    fpl$transfers <- rbind(fpl$transfers,
                           data.frame(Date = as.character(Sys.time()), 
                                      Team = input$selTransferTeam,
                                      OutRef = as.numeric(input$selTransferOut),
                                      Out = paste0(pOut$second_name, ' (', pOut$team, ')'),
                                      InRef = as.numeric(input$selTransferIn),
                                      In = paste0(pIn$second_name, ' (', pIn$team, ')')))
    dbWriteTable(mydb, value = fpl$transfers, name = 'transferlist', row.names = FALSE, overwrite = TRUE)
  })
  
}

ui <- fluidPage(
  uiOutput('header'),
  tabsetPanel(
    tabPanel('Table',
             column(7, DT::dataTableOutput('leagueTable')),
             column(5, div(DT::dataTableOutput('teamTable'), style = "font-size:80%"))
    ),
    tabPanel('Available Players',
             br(),
             fluidRow(
               column(7,
                      fluidRow(
                        column(6, selectizeInput('selPosition', 'Filter by Position', choices = c('GLK', 'DEF', 'MID', 'FWD'), selected = c('GLK', 'DEF', 'MID', 'FWD'), multiple = TRUE)),
                        column(6, checkboxInput('chkAvailable', 'Only Available Players', value = TRUE))
                      ),
                      DT::dataTableOutput('playerList')
               ),
                 column(5,
                        wellPanel(
                          h3('Transfers'),
                          fluidRow(
                            div(DT::dataTableOutput('tableTransfers'), style = "font-size:80%")
                          ),
                          br(),
                          hr(),
                          br(),
                          fluidRow(
                            column(7,
                                   uiOutput('uiTransferTeamName')
                            ),
                            column(4, offset = 1,
                                   actionButton('butTransfer', 'Transfer', icon('upload'), class = 'btn-success')
                            )
                          ),
                        fluidRow(
                          column(6, 
                                 uiOutput('uiTransferPlayerOut')
                          ),
                          column(6, 
                                 uiOutput('uiTransferPlayerIn')
                          )
                        )
                 )
               )
               )

             )
  )
)

shinyApp(server = server, ui = ui)
