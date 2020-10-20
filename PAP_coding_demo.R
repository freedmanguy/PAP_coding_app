filename <- "demo.RDS"       # change according to the relevant filename
user <- "[user]"             # change to the name of the coder
shinyurl <- "papcodingdemo"  # change to the name of the app (affects url)

if(!("xfun" %in% installed.packages())){
  install.packages("xfun")
}
xfun::pkg_attach2(c("shiny","dplyr","rdrop2","DT","rvest"))

mydatetime <- function(){
  temp <- read_html("https://www.timeanddate.com/worldclock/israel/jerusalem")
  temp2 <- html_nodes(temp, "#ct")
  temp3 <- html_nodes(temp, "#ctdat")
  temp4 <- paste(as.character(as.Date(html_text(temp3),"%A, %B %d, %Y")),html_text(temp2))
  if(grepl("am$",temp4,ignore.case = T)){
    temp4 <- gsub(" *am$","",temp4, ignore.case = T)
    temp4 <- gsub(" 12:"," 00:",temp4, ignore.case = T)
    temp4 <- gsub(" 24:"," 00:",temp4, ignore.case = T)
  }
  if(grepl("pm$",temp4,ignore.case = T)){
    temp4 <- gsub(" *pm$","",temp4, ignore.case = T)
    temp4 <- gsub(" 0?1:"," 13:",temp4, ignore.case = T)
    temp4 <- gsub(" 0?2:"," 14:",temp4, ignore.case = T)
    temp4 <- gsub(" 0?3:"," 15:",temp4, ignore.case = T)
    temp4 <- gsub(" 0?4:"," 16:",temp4, ignore.case = T)
    temp4 <- gsub(" 0?5:"," 17:",temp4, ignore.case = T)
    temp4 <- gsub(" 0?6:"," 18:",temp4, ignore.case = T)
    temp4 <- gsub(" 0?7:"," 19:",temp4, ignore.case = T)
    temp4 <- gsub(" 0?8:"," 20:",temp4, ignore.case = T)
    temp4 <- gsub(" 0?9:"," 21:",temp4, ignore.case = T)
    temp4 <- gsub(" 10:"," 22:",temp4, ignore.case = T)
    temp4 <- gsub(" 11:"," 23:",temp4, ignore.case = T)
    temp4 <- gsub(" 24:"," 00:",temp4, ignore.case = T)
  }
  
  return(temp4)
}


# Define UI
ui <- navbarPage("PAP Coding App", collapsible = T, inverse = T,
                 tabPanel("To Code",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                br(),
                                uiOutput("myrows"),
                                br(),
                                textInput("minorc","Minor Code"),
                                br(),
                                actionButton("submit", "Submit"),
                                br(),
                                uiOutput("completed"),
                                br(),
                                actionButton("nextobs", "Next Observation",
                                             onclick = paste0("location.href='https://freedmanguy.shinyapps.io/",shinyurl,"/';"))
                              ),
                              mainPanel(
                                br(),
                                uiOutput("alldone"),
                                br(),
                                uiOutput("title"),
                                uiOutput("description"),
                                br(),
                                uiOutput("id"),
                                uiOutput("Congress"),
                                uiOutput("year"),
                                uiOutput("Chamber"),
                                uiOutput("Committee1"),
                                uiOutput("Subcommittee1"),
                                uiOutput("Committee2"),
                                uiOutput("Subcommittee2"),
                                br(),
                              )
                            ),
                            fluidRow(
                              column(12,
                                     h3("PAP Codebook:"),
                                     DTOutput("codebook"))
                            )
                          )),
                 tabPanel("Coded",
                          fluidPage(
                            mainPanel(
                              actionButton("displaytable", "Refresh Table"),
                              br(),
                              DTOutput("mycoded")
                            )
                          )),
                 tabPanel("Search & Recode",
                          fluidPage(
                            sidebarLayout(
                              sidebarPanel(
                                textInput("id2","id"),
                                actionButton("search2", "Search"),
                                br(),
                                br(),
                                h4("If you wish to recode:"),
                                textInput("minor2","Minor Code"),
                                actionButton("submit2", "Submit"),
                                br(),
                                uiOutput("completed2"),
                                br(),
                                # actionButton("next2", "Refresh", 
                                #              onclick = "location.href='https://capcoding.shinyapps.io/demo/';")
                              ),
                              mainPanel(
                                br(),
                                uiOutput("title2"),
                                uiOutput("description2"),
                                uiOutput("minor2"),
                                br(),
                                uiOutput("id2"),
                                #br(),
                                uiOutput("Chamber2"),
                                #br(),
                                uiOutput("Congress2"),
                                #br(),
                                uiOutput("year2"),
                                #br(),
                                uiOutput("Committee12"),
                                #br(),
                                uiOutput("Subcommittee12"),
                                uiOutput("Committee22"),
                                uiOutput("Subcommittee22"),
                                br(),
                                br(),
                              )
                            )))
)
# Define server
server <- function(input, output, session) {
  
  output$codebook <- renderDT({
    codebook <- readRDS("PapCodebook.RDS")  
    codebook$`Major Code` <- as.character(codebook$`Major Code`)
    codebook$`Minor Code` <- as.character(codebook$`Minor Code`)
    datatable(codebook, options = list(dom = 't', pageLength = nrow(codebook)), rownames = T, filter = "top")
  })
  
  drop_auth(rdstoken = "token.rds")
  drop_download(paste0("PAP/",filename), overwrite = T)
  mydata <- readRDS(filename)
  mydata2code <- filter(mydata, is.na(minor))
  i <- sample(mydata2code$id, 1)
  tocode <- filter(mydata, id==i)
  
  if(nrow(tocode)>0){
    output$id <- renderUI({HTML(paste0('<p><strong>Hearing ID: </strong>',tocode$id,"</p>"))})
    output$Chamber <- renderUI({HTML(paste0('<p><strong>Chamber: </strong>',tocode$Chamber,"</p>"))})
    output$Congress <- renderUI({HTML(paste0('<p><strong>Congress: </strong>',tocode$Congress,"</p>"))})
    output$year <- renderUI({HTML(paste0('<p><strong>Year: </strong>',tocode$year,"</p>"))})
    output$Committee1 <- renderUI({HTML(paste0('<p><strong>Committee1: </strong>',tocode$CName1,"</p>"))})
    output$Subcommittee1 <- renderUI({HTML(paste0('<p><strong>Subcommittee1: </strong>',tocode$SName1,"</p>"))})
    output$Committee2 <- renderUI({HTML(paste0('<p><strong>Committee2: </strong>',tocode$CName2,"</p>"))})
    output$Subcommittee2 <- renderUI({HTML(paste0('<p><strong>Subcommittee2: </strong>',tocode$SName2,"</p>"))})
    output$title <- renderUI({HTML(paste0('<p><strong>Hearing Title: </strong>',tocode$title,"</p>"))})
    output$description <- renderUI({HTML(paste0('<p><strong>Hearing Description: </strong>',tocode$description,"</p>"))})
    output$myrows <- renderUI({HTML(paste0('<h3> Hi ',user,", you have ",nrow(mydata2code)," observations remaining.","</h3>"))})
  } else {
    output$alldone <- renderUI({
      HTML(paste0('<p style="color:blue">',"Looks like you're all done. Good job! </p>"))
    })
  }
goodcodes <- goodcodes <- c(100, 101, 103, 104, 105, 107, 108, 110, 199, 200, 201, 202, 204, 205, 206, 207, 208, 209, 299,
                              300, 301, 302, 321, 322, 323, 324, 325, 331, 332, 333, 334, 335, 336, 341, 342, 398, 399, 400,
                              401, 402, 403, 404, 405, 408, 498, 499, 500, 501, 502, 503, 504, 505, 506, 529, 599, 600, 601,
                              602, 603, 604, 606, 607, 609, 698, 699, 700, 701, 703, 704, 705, 707, 708, 709, 710, 711, 798,
                              799, 800, 801, 802, 803, 805, 806, 807, 898, 899, 900, 1000, 1001, 1002, 1003, 1005, 1006, 1007, 1010,
                              1098, 1099, 1200, 1201, 1202, 1203, 1204, 1205, 1206, 1207, 1208, 1209, 1210, 1211, 1299, 1300, 1301, 1302, 1303,
                              1304, 1305, 1308, 1399, 1400, 1401, 1403, 1404, 1405, 1406, 1407, 1408, 1409, 1410, 1499, 1500, 1501, 1502, 1504,
                              1505, 1507, 1520, 1521, 1522, 1523, 1524, 1525, 1526, 1599, 1600, 1602, 1603, 1604, 1605, 1606, 1608, 1609, 1610,
                              1611, 1612, 1614, 1615, 1616, 1617, 1619, 1620, 1698, 1699, 1700, 1701, 1704, 1705, 1706, 1707, 1708, 1709, 1798,
                              1799, 1800, 1802, 1803, 1804, 1806, 1807, 1808, 1899, 1900, 1901, 1902, 1905, 1906, 1910, 1915, 1921, 1925, 1926,
                              1927, 1929, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015,
                              2030, 2099, 2100, 2101, 2102, 2103, 2104, 2105, 2199)
  codepap <- observeEvent(input$submit, {
    
      if(input$minorc %in% goodcodes){
        i <- tocode$id
        codegiven <- input$minorc
        hearingsampled <- tocode$id
        mydata$DateUpdated[mydata$id==i] <- as.character(mydatetime())
        mydata$minor[mydata$id==i] <- input$minorc
        mydata$title <- as.character(mydata$title)
        mydata$description <- as.character(mydata$description)
        saveRDS(mydata, filename)
        drop_upload(file = filename, path = "PAP", mode = "overwrite")
        output$completed <- renderUI({HTML(paste0("<p>Hearing ",hearingsampled," coded successfully into minor topic ",codegiven,".</p><br><p> Click 'Next Observation' to code the next hearing</p>"))})
        
      } else {
        output$completed <- renderUI({
          HTML(paste0('<p style="color:red">',"Error: The code entered is not valid. </p><p>Please review. Remember: </p><p><strong>Minor topics</strong> can only receive one of the following: ",paste(goodcodes, collapse = ", "),".</p>"))
        })
      }
    
    
  })
  

  showcoded <- observeEvent(input$displaytable,{
    drop_download(paste0("PAP/",filename), overwrite = T)
    mydf <- readRDS(filename) %>% 
      as.data.frame() %>% 
      mutate(id = as.character(id))
    output$mycoded <- renderDT({
      mydf <- select(mydf, id, Chamber, Congress, year, minor, title, description, DateUpdated, Committee1, Subcommittee1,Committee2, Subcommittee2) %>% 
        filter(!is.na(minor)) %>% 
        arrange(desc(DateUpdated))
      
      datatable(mydf, options = list(dom = 't', pageLength = nrow(mydf)), rownames = F, filter = "top")
    }) #  “default”, “bootstrap”, “bootstrap4”, “foundation”, “jqueryui”, “material”, “semanticui”, “uikit”
  })
  
  searchpap <- observeEvent(input$search2, {
    drop_download(paste0("PAP/",filename), overwrite = T)
    mysearch <- readRDS(filename)
    mysearch <- filter(mysearch, id==input$id2)
    output$id2 <- renderUI({HTML(paste0('<p><strong>Hearing ID: </strong>',mysearch$id,"</p>"))})
    output$Chamber2 <- renderUI({HTML(paste0('<p><strong>Chamber: </strong>',mysearch$Chamber,"</p>"))})
    output$Congress2 <- renderUI({HTML(paste0('<p><strong>Congress: </strong>',mysearch$Congress,"</p>"))})
    output$year2 <- renderUI({HTML(paste0('<p><strong>Year: </strong>',mysearch$year,"</p>"))})
    output$Committee12 <- renderUI({HTML(paste0('<p><strong>Committee1: </strong>',mysearch$CName1,"</p>"))})
    output$Subcommittee12 <- renderUI({HTML(paste0('<p><strong>Subcommittee1: </strong>',mysearch$SName1,"</p>"))})
    output$Committee22 <- renderUI({HTML(paste0('<p><strong>Committee2: </strong>',mysearch$CName2,"</p>"))})
    output$Subcommittee22 <- renderUI({HTML(paste0('<p><strong>Subcommittee2: </strong>',mysearch$SName2,"</p>"))})
    output$title2 <- renderUI({HTML(paste0('<p><strong>Hearing Title: </strong>',mysearch$title,"</p>"))})
    output$description2 <- renderUI({HTML(paste0('<p><strong>Hearing Description: </strong>',mysearch$description,"</p>"))})
    output$minor2 <- renderUI({HTML(paste0('<p><strong>Minor Code: </strong>',mysearch$minor,"</p>"))})
  })
  
  recodepap <- observeEvent(input$submit2, {
    if(input$minor2 %in% goodcodes){
      codegiven2 <- input$minor2
      mydata$DateUpdated[mydata$id==input$id2] <- as.character(mydatetime())
      mydata$minor[mydata$id==input$id2] <- input$minor2
      saveRDS(mydata, filename)
      drop_upload(file = filename, path = "PAP", mode = "overwrite")
      
      output$completed2 <- renderUI({HTML(paste0("<p>Hearing ",input$id2," coded successfully into minor topic ",codegiven2,".</p>"))})
      
      updateTextInput(session, "minor2", value = "")
      
    } else {
      output$completed2 <- renderUI({
        HTML(paste0('<p style="color:red">',"Error: The code entered is not valid. </p><p>Please review. Remember: </p><p><strong>Minor topics</strong> can only receive one of the following: ",paste(goodcodes, collapse = ", "),".</p>"))
      })
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
