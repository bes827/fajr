server <- function(input, output, session) {
    
    
    #functions: 
    get_time <- function(time = now()) {
        time %>%
            str_split(" ") %>%
            map_chr(2) %>%
            lubridate::hms()
    }
    
    #upload the zipcode db:
    zipdb = readRDS(file = "www/zipl.rds")
    
    #Define the variables:
    
    nzip <- eventReactive(input$act, {
        req(input$zipp)
        input$zipp
    })
    
    
    timezo <- eventReactive(input$act, {
        req(input$zipp)
        
        zipdb %>% 
            filter(zipdb$zipcode %in% nzip()) %>% 
            select("timezone") %>% 
            as.character("timezone")
    })  
    
    
    output$timezo <- renderText({
        
        paste("reactive timezone", timezo())
    })
    
    
    
    county <- eventReactive(input$act, {
        req(input$zipp)
        
        zipdb %>% 
            filter(zipdb$zipcode %in% nzip()) %>% 
            select("county") %>% 
            as.character("county")
    })  
    
    
    state <- eventReactive(input$act, {
        req(input$zipp)
        
        zipdb %>% 
            filter(zipdb$zipcode %in% nzip()) %>% 
            select("state") %>% 
            as.character("state")
    })  
    
    #Extract 
    #1)Fagr time 
    fgr2 <- eventReactive(input$act, {
        req(input$zipp)
        r <- GET("http://www.islamicfinder.us/index.php/api/prayer_times", 
                 query = list(zipcode = nzip() , country = "US", method=input$calc, time_format ="0"))
        xx = content(r)
        xx = xx$results
        xx = xx$Fajr
        xx = paste(xx, ":00", sep="")  
        lubridate::hms(xx)
    })
    
    
    
    #2)Local time (updates every second):
    ltime <- reactive({
        req(input$zipp)
        invalidateLater(1000, session)
        
        get_time(lubridate::with_tz(now(), tzone = paste("US/",timezo(), sep="")))
    }) 
    
    
    #Times till::
    #time til Fagr: 
    tillFagr <- reactive({
        req(input$zipp)
        invalidateLater(1000, session)
        
        x=as.numeric(fgr2()-ltime())
        x1=ifelse(x >=0, x,86400+x)
        x2=seconds_to_period(x1)
        x2
        
    }) 
    
    
    
    
    tillduaa <- reactive({
        req(input$zipp)
        invalidateLater(1000, session)
        
        x=as.numeric(fgr2()-ltime())
        x1=ifelse(x >=0, x,86400+x)
        x2=x1-(5*60)
        x3=seconds_to_period(x2)
        x3
    }) 
    
    
    tillquran <- reactive({
        req(input$zipp)
        invalidateLater(1000, session)
        
        x=as.numeric(fgr2()-ltime())
        x1=ifelse(x >=0, x,86400+x)
        x2=x1-(1054)
        x3=seconds_to_period(x2)
        x3
    })
    
    
    
    observe({
        req(input$act)
        
        
        htmop = glue('<p>&nbsp;</p>
<p><strong>Zip code:</strong>: {nzip()}&nbsp;</p>
<p><strong>State: </strong>{state()}&nbsp;</p>
<p><strong>County: </strong>{county()}</p>
<p><strong>Time zone: </strong>{timezo()}&nbsp;</p>
<p><strong>Fagr:</strong> {fgr2()}</p>
<hr />
<p><strong>Local time:</strong> {ltime()}</p>
<p><strong>Time till Quran(~18 mins before azan)</strong>: {tillquran()}</p>
<p><strong>Time till Fagr (adan)</strong>: {tillFagr()}</p>
<hr />
<p>&nbsp;</p>')
        
        
        output$inc<-renderUI({
            HTML (htmop)
        })
        
    })
    
    
    observe({
        req(input$act)
        
        
        htmop = glue('<h1><strong>Quran will start in</strong>&nbsp;<span style="background-color: #ffff00;">{tillquran()}</span></h1>
<p>(~18 minutes before adhan)</p>
<hr />
<p>&nbsp;</p>')
        
        
        output$inc1<-renderUI({
            HTML (htmop)
        })
        
    })
    
    #collect(cache) fixed time points (after pressing submit):
    #
    #
    
    ltime.fix <- reactive({
        req(input$act)
        
        
        get_time(lubridate::with_tz(now(), tzone = paste("US/",timezo(), sep="")))
    }) 
    
    
    
    adan.fix <- reactive({
        req(input$act)
        x=as.numeric(fgr2()-ltime.fix())
        x1=ifelse(x >=0, x,86400+x)
        x2=seconds_to_period(x1)
        x2
        
    })
    
    
    tillFagr.fix <- reactive({
        req(input$act)
        x=as.numeric(fgr2()-ltime.fix())
        x1=ifelse(x >=0, x,86400+x)
        x2=seconds_to_period(x1)
        x2
        
    }) 
    
    
    
    
    tillduaa.fix <- reactive({
        req(input$act)
        x=as.numeric(fgr2()-ltime.fix())
        x1=ifelse(x >=0, x,86400+x)
        x2=x1-(5*60)
        x3=seconds_to_period(x2)
        x3
    }) 
    
    
    tillquran.fix <- reactive({
        req(input$act)
        x=as.numeric(fgr2()-ltime.fix())
        x1=ifelse(x >=0, x,86400+x)
        x2=x1-(1054)
        x3=seconds_to_period(x2)
        x3
    })
    
    
    
    
    
    
    ########Play audios
    
    
    #Play quran anytime the app works(if not within the ~ 18 mins before fagr):
    # observeEvent(input$act, {
    #   output$my_audio1 <-renderUI({
    #     tags$audio(src = "https://archive.org/download/jam3_56/003.mp3", 
    #                type = "audio/mp3", autoplay = NA, controls = NA, style="display:none;")
    #   })
    #   print(input$calc)
    # })
    # 
    
    
    
    #Play before Fagr Quran: 
    #
    
    observeEvent(input$act, {
        
        delayq = period_to_seconds(tillquran.fix())*1000
        
        delayq=ifelse(delayq >=0, delayq, 0)
        
        
        
        delay (delayq, 
               
               output$my_audio1 <-renderUI({
                   tags$audio(src = "https://github.com/bes828/files/blob/main/quran-fagr%20(1).mp3?raw=true", 
                              type = "audio/mp3", autoplay = NA, controls = NA, style="display:none;")
               })
               
               
        )  
        
        
        delay (delayq, 
               
               output$inc1<-renderUI({
                   HTML ("")
               })
               
               
        )    
        
    })
    
    
    #Play ibthal then azan (in one file): 
    observeEvent(input$act, {
        
        delay ((period_to_seconds(tillFagr.fix()) - 258)  *1000, 
               
               output$my_audio1 <-renderUI({
                   tags$audio(src = "https://github.com/bes828/files/blob/main/nasr_azan4_18.mp3?raw=true", 
                              type = "audio/mp3", autoplay = NA, controls = NA, style="display:none;")
               })
        )  
    })
    
    
    
    
}