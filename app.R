library(shiny)

jscode <- '
        $(function() {
                var $els = $("[data-proxy-click]");
                $.each(
                        $els,
                        function(idx, el) {
                                var $el = $(el);
                                var $proxy = $("#" + $el.data("proxyClick"));
                                $el.keydown(function (e) {
                                        if (e.keyCode == 13) {
                                        $proxy.click();
                                        }
                                        });
                        }
                );
        });
'


ui =(pageWithSidebar(
        headerPanel("What's Next?"),
        sidebarPanel(
                tags$head(tags$script(HTML(jscode))),
                tagAppendAttributes(
                        textAreaInput("textIn", NULL, ""),
                        `data-proxy-click` = "btn"
                ),
                actionButton("btn", "Gimme your best guess!"),
                div(HTML("Best Guess: ")),
                textOutput(outputId = "best_guess"),
                div(HTML("Top Guesses: ")),
                textOutput(outputId = "guesses"),
                #                 div(HTML("Last Leader: ")),
                #                 textOutput(outputId = "last_leader"),
                #                 div(HTML("Search State: ")),
                #                 textOutput(outputId = "search_state"),
                plotOutput(outputId = "top10", width = "100%", height = "300px")
        ),
        mainPanel(
                h1("Start Typing!"),
                p("After you type a few words, guesses for your next word will appear below the text box and button."),
                p("Press Enter or click the button to add the Best Guess to what you're typing."),
                p("Below the Best Guess, you'll see the top 10 guesses for your next word, presented in a cumulative distribution."),
                p("On a held-out test sample of ~850 trigrams, the model included the right next word in the top 10 guesses 37% of the time."),
                p("After typing 2 and 3 letters of the next word, it delivered an accurate best guess 47% and 53% of the time, respectively."),
                p("The model is predicting based on the last 3 words, using a back-off model with absolute discounting."), 
                p("It includes some part-of-speech analysis as well, to remove from the answer set guesses of inaappropriate parts-of-speech.  But that inclusion actually reduces predictive power.  I'm still working on that..."),
                hr(),
                p(a(href = "https://github.com/daddyprasad5/capstoneshiny", "Here's the code for this shiny app.")),
                p(a(href = "https://github.com/daddyprasad5/capstone_final", "Here's the code for the model construction.")),  
                p(a(href = "http://daddyprasad5.github.io/capstone_final.html", "Here's more info on the project.")) 
        )))

server = function(input, output, session){

        source('ngramBootstrap.R')
        testing = FALSE #TRUE enables some additional display to help with debugging
        
        ll <- NULL ##last leader
        lw <- NULL ##last word
        rs <- NULL ##remainder of sentence, less the last word
        bg <- NULL ##best guess for next word
        g <- data.frame() ##guesses dataframe
        
        distro <- function (line, leaderIn) {
                if (!is.null(leaderIn)) {
                        last_distro <- getDistro(line, leaderIn, triGram, biGram, uniGram, POSDictionary, triplets)
                        last_distro
                }
        }
        
        observeEvent(input$btn, {
                updateTextAreaInput(session, "textIn", value = paste0(rs, bg))
        })
        
        output$guesses <- renderPrint({
                leader <- getLastNGram(input$textIn, 3)
                lw <<- getLastNGram(input$textIn, 1)
                
                if (is.null(leader)) {guessesOut <- ""}
                
                else {
                        leader_leader <- getLeader(getLastNGram(input$textIn, 4))
                        last_ch <- substr(input$textIn, nchar(input$textIn), nchar(input$textIn))
                        state <- "search within"
                        #determine route - either "search within" existing suggested words
                        # or "get new distro".  We want to search within the current list of guesses
                        # unless....
                        # (a) the leader is null (haven't typed anything yet), 
                        # (b) we've finished a new word or 
                        # (c) there are no words left in the current list that start as typed
                        if (is.null(ll)) {state <- "get new distro"}
                        else {  if (isEndOfWord(last_ch)) {
                                state <- "get new distro"
                        }
                        }
                        if ( state == "search within") { 
                                if (testing) {output$search_state <- renderPrint({"search within"})}
                                #gu <- g
                                if (nrow(g) > 0) {
                                        guessesOut <- getStartsWithLastWords(g, lw, 10)
                                        guessesOut <- guessesOut[guessesOut != lw]
                                }
                                if (length(guessesOut) == 0) {state = "get new distro"}
                                else {
                                        output$best_guess <- renderPrint({guessesOut[1]})
                                        bg <<- guessesOut[1]
                                        rs <<- substr(input$textIn, 1, nchar(input$textIn) - nchar(lw))
                                        output$top10 <- renderPlot({getTopCumFreq(g[g$lastWord %in% guessesOut,],10)})
                                }
                        }
                        if (state == "get new distro") { 
                                if (testing) {output$search_state <- renderPrint({"get new distro"})}
                                g <<- distro(input$textIn, leader)
                                guessesOut <- as.character(head(g$lastWord,10))
                                if (testing) {output$last_leader <- renderPrint ({leader})}
                                ll <<- leader
                                output$best_guess <- renderPrint({guessesOut[1]})
                                bg <<- guessesOut[1]
                                rs <<- paste0(input$textIn," ")
                                output$top10 <- renderPlot({getTopCumFreq(g,10)})
                        }
                }
                
                return(guessesOut)
        })



}
shinyApp(ui = ui, server = server)