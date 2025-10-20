library(shiny)
library(shinyWidgets)
library(dplyr)
library(pROC)
library(ggplot2)
library(e1071)
library(plotly)
library(bslib)
library(googlesheets4)
library(caret)
library(reshape2)
library(FSelectorRcpp)
library(randomForest)

set.seed(123)

options(gargle_oauth_cache = ".secrets")
gs4_auth(cache = ".secrets", email = "yourmail")
ss <- gs4_get("put_your_link")
renv::init()

renv::snapshot()

rsconnect::setAccountInfo(name='',
                          token='',
                          secret='')
rsconnect::deployApp(appDir = getwd())



bad_words <- c()

check_nickname <- function(nick){
  nick <- tolower(trimws(nick))
  if(any(sapply(bad_words, function(w) grepl(w, nick, perl=TRUE)))) return(FALSE)
  return(TRUE)
}


ui <- fluidPage(
  theme = bs_theme(
    bootswatch = "minty",
    base_font = font_google("Comic Neue"),
    font_scale = 1.5  
  ),
  
  tags$div(id="confetti-container", style="position:fixed; top:0; left:0; width:100%; height:100%; pointer-events:none; z-index:9999;"),
  tags$script(src="https://cdn.jsdelivr.net/npm/canvas-confetti@1.6.0/dist/confetti.browser.min.js"),
  tags$script(HTML("\n  Shiny.addCustomMessageHandler('confetti', function(message) {\n    confetti({\n      particleCount: 200,\n      spread: 70,\n      origin: { y: 0.6 }\n    });\n  });\n")),
  
  tags$style(HTML("\n  body, h1, h2, h3, h4, h5, h6, p, label, .well, .plot-container, table {\n    font-family: 'Comic Neue', cursive !important;\n    font-size: 22px !important;\n  }\n  \n  body {\n    background: linear-gradient(135deg, #FFDDEE, #CCE5FF, #FFF8CC, #E6CCFF, #FFE6CC);\n    background-size: 1000% 1000%;\n    animation: gradientBG 20s ease infinite;\n  }\n\n  @keyframes gradientBG {\n    0% {background-position:0% 50%;}\n    50% {background-position:100% 50%;}\n    100% {background-position:0% 50%;}\n  }\n\n  .well {\n    background-color: white !important;\n    border-radius: 15px;\n    box-shadow: 0 4px 20px rgba(0,0,0,0.2);\n    padding: 20px;\n  }\n\n  h1 {\n    color: #8B0000;\n    text-align: center;\n    font-size: 45px;\n  }\n\n  .plot-container {\n    background-color: white;\n    border-radius: 15px;\n    box-shadow: 0 4px 15px rgba(0,0,0,0.15);\n    padding: 10px;\n    margin-bottom: 15px;\n  }\n")),
  
  tags$div(
    style = "text-align:center; line-height:1.2;",
    tags$img(
      src = "nutella.png", 
      height = "100px", 
      style = "margin-right:0px; transform: rotate(-5deg); display:inline-block;"
    ),
    tags$span(
      "o non",
      style = "font-family: 'Comic Neue', cursive; font-size: 40px; font-weight:bold; color:black; vertical-align: middle; margin:0 1px;"
    ),
    tags$img(
      src = "nutella.png", 
      height = "100px", 
      style = "margin-left:5px; transform: rotate(4deg); display:inline-block;"
    )
  ),
  
  br(),
  
  tabsetPanel(
    tabPanel("🎮 Questionario Bambini",
             fluidRow(
               column(4,
                      h4("📊 Età"),
                      div(class="plot-container", plotOutput("grafico_genere")),
                      h4("🍀 Adulti vs. bambini!"),
                      div(class="plot-container", plotOutput("grafico_scelta"))
               ),
               column(4,
                      wellPanel(
                        textInput("nick","🌟 Scegli un nickname:"),
                        numericInput("eta","🎂 Quanti anni hai?", value=8, min=2, max=100),
                        prettyRadioButtons("genere","👦👧 Qual è il tuo genere?",
                                           choices=c("Maschio","Femmina"),
                                           animation="pulse", status="info", inline=TRUE),
                        sliderInput("nutella_freq","🍯 Quante volte mangi Nutella a settimana?",
                                    min=0, max=14, value=3),
                        prettyRadioButtons("pref","🍫 Ti piace il cioccolato?",
                                           choices=c("Si","No"),
                                           animation="pulse", status="info", inline=TRUE),
                        prettyRadioButtons("scelta","🤔 Quale assaggio è Nutella?",
                                           choices=c("A","B"),
                                           animation="jelly", status="success", inline=TRUE),
                        prettyRadioButtons("fortuna","🍀 Quanto ti senti fortunat* oggi?",
                                           choices=c("Poco","Molto"),
                                           animation="jelly", status="warning", inline=TRUE),
                        actionBttn("submit","🚀 Invia",
                                   style="gradient", color="primary", size="lg"),
                        br(),
                        uiOutput("feedback_ui"),
                        br(),
                        h4("🤖 Predizioni AI in tempo reale"),
                        uiOutput("predizione_realtime_ui")
                      )
               ),
               column(4,
                      h4("🍯 Frequenza Nutella"),
                      div(class="plot-container", plotOutput("grafico_freq")),
                      h4("🍫 Preferenze Cioccolato"),
                      div(class="plot-container", plotOutput("grafico_pref"))
               )
             )
    ),
    tabPanel("📊 Dashboard",
             fluidRow(
               column(6,
                      h4("📈 Combined ROC Curves"),
                      div(style="margin-bottom:6px;", tags$small("La curva ROC mostra il compromesso tra specificità e sensitività. Più l'AUC è vicina a 1, migliore è la performance del modello.")),
                      div(class="plot-container", plotOutput("roc_combined", height="400px")),
                      h4("💡 Importanza delle tue risposte per modello AI"),
                      div(class="plot-container", plotOutput("var_importance", height="400px"))
               ),
               column(6,
                      h3("📋 Ultime Risposte"),
                      div(class="well", tableOutput("tabella")),
                      br(),
                      uiOutput("model_stats_ui"),
                      br(),
                      h4("🔮 Predizione"),
                      div(class="well", uiOutput("predizione_dashboard"))
               )
             )
    )
    
  )
)

server <- function(input, output, session){
  dati_iniziali <- tryCatch({
    read_sheet(ss)
  }, error = function(e) {
    message("⚠️ Could not read Google Sheet, starting empty: ", e$message)
    data.frame(
      nickname = character(),
      eta = numeric(),
      genere = character(),
      nutella_freq = numeric(),
      fortuna = character(),
      preferenze = character(),
      scelta = character(),
      corretto = logical(),
      stringsAsFactors = FALSE
    )
  })
  risposte <- reactiveVal(dati_iniziali)
  optimal_thresholds <- reactiveVal(list(logistic = 0.5, nb = 0.5, rf = 0.5))
  models_data <- reactive({
    df <- risposte()
    req(nrow(df) >= 50)  
    
    train_idx <- sample(seq_len(nrow(df)), size = 0.7 * nrow(df))
    train_df <- df[train_idx, ]
    test_df  <- df[-train_idx, ]
    
    train_df$corretto_f <- factor(ifelse(train_df$corretto, "Si", "No"))
    preds <- c("eta", "genere", "nutella_freq", "fortuna")
    up_train <- caret::upSample(
      x = train_df[, preds, drop = FALSE],
      y = train_df$corretto_f,
      yname = "corretto_f"
    )
    
    up_train$genere <- as.character(up_train$genere)
    up_train$fortuna <- as.character(up_train$fortuna)
    
    
    fit_log <- glm(as.factor(corretto_f ) ~ eta + genere + nutella_freq + fortuna, 
                   data=up_train, family=binomial)
    prob_log <- predict(fit_log, newdata=test_df, type="response")
    roc_log <- pROC::roc(test_df$corretto, prob_log, quiet=TRUE)
    coords_log <- coords(roc_log, "best", ret="threshold", best.method="youden")
    
    
    fit_nb <- naiveBayes(as.factor(corretto_f) ~ eta + genere + nutella_freq + fortuna, 
                         data=up_train)
    prob_nb <- predict(fit_nb, newdata=test_df, type="raw")[,2]
    roc_nb <- pROC::roc(test_df$corretto, prob_nb, quiet=TRUE)
    coords_nb <- coords(roc_nb, "best", ret="threshold", best.method="youden")
    
    imp_log <- broom::tidy(fit_log) %>%
      filter(term != "(Intercept)") %>%
      mutate(importance = abs(estimate)) %>%
      select(term, importance)
    
    imp_nb <- sapply(names(train_df)[names(train_df) %in% c("eta","genere","nutella_freq","fortuna")],
                     function(v) {
                       information_gain(corretto ~ ., train_df[c(v,"corretto")])$importance 
                     })
    imp_nb <- data.frame(term = names(imp_nb), importance = as.numeric(imp_nb))
    
    
    fit_rf <- randomForest(
      as.factor(corretto_f) ~  eta + genere + nutella_freq + fortuna,
      data = up_train,
      ntree = 100,
      importance = TRUE
    )
    prob_rf =  predict(fit_rf, newdata = test_df, type = "prob")[,2]
    roc_rf = pROC::roc(test_df$corretto, prob_rf, quiet=TRUE)
    coords_rf <- coords(roc_rf, "best", ret="threshold", best.method="youden")
    imp_rf = data.frame(term = names(fit_rf$importance[,4]), importance = as.numeric(fit_rf$importance[,4]) )
    
    list(
      fit_log = fit_log,
      fit_nb = fit_nb,
      fit_rf = fit_rf,
      threshold_log = min(as.numeric(coords_log$threshold)),
      threshold_nb = min(as.numeric(coords_nb$threshold)),
      threshold_rf = min(as.numeric(coords_rf$threshold)),
      roc_log = roc_log,
      roc_nb = roc_nb,
      roc_rf = roc_rf,
      imp_log = imp_log,
      imp_nb = imp_nb,
      imp_rf = imp_rf, 
      train_n = nrow(train_df),
      test_n = nrow(test_df)
    )
  })
  
  observeEvent(input$submit, {
    if(is.null(input$nick) || input$nick == "") {
      output$feedback_ui <- renderUI({
        tags$h4("⚠️ Inserisci un nickname!", style="color:red;")
      })
      return(NULL)
    }
    if(!check_nickname(input$nick)){
      output$feedback_ui <- renderUI({
        tagList(
          tags$h4("⚠️ Nickname non valido!", style="color:red;"),
          tags$p("Riprova con un altro nickname =).")
        )
      })
      return(NULL)
    }
    
    if (is.null(input$eta) || input$eta == "" || is.na(suppressWarnings(as.numeric(input$eta)))) {
      output$feedback_ui <- renderUI({
        tags$h4("⚠️ Inserisci un'età valida (numero)!", style = "color:red;")
      })
      return(NULL)
    }
    
    eta_num <- as.numeric(input$eta)
    if (eta_num < 0 || eta_num > 100) {
      output$feedback_ui <- renderUI({
        tags$h4("⚠️ L'età deve essere compresa tra 0 e 100!", style = "color:red;")
      })
      return(NULL)
    }
    
    corretto <- input$scelta == "A"
    
    nuovo <- data.frame(
      nickname = input$nick,
      eta = input$eta,
      genere = input$genere,
      nutella_freq = input$nutella_freq,
      fortuna = input$fortuna,
      preferenze = input$pref,
      scelta = input$scelta,
      corretto = corretto,
      stringsAsFactors = FALSE
    )
    
    append_ok <- FALSE
    tryCatch({
      sheet_append(ss, nuovo)
      append_ok <- TRUE
    }, error = function(e) {
      showNotification(paste0("Errore scrittura su Google Sheets: ", e$message), type = "error", duration = 8)
      message("Could not write to Google Sheets: ", e$message)
    })
    
    risposte(rbind(risposte(), nuovo))
    
    output$feedback_ui <- renderUI({
      if(corretto) {
        if(!exists("confetti_lanciati", envir = session$userData)) {
          session$sendCustomMessage(type = "confetti", message = list())
          session$userData$confetti_lanciati <- TRUE
        }
        tagList(
          tags$h3(paste0("✅ Bravo ", input$nick, "! Hai indovinato!"), style="color:green;"),
          tags$img(src="https://media2.giphy.com/media/111ebonMs90YLu/giphy.gif", height="150px"),
          if(!append_ok) tags$p("Attenzione: la risposta è stata salvata solo localmente nell'app.", style="color:orange;")
        )
      } else {
        session$userData$confetti_lanciati <- FALSE
        tagList(
          tags$h3(paste0("❌ Oh no ", input$nick, ", non era Nutella."), style="color:red;"),
          tags$img(src="https://media3.giphy.com/media/26ybwvTX4DTkwst6U/giphy.gif", height="150px"),
          if(!append_ok) tags$p("Attenzione: la risposta è stata salvata solo localmente nell'app.", style="color:orange;")
        )
      }
    })
    
  })
  
  # ADDED: Real-time predictions for the main questionnaire tab
  output$predizione_realtime_ui <- renderUI({
    req(input$nick, input$eta, input$genere, input$nutella_freq, input$pref, input$fortuna)
    
    models <- NULL
    try(models <- models_data(), silent=TRUE)
    
    if(is.null(models)) {
      df <- risposte()
      return(HTML(paste0(
        "<div style='background-color:#f0f8ff; padding:10px; border-radius:5px;'>",
        "<p style='color:gray; margin:0;'>",
        "🤖 Modello AI disponibile dopo 50 risposte<br>",
        "Attuali: <b>", nrow(df), "</b>/50",
        "</p>",
        "</div>"
      )))
    }
    
    current_data <- data.frame(
      eta = input$eta,
      genere = input$genere,
      nutella_freq = input$nutella_freq,
      fortuna = input$fortuna,
      preferenze = input$pref,
      stringsAsFactors = FALSE
    )
    
    prob_log <- predict(models$fit_log, newdata=current_data, type="response")
    class_log <- ifelse(prob_log > models$threshold_log, "Corretto ✅", "Sbagliato ❌")
    
    prob_nb <- predict(models$fit_nb, current_data, type="raw")[,2]
    class_nb <- ifelse(prob_nb > models$threshold_nb, "Corretto ✅", "Sbagliato ❌")
    
    prob_rf <- predict(models$fit_rf, current_data, type="prob")[,2]
    class_rf <- ifelse(prob_rf > models$threshold_rf, "Corretto ✅", "Sbagliato ❌")
    
    HTML(paste0(
      "<div style='background-color:#f0f8ff; padding:10px; border-radius:5px;'>",
      "<h5 style='margin-top:0;'>🤖 Modello 1 — AI (Logistic Regression)</h5>",
      "<b>Predizione:</b> <span style='font-size:18px;'>", class_log, "</span><br>",
      "Probabilità di indovinare: <b>", round(prob_log*100,1), "%</b><br>",
      "<hr style='margin:8px 0;'>",
      "<h5>🧠 Modello 2 — AI (Naive Bayes)</h5>",
      "<b>Predizione:</b> <span style='font-size:18px;'>", class_nb, "</span><br>",
      "Probabilità di indovinare: <b>", round(prob_nb*100,1), "%</b><br>",
      "<hr style='margin:8px 0;'>",
      "<h5>🚀 Modello 3 — AI (Random Forest)</h5>",
      "<b>Predizione:</b> <span style='font-size:18px;'>", class_rf, "</span><br>",
      "Probabilità di indovinare: <b>", round(prob_rf*100,1), "%</b><br>",
      "</div>"
    ))
  })
  
  output$predizione_dashboard <- renderUI({
    req(input$nick, input$eta, input$genere, input$nutella_freq, input$pref, input$fortuna)
    models <- NULL
    try(models <- models_data(), silent=TRUE)
    if(is.null(models)) return(HTML("<p style='color:gray;'>Modello non ancora disponibile (min 50 risposte).</p>"))
    
    current_data <- data.frame(
      eta = input$eta,
      genere = input$genere,
      nutella_freq = input$nutella_freq,
      fortuna = input$fortuna,
      preferenze = input$pref,
      stringsAsFactors = FALSE
    )
    
    prob_log <- predict(models$fit_log, newdata=current_data, type="response")
    class_log <- ifelse(prob_log > models$threshold_log, "Corretto ✅", "Sbagliato ❌")
    
    prob_nb <- predict(models$fit_nb, current_data, type="raw")[,2]
    class_nb <- ifelse(prob_nb > models$threshold_nb, "Corretto ✅", "Sbagliato ❌")
    prob_rf <- predict(models$fit_rf, current_data, type="prob")[,2]
    class_rf <- ifelse(prob_rf > models$threshold_rf, "Corretto ✅", "Sbagliato ❌")
    
    HTML(paste0(
      "<div style='background-color:#f0f8ff; padding:10px; border-radius:5px;'>",
      "<h5>🤖 Modello 1 — AI (Logistic Regression)</h5>",
      "<b>Predizione:</b> <span style='font-size:20px;'>", class_log, "</span><br>",
      "Probabilità di indovinare: <b>", round(prob_log*100,1), "%</b><br>",
      "<hr>",
      "<h5>🧠 Modello 2 — AI (Naive Bayes)</h5>",
      "<b>Predizione:</b> <span style='font-size:20px;'>", class_nb, "</span><br>",
      "Probabilità di indovinare: <b>", round(prob_nb*100,1), "%</b><br>",
      "<hr>",
      "<h5>🚀 Modello 3 — AI (Random Forest)</h5>",
      "<b>Predizione:</b> <span style='font-size:20px;'>", class_rf, "</span><br>",
      "Probabilità di indovinare: <b>", round(prob_rf*100,1), "%</b><br>",
      "</div>"
    ))
  })
  
  output$tabella <- renderTable({
    df <- risposte()
    req(nrow(df) > 0)
    
    df_tab <- df %>%
      select(-c(scelta, nutella_freq, preferenze)) %>%
      rename(
        Nickname = nickname,
        Età = eta,
        Genere = genere,
        `Ti senti fortunat*?` = fortuna,
        Corretto = corretto
      ) %>%
      arrange(desc(row_number())) %>% 
      head(5)
    
    df_tab$Corretto <- ifelse(df_tab$Corretto,
                              "<span style='color:green; font-size:20px;'>&#10004;</span>",
                              "<span style='color:red; font-size:20px;'>&#10008;</span>")
    df_tab
  }, sanitize.text.function = function(x) x)
  
  output$grafico_genere <- renderPlot({
    df <- risposte()
    req(nrow(df) > 0)
    
    df$genere_col <- ifelse(df$genere == "Femmina", "femmina", "maschio")
    
    ggplot(df, aes(x = genere, y = eta, fill = genere_col)) +
      geom_boxplot(width = 0.5, color = "#333333", size = 0.5,
                   outlier.shape = 21, outlier.size = 3, outlier.fill = "white") +
      scale_fill_manual(values = c("femmina" = "#FFB6C1", "maschio" = "#87CEFA")) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      labs(x = "Genere", y = "Età") +
      theme_minimal(base_size = 25, base_family = "comic") +
      theme(legend.position = "none", panel.grid = element_blank(),
            panel.background = element_blank(), plot.background = element_blank(),
            axis.title = element_text(face = "bold", color = "#333333"),
            axis.text = element_text(color = "#333333"),
            panel.border = element_rect(color = "#333333", fill = NA, size = 1.2))
  })
  
  output$grafico_freq <- renderPlot({
    df <- risposte()
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = nutella_freq)) +
      geom_histogram(breaks = seq(0, 14, by = 1), fill = "lightgreen",
                     color = "#333333", size = 0.5) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      scale_x_continuous(breaks = 0:13) +
      labs(x = "Volte", y = "Conteggio") +
      theme_minimal(base_size = 25, base_family = "comic") +
      theme(plot.title = element_text(face = "bold", color = "#333333"),
            axis.title = element_text(face = "bold", color = "#333333"),
            axis.text = element_text(color = "#333333"),
            panel.grid = element_blank(), panel.background = element_blank(),
            plot.background = element_blank(),
            panel.border = element_rect(color = "#333333", fill = NA, size = 1.2))
  })
  
  output$grafico_scelta <- renderPlot({
    req(nrow(risposte()) > 0)
    
    df <- risposte() %>%
      mutate(categoria = ifelse(eta < 18, "Bambini", "Adulti"))
    
    plot_data <- df %>%
      count(categoria, scelta) %>%
      group_by(categoria) %>%
      mutate(prop = n / sum(n) * 100)
    
    colori_pastello <- c("A" = "lightgreen", "B" = "lightcoral")
    nomi_legenda <- c("A" = "Giusto", "B" = "Sbagliato")
    
    ggplot(plot_data, aes(x = categoria, y = prop, fill = scelta)) +
      geom_bar(stat = "identity", position = position_dodge(width = 0.8),
               width = 0.7, color = "#333333") +
      geom_text(aes(label = paste0(round(prop,1), "%")),
                position = position_dodge(width = 0.8),
                vjust = -0.5, size = 7, family = "comic") +
      scale_fill_manual(values = colori_pastello, labels = nomi_legenda, name = "Risposta") +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 15), limits = c(0, 100)) +
      labs(x = "Categoria", y = "Percentuale") +
      theme_minimal(base_size = 25, base_family = "comic") +
      theme(legend.position = "top", panel.grid = element_blank(),
            panel.background = element_blank(), plot.background = element_blank(),
            axis.title = element_text(face = "bold", color = "#333333"),
            axis.text = element_text(color = "#333333"),
            panel.border = element_rect(color = "#333333", fill = NA, size = 1.2))
  })
  
  output$grafico_pref <- renderPlot({
    req(nrow(risposte()) > 0)
    
    df <- risposte() %>%
      mutate(categoria = ifelse(eta < 18, "Bambini", "Adulti"))
    
    choco_data <- df %>%
      group_by(categoria, genere) %>%
      summarise(prop = mean(preferenze == "Si", .groups = "drop") )|>
      mutate(gruppo = categoria) %>%
      mutate(colore = case_when(
        categoria == "Bambini" & genere == "Maschio" ~ "lightblue",
        categoria == "Bambini" & genere == "Femmina" ~ "lightpink",
        categoria == "Adulti" & genere == "Maschio" ~ "lightskyblue",
        categoria == "Adulti" & genere == "Femmina" ~ "lightcoral"
      ))
    
    ggplot(choco_data, aes(x = genere, y = gruppo, fill = colore)) +
      geom_tile(color = "#333333", size = 0.5) +
      geom_text(aes(label = scales::percent(prop, accuracy = 1)), size = 6, family = "comic") +
      scale_fill_identity() +
      labs(x = "Genere", y = "Gruppo") +
      theme_minimal(base_size = 25, base_family = "comic") +
      theme(legend.position = "none", panel.grid = element_blank(),
            panel.background = element_blank(), plot.background = element_blank(),
            axis.text = element_text(color = "#333333"),
            axis.title = element_text(face = "bold", color = "#333333"),
            panel.border = element_rect(color = "#333333", fill = NA, size = 1.2))
  })
  output$roc_combined <- renderPlot({
    models <- models_data()
    req(!is.null(models))
    
    roc_df_log <- data.frame(
      fpr = 1 - models$roc_log$specificities,
      tpr = models$roc_log$sensitivities,
      model = "Logistic Regression"
    )
    roc_df_nb <- data.frame(
      fpr = 1 - models$roc_nb$specificities,
      tpr = models$roc_nb$sensitivities,
      model = "Naive Bayes"
    )
    
    roc_df_rf <- data.frame(
      fpr = 1 - models$roc_rf$specificities,
      tpr = models$roc_rf$sensitivities,
      model = "Random Forest"
    )
    
    roc_df <- rbind(roc_df_log, roc_df_nb, roc_df_rf)
    
    auc_log <- round(pROC::auc(models$roc_log), 3)
    auc_nb  <- round(pROC::auc(models$roc_nb), 3)
    auc_rf  <- round(pROC::auc(models$roc_rf), 3)
    
    ggplot(roc_df, aes(x=fpr, y=tpr, color=model)) +
      geom_line(size=1.5) +
      geom_abline(slope=1, intercept=0, linetype="dashed", color="gray60", size=1) +
      scale_color_manual(values=c("Logistic Regression"="#5DADE2", "Naive Bayes"="#F1948A", "Random Forest" = "#7DCEA0")) +
      labs(x="1 - Specificità", y="Sensitività", color="Modello") +
      theme_minimal(base_size = 22, base_family = "comic") +
      theme(panel.grid = element_blank(),
            axis.title = element_text(face="bold"),
            legend.position="top",
            panel.border = element_rect(color="#333333", fill=NA, size=1.2)) +
      coord_equal()
  })
  
  output$var_importance <- renderPlot({
    models <- models_data()
    req(!is.null(models))
    
    imp_log <- models$imp_log %>% mutate(model = "Logistic Regression")
    imp_nb  <- models$imp_nb  %>% mutate(model = "Naive Bayes")
    imp_rf  <- models$imp_rf  %>% mutate(model = "Random Forest")
    
    imp_df <- bind_rows(imp_log, imp_nb, imp_rf)
    
    ggplot(imp_df, aes(x = reorder(term, importance), y = importance, fill = model)) +
      geom_bar(stat = "identity", color = "#333333") +
      coord_flip() +
      facet_wrap(~ model, scales = "free_x") +
      scale_fill_manual(values =c("Logistic Regression"="#5DADE2", "Naive Bayes"="#F1948A", "Random Forest" = "#7DCEA0")) +
      labs(
        x = "Variabile",
        y = "Importanza",
        fill = "Modello",
        title = "Importanza delle tue risposte per modello AI"
      ) +
      theme_minimal(base_size = 22, base_family = "comic") +
      theme(
        panel.grid = element_blank(),
        panel.border = element_rect(color = "#333333", fill = NA, size = 1.2),
        legend.position = "none",
        strip.text = element_text(face = "bold", size = 20)
      )
  })
  
  output$model_stats_ui <- renderUI({
    models <- NULL
    try(models <- models_data(), silent=TRUE)
    req(!is.null(models))
    
    auc_log <- round(pROC::auc(models$roc_log), 3)
    auc_nb <- round(pROC::auc(models$roc_nb), 3)
    auc_rf <- round(pROC::auc(models$roc_rf), 3)
    
    HTML(paste0(
      "<div style='background-color:#f8f9fa; padding:15px; border-radius:10px; box-shadow: 0 2px 10px rgba(0,0,0,0.1);'>",
      "<h4 style='margin-top:0; color:#333333;'>📊 Metriche dei Modelli</h4>",
      "<div style='border-left: 4px solid #4169E1; padding-left: 10px; margin-bottom: 10px;'>",
      "<b>🤖 Modello 1 — AI (Logistic Regression)</b><br>",
      "Performance del modello (AUC): <b>", auc_log, "</b><br>",
      "Soglia Ottimale: <b>", round(models$threshold_log, 3), "</b>",
      "</div>",
      "<div style='border-left: 4px solid #DC143C; padding-left: 10px; margin-bottom: 10px;'>",
      "<b>🧠 Modello 2 — AI (Naive Bayes)</b><br>",
      "Performance del modello (AUC): <b>", auc_nb, "</b><br>",
      "Soglia Ottimale: <b>", round(models$threshold_nb, 3), "</b>",
      "</div>",
      "<div style='border-left: 4px solid #7DCEA0; padding-left: 10px;'>",
      "<b>🚀 Modello 3 — AI (Random Forest)</b><br>",
      "Performance del modello (AUC): <b>", auc_rf, "</b><br>",
      "Soglia Ottimale: <b>", round(models$threshold_rf, 3), "</b>",
      "</div>",
      "</div>"
    ))
  })
}

shinyApp(ui, server)