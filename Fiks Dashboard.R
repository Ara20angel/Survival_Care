# Library
library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(dplyr)
library(tidyr)
library(survival)
library(survminer)
library(ggplot2)
library(janitor)

# Load dan bersihkan data Kaplan-Meier
df <- read_xlsx("Data/Data_R.xlsx") %>%
  janitor::clean_names()

faktor_vars <- c("jenis_kelamin", "jenis_operasi", "lokasi_tumor", "asa",
                 "jenis_anestesi", "status_hipertensi", "status_dm",
                 "status_pernikahan", "hu_care_window")
df[faktor_vars] <- lapply(df[faktor_vars], as.factor)

# Kategorisasi
df$usia_kat <- factor(ifelse(df$usia >= 10 & df$usia <= 18, "Remaja",
                             ifelse(df$usia >= 19 & df$usia <= 59, "Dewasa", "Lansia")))
df$lama_operasi_kat <- factor(ifelse(df$lama_operasi <= 30, "Pendek", "Panjang"))
df$ukuran_tumor_kat <- cut(df$ukuran_tumor,
                           breaks = c(-Inf, 5, 20, 50, Inf),
                           labels = c("Kecil", "Sedang", "Besar", "Sangat Besar"),
                           right = TRUE)
df$hemoglobin_kat <- factor(ifelse(df$hb >= 12 & df$hb <= 16, "Normal", "Abnormal"))

analisis_vars <- c(setdiff(names(df), c("id", "status", "waktu_survival",
                                        "ukuran_tumor", "usia", "hb", "lama_operasi")),
                   "usia_kat", "lama_operasi_kat", "ukuran_tumor_kat", "hemoglobin_kat")

# Label untuk nama variabel yang lebih deskriptif
nama_variabel_recode <- list(
  jenis_kelamin = "Jenis Kelamin",
  jenis_operasi = "Jenis Operasi",
  lokasi_tumor = "Lokasi Tumor",
  asa = "ASA",
  jenis_anestesi = "Jenis Anestesi",
  status_hipertensi = "Status Hipertensi",
  status_dm = "Status Diabetes Melitus",
  status_pernikahan = "Status Pernikahan",
  hu_care_window = "HU-CARE Window"
)

# ============================ COX MODEL ============================ #
Y <- Surv(df$waktu_survival, df$status == 1)
cox_model <- coxph(Y ~ jenis_operasi + jenis_anestesi + status_pernikahan, data = df, ties = "efron")
basehaz_df <- basehaz(cox_model, centered = FALSE)
basehaz_df$hazard_instant <- c(basehaz_df$hazard[1], diff(basehaz_df$hazard))
# ============================================= UI ============================================= #
ui <- dashboardPage(
  dashboardHeader(title = "Survival Care"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data", tabName = "data", icon = icon("database")),
      menuItem("Frequency Table", tabName = "statistika_deskriptif", icon = icon("table")),
      menuItem("Baseline Hazard", tabName = "baseline_hazard", icon = icon("heartbeat")),
      menuItem("Recovery Curve", tabName = "kaplan", icon = icon("chart-line")),
      menuItem("Recovery Model", tabName = "hazard", icon = icon("hourglass-half"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
      .box-header .box-title {
        width: 100%;
        text-align: center;
        display: block;
      }
    "))
    ),
    tabItems(
      tabItem(
        tabName = "data",
        h2("Dataset Pasien Pascaoperasi Tumor"),
        DTOutput("dataTable")
      ),
      tabItem(
        tabName = "statistika_deskriptif",
        uiOutput("varSelector"),
        tableOutput("tabulasiTable"),
        br(),
        fluidRow(
          box(title = textOutput("judulBarChart"), width = 12, status = "primary", solidHeader = TRUE,
              plotOutput("barChart", height = "400px", width = "100%"))
        )
      ),
      tabItem(
        tabName = "baseline_hazard",
        withMathJax(),
        tags$div(
          h2("Nilai Baseline Hazard \\( \\hat{h}_0(t) \\)"),
          p("Tabel dan grafik di bawah ini menunjukkan nilai baseline hazard pada setiap waktu \\( t \\), berdasarkan model Cox proportional hazard yang dibangun menggunakan metode Efron.")
        ),
        fluidRow(
          box(
            title = "Tabel Nilai h₀(t)", width = 6, solidHeader = TRUE, status = "info",
            tableOutput("basehazTable1")
          ),
          box(
            title = "Tabel Nilai h₀(t)", width = 6, solidHeader = TRUE, status = "info",
            tableOutput("basehazTable2")
          )
        )
      ),
      tabItem(
        tabName = "kaplan",
        h2("Kaplan-Meier Curve"),
        selectInput("sur_var", "Select Variabel:", choices = analisis_vars),
        h3(textOutput("caption")),
        plotOutput("plot1"),
        h4("Interpretasi Hasil"),
        textOutput("logrankInterpretation"),
        h4("Log-Rank Test"),
        verbatimTextOutput("logrankTest")
      ),
      tabItem(
        tabName = "hazard",
        fluidRow(
          column(4,
                 selectInput("jenis_operasi", "Jenis Operasi:", choices = levels(df$jenis_operasi)),
                 selectInput("jenis_anestesi", "Jenis Anestesi:", choices = levels(df$jenis_anestesi)),
                 selectInput("status_pernikahan", "Status Pernikahan:", choices = levels(df$status_pernikahan)),
                 sliderInput("waktu_t", "Pilih Waktu t (hari):",
                             min = floor(min(basehaz_df$time, na.rm = TRUE)),
                             max = ceiling(max(basehaz_df$time, na.rm = TRUE)),
                             value = floor(min(basehaz_df$time, na.rm = TRUE)))
          ),
          column(8,
                 withMathJax(),
                 h3("Persamaan Model:"),
                 uiOutput("modelFormula"),
                 h3("Laju Pemulihan:"),
                 tableOutput("hazardResult")
          )
        ),
        
        # Tambahkan fluidRow grafik terpisah di bawahnya
        fluidRow(
          column(6, plotOutput("hazardPlot", height = "350px")),
          column(6, plotOutput("cumHazardPlot", height = "350px"))
        )
      )
    )
  )
)
# ============================================= SERVER ============================================= #
server <- function(input, output, session) {
  
  ########################## Data table ##########################
  output$dataTable <- renderDT({
    datatable(df, options = list(pageLength = 10))
  })
  
  ########################## Frequency Tabel ##########################
  output$varSelector <- renderUI({
    selectInput("selectedVar", "Select Variabel:",
                choices = names(select(df, where(is.factor), -usia_kat, -lama_operasi_kat, -ukuran_tumor_kat, -hemoglobin_kat)))
  })
  
  #Frequency Tabel
  buat_tabulasi <- function(data, var_name) {
    # Recode level berdasarkan nama variabel
    data <- data %>%
      mutate(!!sym(var_name) := case_when(
        var_name == "jenis_kelamin" & !!sym(var_name) == 0 ~ "Laki-Laki",
        var_name == "jenis_kelamin" & !!sym(var_name) == 1 ~ "Perempuan",
        
        var_name == "jenis_operasi" & !!sym(var_name) == 1 ~ "Sedang",
        var_name == "jenis_operasi" & !!sym(var_name) == 2 ~ "Tinggi",
        var_name == "jenis_operasi" & !!sym(var_name) == 3 ~ "Khusus",
        
        var_name == "lokasi_tumor" & !!sym(var_name) == 1 ~ "Sistem Integumen",
        var_name == "lokasi_tumor" & !!sym(var_name) == 2 ~ "Sistem Muskuloskeletal",
        var_name == "lokasi_tumor" & !!sym(var_name) == 3 ~ "Sistem Reproduksi",
        var_name == "lokasi_tumor" & !!sym(var_name) == 4 ~ "Sistem Lain",
        
        var_name == "jenis_anestesi" & !!sym(var_name) == 0 ~ "Umum",
        var_name == "jenis_anestesi" & !!sym(var_name) == 1 ~ "Spinal",
        
        var_name == "asa" & !!sym(var_name) == 1 ~ "I",
        var_name == "asa" & !!sym(var_name) == 2 ~ "II",
        
        var_name == "status_hipertensi" & !!sym(var_name) == 0 ~ "Tidak Memiliki",
        var_name == "status_hipertensi" & !!sym(var_name) == 1 ~ "Memiliki",
        
        var_name == "status_dm" & !!sym(var_name) == 0 ~ "Tidak Memiliki",
        var_name == "status_dm" & !!sym(var_name) == 1 ~ "Memiliki",
        
        var_name == "status_pernikahan" & !!sym(var_name) == 0 ~ "Belum Menikah",
        var_name == "status_pernikahan" & !!sym(var_name) == 1 ~ "Pernah Menikah",
        
        var_name == "hu_care_window" & !!sym(var_name) == 0 ~ "Nyaman",
        var_name == "hu_care_window" & !!sym(var_name) == 1 ~ "Gamang",
        
        TRUE ~ as.character(!!sym(var_name)) # default
      ))
    
    # Tabulasi
    tab <- data %>%
      group_by(!!sym(var_name), status) %>%
      summarise(Jumlah = n(), .groups = 'drop') %>%
      pivot_wider(names_from = status, values_from = Jumlah, values_fill = list(Jumlah = 0)) %>%
      rename("Tersensor" = `0`, "Pulih" = `1`)
    
    status_cols <- intersect(c("Pulih", "Tersensor"), colnames(tab))
    tab <- tab %>%
      mutate(Total = rowSums(across(all_of(status_cols))),
             `Persentase Pulih` = if ("Pulih" %in% colnames(tab)) round(Pulih / Total * 100, 1) else 0,
             `Persentase Sensor` = if ("Tersensor" %in% colnames(tab)) round(Tersensor / Total * 100, 1) else 0) %>%
      rename(!!var_name := !!sym(var_name))
    
    return(tab)
  }
  
  output$tabulasiTable <- renderTable({
    req(input$selectedVar)
    buat_tabulasi(df, input$selectedVar)
  })
  
  ########################### Judul Dinamis untuk Bar Chart ##########################
  output$judulBarChart <- renderText({
    req(input$selectedVar)
    nama <- nama_variabel_recode[[input$selectedVar]]
    if (is.null(nama)) nama <- input$selectedVar
    paste("Bar Chart", nama)
  })
  
  ########################### Bar Chart ##########################
  output$barChart <- renderPlot({
    req(input$selectedVar)
    var <- input$selectedVar
    
    df_temp <- df %>%
      mutate(
        status_f = factor(status, levels = c(1, 0), labels = c("Pulih", "Tersensor")),
        kategori = as.character(.data[[var]])
      )
    
    # Recode label kategori agar lebih manusiawi
    if (var == "jenis_kelamin") {
      df_temp$kategori <- recode(df_temp$kategori, `0` = "Laki-laki", `1` = "Perempuan")
    } else if (var == "jenis_operasi") {
      df_temp$kategori <- recode(df_temp$kategori, `1` = "Sedang", `2` = "Tinggi", `3` = "Khusus")
    } else if (var == "lokasi_tumor") {
      df_temp$kategori <- recode(df_temp$kategori, `1` = "Sistem Integumen", `2` = "Sistem Muskuloskeletal",
                                 `3` = "Sistem Reproduksi", `4` = "Sistem Lain")
    } else if (var == "jenis_anestesi") {
      df_temp$kategori <- recode(df_temp$kategori, `0` = "Umum", `1` = "Spinal")
    } else if (var == "asa") {
      df_temp$kategori <- recode(df_temp$kategori, `1` = "I", `2` = "II")
    } else if (var == "status_hipertensi") {
      df_temp$kategori <- recode(df_temp$kategori, `0` = "Tidak Hipertensi", `1` = "Hipertensi")
    } else if (var == "status_dm") {
      df_temp$kategori <- recode(df_temp$kategori, `0` = "Tidak DM", `1` = "DM")
    } else if (var == "status_pernikahan") {
      df_temp$kategori <- recode(df_temp$kategori, `0` = "Belum Menikah", `1` = "Pernah Menikah")
    } else if (var == "hu_care_window") {
      df_temp$kategori <- recode(df_temp$kategori, `0` = "Nyaman", `1` = "Gamang")
    }
    
    df_temp$kategori <- factor(df_temp$kategori, levels = unique(df_temp$kategori))
    
    ggplot(df_temp, aes(x = kategori, fill = status_f)) +
      geom_bar(position = "dodge") +
      scale_fill_manual(values = c("Pulih" = "green3", "Tersensor" = "red3")) +
      labs(
        title = "",
        x = NULL,
        y = "Jumlah Pasien",
        fill = "Status"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5, size = 14),
        axis.text.x = element_text(hjust = 0.5, size = 11),
        axis.title.y = element_text(size = 12)
      )
  })
  
  ########################### Tabel Baseline Hazard ##########################
  
  output$basehazTable1 <- renderTable({
    n <- nrow(basehaz_df)
    half <- ceiling(n / 2)
    basehaz_df[1:half, c("time", "hazard_instant")] %>%
      rename("Time" = time, "Baseline Hazard" = hazard_instant)
  })
  
  output$basehazTable2 <- renderTable({
    n <- nrow(basehaz_df)
    half <- ceiling(n / 2)
    basehaz_df[(half + 1):n, c("time", "hazard_instant")] %>%
      rename("Time" = time, "Baseline Hazard" = hazard_instant)
  })
  
  ########################### Kaplan-Meier ##########################
  selectedData <- reactive({
    df[[input$sur_var]]
  })
  
  output$caption <- renderText({
    paste("Survival Graph of", input$sur_var)
  })
  
  runSur <- reactive({
    var <- input$sur_var
    form <- as.formula(paste("Surv(waktu_survival, status) ~", paste0("`", var, "`")))
    survfit(form, data = df)
  })
  
  runLogRank <- reactive({
    var <- input$sur_var
    form <- as.formula(paste("Surv(waktu_survival, status) ~", paste0("`", var, "`")))
    survdiff(form, data = df)
  })
  
  output$logrankTest <- renderPrint({
    runLogRank()
  })
  
  output$logrankInterpretation <- renderText({
    var <- input$sur_var
    level_names <- levels(as.factor(df[[var]]))
    
    # Recode level untuk variabel tertentu
    if (var == "jenis_kelamin") {
      level_names <- recode(level_names, `0` = "laki-laki", `1` = "perempuan")
    }
    if (var == "jenis_operasi") {
      level_names <- recode(level_names, `1` = "sedang", `2` = "tinggi", `3` = "khusus")
    }
    if (var == "lokasi_tumor") {
      level_names <- recode(level_names,
                            `1` = "sistem Integumen", `2` = "sistem Muskuloskeletal",
                            `3` = "sistem Reproduksi", `4` = "Sistem Lain")
    }
    if (var == "asa") {
      level_names <- recode(level_names, `1` = "I", `2` = "II")
    }
    if (var == "status_hipertensi") {
      level_names <- recode(level_names,
                            `0` = "tidak memiliki status hipertensi",
                            `1` = "memiliki status hipertensi")
    }
    if (var == "status_dm") {
      level_names <- recode(level_names,
                            `0` = "tidak memiliki riwayat diabetes melitus",
                            `1` = "memiliki riwayat diabetes melitus")
    }
    if (var == "status_pernikahan") {
      level_names <- recode(level_names, `0` = "belum menikah", `1` = "pernah menikah")
    }
    if (var == "hu_care_window") {
      level_names <- recode(level_names, `0` = "nyaman", `1` = "gamang")
    }
    
    pasangan <- paste(level_names, collapse = " dan ")
    
    # Interpretasi berdasarkan variabel
    interpretasi_kurva <- switch(var,
                                 "jenis_kelamin" = "Kedua garis pada kurva Kaplan-Meier tidak saling berhimpitan dan menunjukkan pola berbeda. Hal ini menunjukkan bahwa terdapat perbedaan probabilitas antara kelompok pasien laki-laki dan kelompok pasien perempuan. Pada kurva Kaplan-Meier tersebut juga terlihat bahwa pasien perempuan memiliki probabilitas tidak mengalami pemulihan lebih tinggi dibandingkan pasien laki-laki.",
                                 "jenis_operasi" = "Ketiga garis pada kurva Kaplan-Meier tidak saling berhimpitan dan menunjukkan pola yang berbeda antar jenis operasi. Hal ini menunjukkan bahwa terdapat perbedaan probabilitas pada kelompok jenis operasi pasien. Pada kurva Kaplan-Meier tersebut juga terlihat bahwa pasien dengan operasi yang khusus memiliki probabilitas tidak mengalami pemulihan lebih tinggi dibandingkan pasien dengan jenis operasi lainnya.",
                                 "lama_operasi"  = "Kedua garis pada kurva Kaplan-Meier saling berhimpitan dan memiliki pola paralel. Hal ini menunjukkan bahwa tidak terdapat perbedaan probabilitas survival pada kelompok durasi operasi pasien.",
                                 "lokasi_tumor" = "Keempat garis pada kurva Kaplan-Meier saling berhimpitan dan membentuk pola paralel. Hal ini menunjukkan bahwa tidak terdapat perbedaan probabilitas survival pada keempat kelompok lokasi tumor pasien.",
                                 "ukuran_tumor" = "Keempat garis pada kurva Kaplan-Meier saling berhimpitan dan membentuk pola paralel. Hal ini menunjukkan bahwa tidak terdapat perbedaan probabilitas survival pada kelompok ukuran tumor pasien.",
                                 "asa" = "Kedua garis dalam kurva Kaplan-Meier saling berhimpitan dan membentuk pola paralel. Hal ini menunjukkan bahwa tidak terdapat perbedaan probabilitas survival pada kelompok ASA pasien.",
                                 "jenis_anestesi" = "Kedua garis pada kurva Kaplan-Meier tidak saling berhimpitan. Hal ini menunjukkan bahwa terdapat perbedaan probabilitas survival pada kelompok jenis anestesi pasien. Kurva ini juga menunjukkan bahwa pasien dengan anestesi umum memiliki probabilitas tidak mengalami pemulihan lebih tinggi dibandingkan pasien dengan anestesi spinal.",
                                 "hemoglobin_kat" = "Kedua garis pada kurva Kaplan-Meier saling berhimpitan dan menunjukkan pola paralel. Hal ini menunjukkan bahwa tidak terdapat perbedaan probabilitas survival pada kelompok kadar hemoglobin pasien.",
                                 "status_dm" = "Kedua garis pada kurva Kaplan-Meier saling berhimpitan. Hal ini menunjukkan bahwa tidak terdapat perbedaan probabilitas survival pada kelompok riwayat diabetes melitus pasien.",
                                 "status_hipertensi" = "Kedua garis pada kurva Kaplan-Meier saling berhimpitan. Hal ini menunjukkan bahwa tidak terdapat perbedaan probabilitas survival pada kelompok status hipertensi pasien.",
                                 "status_pernikahan" = "Kedua garis pada kurva Kaplan-Meier pada awal pengamatan berhimpitan, tetapi seiring berjalannya waktu kedua garis tersebut tidak berhimpitan. Hal ini menunjukkan bahwa terdapat perbedaan probabilitas survival antara kelompok pasien yang belum pernah menikah dan pernah menikah. Kurva ini juga menunjukkan bahwa pasien yang belum pernah menikah memiliki probabilitas tidak mengalami pemulihan lebih tinggi dibandingkan pasien yang sudah menikah.",
                                 "hu_care_window" = "Kedua garis pada kurva Kaplan-Meier saling berhimpitan dan menunjukkan pola paralel. Hal ini menunjukkan bahwa tidak terdapat perbedaan probabilitas survival antara kelompok pasien dengan kondisi spiritual nyaman dan pasien dengan kondisi spiritual gamang."
    )
    
    # Tambahkan interpretasi default jika tidak ada di atas
    if (is.null(interpretasi_kurva)) {
      pasangan <- paste(level_names, collapse = " dan ")
      interpretasi_kurva <- paste0(
        "Kurva Kaplan-Meier menunjukkan perbedaan pola survival antara kelompok ", pasangan,
        ". Hal ini menunjukkan bahwa terdapat perbedaan probabilitas pemulihan antara kelompok ", pasangan, "."
      )
    }
    
    return(interpretasi_kurva)
  })
  
  output$plot1 <- renderPlot({
    fit <- runSur()
    var <- input$sur_var 
    levels_var <- levels(as.factor(selectedData()))
    n_levels <- length(levels_var)
    palet_warna <- switch(as.character(n_levels),
                          "2" = c("blue1", "chocolate2"),
                          "3" = c("blue1", "chocolate2", "green1"),
                          "4" = c("blue1", "chocolate2", "green1", "maroon1"),
                          rainbow(n_levels))
    
    # Mapping label khusus untuk variabel tertentu
    label_map <- function(var, levels_var) {
      if (var == "jenis_kelamin") {
        return(recode(levels_var, `0` = "Laki-laki", `1` = "Perempuan"))
      } else if (var == "jenis_operasi") {
        return(recode(levels_var, `1` = "Sedang", `2` = "Tinggi", `3` = "Khusus"))
      } else if (var == "lokasi_tumor") {
        return(recode(levels_var,
                      `1` = "Sistem Integumen",
                      `2` = "Sistem Muskuloskeletal",
                      `3` = "Sistem Reproduksi",
                      `4` = "Sistem Lain"))
      } else if (var == "jenis_anestesi") {
        return(recode(levels_var, `0` = "Umum", `1` = "Spinal"))
      } else if (var == "asa") {
        return(recode(levels_var, `1` = "I", `2` = "II"))
      } else if (var == "status_hipertensi") {
        return(recode(levels_var, `0` = "Tidak Hipertensi", `1` = "Hipertensi"))
      } else if (var == "status_dm") {
        return(recode(levels_var, `0` = "Tidak DM", `1` = "DM"))
      } else if (var == "status_pernikahan") {
        return(recode(levels_var, `0` = "Belum Menikah", `1` = "Pernah Menikah"))
      } else if (var == "hu_care_window") {
        return(recode(levels_var, `0` = "Nyaman", `1` = "Gamang"))
      } else {
        return(levels_var)
      }
    }
    
    label_legenda <- label_map(var, levels_var)
  
    plot(fit, col = palet_warna,
         xlab = "Days", ylab = "S(t)", lwd = 2)
    
    legend("topright", legend = label_legenda, fill = palet_warna, cex = 0.9)
  })
  
  # Cox Model - Recovery Model
  output$modelFormula <- renderUI({
    withMathJax(helpText("$$
      h(t) = h_0(t) \\times \\exp(\\beta_1 \\cdot JenisOperasi + \\beta_2 \\cdot JenisAnestesi + \\beta_3 \\cdot StatusPernikahan)
    $$"))
  })
  
  output$hazardResult <- renderTable({
    req(input$jenis_operasi, input$jenis_anestesi, input$status_pernikahan, input$waktu_t)
    
    newdata <- data.frame(
      jenis_operasi = factor(input$jenis_operasi, levels = levels(df$jenis_operasi)),
      jenis_anestesi = factor(input$jenis_anestesi, levels = levels(df$jenis_anestesi)),
      status_pernikahan = factor(input$status_pernikahan, levels = levels(df$status_pernikahan))
    )
    
    lin_pred <- predict(cox_model, newdata = newdata)
    exp_linpred <- exp(lin_pred)
    
    # Cari waktu paling dekat dengan input$waktu_t
    nearest_index <- which.min(abs(basehaz_df$time - input$waktu_t))
    row_selected <- basehaz_df[nearest_index, ]
    
    result <- data.frame(
      Waktu = round(row_selected$time, 2),
      Baseline.Hazard..h.t.. = round(row_selected$hazard_instant, 4),
      exp.Linear.Predictor. = round(exp_linpred, 4),
      Estimated.Hazard.h.t.x. = round(row_selected$hazard_instant * exp_linpred, 4)
    )
    
    result
  })
  
  output$hazardPlot <- renderPlot({
    newdata <- data.frame(
      jenis_operasi = factor(input$jenis_operasi, levels = levels(df$jenis_operasi)),
      jenis_anestesi = factor(input$jenis_anestesi, levels = levels(df$jenis_anestesi)),
      status_pernikahan = factor(input$status_pernikahan, levels = levels(df$status_pernikahan))
    )
    
    lin_pred <- predict(cox_model, newdata = newdata)
    exp_linpred <- exp(lin_pred)
    
    hazard_df <- basehaz_df %>%
      mutate(h_tx = hazard_instant * exp_linpred)
    
    ggplot(hazard_df, aes(x = time, y = h_tx)) +
      geom_line(color = "blue", size = 1) +
      geom_vline(xintercept = input$waktu_t, linetype = "dashed", color = "red") +
      labs(
        title = "Kurva Hazard h(t;x)",
        x = "Waktu (t)",
        y = expression(h(t,x))
      ) +
      theme_minimal()
  })
  
  output$cumHazardPlot <- renderPlot({
    newdata <- data.frame(
      jenis_operasi = factor(input$jenis_operasi, levels = levels(df$jenis_operasi)),
      jenis_anestesi = factor(input$jenis_anestesi, levels = levels(df$jenis_anestesi)),
      status_pernikahan = factor(input$status_pernikahan, levels = levels(df$status_pernikahan))
    )
    
    lin_pred <- predict(cox_model, newdata = newdata)
    exp_linpred <- exp(lin_pred)
    
    cumhaz_df <- basehaz_df %>%
      mutate(H_tx = hazard * exp_linpred)
    
    ggplot(cumhaz_df, aes(x = time, y = H_tx)) +
      geom_line(color = "darkgreen", size = 1) +
      geom_vline(xintercept = input$waktu_t, linetype = "dashed", color = "red") +
      labs(
        title = "Kurva Cumulative Hazard H(t;x)",
        x = "Waktu (t)",
        y = expression(H(t,x))
      ) +
      theme_minimal()
  })
}

# Jalankan aplikasi
shinyApp(ui, server)
