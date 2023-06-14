rm(list=ls())
setwd(directory)
source("functions.R") 

# Load packages --------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(tools)
library(thematic)
library(shinythemes)
library(hrbrthemes)
library(progress)
library(shinybusy)
library(mathjaxr)
library(clinfun)
library(latex2exp)
library(berryFunctions)

# The app --------------------------------------------------------------

ui <- fluidPage(
  theme = bslib::bs_theme(bg = "#FFFFFF", fg = "#121F6B", primary = "#FD8204", font_scale = 0.7),
  tags$style(HTML(".table.dataTable tbody td.active, .table.dataTable tbody tr.active td {
            background-color: #FD8204;}")),
  add_busy_spinner(spin = "fading-circle", color = "#FD8204", position = "top-right",
                   height = "30px", width = "30px"),
  sidebarLayout(
    sidebarPanel(
      width = 5,
      # Select a number
      h2("Simon's Two Stage Design"),
      HTML(paste("Enter a value for the minumum and maximum number of patients")),
      sliderInput(inputId = "N",
                  label = withMathJax("\\(N_{min}\\) and \\(N_{max}\\)"),
                  min = 2, max = 80,
                  value = c(20, 36),
                  step = 1
      ),
      HTML(paste("Enter a value for the required proportions under \\(H_0\\) and \\(H_1\\)")),
      fluidRow(
        column(6,
          sliderInput(inputId = "p0",
                      label = withMathJax("\\(p_0\\)"),
                      min = 0.01, max = 0.99,
                      value = 0.5,
                      step = 0.01
          )
        ),
        column(6,
          sliderInput(inputId = "p1",
                      label = withMathJax("\\(p_1\\)"),
                      min = 0.01, max = 0.99,
                      value = 0.8,
                      step = 0.01
          )
        )
      ),
      HTML(paste("Enter a value for the required level of signiifcance and power")),
      fluidRow(
        column(6,
          sliderInput(inputId = "alpha",
                      label = withMathJax("\\(\\alpha\\)"),
                      min = 0.01, max = 0.30,
                      value = 0.025,
                      step = 0.005
          )
        ),
        column(6,
          sliderInput(inputId = "beta",
                      label = withMathJax("1 - \\(\\beta\\)"),
                      min = 0.4, max = 0.99,
                      value = 0.8,
                      step = 0.01
          )
        )
      ),
      HTML(paste(strong("Modified Two Stage Design:"), br(), "we choose a \\(n_1\\) and the number of responses \\(X\\) and we see how \\(N\\) and \\(R\\) changes")),
      fluidRow(
        column(4,
          sliderInput(inputId = "n1",
                      label = withMathJax("\\(n_1\\)"),
                      min = 1, max = 50,
                      value = 11,
                      step = 1
          )
        ),
        column(4,
          sliderInput(inputId = "X",
                      label = withMathJax("\\(X_{n_1}\\)"),
                      min = 1, max = 50,
                      value = 8,
                      step = 1
          )
        ),
        column(4,
          sliderInput(inputId = "lev",
                      label = "Level of conditional power required",
                      min = 0, max = 1,
                      value = 0.9,
                      step = 0.1
          )
        )
      ),
      submitButton("Submit"),
    ),
    # Output: 
    mainPanel(
      width = 7,
      tabsetPanel(id = "mainTab", type = "tabs",
                  tabPanel("Theory", tabsetPanel(id = "subTab1",
                                                 tabPanel("Simon's Two-Stage design",
                                                          #h2("The theory"),
                                                          uiOutput("theory")),
                                                 tabPanel("Modified Simon's Two-Stage design",
                                                          #h2("The theory 2"),
                                                          uiOutput("theory2"))
                                                )
                  ),
                  tabPanel("Plot", h2("Simon's Two-Stage Design"), 
                           h3("The plot"),
                            plotOutput(outputId = "plot1",
                                       brush = brushOpts(id = "plot_brush", fill = "#FDCF83", stroke = "#FD8204", 
                                                         opacity = 0.25, delay = 300, delayType = c("debounce", "throttle"), 
                                                         clip = TRUE, direction = c("xy", "x", "y"))),
                           h3("All the possible designs \\( (r_1 / n_1; r/n)\\) given the parameters  \\( (p_0; p_1; \\alpha; \\beta)\\)"),
                              #style="text-align:left; margin:0; padding:0;"),
                           uiOutput(outputId = "textTab"),
                           dataTableOutput(outputId = "tab")
                  ),
                  tabPanel("Regions", h2("Modified Simon's Two-Stage Design"),
                           h2("The monitoring regions"),
                           uiOutput(outputId = "textReg"),
                           plotOutput(outputId = "regions"),
                           plotOutput(outputId = "regions2"),
                           h2("Sample size re-estimation", style="text-align:left; padding:0;"),
                           uiOutput(outputId = "text")
                  ),
                  tabPanel("All plots", h2("Modified Simon's Two-Stage Design"),
                           uiOutput(outputId = "all_reg_theory"),
                           uiOutput(outputId = "text_all_reg"),
                           plotOutput(outputId = "all_regions"),
                           uiOutput(outputId = "text_all_reg2"),
                           plotOutput(outputId = "all_regions2"),
                           
                  )
                )
          )
  )
)

# Define server ----------------------------------------------------------------

server <- function(input, output, session) {
  
  plot_1 <- reactive(fast_Simon2SD(input$p0, input$p1, input$alpha, 1-input$beta))
  
  fix_des <- reactive(fixedDesign(alpha = input$alpha, beta = 1-input$beta, p0 = input$p0, 
                                  p1 = input$p1, nmin = input$N[1], nmax = input$N[2]))
  
  my_text <- reactive(SimonAdj(alpha = input$alpha, beta = 1-input$beta, p0 = input$p0, p1 = input$p1,
                                 nmin = input$N[1], nmax = input$N[2], n1 = input$n1, X = input$X,
                                 lev = input$lev, out = plot_1(), N = fix_des()$N, R = fix_des()$R))
  
  # ---------------  ######## --- Some theory --- ######## --------------- #
  
  output$theory <- renderUI({
    nmin <- input$N[1]
    tagList(
      withMathJax(),
      h5("Simon's Two-Stage Design"),
      p("Let's describe the responses to a treatment with a binary random variable \\(X \\sim Bi(n, p) \\), 
      where \\(X_i = 1\\) with propability \\(p\\) and \\(X_i = 0\\) with probability \\(1-p\\) for \\(i, \\ldots, n\\).",
      br(),
      br(),
      "A", strong("two-stage design"), "is defined by:",
      tags$ul(
        tags$li("the number of patients to be accrued during stages one and two, \\(n_1\\) and \\(n_2\\)"),
        tags$li("the the number of responses we observe, the boundary values, \\(r_1\\) and \\(r\\) (\\(r_1\\) < \\(r\\) ),"),
        style = "margin-bottom: 0px", style = "margin-top: 0px"), 
      "so we denote any two-stage design with (\\(r_1\\)/\\(n_1\\); \\(r\\)/\\(n\\)) 
      where \\(n\\) = \\(n_1\\) + \\(n_2\\), is the maximum sample size."),
      
      p("The \\(r_1\\), \\(n_1\\), \\(r\\) and \\(n\\) values are determined based on some", strong("pre-specified design parameters"),".",
      br(),
      "Let \\(p_0\\) denote the maximum unacceptable probability of response, and \\(p_1\\) the minimum 
      acceptable probability of response with \\(p_0 < p_1\\). We want to test",
      "\\(\\cases{ H_0 : p ≤ p_0 \\newline H_1: p > p_1 } \\)",
      "with type I error probability \\(\\alpha\\) and power 1 − \\(\\beta\\). These parameters (\\(p_0\\); \\(p_1\\); \\(\\alpha\\); 
        \\(\\beta\\)) are termed", strong(" design parameters"), "."),
      
      p("For the binomial distribution we have: \\(b(.; p, m)\\) the probability density function and 
        \\(B(.; p, m)\\) the cumulative probability function, where \\(p\\) is the probability of response 
        and \\(m\\) is the number of trials. So, for a two-stage design, the", strong("probability of 
        rejecting the treatment"), "(accepting \\(H_0\\)), is",
        "\\(R(p) = B(r_1; p, m) + \\sum^{min(r, n_1)}_{x=r_1 + 1} b(x; p, n_1) B(r-x; p, n_2) \\).",
        "where \\(B(r_1; p, n_1) \\) is the probability of", strong("early termination after stage one"), "denoted by \\(PET(p)\\).",
        "The constraints on the", strong("I type error"), "and on the", strong("power"), "are such as to determine many two-stage designs,
        denoted by (\\(r_1\\)/\\(n_1\\); \\(r\\)/\\(n\\)), satisfying the constraints.",
        tags$ul(
          tags$li("\\(R(p_0) ≥ 1-\\alpha \\)"),
          tags$li("\\(R(p_1) ≤ \\beta \\)")
        )),
      
        p("The two criteria to which we refer to select a", strong("good two-stage design"), "are the following:",
        tags$ul(
          tags$li("The", strong("minmax design:"), "which minimizes the maximum sample size, \\(n\\), among the 
          designs satisfying the (\\(\\alpha\\); \\(\\beta\\))-constraint."),
          tags$li("The", strong("optimal design"), "which minimizes the expected sample size \\(EN\\) under 
                  the null hypothesis: \\(EN = PET(p_0) × n_1 + (1 - PET(p_0)) × n\\).")
        ),
      ),
      p("The values for the quantities here presented are displayed on the table in the", em("plot"), "section."),
      paste0("Insights into the Simon's Two Stage Design can be found at the folliwing link:"),
      a("Simon's Two-Stage Design", href = "https://pubmed.ncbi.nlm.nih.gov/14755389/"),
      paste0("."),
      #paste0("Some theory on the Simon's two stage design \\(\\sqrt{",nmin,"}\\) \\(\\xi_{", nmin, "}\\)"),
    )
  })
  
  output$theory2 <- renderUI({
    tagList(
      withMathJax(),
      h5("Modified Simon's Two-Stage Design"),
      p("The modification of the two-stage design proposed in the",
      a("following paper", href = "https://www.tandfonline.com/doi/full/10.1080/19466315.2023.2177332?src="),
      "allows the greater flexibility in conducting the interim analysis and sample size adjustment."),

      p("The modification is done by using the", strong("conditional probability"), "and allows for", strong("early termination"), 
      "as well as extension with", strong("sample size adjustment."),
        br(),
      "Let \\(X_i = 1\\) with probability \\(p\\), \\(X_i = 0\\) with probability \\(1-p\\) for 
             \\(i = 1, ..., N\\), where \\(N\\) is the number of patients, and we consider the following 
             hypotesys test with binary data:", 
      "\\(\\cases{H_0: p = p_0 \\newline H_1: p = p_1 }  \\)",
      "where \\(p_0\\) is the response rate for the standard therapy and \\(p_1\\) is
      the targeted response rate for the experimental therapy under investigation."),
      
      
      h6("Fixed sample size design"),
      p("First we consider the fixed sample size design without any interim analysis to obtain
             the maximal sample size \\(N\\). We would like to have a", strong("power"), "1 − \\(\\beta\\), 
             and a", strong("Type I error rate"), "\\(\\alpha\\). Let \\(X_n = \\sum_{i = 1}^{n} X_i\\) the total 
             number of responses out of \\(n\\) patients where \\(X_n\\) has a binomial distribution.
             The sample size \\(N\\) and the critical boundary \\(R\\) (such that if
             \\(X_N > R\\) we reject \\(H_0\\)) have to satisfy",
      tags$ul(
        tags$li("\\(P( \\) reject \\(H_0\\) | \\(H_0\\) is true \\()\\) = \\(P(X_N > R  | p = p_0) \\) = \\(1 - B(R-1, N, p_0) ≤ \\alpha \\)"),
        tags$li("\\(1 - B(R - 1, N, p_1) ≥ 1 - \\beta \\)")
      ),
      ),
      h6("Monitoring regions and sample size re-estimation"),
        p("Suppose we wanto to conduct the interim analysis when \\(n_1\\) patients are accrued.
        If a total of \\(R\\) responses is necessary to reject the null hypothesis at the final analysis, 
        then we want to spend a small amount of \\(\\alpha\\), denoted by \\(\\alpha_1\\), for early stop for
overwhelmingly strong efficacy at the interim analysis. Given \\(n_1\\) and the threshold number of 
responses needed for early termination of the trial for overwhelming efficacy, we are able to determine
        \\(\\alpha_1\\) and then, after that, we need to adjust \\(R\\) to \\(R'\\) \\((R ≥ R')\\).",
          br(),
        "Suppose we observe \\(X_{n_1}\\) responses after the interim analysis.
        We calculate the", strong("conditional power"), "under the", strong("alternative hypothesys"), "and under", strong("the current trend:"),
        tags$ul(
          tags$li("\\(CP_a = 1 - B(R - X_{n_1} -1, N - n_1, p_1)\\),"),
          tags$li("\\(CP_c = 1 - B(R - X_{n_1} -1, N - n_1, X_{n_1}/n_1)\\).")
        ),
        "If the conditional power:",
        tags$ul(
          tags$li("\\( 0.05 < CP ≤ 0.9\\) then the trial is in the", strong("hopeful region"), "and it should", strong("continue"), ";"),
          tags$li("\\(CP < 0.05\\) the trial is", strong("hopeless"), "and may be terminate early due to", strong("futility"), ";"),
          tags$li("\\(CP > 0.9\\) the results fall into the ", strong("favorable"), "region and the trial may be terminate early because is", strong("overwhelmingly positive"), "."),
        )),
      p("If our trial is in the", strong("hopeful"), "region it should continue; when continuing the trial, 
      we may either keep the original sample size \\(N\\) or increase the sample size beyond \\(N\\) (denoted with \\(N^*\\)) 
      to enhance the power at a desired level depending on the observed response rate at interim.",
        br(),
        "To", strong("re-estimate the sample size"), "the new values of \\(N^*\\) and \\(R^*\\) must satisfy these two conditions:",
        tags$ul(
          tags$li("\\( CP_0(N^*, R^*) ≤ CP_0(N, R)\\), where \\(CP_0\\) is the conditional power under \\(H_0\\);"),
          tags$li("\\( CP_0(N^*, R^*) ≥ CP_{required}\\), where \\(CP_{required}\\) is a desired level of conditional power."),
        ),
        "All solutions for \\( N^*\\) and \\(R^*\\) that satisfy the condition above are feasible solutions. 
        The", strong("optimal solution"), "\\( (N^*, R^*)\\) is the one where \\(N^*\\) is the smallest new 
        sample size among all the feasible solutions."
        )
    )
  })
  
  # ---------------  ######## --- The Plot --- ######## --------------- #
  
  output$plot1 <- renderPlot({
       plot(plot_1()$values$N, plot_1()$values$EN, xlim = c(input$N[1], input$N[2]),
            xlab = "N (total number of subjects)",
            pch = 19, cex = 3, ylab = "EN (expected sample size under H0)",
            main = "Two stage Phase II Design", cex.axis = 1.2, cex.main = 1.7, cex.lab = 1.2)
       grid()
       lines(plot_1()$values$N, plot_1()$values$EN, type = "l", lty = 1, col = "lightgrey")
       points(plot_1()$values$N, plot_1()$values$EN, col = "#F4F0EC", pch = 19, cex = 2.8)
       points(x= c(plot_1()$optimal$N, plot_1()$minmax$N), y = c(plot_1()$optimal$EN, plot_1()$minmax$EN),
              col = c("#FD8204", "#121F6B"), pch = 19, cex = 2.8)
       legend("topright", legend= c("Minmax design", "Optimal design"),
              pch = rep(19, 2), cex = c(1.5, 1.5), col = c("#121F6B", "#FD8204"))
  }, height = 370, width = 670)
  
  # ---------------  ######## --- The table --- ######## --------------- # 
  
  output$tab <- renderDataTable({
    
    N_r <- round(plot_1()$values$N, 2); EN_r <- round(plot_1()$values$EN, 2)
    PET0_r <- round(plot_1()$values$Pet0, 2)
    lab <- rep(1:length(N_r)); lab[1] <- "Minmax"; lab[which.min(EN_r)] <- "Optimal"
    data <- as.data.frame(cbind(plot_1()$values$r1, plot_1()$values$n1, plot_1()$values$r, 
                          N_r, EN_r, PET0_r))
    row.names(data) <- lab
    colnames(data) <- c("r1", "n1", "r", "N", "EN", "Pet0")
    col_dat <- c("\\(r_1\\)", "\\(n_1\\)", "\\(r\\)", "\\(N\\)", "\\(EN\\)", "\\(PET_0\\)")
      brushedPoints(data, brush = input$plot_brush, xvar = "N", yvar = "EN") %>% # very nice
        datatable(caption = "Select on the graph the values you want to display on the table.")
  })
  
  # ---------  ######## --- Some text for the modified section --- ######## -------- #
  
  output$textReg <- renderUI({
    N1 <- fix_des()$N
    R1 <- fix_des()$R
    cpc <- CPc(N1, R1, input$X, input$n1)
    tagList(
      withMathJax(),
      p("Suppose we want to do the interim analysis once \\(n_1\\) = ", input$n1, " patients 
        are enrolled. Then suppose we observe \\(X_{n_1}\\) = ", input$X, " responses.", 
        br(),
        "We can draw the following monitornig regions' graph to see where our trial is located.",
        tags$ul(
          tags$li("If we are in the", strong("favorable"), "region the trial is overwhelmingly 
      positive and may be terminated early."),
          tags$li("If we are in the", strong("unfavorable"), "region the trial may be terminated early
        due to futility."),
          tags$li("If we are in the", strong("hopeful"), "region, under ", em("suitable conditions"), 
          " we can determine \\(N^*\\) and \\(R^*\\) such that we can enhance the probability of rejecting \\(H_0\\)."),
        )
      )
    )
  })
  
  # ---------------  ######## --- The regions plots --- ######## --------------- #
  
  output$regions <- renderPlot({
    print_regions_2(N = fix_des()$N, R = fix_des()$R, p = input$p1, lev1 = 0.9,
                  lev2 = 0.05, string = "Monitoring Regions - Under Alternative hypothesis")
    points(input$n1, input$X, pch = "O", cex = 1.4, col = "green")
    set.seed(123)
    orange <- cbind(c(1:fix_des()$N), sort(rbinom(fix_des()$N, c(1:fix_des()$N), prob = input$p0)))
    #lines(orange[,1], orange[,2], lty = 2, lwd = 3, col = "#5D8AA8")
    legend("topleft", col = c("#8AB3B8", "#FD8204",  "#121FC5", "#E62020"),
           lty = c(rep(1, 3), 2), 
           lwd = c(rep(10, 3), 2), legend = c("Favorable", "Hopeful", "Unfavorable", "CPa = 0.5"))
  }, height = 370, width = 670)
  
  output$regions2 <- renderPlot({
    print_regions_2(N = fix_des()$N, R = fix_des()$R, p = input$X/input$n1, lev1 = 0.9,
                  lev2 = 0.05, string = "Monitoring Regions - Under the Current Trend")
    points(input$n1, input$X, pch = "O", cex = 1.4, col = "green")
    set.seed(123)
    orange <- cbind(c(1:fix_des()$N), sort(rbinom(fix_des()$N, c(1:fix_des()$N), prob = input$p0)))
    # lines(orange[,1], orange[,2], lty = 2, lwd = 3, col = "#5D8AA8")
    legend("topleft", col = c("#8AB3B8", "#FD8204",  "#121FC5", "#E62020"),
           lty = c(rep(1, 3), 2), 
           lwd = c(rep(10, 3), 2), legend = c("Favorable", "Hopeful", "Unfavorable", "CPc = 0.5"))
  }, height = 370, width = 670)
  
  output$text <- renderUI({
    pt1 <- print_regions_2(N = fix_des()$N, R = fix_des()$R, p = input$X/input$n1, lev1 = 0.9,
                        lev2 = 0.05, string = "Monitoring Regions - Under the Current Trend",
                        res = TRUE)
    pt2 <- print_regions_2(N = fix_des()$N, R = fix_des()$R, p = input$p1, lev1 = 0.9,
                           lev2 = 0.05, string = "Monitoring Regions - Under the Alternative Hypothesis",
                           res = TRUE)
    v <- 0
    for(i in 1:nrow(pt1$red)){
      if(pt1$red[i,1] == c(input$n1, input$X)[1] & pt1$red[i,2] == c(input$n1, input$X)[2]){
        print("There is no feasible solition with the current values (under alternative hypothesis).")
        v <- 1
      } 
    }
    for(i in 1:nrow(pt2$red)){
      if(pt2$red[i,1] == c(input$n1, input$X)[1] & pt2$red[i,2] == c(input$n1, input$X)[2]){
        print("There is no feasible solition with the current values (under the current values).")
        v <- 2
      } 
    }
    for(i in 1:nrow(pt1$green)){
      if(pt1$green[i,1] == c(input$n1, input$X)[1] & pt1$green[i,2] == c(input$n1, input$X)[2]){
        print("There is no feasible solition with the current values (under alternative hypothesis).")
        v <- 1
      } 
    }
    for(i in 1:nrow(pt2$green)){
      if(pt2$green[i,1] == c(input$n1, input$X)[1] & pt2$green[i,2] == c(input$n1, input$X)[2]){
        print("There is no feasible solition with the current values (under the current values).")
        v <- 2
      } 
    }
    if(v == 0){
      if(is.error(my_text()) == TRUE) print("There is no feasible solition with the current values.")
      else{
        N1 <- fix_des()$N
        R1 <- fix_des()$R
        N_new <- my_text()$N_new
        R_new <- my_text()$R_new
        tagList(
          withMathJax(),
          p("We want to increase the sample size to achieve at least ", input$lev ," conditional
            power under the current trend. The values for \\(N\\) and \\(R\\) for the", strong("fixed design"), "are \\(N\\) = ", N1, " and \\(R\\) = ", R1, ". Then after the
      interim analysis we update \\(R\\) to \\(R'\\) = ", my_text()$R_, "\\((R' ≥ R)\\). Adjusting following the 
        theory of the", strong("Simon's Modified Two Stage Design"), "we have \\(N^*\\) = ", N_new, " and \\(R^*\\) = ", R_new, ".")
        )
      }
    }
    
    
  })

  
  # ---------------  ######## --- All the regions plots --- ######## --------------- # 
  
  output$all_reg_theory <- renderUI({
    tagList(
      withMathJax(),
      p("Here are presented the monitoring regions, under the alternative hypothesis and
        under the currrent trend, with data (blue dotted line) generated when the true response rates are
        \\(p_0\\), \\(p_1\\) and \\(\\bar{p} = \\frac{p_0 + p_1}{2}\\).",
        br(),
        "More can be found", a("here in Section 6.", href = "https://www.tandfonline.com/doi/full/10.1080/19466315.2023.2177332?src=")),
      )})
  output$text_all_reg <- renderUI({
    tagList(
      withMathJax(),
      h2("The monitoring regions under the alternative hypothesis"),
    )
  })
  
  output$all_regions <- renderPlot({
    par(mfrow = c(2, 2), mar = c(4, 4, 1, 1))
    print_regions_2(N = fix_des()$N, R = fix_des()$R, p = input$p1, lev1 = 0.9,
                    lev2 = 0.05, string = "True response rate is p0")
    set.seed(123)
    orange <- cbind(c(1:fix_des()$N), sort(rbinom(fix_des()$N, c(1:fix_des()$N), prob = input$p0)))
    lines(orange[,1], orange[,2], lty = 2, lwd = 3, col = "#5D8AA8")
    legend("topleft", col = c("#8AB3B8", "#FD8204",  "#121FC5", "#E62020"),
           lty = c(rep(1, 3), 2), 
           lwd = c(rep(10, 3), 2), legend = c("Favorable", "Hopeful", "Unfavorable", "CPa = 0.5"))
    
    print_regions_2(N = fix_des()$N, R = fix_des()$R, p = input$p1, lev1 = 0.9,
                    lev2 = 0.05, string = "True response rate is p1")
    set.seed(123)
    orange <- cbind(c(1:fix_des()$N), sort(rbinom(fix_des()$N, c(1:fix_des()$N), prob = input$p1)))
    lines(orange[,1], orange[,2], lty = 2, lwd = 3, col = "#5D8AA8")
    legend("topleft", col = c("#8AB3B8", "#FD8204",  "#121FC5", "#E62020"),
           lty = c(rep(1, 3), 2), 
           lwd = c(rep(10, 3), 2), legend = c("Favorable", "Hopeful", "Unfavorable", "CPa = 0.5"))
    
    print_regions_2(N = fix_des()$N, R = fix_des()$R, p = input$p1, lev1 = 0.9,
                    lev2 = 0.05, string = "True response rate is p01")
    set.seed(123)
    orange <- cbind(c(1:fix_des()$N), sort(rbinom(fix_des()$N, c(1:fix_des()$N), prob = (input$p0+input$p1)/2)))
    lines(orange[,1], orange[,2], lty = 2, lwd = 3, col = "#5D8AA8")
    legend("topleft", col = c("#8AB3B8", "#FD8204",  "#121FC5", "#E62020"),
           lty = c(rep(1, 3), 2), 
           lwd = c(rep(10, 3), 2), legend = c("Favorable", "Hopeful", "Unfavorable", "CPa = 0.5"))
    
  }, height = 370, width = 670)
  
  output$text_all_reg2 <- renderUI({
    tagList(
      withMathJax(),
      h2("The monitoring regions under the current trend"),
      )
  })
  
  
  output$all_regions2 <- renderPlot({
    par(mfrow = c(2, 2), mar = c(4, 4, 1, 1))
    print_regions_2(N = fix_des()$N, R = fix_des()$R, p = input$X/input$n1, lev1 = 0.9,
                    lev2 = 0.05, string = "True response rate is p0")
    set.seed(123)
    orange <- cbind(c(1:fix_des()$N), sort(rbinom(fix_des()$N, c(1:fix_des()$N), prob = input$p0)))
    lines(orange[,1], orange[,2], lty = 2, lwd = 3, col = "#5D8AA8")
    legend("topleft", col = c("#8AB3B8", "#FD8204",  "#121FC5", "#E62020"),
           lty = c(rep(1, 3), 2), 
           lwd = c(rep(10, 3), 2), legend = c("Favorable", "Hopeful", "Unfavorable", "CPc = 0.5"))
    
    print_regions_2(N = fix_des()$N, R = fix_des()$R, p = input$X/input$n1, lev1 = 0.9,
                    lev2 = 0.05, string = "True response rate is p1")
    set.seed(123)
    orange <- cbind(c(1:fix_des()$N), sort(rbinom(fix_des()$N, c(1:fix_des()$N), prob = input$p1)))
    lines(orange[,1], orange[,2], lty = 2, lwd = 3, col = "#5D8AA8")
    legend("topleft", col = c("#8AB3B8", "#FD8204",  "#121FC5", "#E62020"),
           lty = c(rep(1, 3), 2), 
           lwd = c(rep(10, 3), 2), legend = c("Favorable", "Hopeful", "Unfavorable", "CPc = 0.5"))
    
    print_regions_2(N = fix_des()$N, R = fix_des()$R, p = input$X/input$n1, lev1 = 0.9,
                    lev2 = 0.05, string = "True response rate is p01")
    set.seed(123)
    orange <- cbind(c(1:fix_des()$N), sort(rbinom(fix_des()$N, c(1:fix_des()$N), prob = (input$p0+input$p1)/2)))
    lines(orange[,1], orange[,2], lty = 2, lwd = 3, col = "#5D8AA8")
    legend("topleft", col = c("#8AB3B8", "#FD8204",  "#121FC5", "#E62020"),
           lty = c(rep(1, 3), 2), 
           lwd = c(rep(10, 3), 2), legend = c("Favorable", "Hopeful", "Unfavorable", "CPc = 0.5"))
  }, height = 370, width = 670)
  
}


# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)
