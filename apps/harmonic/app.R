library(shiny)


ui <- fluidPage(

  titlePanel("Harmonic Regression"),

  sidebarLayout(

      sidebarPanel(
          fluidRow(
              tags$h3("Trend")
           ),
          fluidRow(
              tags$p(withMathJax("The parameters \\(\\beta_0\\) and \\(\\beta_1\\) control the intercept and slope of trend."))
           ),
          fluidRow(class="sliders",
                  sliderInput(inputId = "beta0", step=0.01, 
                      label = withMathJax("Intercept \\(\\beta_0\\)"),
                      min=-6, max=6, value=0, ticks=FALSE
                      )
                      ),
           fluidRow(class="sliders",
                  sliderInput(inputId = "beta1", step=0.01,
                      label = withMathJax("Slope \\(\\beta_1\\)"),
                      min=-0.5, max=0.5, value=0, ticks=FALSE
                      )
           ),
          fluidRow(
              tags$h3("Sesonal component")
           ),
           fluidRow(
               tags$p(withMathJax("You can control the phase shift and the amplitude of the sine curce by either adjusting \\(\\beta_2\\) and \\(\\beta_3\\), or by adjusting the sliders for phase shift \\(\\phi\\) and amplitide \\(A\\)."))
           ),
           fluidRow(           
               column(width=6,class="sliders",
                 sliderInput(inputId = "beta2", step=0.01,
                      label = withMathJax("\\(\\beta_2\\)"),
                      min=-4, max=4, value=0, ticks=FALSE
                      ),
                  sliderInput(inputId = "beta3", step=0.01,
                      label = withMathJax("\\(\\beta_3\\)"),
                      min=-4, max=4, value=0, ticks=FALSE
                      )
                 ),

               column(width=6,class="sliders",
                 sliderInput(inputId = "shift",
                      label = "Phase shift \\(\\phi\\)", step=0.01,
                      min=0, max=24, value=0, ticks=FALSE
                      ),
                  sliderInput(inputId = "amplitude",
                      label = "Amplitude \\(A\\)", step=0.01,
                      min=0, max=5.7, value=0, ticks=FALSE
                      )
                 )



               
            )
          

    ),

    mainPanel(
      tags$p(withMathJax("The plot below shows the function
$$
\\beta_0+\\beta_1 t + A\\cdot\ \\sin\\left(2\\pi \\frac{t+\\phi}{24}\\right),$$
which can be rewritten as $$
\\beta_0+\\beta_1 t + \\beta_2\\cdot \\sin\\left(2\\pi \\frac{t}{24}\\right)
+ \\beta_3\\cdot \\cos\\left(2\\pi \\frac{t}{24}\\right)
$$
")),
      plotOutput("funplot")
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
      observeEvent( {
       input$amplitude
       input$shift
      }, {
         f <- function(x) pmin(x, 4*sqrt(2))
         updateNumericInput(session, "beta2", value=f(input$amplitude)*cos(input$shift/24*2*pi))
         updateNumericInput(session, "beta3", value=f(input$amplitude)*sin(input$shift/24*2*pi))
        

    })
    
    observeEvent( {
      input$beta2
      input$beta3
    }, {
        f <- function(x) ifelse(x<0, x+2*pi, x)
        updateNumericInput(session, "amplitude", value=sqrt(input$beta2^2+input$beta3^2))
        updateNumericInput(session, "shift", value=24*f(atan2(input$beta3, input$beta2))/(2*pi))
        

    })


    output$funplot <- renderPlot( {
        t <- seq(1,100,len=1e5)
        y <- input$beta0+input$beta1*t+input$beta2*sin(2*pi*t/24) + input$beta3*cos(2*pi*t/24)
        plot(t, y, xlab="Time t", ylab="Mean respose", type="l", col="#850acc", lwd=2, ylim=c(-10,10), xaxt="n")
        axis(1, at=(0:4)*24)
     } )
    
}

# Create Shiny app ----
shinyApp(ui, server)
