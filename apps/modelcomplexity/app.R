library(shiny)

set.seed(123)
load("weatherdata.RData")
x <- sample(1:(365*2),30)
data <- weatherdata$temp_avg[x]
data <- data+0.5-runif(length(data))

fwd <- function(x)
    sign(x) * log(abs(x)+1)

bwd <- function(x)
    sign(x) * (exp(abs(x))-1)

unname.me <- function(x) {
    names(x) <- NULL
    x
}

create.harmonic.design <- function(x, p) {
    X <- matrix(1, nrow=length(x), ncol=1)
    if (p==1)
        return(X)
    X <- cbind(X, x/365)
    if (p==2)
        return(X)
    for (i in 1:((p-1)%/%2)) {
        X <- cbind(X, sin(pi*x/365*2^i))
        if (ncol(X)==p)
            return(X)
        X <- cbind(X, cos(pi*x/365*2^i))
    }
    return(X)
}

ui <- fluidPage(

  tags$head(tags$style(HTML('#sliders .irs-from, #sliders .irs-to, #sliders .irs-min, #sliders .irs-max, #sliders .irs-single {
            visibility: hidden !important;
    }
    #sliders label {
      display: none;
    }
    #sliders .form-group {
      margin-bottom: 0px !important;
    }'))),

  titlePanel("Model Complexity"),

  sidebarLayout(

      sidebarPanel(
          fluidRow(
              column(width=2,
                     div(1, style="border-radius:50%;border: 1px solid;text-align:center;width:32px; height:32px; font-size: 16pt")
                     ),
              column(width=10,
                  tags$p("Choose the complexity of the model which you think is appropriate."),
                       

                  sliderInput(inputId = "complexity",
                      label = "Model complexity (number of parameters)",
                      min=1, max=25, value=4, ticks=FALSE
                  )
             )
           ),

          fluidRow(style="margin-top: 10px",
              column(width=2,
                     div(2, style="border-radius:50%;border: 1px solid;text-align:center;width:32px; height:32px; font-size: 16pt")
                     ),
              column(width=10,
                  tags$p("Manually tune the parameters by moving the sliders below or press the \"Estimate\" button."),

                  actionButton("estimate", "Estimate parameters", icon=icon("cogs")),

                  tags$div(id="sliders",
                     sliderInput("parameter1", "Parameter 1", min=-4, max=4, value=0, step=0.001, ticks=FALSE)
                   )
              )           
          ),

          fluidRow(style="margin-top: 10px",
              column(width=2,
                     div(3, style="border-radius:50%;border: 1px solid;text-align:center;width:32px; height:32px; font-size: 16pt")
                     ),
              column(width=10,
                  tags$p("Check the quality of your forecast. Does it seem reliable and faithful to the data to you? How does it relate to the model complexity?",tags$br(),"If you are not happy with your forecast, adjust the model complexity and the parameters.")
              )           
          )


    ),

    mainPanel(
      plotOutput("predplot")
    )
  )
)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
    locals <- reactiveValues(p=1)

    observeEvent(input$estimate, {
        X <- create.harmonic.design(x, locals$p)
        colnames(X)  <-  paste0("X",1:ncol(X))
        dat <- data.frame(X, y=data)
        mod <- lm(y~.-1, data=dat)
        for (i in 1:locals$p) {
          updateSliderInput(session, paste0("parameter",i), value=unname.me(round(fwd(coef(mod)[i]),3)))
        }

    })
    
    observeEvent(input$complexity, {
        while (input$complexity>locals$p) {
            locals$p <- locals$p+1
            insertUI("div#sliders",
                   "beforeEnd",
                   div(id=paste0("parameterslider",locals$p),
                       sliderInput(paste0("parameter",locals$p),
                                   paste0("Parameter ",locals$p),
                                   min=-3, max=3, value=round(rnorm(1),3), step=0.001,
                                   ticks=FALSE)
                       )
                   )
        }
        while (input$complexity<locals$p) {
            removeUI(paste0("div#parameterslider",locals$p))
            locals$p <- locals$p-1
        }        
    } )



    
    output$predplot <- renderPlot( {
        plot(x, data, xlab="Years", ylab="Temperature", pch=16,xlim=c(1,365*3), ylim=range(data)+c(-10,10), xaxt="n")
        abline(v=2*365)
        axis(1, at=1+365*0:3,0:3)
#        X <- create.harmonic.design(x, locals$p)
#        colnames(X)  <-  paste0("X",1:ncol(X))
#        dat <- data.frame(X, y=data)
#        mod <- lm(y~.-1, data=dat)
#        xx <- seq(1, 365*3)
        XX <- create.harmonic.design(xx, locals$p)
#        colnames(XX) <- colnames(X)
                                        #        yy <- predict(mod, data.frame(XX), interval="prediction")# prediction")
        coef <- rep(0,locals$p)
        lst <- reactiveValuesToList(input)
        for (i in 1:locals$p) {
            v <- lst[[paste0("parameter",i)]]
            if (is.numeric(v))
                coef[i] <- bwd(v)
        }
        yy <- matrix(XX%*%coef, ncol=1)
        lines(xx, yy[,1], col="#850acc")
        sel <- (365*2):nrow(XX)
#        polygon(c(xx[sel],rev(xx[sel])), c(yy[sel,2],rev(yy[sel,3])), col="#fee658", border=NA)
        lines(xx[sel], yy[sel,1], col="#850acc", lwd=2)
    } )
    
}

# Create Shiny app ----
shinyApp(ui, server)
