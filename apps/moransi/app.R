library(MASS)
library(colorspace)
library(fields)
library(geoR)
library(mgcv)

# Global variables, which will be shared across all instances
load("lakecoords.RData")
load("peipsi.RData")
res <- 50
xbase <- seq(min(lakecoords[,1]), max(lakecoords[,1]), len=res)
ybase <- seq(min(lakecoords[,2]), max(lakecoords[,2]), len=res)
grid <- expand.grid(lon=xbase, lat=ybase)
keep <- in.out(bnd=lakecoords,x=as.matrix(grid))
grid <- grid[keep,]
grid.length <- nrow(grid)
grid <- rbind(grid, peipsi[,1:2])
dists <- sqrt(outer(grid$lon, grid$lon, "-")^2+outer(grid$lat, grid$lat, "-")^2)
dists <- matrix(dists, nrow=nrow(grid))

ui <- fluidPage(

  titlePanel("Variograms and Kriging"),

  sidebarLayout(

      sidebarPanel(
          fluidRow(
              tags$h3("Parameters of Matérn kernel")
           ),
          fluidRow(
                  sliderInput(inputId = "phi", step=0.01, 
                      label = withMathJax("Correlation length \\(\\phi\\)"),
                      min=0.01, max=1, value=0.1
                      )
          ),
          fluidRow(
                  sliderInput(inputId = "kappa", step=0.25, 
                      label = withMathJax("Shape parameter \\(\\kappa\\)"),
                      min=0.25, max=2.5, value=1.5
                      )
          ),
          fluidRow(
              tags$h3("Signal and noise variance")
           ),
          fluidRow(
                  sliderInput(inputId = "tau2", step=0.5, 
                      label = withMathJax("Signal variance \\(\\tau^2\\)"),
                      min=0, max=30, value=20
                      )
          ),
          fluidRow(
                  sliderInput(inputId = "sigma2", step=0.5, 
                      label = withMathJax("Noise variance / \"Nugget effect\" \\(\\sigma^2\\)"),
                      min=0, max=30, value=5
                      )
          ),
          fluidRow(
              tags$h3("Resimulate")
          ),
           fluidRow(
              actionButton("resimulate", "Resimulate", icon=icon("refresh"))
          )
      ),
      mainPanel(
         tabsetPanel(type = "tabs",
                  tabPanel("Simulated signal and observations", plotOutput("map", height="600px")),
                  tabPanel("Estimated surface", plotOutput("maphat", height="600px"))
                  ),
          plotOutput("vario")
      )
  )
)

server <- function(input, output, session) {

    cov.chol <- reactive( {
        C <- matern(dists, phi=input$phi, kappa=input$kappa)
        chol(C+sqrt(.Machine$double.eps)*diag(nrow(C)))
    } )

    noise.values <- reactive( {
        input$resimulate
        list(noise=rnorm(nrow(grid)), white.noise=rnorm(nrow(peipsi)))
    } )
    
    signal <-  reactive( {
        s <- t(cov.chol())%*%noise.values()$noise
        s<- s * sqrt(input$tau2) + 10
        full.signal <- rep(NA, res^2)
        full.signal[keep] <- s[1:grid.length]
        full.signal <- matrix(full.signal, nrow=res) 
        lwst <-  s[-(1:grid.length)] + sqrt(input$sigma2) * noise.values()$white.noise
        list(full.signal=full.signal, lwst=lwst)
    } )

    plot.pars <- reactiveValues(plot.x=c(0,2))
    
    output$map <- renderPlot( {

        
        full.signal <- signal()$full.signal
        lwst <-  signal()$lwst
        
        zlim <- range(c(0,20,full.signal,lwst), na.rm=TRUE)
        grp <- as.integer(cut(c(zlim[1],zlim[2], lwst), breaks=64, labels=1:64)[-(1:2)])
        
        eqscplot(lakecoords, type="l", xaxt="n", yaxt="n", bty="n")
        cols = diverge_hcl(64)
        
        image.plot(xbase, ybase, matrix(full.signal, ncol=length(xbase)), col=cols, add=TRUE, zlim=zlim)
        points(peipsi[,1:2], pch=21, bg=cols[grp], cex=1.25)

        usr <- par()$usr
        plot.pars$plot.x <- par()$usr[1:2]
        
     }, height = 600 )
    
    output$vario <- renderPlot( {
        d <- seq(0, max(0.1,diff(plot.pars$plot.x)), length=1e2)
        dc <- matern(d, phi=input$phi, kappa=input$kappa)     
        plot(d, input$sigma2+input$tau2*(1-dc), xlab="Distance", ylab="Variogram", type="l", lwd=2, ylim=range(c(0,30,input$sigma2+input$tau2)))
        abline(h=input$sigma2+c(0,1)*input$tau2, lty=2)
        points(0, 0, pch=21, bg=1)
        points(0, input$sigma2, pch=21, bg=0)
        title("Semivariogram")
        
    } )

    output$maphat <- renderPlot( {
        lwst <-  signal()$lwst
        data <- as.geodata(cbind(peipsi$lon, peipsi$lat, lwst))

        reml.est <- likfit(data, ini.cov.pars=c(input$tau2, input$phi), method = "RML")

        
        kriging <- krige.conv(data, locations=grid[1:grid.length,],
                              krige=krige.control(obj.model=reml.est))
        
        zlim <- range(c(0,20,lwst, kriging$predict), na.rm=TRUE)
        grp <- as.integer(cut(c(zlim[1],zlim[2], lwst), breaks=64, labels=1:64)[-(1:2)])
        
        eqscplot(lakecoords, type="l", xaxt="n", yaxt="n", bty="n")
        cols = diverge_hcl(64)
        
        full.pred <- rep(NA, res^2)
        full.pred[keep] <- kriging$predict
        full.pred <- matrix(full.pred, nrow=res) 
        
       image.plot(xbase, ybase, matrix(full.pred, ncol=length(xbase)), col=cols, add=TRUE, zlim=zlim)
        points(peipsi[,1:2], pch=21, bg=cols[grp], cex=1.25)
        



    } )


}

# Create Shiny app ----
shinyApp(ui, server)


