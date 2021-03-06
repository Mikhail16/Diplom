
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(plotly)
library(ggplot2movies)
shinyServer(function(input, output, session) {
       
         selectedData2<-reactive({
                table<-matrix(,nrow = 1,ncol=8)
                table[1,1]<-as.numeric(input$textd)
                table[1,2]<-as.numeric(input$textdel)
                table[1,3]<-as.numeric(input$textE)
                table[1,4]<-as.numeric(input$textP)
                
                if (input$radio == "nafta") {
                        # ro <- (825)
                        table[1,5] <- (840)      
                        #ksi <- (9.7*10^-6)
                        table[1,6]  <- (5*10^-6) # kinemat vyazkist'
                        if (input$textT == 273) {
                                table[1,7] <- 1400
                        }
                        if (input$textT == 283) {
                                table[1,7] <- 1332
                        }
                        if (input$textT == 293) {
                                table[1,7] <- 1292
                        }
                        if (input$textT == 303) {
                                table[1,7] <- 1253
                        }
                        if (input$textT == 313) {
                                table[1,7] <- 1216
                        }
                }
                if (input$radio == "palevo") {
                        table[1,5] <- (860)
                        table[1,6] <- (4.5*10^-6)
                        updateSelectInput(session, "textT", choices = ("293" = 293))
                        table[1,7] <- 1392
                }
                if (input$radio == "gas") {
                        table[1,5] <- (400)
                        table[1,6] <- (14.3*10^-6)
                        updateSelectInput(session, "textT", choices = ("293" = 293))
                        table[1,7]<-460
                }
                if (input$radio == "povitrya") {
                  table[1,5] <- (1.203268)
                  table[1,6] <- (1.49663*10^-5)
                  updateSelectInput(session, "textT", choices = ("293" = 293))
                  table[1,7]<-sqrt(1.4019*table[1,4]/table[1,5])
                }
                if (input$radio == "amiak") {
                        table[1,5] <- (0.771)
                        table[1,6] <- (1.20623*10^-5)
                        updateSelectInput(session, "textT", choices = ("293" = 293))
                        table[1,7]<-sqrt(1.31*table[1,4]/table[1,5]) 
                }
                if (input$radio == "metan") {
                        table[1,5] <- (0.667016)
                        table[1,6] <- (1.55946*10^-5)
                        updateSelectInput(session, "textT", choices = ("293" = 293))
                        table[1,7]<-sqrt(1.31*table[1,4]/table[1,5]) 
                }
                table[1,8]<-as.numeric(input$textT)
                colnames(table)<-c("d","del","E","P","ro","ksi","c","T")
                table
                
                
        })
        

  output$rezultC <- renderText({
        data<-as.data.frame(selectedData2())
        B <- (data$c ^ 2) * data$ro
        Ct <- data$c / sqrt( 1 + (data$d * B) / (data$del * data$E) )
        rezultC <- Ct
  })
  
    output$rezultf <- renderText({
      data<-as.data.frame(selectedData2())
      updateTextInput(session, "textc", value = data$c)
      updateTextInput(session, "textro", value = data$ro)
      updateTextInput(session, "textksi", value = data$ksi)
      r <- (data$d / 2)
      f <- (0.61 * data$c / r)
      #fmax <- f%/%1
      fmax <- floor(f)
      updateSliderInput(session, "rezultf_sl", max = fmax)
      rezultf <- f
    })
  
        
      output$rezultBet <- renderText({
        data<-as.data.frame(selectedData2())
        eta <- data$ksi * data$ro
        updateTextInput(session, "texteta", value = eta)
        b <- (4 / 3 * eta + data$ksi)
        w <- 2 * pi * input$rezultf_sl
        a <- (data$d / 2)
        #Bet <- ( (b * w^2) / (2 * c^3 * ro) ) + ( (1 / a) * ( (eta * w) / (2 * c^2 * ro)  )^(1/2) )
        Bet <- ( (b * w^2) / (2 * data$c^3 * data$ro) ) + ( (1 / a) * ( (eta * w) / (2 * data$c^2 * data$ro)  )^(1/2) )*8.68*1000
        output$rezultCw <- renderText({
          Cw <- data$c *( 1 - (eta / (2 * data$ro * w * a^2))^(1/2) )
          rezultCw <- Cw
        })
        rezultBet <- Bet
      })
      
      
    output$TempPlot1 <- renderPlot({
        data<-as.data.frame(selectedData2())
        B <- (data$c ^ 2) * data$ro
      x <- data.frame( nafta = c("nafta", "nafta", "nafta", "nafta", "nafta"), Temperatura = c(273, 283, 293, 303, 313), C = c(1374, 1332, 1292, 1253, 1216) )
     # x$Ct_ <- 1/sqrt( 1 + (data$d * data$B) / (data$del * data$E) )
      x$Ct_ <- 1/sqrt( 1 + (data$d * B) / (data$del * data$E) )
      ggplot(x, aes(Temperatura, y = value, color = variable))+ 
        
        geom_line(aes(y = Ct_, col = "Ct_"))+
        
        geom_point(aes(y = Ct_, col = "Ct_"))
      
    })
    
    output$TempPlot <- renderPlot({
        data<-as.data.frame(selectedData2())
        B <- (data$c ^ 2) * data$ro
      x <- data.frame( nafta = c("nafta", "nafta", "nafta", "nafta", "nafta"), Temperatura = c(273, 283, 293, 303, 313), C = c(1374, 1332, 1292, 1253, 1216) )
      x$Ct <- x$C/sqrt( 1 + (data$d * B) / (data$del * data$E) )

      ggplot(x, aes(Temperatura, y = value, color = variable))+ 
        geom_line(aes(y = C, col = "C")) + 
        geom_line(aes(y = Ct, col = "Ct"))+
        geom_point(aes(y = C, col = "C"))+
        geom_point(aes(y = Ct, col = "Ct"))
      
    })
    
    #згасання в середовищі
    selectedData3<-reactive({
            # colnames(table)<-c("d","del","E","P","ro","ksi","c","T")
            data<-as.data.frame(selectedData2())
            eta <- data$ksi * data$ro
            updateTextInput(session, "texteta", value = eta)
            b <- (4 / 3 * eta + data$ksi)
            #change of diaments
            dd<-seq(0.025,data$d, 0.025)
            w<-NULL
            a<-NULL
            Bet<-NULL
            r <- (data$d / 2)
            f <- (0.61 * data$c / r)
            freq<-seq(as.numeric(input$textfreq1),as.numeric(input$textfreq2),50)
            Bet<-matrix(, nrow = length(freq), ncol = length(dd))
            
            for(i in (1:length(freq))){
                    w[i] <- 2 * pi *freq[i]  
                    for(j in (1:length(dd))){
                            a[j] <- (dd[j] / 2)
                            Bet[i,j] <- ( (b * w[i]^2) / (2 * data$c^3 * data$ro) ) + ( (1 / a[j]) * ( (eta * w[i]) / (2 * data$c^2 * data$ro)  )^(1/2) )*8.68*1000
                    }
                    
            }
            Bet 
            
    })

  
  output$trendPlot <- renderPlotly({
    data<-as.data.frame(selectedData2())
    dd<-seq(0.025,data$d, 0.025)
    r <- (data$d / 2)
    f <- (0.61 * data$c / r)
    freq<-seq(as.numeric(input$textfreq1),as.numeric(input$textfreq2),50)
    Bet<-selectedData3()
    
    
    plot_ly(x = dd, y = freq, z = Bet, type = "surface")
  })
  
  output$trendс1 <-renderPlotly({
          data<-as.data.frame(selectedData2())
        
          
          eta <- data$ksi * data$ro
          updateTextInput(session, "texteta", value = eta)
          b <- (4 / 3 * eta + data$ksi)
         
          #change shvydkist
          dd<-seq(0.025,data$d, 0.025)
          w<-NULL
          a<-NULL
          Cw<-NULL
          r <- (data$d / 2)
        
          freq<-seq(as.numeric(input$textfreq1),as.numeric(input$textfreq2),50)
          Cw<-matrix(, nrow = length(freq), ncol = length(dd))
          
          for(i in (1:length(freq))){
                  w[i] <- 2 * pi *freq[i]  
                  for(j in (1:length(dd))){
                          a[j] <- (dd[j] / 2)
                          Cw[i,j] <- data$c *( 1 - (eta / (2 * data$ro * w[i] * a[j]^2))^(1/2) )
                  }
                  
          }
          Cw 
          plot_ly(x = dd, y = freq, z = Cw, type = "surface")
  })
  
  Data4<-reactive({
          data<-as.data.frame(selectedData2())
          dd<-seq(0.025,data$d, 0.025)
          w<-NULL
          a<-NULL
          Y<-NULL
          k<-NULL
          Y<-2*pi*(as.numeric(input$textdotv)/2)^2/(2*as.numeric(input$textlotv)+pi*(as.numeric(input$textdotv)/2))
          freq<-seq(as.numeric(input$textfreq1),as.numeric(input$textfreq2),50)
          Vabs<-matrix(, nrow = length(freq), ncol = length(dd))
          B1<-matrix(, nrow = length(freq), ncol = length(dd))
          A1<-matrix(, nrow = length(freq), ncol = length(dd))
          for(i in (1:length(freq))){
                  k[i] <- 2 * pi *freq[i]/data$c  
                  for(j in (1:length(dd))){
                          B1[i,j]<-data$ro*data$c*k[i]/Y;
                          A1[i,j]<-data$ro*data$c/(pi*(dd[j]/2)^2)
                          Vabs[i,j] <- sqrt((A1[i,j]^2/(A1[i,j]^2+4*B1[i,j]^2))^2+(2*A1[i,j]*B1[i,j]/(A1[i,j]^2+4*B1[i,j]^2))^2)
                  }
                  
          }
          Vabs 
  })
  
  output$trendVabs<-renderPlotly({
          data<-as.data.frame(selectedData2())
          dd<-seq(0.025,data$d, 0.025)
         
          freq<-seq(as.numeric(input$textfreq1),as.numeric(input$textfreq2),50)
          Vabs<-Data4()
          plot_ly(x = dd, y = freq, z =Vabs , type = "surface")
          
  })
  
  Dataotvir<-reactive({
          data<-as.data.frame(selectedData2())
         
          if (input$radio1 == "otvir") {
                  freq<-seq(as.numeric(input$textfreq1),as.numeric(input$textfreq2),25)
                  h<-seq(0.002,0.04,0.00125)
                  dotv<-as.numeric(input$textdotv)
                  hotv<-as.numeric(input$textlotv)
                  Y<-matrix(, nrow = length(freq), ncol = length(h))
                  k<-matrix(, nrow = length(freq), ncol = length(h))
                  Vabs1<-matrix(, nrow = length(freq), ncol = length(h))
                  B1<-matrix(, nrow = length(freq), ncol = length(h))
                  A1<-matrix(, nrow = length(freq), ncol = length(h))
                  for(i in (1:length(freq))){
                          for(j in (1:length(h))){
                                 
                                  Y[i,j]<-2*pi*(dotv/2)^2/(2*h[j]+pi*(dotv/2))
                                  k[i,j]<-2*pi*freq[i]/data$c
                                  B1[i,j]<-data$ro*data$c*k[i,j]/Y[i,j]
                                  A1[i,j]<-data$ro*data$c/(pi*(as.numeric(input$textd)/2)^2)
                                  Vabs1[i,j] <- sqrt((A1[i,j]^2/(A1[i,j]^2+4*B1[i,j]^2))^2+(2*A1[i,j]*B1[i,j]/(A1[i,j]^2+4*B1[i,j]^2))^2)
                                  
                          } }
                  
                  output$rezult <- renderText({
                          
                          
                          Y1<-2*pi*(dotv/2)^2/(2*hotv+pi*(dotv/2))
                          k1<-2*pi*as.numeric(input$rezultf_sl)/data$c
                          B11<-data$ro*data$c*k1/Y1
                          A11<-data$ro*data$c/(pi*(as.numeric(input$textd)/2)^2)
                          Vabs11 <- sqrt((A11^2/(A11^2+4*B11^2))^2+(2*A11*B11/(A11^2+4*B11^2))^2)
                          rezult1 <- abs(20*log10(Vabs11))
                          ####
                          data<-as.data.frame(selectedData2())
                          eta <- data$ksi * data$ro
                          updateTextInput(session, "texteta", value = eta)
                          b <- (4 / 3 * eta + data$ksi)
                          w <- 2 * pi * input$rezultf_sl
                          a <- (data$d / 2)
                          #Bet <- ( (b * w^2) / (2 * c^3 * ro) ) + ( (1 / a) * ( (eta * w) / (2 * c^2 * ro)  )^(1/2) )
                          Bet <- ( (b * w^2) / (2 * data$c^3 * data$ro) ) + ( (1 / a) * ( (eta * w) / (2 * data$c^2 * data$ro)  )^(1/2) )*8.68*1000
                          
                          ####
                          output$rezultPower<-renderText({
                                  rezultPower<-(as.numeric(input$textPower)- rezult1)/(2*Bet)
                          })
                          rezult<-rezult1
                          
                  })
                  
                  }
          if (input$radio1 == "elips") {
                  botv<-as.numeric(input$textdotv)/2
                  h<-as.numeric(input$textlotv)
                  ee<-NULL
                  aotv<-NULL
                  b<-NULL
                  q<-NULL
                  b<-seq(0.0005,botv*2, 0.0005)
                  q<-seq(1,30,1)
                  Ke<-matrix(, nrow = length(b), ncol = length(q))
                  Ktr<-matrix(, nrow = length(b), ncol = length(q))
                  Y1<-matrix(, nrow = length(b), ncol = length(q))
                  Vabs1<-matrix(, nrow = length(b), ncol = length(q))
                  B1<-matrix(, nrow = length(b), ncol = length(q))
                  A1<-matrix(, nrow = length(b), ncol = length(q))
                  ee1<-matrix(, nrow = length(b), ncol = length(q))
                  for(i in (1:length(b))){
                         # aotv[i] <- q[i]*botv  
                         # ee[i]<-sqrt(1-botv^2/aotv[i]^2) 
                          
                          for(j in (1:length(q))){
                                ee1[i,j]<-sqrt(1-1/q[j]^2) 
                                Ke[i,j]<-2*sqrt(pi*b[i]^2*q[j]/pi)*(1+ee1[i,j]^4/64+ee1[i,j]^8/64)
                                Ktr[i,j]<-pi*b[i]^2*q[j]/h
                                Y1[i,j]<-Ktr[i,j]*Ke[i,j]/(Ktr[i,j]+Ke[i,j])
                               # k<-2*pi*50/data$c
                                k<-2*pi*as.numeric(input$rezultf_sl)/data$c
                                B1[i,j]<-data$ro*data$c*k/Y1[i,j]
                                A1[i,j]<-data$ro*data$c/(pi*(as.numeric(input$textd)/2)^2)
                                Vabs1[i,j] <- sqrt((A1[i,j]^2/(A1[i,j]^2+4*B1[i,j]^2))^2+(2*A1[i,j]*B1[i,j]/(A1[i,j]^2+4*B1[i,j]^2))^2)
                                
                          } }
                  
                  output$rezult <- renderText({
                      
                          rezult1 <- abs(20*log10(Vabs1[which(b==botv), which(q==3)]))
                                  #Vabs1[which(b==botv), which(q==3)]
                          ####
                          
                          
                          data<-as.data.frame(selectedData2())
                          eta <- data$ksi * data$ro
                          updateTextInput(session, "texteta", value = eta)
                          b <- (4 / 3 * eta + data$ksi)
                          w <- 2 * pi * input$rezultf_sl
                          a <- (data$d / 2)
                          #Bet <- ( (b * w^2) / (2 * c^3 * ro) ) + ( (1 / a) * ( (eta * w) / (2 * c^2 * ro)  )^(1/2) )
                          Bet <- ( (b * w^2) / (2 * data$c^3 * data$ro) ) + ( (1 / a) * ( (eta * w) / (2 * data$c^2 * data$ro)  )^(1/2) )*8.68*1000
                          
                          ####
                          output$rezultPower<-renderText({
                                  rezultPower<-(as.numeric(input$textPower)- rezult1)/(2*Bet)
                          })
                          rezult<-rezult1
                          
                  })
                  
                 }
          if (input$radio1 == "close") {
                  
          }
          Vabs1
          #ee1
          
         })
  
  output$trendVabs1<-renderPlotly({
        #  data<-as.data.frame(selectedData2())
          if (input$radio1 == "elips") {
          b<-seq(0.0005,as.numeric(input$textdotv), 0.0005)
          qq<-seq(1,30,1)}
          if (input$radio1 == "otvir") {
                  b<-seq(as.numeric(input$textfreq1),as.numeric(input$textfreq2),25)
                 qq<-seq(0.002,0.04,0.00125)
          }
          
       #   dd<-seq(0.001,as.numeric(input$textdotv)/2, 0.001)
#          kk<-seq(1,30,1)
          Vabs1 <- 20*log10(Dataotvir())
         p<- plot_ly(x = qq, y = b, z =Vabs1 , type = "surface") %>%
                  layout(title = "Різниця амплітуд відбитого і падаючого сигналів від довжини відгалуження та чатоти несучої сигналу",
                         scene = list(
                                 xaxis = list(title = "Довжина відгалуження, м"), 
                                 yaxis = list(title = "Частота, Гц"), 
                                 zaxis = list(title = "Спад амплітуди, дБ")))
          p
          
  })
  
  selectedData1<-reactive({
          table<-matrix(1:6,nrow = 2,ncol = 3)
  })
  output$table1<-renderTable({
         # Data4()
          Dataotvir()
  })
  br()
  output$table2<-renderTable({
          selectedData2()
  })

  br()
  output$table3<-renderTable({
          selectedData3()
  })
})
