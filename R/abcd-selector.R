#' Shiny module for abcd selection widget
#' @export
abcdUI <- function(id) {
  ns <- NS(id)
  tagList(
    plotOutput(ns("ABplot"),
               click=ns("selected_point"), height="200px"),
    div(uiOutput(ns("show_values")), style="width:200px;"),
    sliderInput(ns("cvalue"),
                "c: ",
                min = -1,
                max = 1,
                step = 0.01,
                value = 1),
    sliderInput(ns("dvalue"),
                "d: ",
                min = -1,
                max = 1,
                step = 0.01,
                value = 0)
  )
}

eigenvalues <- function(a, b, c=1, d=0) {
  discrim2 <- sqrt((a-d)^2 + 4*c*b + 0i)/2
  c(discrim2, -discrim2) + (a+d)/2
}

#' @export
abcdServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      avalue <- reactiveVal(0.5)
      bvalue <- reactiveVal(0)
      observe({
        if ( ! is.null(input$selected_point)) {
          avalue(input$selected_point$x)
          bvalue(input$selected_point$y)
        }
      })

      observe({
        lambdas <- eigenvalues(avalue(), bvalue(), c=input$cvalue, d=input$dvalue)
        output$show_values <- renderUI({
          withMathJax(
            div(glue::glue("$$\\lambda_1 = {signif(lambdas[1], 4)}$$ $$\\lambda_2 = {signif(lambdas[2], 4)}$$"))
          )
        })
      })

      output$ABplot <- renderPlot({
        alphamaster <- 0.5
        Pts <- tibble::tibble(
          a = seq(-2, 2, length=101),
          c = input$cvalue,
          d = input$dvalue,
          apd = a + d,
          amd = a - d,
          adoverc = a*d/c,
          b = amd^2 / (4 * c),
          bottom = sign(c)*Inf,
          fourcd = -4*c*b)
        saddleheight <- sign(input$cvalue)*0.5
        saddlehoriz <- -sign(input$dvalue)*(pmin(0.6, abs(input$dvalue)))
        stableheight <- -sign(input$cvalue)*input$dvalue - sign(input$cvalue)*0.3 - sign(input$cvalue)*ifelse(input$dvalue > 0, input$dvalue, 0)
        stablehoriz <- - 0.75
        oscillatoryheight <- -(sign(input$cvalue)*0.4 + input$cvalue/2)
        oscillatoryhoriz <- -saddlehoriz
        gf_ribbon(-b + -sign(c)*Inf ~ a, data = Pts, color=NA,
                  fill="blue", alpha=I(0.3*alphamaster)) %>%
          gf_ribbon(adoverc + sign(c)*Inf ~ a, fill="orange",
                    alpha=0.2*alphamaster) %>%
          gf_text(saddleheight ~ saddlehoriz, label="Saddle",
                  color="orange", alpha=alphamaster, size=6) %>%
          gf_text(oscillatoryheight ~ oscillatoryhoriz, label="Oscillatory", color="blue",
                  alpha = alphamaster, size=6) %>%
          gf_ribbon(adoverc + - sign(c)*Inf ~  a, clip = FALSE,
                    data = Pts[(Pts$a + Pts$d) < 0, ], color=NA,
                    alpha=I(0.2*alphamaster), fill="black") %>%
          gf_text(stableheight ~ stablehoriz, label="Stable",
                  color = "black",
                  alpha = alphamaster, size=5) %>%
          gf_point(bvalue() ~ avalue(), inherit=FALSE) %>%
          gf_labs(y="b", x = "a")
        # gf_refine(
        #     coord_fixed(ratio=1, xlim=c(-1, 1), ylim=c(-1, 1), clip="on")
        # )
      })

      reactive({matrix(c(avalue(), bvalue(), input$cvalue, input$dvalue), nrow=2, byrow=TRUE)})
    }
  )
}
