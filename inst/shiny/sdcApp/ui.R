library(shiny)

addResourcePath("sdcwww", file.path(
  getShinyOption(".appDir", getwd()),
  "www")
)

shinyUI(
  navbarPage(id="mainnav", theme = paste0("sdcwww/", getShinyOption(".guitheme")), title = "sdcMicro GUI",
    tabPanel("About/Help", uiOutput("ui_about")),
    tabPanel("Microdata", uiOutput("ui_inputdata")),
    tabPanel("Anonymize", uiOutput("ui_anonymize")),
    tabPanel("Risk/Utility", uiOutput("ui_results")),
    tabPanel("Export Data", uiOutput("ui_export")),
    tabPanel("Reproducibility", uiOutput("ui_script")),
    tabPanel("AI-Assisted", uiOutput("ui_ai_panel")),
    tabPanel("Undo", uiOutput("ui_undo")),
    header = tags$head(
      tags$script(src = paste0("sdcwww/", getShinyOption(".guijsfile"))),
      tags$script(HTML("
        // bsButton update handler (shinyBS replacement)
        Shiny.addCustomMessageHandler('bsButtonUpdate', function(data) {
          var el = document.getElementById(data.id);
          if (!el) return;
          if (data.label) { el.innerHTML = data.label; }
          if (data.style) {
            el.className = el.className.replace(/btn-(default|primary|success|info|warning|danger)/g, '');
            el.classList.add('btn-' + data.style);
          }
          if (data.disabled === true) { el.setAttribute('disabled', 'disabled'); }
          if (data.disabled === false) { el.removeAttribute('disabled'); }
        });
        // bsModal trigger binding (shinyBS replacement)
        $(document).on('click', '[data-sbs-trigger]', function() {
          // handled natively by data-toggle=modal if present
        });
        $(document).on('click', '.action-button, .btn', function() {
          var id = $(this).attr('id');
          if (!id) return;
          var modal = $('[data-sbs-trigger=\"' + id + '\"]');
          if (modal.length) { modal.modal('show'); }
        });
      "))
    )
  )
)
