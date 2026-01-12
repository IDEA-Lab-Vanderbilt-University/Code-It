library(shiny)
library(bslib)
library(tidyverse)
library(readxl)
library(DT)
library(shinythemes)
library(bsicons)
library(shinyjs)

# Load AWS configuration
source("aws-config.R")

# for predicting keywords using naive bayes
source("predicting_keywords.R")

# carl's util functions
source("util.R")

#### UI ####
ui <-
  navbarPage(
    title = div(bs_icon("ui-checks"), "Code It!"),
    windowTitle = "Code It!",
    id = "tabs",
    selected = "ud",
    inverse = TRUE,

    shinyjs::useShinyjs(),

    #### TAGS AND LOGIN ####
    tags$head(
      tags$style(HTML("

       /* navbar logout alignment */
      .navbar-nav > li > a {
        color: #9d9d9d !important;
      }

      .navbar-nav > li > a:hover {
        color: #fff !important;
        background-color: transparent !important;
      }

      /* editable datatable in Create Code tab */
      .dataTables_wrapper .dataTable tbody td {
        padding: 12px 8px !important;
        border: 1px solid #dee2e6 !important;
        background-color: #ffffff !important;
        color: #333333 !important;
      }

      /* Style for editable cells on hover */
      .dataTables_wrapper .dataTable tbody td:hover {
        background-color: #e3f2fd !important;
        cursor: pointer !important;
        border: 1px solid #2196F3 !important;
      }

      /* Critical: Style when cell is being edited */
      .dataTables_wrapper .dataTable tbody td input {
        width: 100% !important;
        padding: 8px 12px !important;
        border: 2px solid #007bff !important;
        border-radius: 4px !important;
        font-size: 14px !important;
        background-color: #ffffff !important;
        color: #333333 !important;
        box-shadow: 0 0 0 0.2rem rgba(0,123,255,.25) !important;
        font-family: inherit !important;
        line-height: 1.5 !important;
      }

      /* Focus state for input - most important for visibility */
      .dataTables_wrapper .dataTable tbody td input:focus {
        outline: none !important;
        border-color: #0056b3 !important;
        box-shadow: 0 0 0 0.3rem rgba(0,123,255,.4) !important;
        background-color: #ffffff !important;
        color: #000000 !important;
      }

      /* Text selection highlighting */
      .dataTables_wrapper .dataTable tbody td input::selection {
        background-color: #007bff !important;
        color: white !important;
      }

      .dataTables_wrapper .dataTable tbody td input::-moz-selection {
        background-color: #007bff !important;
        color: white !important;
      }

      /* Placeholder text styling */
      .dataTables_wrapper .dataTable tbody td input::placeholder {
        color: #6c757d !important;
        opacity: 1 !important;
      }

      /* Override any conflicting styles */
      .dataTables_wrapper .dataTable tbody td input[type='text'] {
        background: #ffffff !important;
        color: #000000 !important;
      }

      /* Authentication styles */
      .auth-container {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: linear-gradient(45deg, #f8f9fa, #e9ecef);
        z-index: 9999;
        display: flex;
        justify-content: center;
        align-items: center;
      }
      .auth-panel {
        background: white;
        padding: 40px;
        border-radius: 10px;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        max-width: 400px;
        width: 90%;
      }
      .auth-title {
        text-align: center;
        color: #2c3e50;
        margin-bottom: 10px;
      }
      .auth-subtitle {
        text-align: center;
        color: #7f8c8d;
        font-size: 16px;
        margin-bottom: 30px;
      }
      .btn-auth {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border: none;
        padding: 12px 20px;
        border-radius: 25px;
        color: white;
        width: 100%;
        margin-bottom: 10px;
      }
      .btn-auth:hover {
        opacity: 0.9;
        color: white;
      }
      .auth-links {
        text-align: center;
        margin-top: 20px;
        padding-top: 20px;
        border-top: 1px solid #dee2e6;
      }
      .auth-links a {
        color: #667eea;
        text-decoration: none;
        margin: 0 10px;
        font-size: 14px;
      }
      .auth-links a:hover {
        color: #764ba2;
        text-decoration: underline;
      }
    ")),

      tags$script(HTML("
     function showView(viewId) {
        $('.auth-view').hide();
        $('#' + viewId).show();
     }

      function deleteKeyword(groupId, keywordIndex) {
        Shiny.setInputValue('delete_individual_keyword', {
          group_id: groupId,
          keyword_index: keywordIndex,
          timestamp: new Date().getTime()
        }, {priority: 'event'});
      }

      // Handle add keyword button clicks from suggestions
      $(document).on('click', '.add-keyword-btn', function() {
        var keyword = $(this).data('keyword');
        Shiny.setInputValue('add_suggested_keyword', keyword, {priority: 'event'});
      });

  "))
    ),

    # Authentication overlay
    conditionalPanel(
      condition = "output.authenticated == false",
      div(
        class = "auth-container",
        div(
          class = "auth-panel",

          # Login View
          div(
            id = "login-view",
            class = "auth-view",
            h2(bs_icon("ui-checks"),"Code It!", class = "auth-title"),
            p("Develop, Automate, and Validate", class = "auth-subtitle"),

            div(id = "login-message", style = "margin-bottom: 15px;"),

            div(
              class = "form-group",
              tags$label("Username"),
              textInput("login_username", NULL, placeholder = "Enter username")
            ),
            div(
              class = "form-group",
              tags$label("Password"),
              passwordInput("login_password", NULL, placeholder = "Enter password")
            ),
            actionButton("login_submit", "Login", class = "btn-auth"),

            div(
              class = "auth-links",
              a(href = "#", onclick = "showView('register-view')", "Create New Account"), " | ",
              a(href = "#", onclick = "showView('forgot-view')", "Forgot Password?")
            ),
            div(
              style = "text-align: center; margin-top: 30px; padding-top: 15px; border-top: 1px solid #e9ecef;",
              p("This app created by the ",
                a("IDEA Lab", href = "https://lab.vanderbilt.edu/idealab/", target="_blank"),
                style = "color: #95a5a6; font-size: 12px; margin: 0;")
            )
          ),

          # Registration View
          div(
            id = "register-view",
            class = "auth-view",
            style = "display: none;",
            h3("Create New Account", class = "auth-title"),

            div(id = "register-message", style = "margin-bottom: 15px;"),

            div(
              class = "form-group",
              tags$label("Username"),
              textInput("register_username", NULL, placeholder = "Choose username")
            ),
            div(
              class = "form-group",
              tags$label("Email"),
              textInput("register_email", NULL, placeholder = "Enter email address")
            ),
            div(
              class = "form-group",
              tags$label("Password"),
              passwordInput("register_password", NULL, placeholder = "Choose password")
            ),
            div(
              class = "form-group",
              tags$label("Confirm Password"),
              passwordInput("register_confirm", NULL, placeholder = "Confirm password")
            ),
            actionButton("register_submit", "Create Account", class = "btn-auth"),

            div(
              class = "auth-links",
              a(href = "#", onclick = "showView('login-view')", "Back to Login")
            )
          ),

          # Email Confirmation View
          div(
            id = "confirm-view",
            class = "auth-view",
            style = "display: none;",
            h3("Verify Your Email", class = "auth-title"),
            p("Please check your email and enter the verification code below.", style = "text-align: center; margin-bottom: 20px;"),

            div(id = "confirm-message", style = "margin-bottom: 15px;"),

            div(
              class = "form-group",
              tags$label("Verification Code"),
              textInput("confirm_code", NULL, placeholder = "Enter code from email")
            ),
            actionButton("confirm_submit", "Verify Account", class = "btn-auth"),

            div(
              class = "auth-links",
              a(href = "#", onclick = "showView('login-view')", "Back to Login")
            )
          ),

          # Forgot Password View
          div(
            id = "forgot-view",
            class = "auth-view",
            style = "display: none;",
            h3("Reset Password", class = "auth-title"),

            div(id = "forgot-message", style = "margin-bottom: 15px;"),

            # Step 1: Enter username
            div(
              id = "forgot-step1",
              div(
                class = "form-group",
                tags$label("Username"),
                textInput("forgot_username", NULL, placeholder = "Enter your username")
              ),
              actionButton("forgot_submit", "Send Reset Code", class = "btn-auth")
            ),

            # Step 2: Enter code and new password
            div(
              id = "forgot-step2",
              style = "display: none;",
              div(
                class = "form-group",
                tags$label("Reset Code"),
                textInput("reset_code", NULL, placeholder = "Enter code from email")
              ),
              div(
                class = "form-group",
                tags$label("New Password"),
                passwordInput("new_password", NULL, placeholder = "Enter new password")
              ),
              div(
                class = "form-group",
                tags$label("Confirm New Password"),
                passwordInput("confirm_new_password", NULL, placeholder = "Confirm new password")
              ),
              actionButton("reset_submit", "Reset Password", class = "btn-auth")
            ),

            div(
              class = "auth-links",
              a(href = "#", onclick = "showView('login-view')", "Back to Login")
            )
          )
        )
      )
    ),

    #### ABOUT ####
    tabPanel(
      title = " ",
      icon = icon("info-circle"),
      value = "about",
      conditionalPanel(
        condition = "output.authenticated == true",
        fluidRow(
          column(12,
                 wellPanel(
                   #h3("About Code It!", style = "color: #2c3e50; margin-top: 0;"),

                   h4("Overview", style = "color: #34495e; margin-top: 25px;"),
                   p(strong("Code It!"), "streamlines automated qualitative coding by combining keyword-based classifiers with statistical validation. Keyword-based classifiers allow for fair and transparent automated coding processes. Build, train, and validate your coding system with confidence using perfect sampling methodology."),

                   h4("Workflow", style = "color: #34495e; margin-top: 25px;"),
                   tags$ol(
                     tags$li(strong("Upload Data"), "– Import your CSV or Excel file and select the column with data"),
                     tags$li(strong("Create Code"), "– Validate one code at a time. Define your code with a name, definition, and examples."),
                     tags$li(strong("Create Classifiers"), "– Add keywords or regex patterns to identify your code."),
                     tags$li(strong("Training"), "– Review examples and refine your classifier. Keep track of Cohen's Kappa, False Discovery Rate, and False Omission Rate."),
                     tags$li(strong("Validation"), "– Achieve κ ≥ 0.80 through perfect sampling cycles."),
                     tags$li(strong("Code Dataset"), "– Apply your validated classifier to all data and download final metrics.")
                   ),

                   h4("Perfect Sampling Validation", style = "color: #34495e; margin-top: 25px;"),
                   p("The app uses a cycle-based perfect validation approach ",
                     a("(Shaffer & Cai's 2024)", href = "https://doi.org/10.1007/978-3-031-76335-9_4", target="_blank")),
                   tags$ul(
                     tags$li("Calculates required sample size (Cai's N) based on your classifier's performance"),
                     tags$li("Tracks consecutive perfect agreements between you and the classifier"),
                     tags$li("Any disagreement ends the cycle, moves the item to training, and prompts classifier refinement"),
                     tags$li("Validation is complete when you achieve the required number of consecutive agreements (κ > 0.80, α = 0.025)")
                   ),
                   p("This ensures statistical confidence before coding your full dataset."),

                   h4("Acknowledgments and References", style = "color: #34495e; margin-top: 25px;"),
                   p("Inspired by the ", a("Epistemic Analytics Lab ", href="https://www.epistemicanalytics.org", target="_blank"), "and developed with assistance from Claude's Sonnet v4.5 LLM model."),
                   p("Shaffer, D.W. & Cai, Z. (2024). ", a("Perfect Sampling.", href = "https://doi.org/10.1007/978-3-031-76335-9_4", target="_blank")),
                   p("Shaffer, D.W. & Ruis, A.R. (2021). ", a("How We Code.", href = "https://doi.org/10.1007/978-3-030-67788-6_5", target="_blank")),
                   p("Eagan, B. & colleagues. (2015). ", a("Can We Rely on IRR?", href="https://repository.isls.org/handle/1/275", target="_blank")),
                   p("Arastoopour Irgens, G. & Eagan, B. (2023). ", a("The Foundations and Fundamentals of Quantitative Ethnography", href="https://par.nsf.gov/servlets/purl/10442783", target="_blank")),

                   h4("Author", style = "color: #34495e; margin-top: 25px;"),
                   p(strong("Golnaz Arastoopour Irgens")),

                   h4("Recommended Citation", style = "color: #34495e; margin-top: 25px;"),
                   p("If you use Code It! in your research, please cite:"),
                   div(
                     style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #667eea; margin: 15px 0; border-radius: 4px;",
                     p("Arastoopour Irgens, G. (2025).", em("Code It!: A web-based application for developing and validating automated qualitative coding systems."), "[URL]", style = "margin: 0;")
                   ),

                   hr(),

                   p(em("Secure user authentication via AWS Cognito • Auto-saves your progress • Private user data storage"),
                     style = "text-align: center; color: #7f8c8d; font-size: 14px; margin-top: 20px;")
                 )
          )
        )
      )
    ),

    #### UPLOAD DATA ####
    tabPanel(
      title = "Upload Data",
      value = "ud",
      conditionalPanel(
        condition = "output.authenticated == true",

        fluidRow(
          column(12,
                 wellPanel(
                   h4("Upload Data"),
                   fluidRow(
                     column(4,
                            fileInput("data_upload", "Upload CSV or Excel File",
                                      accept = c(".csv", ".xls", ".xlsx"))
                     ),
                     column(2),
                     column(4,
                            selectInput("textColumn", "Select the column with text data", choices = "")
                     )
                   ),
                   fluidRow(
                     column(12,
                            DT::dataTableOutput("uploaded_data")
                     )
                   )
                 )
          )
        ),

        # Next Step Box
        fluidRow(
          column(12,
                 div(
                   class = "alert alert-info",
                   style = "margin-top: 20px;",
                   h5(icon("lightbulb"), " Next Step", style = "color: #004085; display: inline; margin-left: 5px;"),
                   p("Once you've uploaded your data and selected the text column, proceed to define your code.",
                     style = "margin: 10px 0;"),
                   actionButton("goto_create_code", "Go to Create Code →",
                                class = "btn-primary")
                 )
          )
        )
      )
    ),


    #### CREATE CODE ####
    tabPanel(
      title = "Create Code",
      value = "create_codebook",
      conditionalPanel(
        condition = "output.authenticated == true",
        fluidRow(
          column(12,
                 wellPanel(
                   h4("Create Your Code"),
                   p("Define a single code for your analysis",
                     style = "color: #6c757d; font-size: 14px; margin-bottom: 15px;"),
                   fluidRow(
                     column(12,
                            DT::dataTableOutput("created_codebook")
                     )
                   )
                 )
          )
        ),
        # Next Step Box
        fluidRow(
          column(12,
                 div(
                   class = "alert alert-info",
                   style = "margin-top: 20px;",
                   h5(icon("lightbulb"), " Next Step", style = "color: #004085; display: inline; margin-left: 5px;"),
                   p("After defining your code name, definition, and examples, proceed to add classifiers/keywords.",
                     style = "margin: 10px 0;"),
                   actionButton("goto_create_classifiers", "Go to Create Classifiers →",
                                class = "btn-primary")
                 )
          )
        )
      )
    ),

    #### CREATE CLASSIFIERS ####
    tabPanel(
      title = "Create Classifiers",
      value = "create_classifiers",
      conditionalPanel(
        condition = "output.authenticated == true",
        fluidRow(
          column(12,
                 wellPanel(
                   h4("Add Classifier for: ", style = "display: inline;"),
                   h4(textOutput("current_code_name", inline = TRUE),
                      style = "display: inline; color: #007bff; font-weight: bold;")
                 )
          )
        ),
        fluidRow(
          column(12,
                 wellPanel(
                   fluidRow(
                     column(8,
                            div(
                              tags$label(
                                "Keywords ",
                                bslib::tooltip(
                                  span(icon("info-circle"), style = "cursor: help; color: #007bff;"),
                                  "Enter comma-separated keywords or regex patterns. Example: love, \\bdevot, (?<=eternal\\s)adoration",
                                  placement = "right"
                                )
                              ),
                              textInput("classifier_keywords",
                                        label = NULL,
                                        placeholder = "Enter keywords or regex patterns (comma-separated)")
                            )
                     ),

                     column(4,
                            br(),
                            actionButton("add_classifier_submit",
                                         "Add Keywords",
                                         class = "btn-primary btn-block",
                                         style = "margin-top: 5px;")
                     )
                   )
                 )
          )
        )
      ),

      fluidRow(
        column(12,
               wellPanel(
                 h4("Classifier List"),
                 fluidRow(
                   column(12,
                          DT::dataTableOutput("classifier_list")
                   )
                 )
               )
        )
      ),

      fluidRow(
        column(12,
               wellPanel(
                 h4("Keyword Suggester", style = "margin-bottom: 15px;"),
                 p("After training your classifier, use Naive Bayes AI to suggest relevant keywords based on your coded examples. Requires at least 10 trained examples.",
                   style = "color: #6c757d; font-size: 14px; margin-bottom: 15px;"),

                 fluidRow(
                   column(4,
                          actionButton("predict_classifiers",
                                       "Generate Keyword Suggestions",
                                       icon = icon("magic"),
                                       class = "btn-info btn-block")
                   )
                 ),

                 # Show suggestions when available
                 conditionalPanel(
                   condition = "output.has_suggestions",
                   hr(),
                   h5("Suggested Keywords", style = "margin-top: 20px; margin-bottom: 10px;"),
                   p("These keywords appear more frequently in your positive examples. Click 'Add' to include them in your classifier.",
                     style = "color: #6c757d; font-size: 13px; margin-bottom: 15px;"),
                   DT::dataTableOutput("suggested_keywords_table")
                 )
               )
        )
      ),

      # Next Step Box
      fluidRow(
        column(12,
               div(
                 class = "alert alert-info",
                 style = "margin-top: 20px;",
                 h5(icon("lightbulb"), " Next Step", style = "color: #004085; display: inline; margin-left: 5px;"),
                 p("After adding your keywords/classifiers, proceed to train your classifier.",
                   style = "margin: 10px 0;"),
                 actionButton("goto_training", "Go to Training →",
                              class = "btn-primary")
               )
        )
      )
    ),

    #### TRAINING ####
    tabPanel(
      title = "Training",
      value = "training",
      conditionalPanel(
        condition = "output.authenticated == true",
        fluidRow(
          column(12,
                 wellPanel(
                   h4("Training for: ", style = "display: inline;"),
                   h4(textOutput("training_code_name", inline = TRUE),
                      style = "display: inline; color: #007bff; font-weight: bold;")
                 )
          )
        ),

        fluidRow(
          column(6,
                 wellPanel(
                   h4("Confusion Matrix"),
                   p("This shows how well the automated classifier matches your coding decisions",
                     style = "color: #6c757d; font-size: 14px; margin-bottom: 15px;"),
                   DT::dataTableOutput("confusion_matrix_table")
                 )
          ),
          column(6,
                 wellPanel(
                   h4("Training Metrics"),
                   p("These metrics tell you how well the automated classifier is performing",
                     style = "color: #6c757d; font-size: 14px; margin-bottom: 15px;"),
                   DT::dataTableOutput("training_metrics")
                 )
          )
        ),
        fluidRow(
          column(9,
                 wellPanel(
                   h4("Train the Autocoder"),
                   fluidRow(
                     column(6,
                            actionButton("show_positive",
                                         "Show Positive Example",
                                         class = "btn-block",
                                         style = "margin-bottom: 10px;")
                     ),
                     column(6,
                            actionButton("show_negative",
                                         "Show Negative Example",
                                         class = "btn-block",
                                         style = "margin-bottom: 10px;")
                     )
                   ),
                   fluidRow(
                     column(12,
                            div(
                              style = "background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 15px; border-radius: 5px; min-height: 300px; max-height: 400px; overflow-y: auto; word-wrap: break-word; white-space: pre-wrap;",
                              uiOutput("sample_text")
                            )
                     )
                   ),
                   fluidRow(
                     column(12,
                            br(),
                            align = "center",
                            shinyjs::hidden(
                              actionButton("training_yes",
                                           "YES -- This Should be Coded",
                                           icon = icon("check"),
                                           class = "btn-success btn-lg"
                              )
                            ),
                            shinyjs::hidden(
                              actionButton("training_no",
                                           "NO -- This Should NOT be Coded",
                                           icon = icon("times"),
                                           class = "btn-danger btn-lg"
                              )
                            )
                     )
                   )
                 )
          ),
          column(3,
                 wellPanel(
                   h4("Next Steps"),
                   actionButton(
                     "move_to_testing",
                     "Move to Validation",
                     icon = icon("arrow-right"),
                     class = "btn-primary btn-block",
                     style = "margin-bottom: 10px;"
                   ),
                   actionButton(
                     "move_back_classifier",
                     "Revise Classifiers",
                     icon = icon("arrow-left"),
                     class="btn-info btn-block",
                     style = "margin-bottom: 10px;"
                   ),
                   br(),
                   br(),
                   actionButton(
                     "reset_training_metrics",
                     "Reset All Training Metrics",
                     icon = icon("refresh"),
                     class = "btn-warning btn-block",
                     style = "margin-bottom: 10px;"
                   ),
                   br(),
                   br(),
                   downloadButton(
                     "download_training_data",
                     "Download Training Results",
                     class = "btn-secondary btn-block",
                     style = "margin-bottom: 10px;"
                   ),
                   downloadButton(
                     "download_training_metrics",
                     "Download Training Metrics",
                     class = "btn-secondary btn-block"
                   )

                 )
          )
        ),

        # Next Step Box
        fluidRow(
          column(12,
                 div(
                   class = "alert alert-info",
                   style = "margin-top: 20px;",
                   h5(icon("lightbulb"), " Ready for Validation?", style = "color: #004085; display: inline; margin-left: 5px;"),
                   p("Once you are satified with your Cohen's Kappa, FDR, and FOR, move on to validation.",
                     style = "margin: 10px 0;"),
                   actionButton("goto_validation", "Go to Validation →",
                                class = "btn-primary")
                 )
          )
        )
      )
    ),

    #### VALIDATION ####
    tabPanel(
      title = "Validation",
      value = "validation",
      conditionalPanel(
        condition = "output.authenticated == true",
        fluidRow(
          column(6,
                 wellPanel(
                   style = "height: 140px;",
                   h4("Perfect Sampling", style = "display: inline-block; margin-bottom: 15px;"),
                   p("Validation complete when Kappa ≥ 0.80.",
                     style = "color: #6c757d; font-size: 14px; margin-bottom: 2px;"),
                   p("Any disagreement will immediately end this cycle and require classifier refinement.",
                     style = "color: #6c757d; font-size: 14px; margin-bottom: 15px;")
                 )
          ),
          column(6,
                 wellPanel(
                   style = "height: 140px;",
                   h4("Validation for: ", style = "display: inline;"),
                   h4(textOutput("validation_code_name", inline = TRUE),
                      style = "display: inline; color: #007bff; font-weight: bold;")
                 )
          )
        ),

        fluidRow(
          column(6,
                 wellPanel(
                   h4("Current Cycle Metrics"),
                   DT::dataTableOutput("validation_current_cycle")
                 )
          ),
          column(6,
                 wellPanel(
                   h4("Overall Validation Metrics"),
                   DT::dataTableOutput("validation_overall")
                 )
          )
        ),

        fluidRow(
          column(8,
                 wellPanel(
                   h4("Validation Item"),
                   div(
                     id = "validation_controls",
                     fluidRow(
                       column(12,
                              div(
                                style = "background-color: #f8f9fa; border: 1px solid #dee2e6; padding: 15px; border-radius: 5px; min-height: 300px; max-height: 400px; overflow-y: auto; word-wrap: break-word; white-space: pre-wrap;",
                                uiOutput("validation_text")
                              )
                       )
                     ),
                     fluidRow(
                       column(12,
                              br(),
                              align = "center",
                              shinyjs::hidden(
                                actionButton("validate_yes",
                                             "YES - This should be coded",
                                             icon = icon("check"),
                                             class = "btn-success btn-lg",
                                             style = "margin-right: 10px;"
                                )
                              ),
                              shinyjs::hidden(
                                actionButton("validate_no",
                                             "NO - This should not be coded",
                                             icon = icon("times"),
                                             class = "btn-danger btn-lg"
                                )
                              )
                       )
                     )
                   ),
                   shinyjs::hidden(
                     div(
                       id = "validation_complete",
                       div(
                         class = "alert alert-success",
                         style = "text-align: center; margin-top: 20px;",
                         h4("? Validation Complete!"),
                         p("Kappa threshold reached. Your classifier is performing well!")
                       )
                     )
                   )
                 )
          ),

          column(4,
                 wellPanel(
                   h4("Next Steps"),

                   # Show before validation is complete
                   conditionalPanel(
                     condition = "!output.validation_complete_flag",
                     p("Complete perfect sampling validation to proceed.",
                       style = "color: #6c757d; font-size: 14px; margin-bottom: 15px;"),
                     actionButton("restart_validation",
                                  "Restart Validation",
                                  icon = icon("refresh"),
                                  class = "btn-warning btn-block",
                                  style = "margin-bottom: 10px;"
                     )
                   ),

                   # Show after validation is complete
                   conditionalPanel(
                     condition = "output.validation_complete_flag",
                     div(
                       class = "alert alert-success",
                       style = "text-align: center; margin-bottom: 20px;",
                       h5("? Validation Complete!", style = "color: #28a745; margin-bottom: 10px;"),
                       p("κ > 0.80 achieved through perfect sampling", style = "margin: 0; font-weight: bold;")
                     ),
                     actionButton("code_entire_dataset",
                                  "Code Entire Dataset",
                                  icon = icon("magic"),
                                  class = "btn-success btn-block btn-lg",
                                  style = "margin-bottom: 15px;"),
                     p("This will apply your validated classifier to all data and provide download.",
                       style = "color: #6c757d; font-size: 13px; text-align: center; margin-bottom: 20px;"),
                     actionButton("restart_validation",
                                  "Restart Validation",
                                  icon = icon("refresh"),
                                  class = "btn-warning btn-block btn-sm")
                   ),

                   # Always available downloads
                   hr(),
                   downloadButton("download_validation_results",
                                  "Download Validation Results",
                                  class = "btn-secondary btn-block"
                   ),
                   downloadButton("download_validation_current_cycle",
                                  "Download Validation Metrics",
                                  class = "btn-secondary btn-block",
                                  style = "margin-top: 10px;"
                   )
                 )
          )
        )
      )
    ),

    #### LOGOUT ####

    tabPanel(
      title = tags$a(
        icon("sign-out-alt"),
        "Logout",
        href = "#",
        onclick = "Shiny.setInputValue('logout_clicked', Math.random(), {priority: 'event'}); return false;",
        style = "color: #333; text-decoration: none; display: block; padding: 3px 20px;"
      ),
      value = "logout_panel",
    )

  ) # end of UI


##### SERVER #####
server <- function(input, output, session) {

  options(shiny.maxRequestSize = 200 * 1024^2) # Sets limit to 200MB

  ##### AUTHENTICATION STATE MANAGEMENT #####

  # Define user data directory function
  user_data_dir <- function() {
    req(current_user())

    # Base directory for all user data
    base_dir <- "/srv/codeit-app/user_data"

    # Create base directory if it doesn't exist
    if (!dir.exists(base_dir)) {
      dir.create(base_dir, recursive = TRUE, showWarnings = FALSE)
    }

    # User-specific directory
    user_dir <- file.path(base_dir, current_user())

    # Create user directory if it doesn't exist
    if (!dir.exists(user_dir)) {
      dir.create(user_dir, recursive = TRUE, showWarnings = FALSE)
    }

    return(user_dir)
  }


  # Authentication reactive values
  auth_values <- reactiveValues(
    authenticated = FALSE,
    user_info = NULL,
    access_token = NULL,
    pending_username = NULL
  )

  # Control authentication display
  output$authenticated <- reactive({
    auth_values$authenticated
  })
  outputOptions(output, "authenticated", suspendWhenHidden = FALSE)

  # Get current user
  current_user <- reactive({
    req(auth_values$user_info)
    auth_values$user_info$username
  })

  ##### AUTHENTICATION HANDLERS #####

  # Login handler
  observeEvent(input$login_submit, {
    req(input$login_username, input$login_password)

    if (nchar(trimws(input$login_username)) == 0 || nchar(trimws(input$login_password)) == 0) {
      output$login_message <- renderUI({
        div(class = "alert alert-danger", "Please enter both username and password")
      })
      return()
    }

    result <- authenticate_cognito_user(input$login_username, input$login_password)

    if (result$success) {
      auth_values$authenticated <- TRUE
      auth_values$user_info <- list(username = result$user)
      auth_values$access_token <- result$tokens$AccessToken

      # Clear login form
      updateTextInput(session, "login_username", value = "")
      updateTextInput(session, "login_password", value = "")

      showNotification("Login successful!", type = "message", duration = 3)

    } else {
      output$login_message <- renderUI({
        div(class = "alert alert-danger", result$message)
      })
    }
  })

  # Registration handler
  observeEvent(input$register_submit, {
    req(input$register_username, input$register_email, input$register_password, input$register_confirm)

    # Validation
    if (nchar(trimws(input$register_username)) == 0 || nchar(trimws(input$register_email)) == 0 ||
        nchar(trimws(input$register_password)) == 0 || nchar(trimws(input$register_confirm)) == 0) {
      output$register_message <- renderUI({
        div(class = "alert alert-danger", "Please fill in all fields")
      })
      return()
    }

    if (input$register_password != input$register_confirm) {
      output$register_message <- renderUI({
        div(class = "alert alert-danger", "Passwords do not match")
      })
      return()
    }

    password <- input$register_password
    validation_errors <- character(0)

    if (nchar(password) < 8) {
      validation_errors <- c(validation_errors, "Must be at least 8 characters long.")
    }
    if (!grepl("[0-9]", password)) {
      validation_errors <- c(validation_errors, "Must contain at least 1 number.")
    }
    if (!grepl("[^A-Za-z0-9]", password)) {
      validation_errors <- c(validation_errors, "Must contain at least 1 special character.")
    }
    if (!grepl("[A-Z]", password)) {
      validation_errors <- c(validation_errors, "Must contain at least 1 uppercase letter.")
    }
    if (!grepl("[a-z]", password)) {
      validation_errors <- c(validation_errors, "Must contain at least 1 lowercase letter.")
    }

    if (length(validation_errors) > 0) {
      # Show error notification and STOP execution
      showNotification(
        paste("Password does not meet requirements:", paste(validation_errors, collapse = " ")),
        type = "error",
        duration = 10
      )
      return() # Stops the code from running register_cognito_user
    }

    result <- register_cognito_user(input$register_username, input$register_password, input$register_email)

    if (result$success) {
      # Store username for confirmation
      auth_values$pending_username <- input$register_username

      # Clear form
      updateTextInput(session, "register_username", value = "")
      updateTextInput(session, "register_email", value = "")
      updateTextInput(session, "register_password", value = "")
      updateTextInput(session, "register_confirm", value = "")

      # Show confirmation view
      runjs("showView('confirm-view')")

      output$confirm_message <- renderUI({
        div(class = "alert alert-success", result$message)
      })

    } else {
      output$register_message <- renderUI({
        div(class = "alert alert-danger", result$message)
      })
    }
  })

  # Email confirmation handler
  observeEvent(input$confirm_submit, {
    req(input$confirm_code, auth_values$pending_username)

    result <- confirm_cognito_user(auth_values$pending_username, input$confirm_code)

    if (result$success) {
      output$confirm_message <- renderUI({
        div(class = "alert alert-success", result$message)
      })

      # Clear form and redirect to login
      updateTextInput(session, "confirm_code", value = "")
      session$userData$pending_username <- NULL

      shinyjs::delay(2000, runjs("showView('login-view')"))

    } else {
      showNotification(
        paste("Verification Failed:", result$message),
        type = "error",
        duration = 8
      )
      output$confirm_message <- renderUI({
        div(class = "alert alert-danger", result$message)
      })
    }
  })

  # Forgot password step 1
  observeEvent(input$forgot_submit, {
    req(input$forgot_username)

    result <- forgot_password_cognito(input$forgot_username)

    if (result$success) {
      session$userData$reset_username <- input$forgot_username

      output$forgot_message <- renderUI({
        div(class = "alert alert-success", result$message)
      })

      # Show step 2
      shinyjs::hide("forgot-step1")
      shinyjs::show("forgot-step2")

    } else {
      output$forgot_message <- renderUI({
        div(class = "alert alert-danger", result$message)
      })
    }
  })

  # Password reset step 2
  observeEvent(input$reset_submit, {
    req(input$reset_code, input$new_password, input$confirm_new_password, session$userData$reset_username)

    if (input$new_password != input$confirm_new_password) {
      output$forgot_message <- renderUI({
        div(class = "alert alert-danger", "Passwords do not match")
      })
      return()
    }

    if (nchar(input$new_password) < 8) {
      output$forgot_message <- renderUI({
        div(class = "alert alert-danger", "Password must be at least 8 characters")
      })
      return()
    }

    result <- confirm_forgot_password_cognito(session$userData$reset_username, input$reset_code, input$new_password)

    if (result$success) {
      output$forgot_message <- renderUI({
        div(class = "alert alert-success", result$message)
      })

      # Clear form and redirect to login
      updateTextInput(session, "reset_code", value = "")
      updateTextInput(session, "new_password", value = "")
      updateTextInput(session, "confirm_new_password", value = "")
      session$userData$reset_username <- NULL

      shinyjs::delay(2000, runjs("showView('login-view')"))

    } else {
      output$forgot_message <- renderUI({
        div(class = "alert alert-danger", result$message)
      })
    }
  })

  # Logout handler
  observeEvent(input$logout_clicked, {
    # Save all user data before logout
    if (auth_values$authenticated) {
      save_all_user_data()
    }

    # Clear authentication state
    auth_values$authenticated <- FALSE
    auth_values$user_info <- NULL
    auth_values$access_token <- NULL

    # Clear any messages
    output$login_message <- renderUI({ NULL })
    output$register_message <- renderUI({ NULL })
    output$confirm_message <- renderUI({ NULL })
    output$forgot_message <- renderUI({ NULL })

    # Reset views
    shinyjs::show("forgot-step1")
    shinyjs::hide("forgot-step2")
    runjs("showView('login-view')")

    showNotification("Logged out successfully", type = "message", duration = 3)
  })

  ##### INITIALIZE ALL REACTIVE VALUES ####

  # Create Codebook values
  values <- reactiveValues(
    codebook_input = data.frame(
      CodeName = c("Click to enter Code Name"),
      CodeDefinition = c("Click to enter Code Definition"),
      Examples = c("Click to enter Code Examples"),
      stringsAsFactors = FALSE
    )
  )

  # Create Classifier values
  classifier_values <- reactiveValues(
    classifier_input = data.frame(
      Keywords = character(),
      stringsAsFactors = FALSE
    )
  )

  # Coded data values to store coded data
  coded_data_values <- reactiveValues(
    coded_data = NULL
  )

  # Training values
  training_values <- reactiveValues(
    current_sample = NULL,
    current_.row_id = NULL,
    shown_indices = integer(),
    current_example_type = NULL,
    training_results = data.frame(
      .row_id = integer(),
      TextData = character(),
      auto.coding = integer(),
      user.coding = integer(),
      stringsAsFactors = FALSE
    )
  )

  # Validation Values
  validation_values <- reactiveValues(
    validation_pool = NULL,
    current_validation_item = NULL,
    current_pool_index = NULL,
    current_cycle_results = data.frame(
      pool_index = integer(),
      original_.row_id = integer(),
      TextData = character(),
      auto.coding = integer(),
      user.coding = integer(),
      cycle = integer(),
      stringsAsFactors = FALSE
    ),
    all_validation_results = data.frame(),
    cais_n = 0,
    current_cycle = 1,
    perfect_agreements_current_cycle = 0,
    validation_complete = FALSE,
    cycle_failed = FALSE,
    total_items_coded = 0,
    estimated_baserate = 0,
    adjusted_baserate = 0,
    a_max = 0
  )

  # Session state values
  session_values <- reactiveValues(
    saved_dataset = NULL,
    saved_text_column = NULL,
    current_tab = "ud",
    data_loaded = FALSE
  )


  ##### USER DATA PERSISTENCE FUNCTIONS #####

  # Save all user data
  save_all_user_data <- function() {
    if (is.null(current_user())) return()

    tryCatch({
      user_dir <- user_data_dir()

      # Save dataset and text column if they exist
      if (!is.null(session_values$saved_dataset)) {
        saveRDS(session_values$saved_dataset, file.path(user_dir, "dataset.rds"))
      }
      if (!is.null(session_values$saved_text_column)) {
        saveRDS(session_values$saved_text_column, file.path(user_dir, "text_column.rds"))
      }

      # Save codebook
      saveRDS(values$codebook_input, file.path(user_dir, "codebook.rds"))

      # Save classifiers
      saveRDS(classifier_values$classifier_input, file.path(user_dir, "classifiers.rds"))

      # Save coded data if it exists
      if (!is.null(coded_data_values$coded_data)) {
        saveRDS(coded_data_values$coded_data, file.path(user_dir, "coded_data.rds"))
      }

      # Save training data
      training_data <- list(
        training_results = training_values$training_results,
        shown_indices = training_values$shown_indices
      )
      saveRDS(training_data, file.path(user_dir, "training_data.rds"))

      # Save validation data
      validation_data <- list(
        validation_pool = validation_values$validation_pool,
        current_cycle_results = validation_values$current_cycle_results,
        all_validation_results = validation_values$all_validation_results,
        cais_n = validation_values$cais_n,
        current_cycle = validation_values$current_cycle,
        perfect_agreements_current_cycle = validation_values$perfect_agreements_current_cycle,
        validation_complete = validation_values$validation_complete,
        cycle_failed = validation_values$cycle_failed,
        total_items_coded = validation_values$total_items_coded,
        estimated_baserate = validation_values$estimated_baserate,
        adjusted_baserate = validation_values$adjusted_baserate,
        a_max = validation_values$a_max
      )
      saveRDS(validation_data, file.path(user_dir, "validation_data.rds"))

      # Save current tab
      saveRDS(input$tabs, file.path(user_dir, "current_tab.rds"))

      cat("User data saved successfully for user:", current_user(), "\n")

    }, error = function(e) {
      cat("Error saving user data:", e$message, "\n")
    })
  }

  # Load all user data
  load_all_user_data <- function() {
    if (is.null(current_user()) || session_values$data_loaded) return()

    tryCatch({
      user_dir <- user_data_dir()

      # Load dataset and text column
      dataset_file <- file.path(user_dir, "dataset.rds")
      text_column_file <- file.path(user_dir, "text_column.rds")

      if (file.exists(dataset_file)) {
        session_values$saved_dataset <- readRDS(dataset_file)
        cat("Dataset loaded for user:", current_user(), "\n")
      }

      if (file.exists(text_column_file)) {
        session_values$saved_text_column <- readRDS(text_column_file)
        # Update the text column selection in UI
        if (!is.null(session_values$saved_dataset)) {
          choices <- names(session_values$saved_dataset)
          updateSelectInput(session, "textColumn",
                            choices = choices,
                            selected = session_values$saved_text_column)
        }
        cat("Text column loaded for user:", current_user(), "\n")
      }

      # Load codebook
      codebook_file <- file.path(user_dir, "codebook.rds")
      if (file.exists(codebook_file)) {
        values$codebook_input <- readRDS(codebook_file)
        cat("Codebook loaded for user:", current_user(), "\n")
      }

      # Load classifiers
      classifiers_file <- file.path(user_dir, "classifiers.rds")
      if (file.exists(classifiers_file)) {
        classifier_values$classifier_input <- readRDS(classifiers_file)
        cat("Classifiers loaded for user:", current_user(), "\n")
      }

      # Load coded data
      coded_data_file <- file.path(user_dir, "coded_data.rds")
      if (file.exists(coded_data_file)) {
        coded_data_values$coded_data <- readRDS(coded_data_file)
        cat("Coded data loaded for user:", current_user(), "\n")
      }

      # Load training data
      training_file <- file.path(user_dir, "training_data.rds")
      if (file.exists(training_file)) {
        training_data <- readRDS(training_file)
        training_values$training_results <- training_data$training_results
        training_values$shown_indices <- training_data$shown_indices
        cat("Training data loaded for user:", current_user(), "\n")
      }

      # Load validation data
      validation_file <- file.path(user_dir, "validation_data.rds")
      if (file.exists(validation_file)) {
        validation_data <- readRDS(validation_file)
        validation_values$validation_pool <- validation_data$validation_pool
        validation_values$current_cycle_results <- validation_data$current_cycle_results
        validation_values$all_validation_results <- validation_data$all_validation_results
        validation_values$cais_n <- validation_data$cais_n
        validation_values$current_cycle <- validation_data$current_cycle
        validation_values$perfect_agreements_current_cycle <- validation_data$perfect_agreements_current_cycle
        validation_values$validation_complete <- validation_data$validation_complete
        validation_values$cycle_failed <- validation_data$cycle_failed
        validation_values$total_items_coded <- validation_data$total_items_coded
        validation_values$estimated_baserate <- validation_data$estimated_baserate
        validation_values$adjusted_baserate <- validation_data$adjusted_baserate
        validation_values$a_max <- validation_data$a_max
        cat("Validation data loaded for user:", current_user(), "\n")
      }

      # Load and restore current tab
      tab_file <- file.path(user_dir, "current_tab.rds")
      if (file.exists(tab_file)) {
        saved_tab <- readRDS(tab_file)
        session_values$current_tab <- saved_tab

        # Navigate to saved tab after a short delay
        shinyjs::delay(500, {
          updateNavbarPage(session, "tabs", selected = saved_tab)
        })

        cat("Tab state restored for user:", current_user(), "- Tab:", saved_tab, "\n")
      }

      session_values$data_loaded <- TRUE

      # Show success message
      showNotification(
        "Welcome back! Your previous session has been restored.",
        type = "message", duration = 5
      )

    }, error = function(e) {
      cat("Error loading user data:", e$message, "\n")
      showNotification(
        "Error restoring previous session. Starting fresh.",
        type = "warning", duration = 3
      )
    })
  }

  # Auto-save function (called whenever important data changes)
  auto_save <- function() {
    req(current_user())

    if (!is.null(current_user()) && session_values$data_loaded) {
      save_all_user_data()
    }
  }

  # Load user data when authentication succeeds
  observe({
    req(current_user())
    if (!session_values$data_loaded) {
      shinyjs::delay(1000, {  # Small delay to ensure UI is ready
        load_all_user_data()
      })
    }
  })

  ##### NAVIGATION NEXT STEP HANDLERS ####

  observeEvent(input$goto_create_code, {
    updateNavbarPage(session, "tabs", selected = "create_codebook")
    showNotification("Define your code: name, definition, and examples", type = "message", duration = 3)
  })

  observeEvent(input$goto_create_classifiers, {
    if (!is.null(current_code())) {
      updateNavbarPage(session, "tabs", selected = "create_classifiers")
      showNotification("Add keywords or regex patterns to identify your code", type = "message", duration = 3)
    } else {
      showNotification("Please define your code first", type = "warning", duration = 3)
    }
  })

  observeEvent(input$goto_training, {
    if (nrow(classifier_values$classifier_input) > 0) {
      updateNavbarPage(session, "tabs", selected = "training")
      showNotification("Train your classifier by coding examples", type = "message", duration = 3)
    } else {
      showNotification("Please add at least one classifier first", type = "warning", duration = 3)
    }
  })

  observeEvent(input$goto_validation, {
    metrics <- training_metrics()
    if (nrow(training_values$training_results) >= 20 && !is.null(metrics) && metrics$kappa >= 0.60) {
      updateNavbarPage(session, "tabs", selected = "validation")
      showNotification("Begin perfect sampling validation", type = "message", duration = 3)
    } else {
      showNotification("Please complete more training first (≥20 items, κ ≥ 0.60)", type = "warning", duration = 4)
    }
  })

  ##### UPLOAD DATA #####

  # first clear existing dataset (if any) when user uploads a dataset AND clear stuff from other tabs to avoid mismatched rows and other issues
  # Observer to reset ALL data when new file is uploaded
  observeEvent(input$data_upload, {
    tryCatch({
      # Clear dataset
      session_values$saved_dataset <- NULL
      session_values$saved_text_column <- NULL

      # Clear codebook
      # commented out for now -- user may want to keep code and codebook even after uploading new dataset
      # values$codebook_input <- data.frame(
      #   CodeName = c("Click to enter Code Name"),
      #   CodeDefinition = c("Click to enter Code Definition"),
      #   Examples = c("Click to enter Code Examples"),
      #   stringsAsFactors = FALSE
      # )

      # Clear classifiers
      # commented out for now -- user may want to keep classifier list even after uploading new dataset
      # classifier_values$classifier_input <- data.frame(
      #   Keywords = character(),
      #   stringsAsFactors = FALSE
      # )

      # Clear training data
      training_values$training_results <- data.frame(
        .row_id = integer(),
        text = character(),
        user.coding = numeric(),
        auto.coding = numeric(),
        stringsAsFactors = FALSE
      )
      training_values$shown_indices <- integer()
      training_values$current_sample <- NULL
      training_values$current_.row_id <- NULL
      training_values$current_example_type <- NULL

      # Clear coded data
      coded_data_values$coded_data <- NULL

      # Clear validation data
      validation_values$validation_pool <- NULL
      validation_values$current_validation_item <- NULL
      validation_values$current_pool_index <- NULL
      validation_values$current_cycle_results <- data.frame(
        .row_id = integer(),
        text = character(),
        user.coding = numeric(),
        auto.coding = numeric(),
        stringsAsFactors = FALSE
      )
      validation_values$all_validation_results <- data.frame()
      validation_values$cais_n <- 0
      validation_values$current_cycle <- 1
      validation_values$perfect_agreements_current_cycle <- 0
      validation_values$validation_complete <- FALSE
      validation_values$cycle_failed <- FALSE
      validation_values$total_items_coded <- 0
      validation_values$estimated_baserate <- 0
      validation_values$adjusted_baserate <- 0
      validation_values$a_max <- 0

      # Auto-save the cleared state
      auto_save()

      showNotification(
        "New dataset uploaded. All previous training and test metrics have been cleared.",
        type = "message",
        duration = 5
      )

    }, error = function(e) {
      showNotification(
        paste("Error uploading datafile and resetting", e$message),
        type = "error",
        duration = 5
      )
    })
  })

  dataset <- reactive({
    # First check if we have a saved dataset
    if (!is.null(session_values$saved_dataset)) {
      return(session_values$saved_dataset)
    }

    # Otherwise, load from file input
    req(input$data_upload)
    file_ext <- tools::file_ext(input$data_upload$datapath)

    if (file_ext == "csv") {
      data <- read.csv(input$data_upload$datapath, stringsAsFactors = FALSE)
    } else if (file_ext %in% c("xls", "xlsx")) {
      data <- read_excel(input$data_upload$datapath)
    } else {
      stop("Unsupported file type. Please upload a CSV or Excel file.")
    }

    #add a stable row id when data file is loaded
    if (!".row_id" %in% names(data)) {
      data$.row_id <- seq_len(nrow(data))
    }

    # Save the newly uploaded dataset
    session_values$saved_dataset <- data
    auto_save()
    return(data)
  })

  output$uploaded_data <- DT::renderDataTable({
    dataset() %>%
      datatable(
        rownames = FALSE,
        class = "cell-border stripe",
        options = list(
          searching = FALSE,
          autoWidth = TRUE,
          pageLength = 5
        )
      )
  })

  # Update text column selection
  observeEvent(dataset(), {
    req(dataset())
    choices <- names(dataset())

    # Only update if we don't already have a saved selection
    if (is.null(session_values$saved_text_column)) {
      updateSelectInput(inputId = "textColumn", choices = choices, selected = character(0))
    } else {
      updateSelectInput(inputId = "textColumn", choices = choices, selected = session_values$saved_text_column)
    }

  })

  # Save text column selection when it changes
  observeEvent(input$textColumn, {
    if (!is.null(input$textColumn) && input$textColumn != "") {
      session_values$saved_text_column <- input$textColumn
      auto_save()
    }
  })

  ##### CREATE CODEBOOK (SINGLE CODE) #####

  # Render the editable single-row table
  output$created_codebook <- DT::renderDataTable({
    datatable(
      values$codebook_input,
      rownames = FALSE,
      class = "cell-border stripe compact",
      editable = list(target = 'cell', disable = list(columns = NULL)),
      options = list(
        dom = 't',
        ordering = FALSE,
        scrollX = TRUE,
        columnDefs = list(
          list(className = 'dt-left', targets = '_all'),
          list(width = '200px', targets = 0),
          list(width = '300px', targets = 1),
          list(width = '300px', targets = 2)
        ),
        initComplete = JS(
          "function(settings, json) {",
          "  $(this.api().table().container()).find('input').css({",
          "    'background-color': '#ffffff',",
          "    'color': '#000000',",
          "    'border': '2px solid #007bff'",
          "  });",
          "}"
        )
      ),
      callback = JS(
        "table.on('click', 'td', function() {",
        "  setTimeout(function() {",
        "    $('input').css({",
        "      'background-color': '#ffffff !important',",
        "      'color': '#000000 !important',",
        "      'border': '2px solid #007bff !important'",
        "    });",
        "  }, 100);",
        "});"
      )
    )
  })

  # Update codebook when cell is edited and auto-save
  observeEvent(input$created_codebook_cell_edit, {
    info <- input$created_codebook_cell_edit
    values$codebook_input[info$row, info$col + 1] <- info$value
    auto_save()
  })

  # Reactive to get the current code name
  current_code <- reactive({
    code_name <- values$codebook_input$CodeName[1]
    if (is.na(code_name) || code_name == "" || code_name == "DOUBLE Click to enter Code Name") {
      return(NULL)
    }
    return(code_name)
  })

  # Display current code name in Create Classifiers tab
  output$current_code_name <- renderText({
    code <- current_code()
    if (is.null(code)) {
      return("[Please enter a code name first]")
    }
    return(code)
  })

  # Display current code name in Training tab
  output$training_code_name <- renderText({
    code <- current_code()
    if (is.null(code)) {
      return("[No code defined]")
    }
    return(code)
  })

  ##### CREATE CLASSIFIERS ####

  # Apply coding function
  apply_coding <- function() {
    if (!is.null(dataset()) &&
        !is.null(input$textColumn) &&
        input$textColumn != "" &&
        !is.null(current_code()) &&
        nrow(classifier_values$classifier_input) > 0) {

      tryCatch({
        data <- dataset()
        text_col <- input$textColumn
        code_name <- current_code()
        new_col_name <- paste0(code_name, ".Positive")

        # Get all keywords (regex supported)
        all_keywords <- classifier_values$classifier_input %>%
          rowwise() %>%
          do({
            keywords <- trimws(strsplit(.$Keywords, ",")[[1]])

            # drop empty strings explicitly
            keywords <- keywords[keywords != ""]

            data.frame(
              Keyword = keywords,
              stringsAsFactors = FALSE
            )
          }) %>%
          ungroup() %>%
          pull(Keyword)

        # stop if nothing valid remains
        if (length(all_keywords) == 0) {
          coded_data_values$coded_data <- NULL
          return()
        }

        # prevent accidental empty OR groups. This avoids patterns like (word1||word2)
        all_keywords <- all_keywords[!is.na(all_keywords) & nzchar(all_keywords)]

        if (length(all_keywords) == 0) {
          coded_data_values$coded_data <- NULL
          return()
        }

        # Combine all keywords with OR operator
        regex_pattern <- paste0("(", paste(all_keywords, collapse = "|"), ")")

        # Test the regex pattern to catch syntax errors
        test_result <- tryCatch({
          grepl(regex_pattern, "test", ignore.case = TRUE)
          TRUE
        }, error = function(e) {
          showNotification(
            paste(
              "Invalid regex pattern:",
              e$message,
              "- Check your keyword syntax"
            ),
            type = "error",
            duration = 5
          )
          FALSE
        })

        if (!test_result) {
          coded_data_values$coded_data <- NULL
          return()
        }

        # Apply the regex pattern
        matches <- grepl(
          regex_pattern,
          data[[text_col]],
          ignore.case = TRUE
        )

        # Create coded dataframe
        coded_df <- data.frame(
          TextData = data[[text_col]],
          stringsAsFactors = FALSE
        )
        coded_df[[new_col_name]] <- as.integer(matches)

        coded_data_values$coded_data <- coded_df
        auto_save()
        showNotification(
          "Coding updated with regex support!",
          type = "message",
          duration = 2
        )

      }, error = function(e) {
        showNotification(
          paste("Error applying coding:", e$message),
          type = "error"
        )
        coded_data_values$coded_data <- NULL
      })
    } else {
      coded_data_values$coded_data <- NULL
    }
  }

  # Trigger coding updates
  observeEvent(
    list(
      current_code(),
      input$textColumn,
      classifier_values$classifier_input,
      dataset()
    ),
    {
      apply_coding()
    },
    ignoreInit = TRUE
  )

  # Add new classifier and auto-save
  observeEvent(input$add_classifier_submit, {
    req(input$classifier_keywords)

    if (nchar(trimws(input$classifier_keywords)) == 0) {
      showNotification("Please enter keywords", type = "warning")
      return()
    }

    new_row <- data.frame(
      Keywords = trimws(input$classifier_keywords),
      stringsAsFactors = FALSE
    )

    classifier_values$classifier_input <- rbind(classifier_values$classifier_input, new_row)
    updateTextInput(session, "classifier_keywords", value = "")
    auto_save()  # Auto-save when classifiers change
  })

  # Handle individual keyword deletion
  observeEvent(input$delete_individual_keyword, {
    delete_info <- input$delete_individual_keyword

    if (!is.null(delete_info) && !is.null(delete_info$group_id) && !is.null(delete_info$keyword_index)) {
      group_id <- delete_info$group_id
      keyword_index <- delete_info$keyword_index

      if (group_id > 0 && group_id <= nrow(classifier_values$classifier_input)) {
        # Get current keywords for this group
        current_keywords_string <- classifier_values$classifier_input$Keywords[group_id]
        keywords_list <- trimws(strsplit(current_keywords_string, ",")[[1]])
        keywords_list <- keywords_list[keywords_list != ""]

        if (keyword_index > 0 && keyword_index <= length(keywords_list)) {
          keyword_to_delete <- keywords_list[keyword_index]

          showModal(modalDialog(
            title = "Confirm Keyword Deletion",
            p("Are you sure you want to delete this keyword?"),
            div(
              style = "background-color: #f8d7da; padding: 15px; border-radius: 5px; margin: 15px 0; border: 1px solid #f5c6cb; text-align: center;",
              h5(keyword_to_delete, style = "color: #721c24; font-family: 'Courier New', monospace; margin: 0;")
            ),
            p("This action cannot be undone.", style = "color: #dc3545; font-size: 13px;"),
            br(),
            fluidRow(
              column(6,
                     modalButton("Cancel")
              ),
              column(6,
                     actionButton("confirm_delete_keyword",
                                  "Delete Keyword",
                                  class = "btn-danger btn-block")
              )
            )
          ))

          # Store deletion info for confirmation
          session$userData$delete_info <- list(
            group_id = group_id,
            keyword_index = keyword_index,
            keyword_to_delete = keyword_to_delete
          )
        }
      }
    }
  })
  # Confirm individual keyword deletion and auto-save
  observeEvent(input$confirm_delete_keyword, {
    delete_info <- session$userData$delete_info

    tryCatch({
      if (!is.null(delete_info)) {
        group_id <- delete_info$group_id
        keyword_index <- delete_info$keyword_index

        # Get current keywords for this group
        current_keywords_string <- classifier_values$classifier_input$Keywords[group_id]
        keywords_list <- trimws(strsplit(current_keywords_string, ",")[[1]])
        keywords_list <- keywords_list[keywords_list != ""]

        # Remove the specific keyword
        keywords_list <- keywords_list[-keyword_index]

        if (length(keywords_list) > 0) {
          # Update the keywords string for this group
          new_keywords_string <- paste(keywords_list, collapse = ", ")
          classifier_values$classifier_input$Keywords[group_id] <- new_keywords_string

          showNotification(
            paste("Deleted keyword:", delete_info$keyword_to_delete),
            type = "message", duration = 3
          )
        } else {
          # If no keywords left, remove the entire row
          classifier_values$classifier_input <- classifier_values$classifier_input[-group_id, , drop = FALSE]

          showNotification(
            paste("Deleted keyword:", delete_info$keyword_to_delete, "and removed empty group"),
            type = "message", duration = 3
          )
        }

        auto_save()  # Auto-save when classifiers change

        # Clear stored info
        session$userData$delete_info <- NULL
      }
      removeModal()
    }, error = function(e) {
      showNotification(paste("Error deleting keyword:", e$message), type = "error")
      removeModal()
    })
  })

  output$classifier_list <- DT::renderDataTable({
    # Force reactive dependency on classifier_values$classifier_input
    current_classifiers <- classifier_values$classifier_input

    if (nrow(current_classifiers) == 0) {
      datatable(
        data.frame(Message = "No classifiers added yet. Use the form above to add classifiers."),
        rownames = FALSE,
        colnames = NULL,
        class = "cell-border stripe",
        editable = FALSE,
        options = list(dom = 't', ordering = FALSE)
      )
    } else {
      # Expand keywords into individual rows with tracking info
      # Use seq_len and lapply to ensure correct group_id assignment
      expanded_data <- do.call(rbind, lapply(seq_len(nrow(current_classifiers)), function(i) {
        keywords <- trimws(strsplit(current_classifiers$Keywords[i], ",")[[1]])
        keywords <- keywords[keywords != ""]
        if (length(keywords) > 0) {
          data.frame(
            Group_ID = i,  # Use actual row index from current state
            Keyword = keywords,
            Keyword_Index = 1:length(keywords),
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            Group_ID = integer(0),
            Keyword = character(0),
            Keyword_Index = integer(0),
            stringsAsFactors = FALSE
          )
        }
      }))

      if (nrow(expanded_data) == 0) {
        datatable(
          data.frame(Message = "No valid keywords found."),
          rownames = FALSE,
          colnames = NULL,
          options = list(dom = 't', ordering = FALSE)
        )
      } else {
        # Add delete buttons for each individual keyword
        expanded_data$Delete_Button <- paste0(
          '<button class="btn btn-danger btn-xs" onclick="deleteKeyword(',
          expanded_data$Group_ID, ',', expanded_data$Keyword_Index,
          ')" style="padding: 2px 6px; font-size: 11px;">',
          '<i class="fa fa-trash"></i>',
          '</button>'
        )

        # Create final display (only Keyword and Delete columns)
        final_display <- data.frame(
          `Keyword` = expanded_data$Keyword,
          `Delete` = expanded_data$Delete_Button,
          stringsAsFactors = FALSE,
          check.names = FALSE
        )

        datatable(
          final_display,
          rownames = FALSE,
          colnames = NULL,
          class = "cell-border stripe",
          escape = FALSE,
          editable = FALSE,
          options = list(
            dom = 't',
            ordering = FALSE,
            pageLength = 25,
            columnDefs = list(
              list(width = '80%', targets = 0, className = 'dt-left'),     # Keyword
              list(width = '20%', targets = 1, className = 'dt-center')    # Delete
            )
          )
        )
      }
    }
  })

  ##### KEYWORD SUGGESTER (NAIVE BAYES) #####

  ##### KEYWORD SUGGESTER (NAIVE BAYES) - FIXED VERSION #####

  # Reactive to store suggested keywords
  suggested_keywords <- reactiveValues(
    keywords = NULL
  )

  # Run Naive Bayes keyword suggester
  observeEvent(input$predict_classifiers, {

    # Pre-flight checks BEFORE withProgress
    if (nrow(training_values$training_results) < 10) {
      showNotification(
        "Need at least 10 trained examples to suggest keywords. Please complete more training first.",
        type = "warning",
        duration = 5
      )
      return()
    }

    # Prepare data in the format expected by the NB model
    nb_data <- data.frame(
      text = training_values$training_results$TextData,
      class = training_values$training_results$user.coding,
      stringsAsFactors = FALSE
    )

    # Check if we have both classes
    if (length(unique(nb_data$class)) < 2) {
      showNotification(
        "Need examples of both positive and negative cases to suggest keywords. Please code examples from both categories.",
        type = "warning",
        duration = 5
      )
      return()
    }

    # Check minimum examples per class
    class_counts <- table(nb_data$class)
    if (any(class_counts < 5)) {
      showNotification(
        paste("Need at least 5 examples of each class. Current counts:",
              "Positive =", class_counts["1"], "| Negative =", class_counts["0"]),
        type = "warning",
        duration = 5
      )
      return()
    }

    # All checks passed, proceed with training
    withProgress(message = 'Training Naive Bayes model...', value = 0, {
      tryCatch({
        incProgress(0.2, detail = "Preparing training data...")

        # Create a temporary file to save the data
        temp_file <- tempfile(fileext = ".csv")
        write_csv(nb_data, temp_file)

        incProgress(0.3, detail = "Building vocabulary...")

        # Check if temp file was created successfully
        if (!file.exists(temp_file)) {
          stop("Failed to create temporary data file")
        }

        incProgress(0.2, detail = "Training model...")

        # Run the Naive Bayes analysis
        model_results <- train_and_analyze_model(temp_file)

        # Clean up temp file
        unlink(temp_file)

        # Check if we got valid results
        if (is.null(model_results) || is.null(model_results$positive_tokens)) {
          stop("Model training failed to produce results")
        }

        if (nrow(model_results$positive_tokens) == 0) {
          stop("No positive keywords found. This may indicate an issue with the training data.")
        }

        incProgress(0.2, detail = "Analyzing keywords...")

        # Get top 10 positive keywords
        top_keywords <- model_results$positive_tokens %>%
          head(10) %>%
          select(
            Keyword = token,
            `Predictive Ratio` = predictive_ratio,
            `Positive Count` = class_1_freq,
            `Negative Count` = class_0_freq
          ) %>%
          mutate(
            `Predictive Ratio` = round(`Predictive Ratio`, 2)
          )

        suggested_keywords$keywords <- top_keywords

        incProgress(0.1, detail = "Complete!")

        showNotification(
          paste("✓ Generated", nrow(top_keywords), "keyword suggestions!"),
          type = "message",
          duration = 3
        )

      }, error = function(e) {
        # Clean up temp file if it exists
        if (exists("temp_file") && file.exists(temp_file)) {
          unlink(temp_file)
        }

        showNotification(
          paste("Error generating keywords:", e$message),
          type = "error",
          duration = 8
        )

        # Clear any partial results
        suggested_keywords$keywords <- NULL
      })
    })
  })

  # Render suggested keywords table
  output$suggested_keywords_table <- DT::renderDataTable({
    req(suggested_keywords$keywords)

    # Add action buttons to each row
    keywords_with_buttons <- suggested_keywords$keywords
    keywords_with_buttons$Add <- paste0(
      '<button class="btn btn-primary btn-xs add-keyword-btn" ',
      'data-keyword="', keywords_with_buttons$Keyword, '" ',
      'style="padding: 4px 8px; font-size: 12px;">',
      '<i class="fa fa-plus"></i> Add',
      '</button>'
    )

    datatable(
      keywords_with_buttons,
      rownames = FALSE,
      escape = FALSE,
      class = "cell-border stripe",
      options = list(
        dom = 't',
        ordering = TRUE,
        pageLength = 10,
        columnDefs = list(
          list(className = 'dt-left', targets = 0),
          list(className = 'dt-center', targets = 1:4)
        )
      )
    )
  })

  # Handle adding suggested keywords
  observeEvent(input$add_suggested_keyword, {
    keyword_to_add <- input$add_suggested_keyword

    if (!is.null(keyword_to_add) && nchar(keyword_to_add) > 0) {
      # Check if keyword already exists
      existing_keywords <- classifier_values$classifier_input %>%
        rowwise() %>%
        do({
          keywords <- trimws(strsplit(.$Keywords, ",")[[1]])
          data.frame(Keyword = keywords, stringsAsFactors = FALSE)
        }) %>%
        pull(Keyword)

      if (keyword_to_add %in% existing_keywords) {
        showNotification(
          paste("Keyword '", keyword_to_add, "' already exists in your classifier"),
          type = "warning",
          duration = 3
        )
        return()
      }

      # Add to classifier input
      new_row <- data.frame(
        Keywords = keyword_to_add,
        stringsAsFactors = FALSE
      )

      classifier_values$classifier_input <- rbind(classifier_values$classifier_input, new_row)
      auto_save()

      showNotification(
        paste("✓ Added keyword:", keyword_to_add),
        type = "message",
        duration = 2
      )
    }
  })

  # Control suggested keywords visibility
  output$has_suggestions <- reactive({
    !is.null(suggested_keywords$keywords)
  })
  outputOptions(output, "has_suggestions", suspendWhenHidden = FALSE)

  # Save current tab when user switches tabs
  observeEvent(input$tabs, {
    if (!is.null(input$tabs) && session_values$data_loaded) {
      session_values$current_tab <- input$tabs
      auto_save()
    }
  })



  ##### TRAINING ####

  # Helper function to show training example
  show_training_example <- function(example_type) {
    req(coded_data_values$coded_data, current_code(), dataset())

    tryCatch({
      coded_data <- coded_data_values$coded_data
      original_data <- dataset()
      code_name <- current_code()
      new_col_name <- paste0(code_name, ".Positive")

      # Determine which indices to use based on example type
      target_value <- if (example_type == "positive") 1 else 0
      target_indices <- which(coded_data[[new_col_name]] == target_value)
      available_indices <- setdiff(target_indices, training_values$shown_indices)

      if (length(available_indices) == 0) {
        output$sample_text <- renderUI({
          p(paste("No more new", example_type, "items to show. All", example_type, "examples have been coded!"),
            style = "color: #ffc107; font-weight: bold;")
        })
        training_values$current_sample <- NULL
      } else {
        # Sample and retrieve data
        random_idx <- sample(available_indices, 1)
        selected_text <- coded_data[random_idx, "TextData"]
        auto_coding <- coded_data[random_idx, new_col_name]
        actual_row_id <- original_data$.row_id[random_idx]

        # Update training state
        training_values$shown_indices <- c(training_values$shown_indices, random_idx)
        training_values$current_sample <- list(
          text = selected_text,
          auto_coding = auto_coding,
          .row_id = actual_row_id
        )
        training_values$current_example_type <- example_type

        # Show buttons
        shinyjs::show("training_yes")
        shinyjs::show("training_no")

        # Display example with appropriate styling
        label_text <- if (example_type == "positive") "POSITIVE EXAMPLE:" else "NEGATIVE EXAMPLE:"
        label_color <- if (example_type == "positive") "#28a745" else "#dc3545"

        output$sample_text <- renderUI({
          div(
            h5(label_text, style = paste0("color: ", label_color, "; font-weight: bold; margin-bottom: 10px;")),
            p(selected_text, style = "line-height: 1.5; margin: 0;")
          )
        })
      }
    }, error = function(e) {
      output$sample_text <- renderUI({
        p(paste("Error retrieving", example_type, "example."), style = "color: #dc3545;")
      })
      training_values$current_sample <- NULL
    })
  }

  # Show positive example
  observeEvent(input$show_positive, {
    show_training_example("positive")
  })

  # Show negative example
  observeEvent(input$show_negative, {
    show_training_example("negative")
  })


  # Handle training YES
  observeEvent(input$training_yes, {
    req(training_values$current_sample)

    user_coding <- 1  # User says YES, should be coded
    auto_coding <- training_values$current_sample$auto_coding

    new_training_row <- data.frame(
      .row_id = training_values$current_sample$.row_id,
      TextData = training_values$current_sample$text,
      auto.coding = auto_coding,        # What classifier predicted
      user.coding = user_coding,        # What user decided
      stringsAsFactors = FALSE
    )

    training_values$training_results <- rbind(training_values$training_results, new_training_row)
    training_values$current_sample <- NULL

    shinyjs::hide("training_yes")
    shinyjs::hide("training_no")

    # Show agreement/disagreement feedback
    agreement <- ifelse(auto_coding == user_coding, "Agreement! ✓", "Disagreement noted.")
    color <- ifelse(auto_coding == user_coding, "#28a745", "#ffc107")

    output$sample_text <- renderUI({
      div(
        p(agreement, style = paste0("color: ", color, "; font-weight: bold; text-align: center; margin-bottom: 10px;")),
        p("Response recorded! Click a button above to see another example.",
          style = "color: #6c757d; font-style: italic; text-align: center;")
      )
    })
  })

  # Handle training NO
  observeEvent(input$training_no, {
    req(training_values$current_sample)

    user_coding <- 0  # User says NO, should not be coded
    auto_coding <- training_values$current_sample$auto_coding

    new_training_row <- data.frame(
      .row_id = training_values$current_sample$.row_id,
      TextData = training_values$current_sample$text,
      auto.coding = auto_coding,        # What classifier predicted
      user.coding = user_coding,        # What user decided
      stringsAsFactors = FALSE
    )

    training_values$training_results <- rbind(training_values$training_results, new_training_row)
    training_values$current_sample <- NULL

    shinyjs::hide("training_yes")
    shinyjs::hide("training_no")

    # Show agreement/disagreement feedback
    agreement <- ifelse(auto_coding == user_coding, "Agreement! ✓", "Disagreement noted.")
    color <- ifelse(auto_coding == user_coding, "#28a745", "#ffc107")

    output$sample_text <- renderUI({
      div(
        p(agreement, style = paste0("color: ", color, "; font-weight: bold; text-align: center; margin-bottom: 10px;")),
        p("Response recorded! Click a button above to see another example.",
          style = "color: #6c757d; font-style: italic; text-align: center;")
      )
    })
  })

  # Calculate training metrics
  training_metrics <- reactive({
    req(nrow(training_values$training_results) > 0)

    training_data <- training_values$training_results

    tp <- sum(training_data$auto.coding == 1 & training_data$user.coding == 1)
    fp <- sum(training_data$auto.coding == 1 & training_data$user.coding == 0)
    fn <- sum(training_data$auto.coding == 0 & training_data$user.coding == 1)
    tn <- sum(training_data$auto.coding == 0 & training_data$user.coding == 0)

    total <- tp + fp + fn + tn
    baserate <- (tp + fp) / total
    baserate2 <- (tp + fn) / total
    accuracy <- (tp + tn) / total

    pe <- (baserate * baserate2) + ((1 - baserate) * (1 - baserate2))
    kappa <- if (pe < 1) (accuracy - pe) / (1 - pe) else 0

    FDR <- if ((tp + fp) > 0) fp / (tp + fp) else 0
    FOR <- if ((fn + tn) > 0) fn / (fn + tn) else 0
    precision <- if ((tp + fp) > 0) tp / (tp + fp) else 0
    recall <- if ((tp + fn) > 0) tp / (tp + fn) else 0

    list(
      kappa = kappa,
      FDR = FDR,
      FOR = FOR,
      precision = precision,
      recall = recall,
      accuracy = accuracy
    )
  })

  # Output training metrics with NULL safety
  output$training_metrics <- DT::renderDataTable({
    metrics <- training_metrics()

    if (is.null(metrics) || nrow(training_values$training_results) == 0) {
      datatable(
        data.frame(Message = "Start coding to see the training metrics!"),
        rownames = FALSE,
        colnames = NULL,
        class = "cell-border stripe",
        options = list(dom = 't', ordering = FALSE)
      )
    } else {
      metrics_df <- data.frame(
        Metric = c("Cohen's Kappa",
                   paste0("False Discovery Rate (FDR) ",
                          '<i class="fa fa-info-circle" data-toggle="tooltip" ',
                          'data-placement="right" ',
                          'title="Proportion of positive predictions that were incorrect. Lower is better. High FDR means your classifier is coding too many false positives."></i>'),
                   paste0("False Omission Rate (FOR) ",
                          '<i class="fa fa-info-circle" data-toggle="tooltip" ',
                          'data-placement="right" ',
                          'title="Proportion of negative predictions that were incorrect. Lower is better. High FOR means your classifier is missing true positives."></i>')
        ),
        Value = c(
          round(metrics$kappa, 3),
          round(metrics$FDR, 3),
          round(metrics$FOR, 3)
        ),
        stringsAsFactors = FALSE
      )

      datatable(
        metrics_df,
        rownames = FALSE,
        class = "cell-border stripe",
        escape = FALSE,
        options = list(
          dom = 't',
          ordering = FALSE,
          initComplete = JS(
            "function(settings, json) {",
            "  $('[data-toggle=\"tooltip\"]').tooltip();",
            "}"
          ),
          drawCallback = JS(
            "function(settings) {",
            "  $('[data-toggle=\"tooltip\"]').tooltip();",
            "}"
          )
        )
      )
    }
  })

  # Reset training metrics handler with auto-save
  observeEvent(input$reset_training_metrics, {
    if (nrow(training_values$training_results) > 0) {
      showModal(modalDialog(
        title = "Reset Training Data",
        div(
          div(
            class = "alert alert-warning",
            style = "margin-bottom: 20px;",
            h5(icon("exclamation-triangle"), " Warning", style = "color: #856404; margin-bottom: 10px;"),
            p("This will permanently delete all your training progress:", style = "margin-bottom: 10px;"),
            tags$ul(
              tags$li(paste("All", nrow(training_values$training_results), "coded training items")),
              tags$li("Training metrics calculations"),
              tags$li("Confusion matrix data"),
              tags$li("Progress on examples shown")
            )
          ),
          p("You will need to start training from scratch.",
            style = "font-weight: bold; margin-bottom: 15px;"),
          p("Are you sure you want to reset all training data?",
            style = "color: #dc3545; font-weight: bold;")
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_reset_training",
                       "Yes, Reset Training Data",
                       class = "btn-danger",
                       icon = icon("trash"))
        )
      ))
    } else {
      showNotification("No training data to reset.", type = "message", duration = 3)
    }
  })

  # Confirm training reset with auto-save
  observeEvent(input$confirm_reset_training, {
    tryCatch({
      # Store count for notification
      items_cleared <- nrow(training_values$training_results)

      # Reset all training data
      training_values$training_results <- data.frame(
        .row_id = integer(),
        TextData = character(),
        auto.coding = integer(),
        user.coding = integer(),
        stringsAsFactors = FALSE
      )

      # Reset shown indices so user can see examples again
      training_values$shown_indices <- integer()

      # Clear current sample if any
      training_values$current_sample <- NULL
      training_values$current_.row_id <- NULL
      training_values$current_example_type <- NULL

      # Clear suggested keywords
      suggested_keywords$keywords <- NULL

      auto_save()  # Auto-save the reset state

      # Hide any visible agree/disagree buttons
      shinyjs::hide("agree")
      shinyjs::hide("disagree")

      # Reset sample text display
      output$sample_text <- renderUI({
        p("Training data reset. Click 'Show Positive Example' or 'Show Negative Example' to start training again.",
          style = "color: #6c757d; font-style: italic; text-align: center; margin: 20px 0;")
      })

      removeModal()

      showNotification(
        paste("Training data reset successfully! Cleared", items_cleared, "training items."),
        type = "message", duration = 5
      )

    }, error = function(e) {
      showNotification(paste("Error resetting training data:", e$message), type = "error")
      removeModal()
    })
  })

  # Output confusion matrix
  output$confusion_matrix_table <- DT::renderDataTable({
    if (nrow(training_values$training_results) == 0) {
      datatable(
        data.frame(Message = "Start coding to see the confusion matrix!"),
        rownames = FALSE,
        colnames = NULL,
        class = "cell-border stripe",
        options = list(dom = 't', ordering = FALSE)
      )
    } else {
      training_data <- training_values$training_results

      tp <- sum(training_data$auto.coding == 1 & training_data$user.coding == 1)
      fp <- sum(training_data$auto.coding == 1 & training_data$user.coding == 0)
      fn <- sum(training_data$auto.coding == 0 & training_data$user.coding == 1)
      tn <- sum(training_data$auto.coding == 0 & training_data$user.coding == 0)

      confusion_df <- data.frame(
        ` ` = c("Classifier said YES", "Classifier said NO"),
        `You said YES` = c(tp, fn),
        `You said NO` = c(fp, tn),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )

      datatable(
        confusion_df,
        rownames = FALSE,
        class = "cell-border stripe",
        options = list(
          dom = 't',
          ordering = FALSE,
          columnDefs = list(
            list(className = 'dt-left', targets = 0),
            list(className = 'dt-center', targets = 1:2)
          )
        )
      )
    }
  })

  # Download handler
  output$download_training_data <- downloadHandler(
    filename = function() {
      code_name <- if (!is.null(current_code())) current_code() else "training"
      paste0(code_name, "_training_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (nrow(training_values$training_results) == 0) {
        write.csv(
          data.frame(Message = "No training data available yet."),
          file, row.names = FALSE
        )
      } else {
        training_data <- training_values$training_results
        metrics <- training_metrics()

        export_data <- training_data
        export_data$Agreement <- ifelse(
          export_data$auto.coding == export_data$user.coding,
          "Agreement", "Disagreement"
        )
        export_data$Classification_Type <- dplyr::case_when(
          export_data$auto.coding == 1 & export_data$user.coding == 1 ~ "True Positive",
          export_data$auto.coding == 1 & export_data$user.coding == 0 ~ "False Positive",
          export_data$auto.coding == 0 & export_data$user.coding == 1 ~ "False Negative",
          export_data$auto.coding == 0 & export_data$user.coding == 0 ~ "True Negative"
        )

        write.csv(export_data, file, row.names = FALSE)
      }
    }
  )

  # Download training metrics handler
  output$download_training_metrics <- downloadHandler(
    filename = function() {
      code_name <- if (!is.null(current_code())) current_code() else "training"
      paste0(code_name, "_training_metrics_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (nrow(training_values$training_results) == 0) {
        write.csv(
          data.frame(Message = "No training data available yet."),
          file, row.names = FALSE
        )
      } else {
        # Calculate metrics and confusion matrix
        metrics <- training_metrics()
        training_data <- training_values$training_results

        # Calculate confusion matrix values
        tp <- sum(training_data$auto.coding == 1 & training_data$user.coding == 1)
        fp <- sum(training_data$auto.coding == 1 & training_data$user.coding == 0)
        fn <- sum(training_data$auto.coding == 0 & training_data$user.coding == 1)
        tn <- sum(training_data$auto.coding == 0 & training_data$user.coding == 0)

        # Create comprehensive metrics export
        metrics_export <- data.frame(
          Metric_Category = c(
            "Performance Metrics", "Performance Metrics", "Performance Metrics",
            "Performance Metrics", "Performance Metrics", "Performance Metrics",
            "Confusion Matrix", "Confusion Matrix", "Confusion Matrix", "Confusion Matrix",
            "Summary Statistics", "Summary Statistics"
          ),
          Metric_Name = c(
            "Cohen's Kappa", "False Discovery Rate (FDR)", "False Omission Rate (FOR)",
            "Precision", "Recall", "Accuracy",
            "True Positives", "False Positives", "False Negatives", "True Negatives",
            "Total Coded Items", "Agreement Rate"
          ),
          Value = c(
            round(metrics$kappa, 4),
            round(metrics$FDR, 4),
            round(metrics$FOR, 4),
            round(metrics$precision, 4),
            round(metrics$recall, 4),
            round(metrics$accuracy, 4),
            tp, fp, fn, tn,
            nrow(training_data),
            round(sum(training_data$auto.coding == training_data$user.coding) / nrow(training_data), 4)
          ),
          Description = c(
            "Inter-rater reliability measure",
            "Proportion of positive predictions that were wrong",
            "Proportion of negative predictions that were wrong",
            "Proportion of positive predictions that were correct",
            "Proportion of actual positives correctly identified",
            "Overall proportion of correct predictions",
            "Classifier=YES, You=YES",
            "Classifier=YES, You=NO",
            "Classifier=NO, You=YES",
            "Classifier=NO, You=NO",
            "Total number of items coded",
            "Proportion of items where you agreed with classifier"
          ),
          stringsAsFactors = FALSE
        )

        write.csv(metrics_export, file, row.names = FALSE)
      }
    }
  )

  # Navigation buttons
  observeEvent(input$move_to_testing, {
    updateNavbarPage(session, "tabs", selected = "validation")
    showNotification("Moved to Validation", type = "message", duration = 2)
  })

  observeEvent(input$move_back_classifier, {
    updateNavbarPage(session, "tabs", selected = "create_classifiers")
    showNotification("Returned to Classifier Creation", type = "message", duration = 2)
  })

  ##### VALIDATION (PERFECT SAMPLING WITH CYCLING) ####

  # Display current code name in Validation tab
  output$validation_code_name <- renderText({
    code <- current_code()
    if (is.null(code)) {
      return("[No code defined]")
    }
    return(code)
  })

  # Helper function to load next validation item
  load_next_validation_item <- function() {
    if (is.null(validation_values$validation_pool)) return()
    if (validation_values$validation_complete) return()
    if (validation_values$cycle_failed) return()

    # Find next unvalidated item from current cycle
    current_cycle_coded <- validation_values$current_cycle_results$pool_index
    available_pool_indices <- setdiff(1:nrow(validation_values$validation_pool), current_cycle_coded)

    if (length(available_pool_indices) == 0) {
      showNotification("Validation pool exhausted unexpectedly", type = "error")
      return()
    }

    # Select next item from pool
    selected_pool_index <- sample(available_pool_indices, 1)
    selected_item <- validation_values$validation_pool[selected_pool_index, ]

    validation_values$current_validation_item <- selected_item
    validation_values$current_pool_index <- selected_pool_index

    # Display the item
    output$validation_text <- renderUI({
      div(
        style = "background: linear-gradient(90deg, #f8f9fa, #ffffff); padding: 25px; border-left: 4px solid #007bff; border-radius: 6px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom: 20px;",
        p(selected_item$TextData, style = "line-height: 1.8; margin: 0; font-size: 16px; color: #333;")
      )
    })

    shinyjs::show("validate_yes")
    shinyjs::show("validate_no")
  }

  # Calculate Cai's N with proper edge case handling

  # Carl's note: added argument data_size for this function

  calculate_cais_n <- function(tau_kappa = 0.80, base_rate, alpha = 0.025, data_size,previously_coded = 0) {
    req(base_rate >= 0, base_rate <= 1)

    # Handle extreme base rates first
    if (base_rate <= 0.01 || base_rate >= 0.99) {
      showNotification(
        paste("Extreme base rate detected:", round(base_rate, 3),
              "- Using observed rate without adjustment"),
        type = "warning"
      )
      b1_hat <- base_rate
    } else if (previously_coded > 10) {
      z_score <- qnorm(1 - alpha/2)
      se <- sqrt((base_rate * (1 - base_rate)) / previously_coded)
      adjustment <- z_score * se

      # Apply adjustment but keep reasonable bounds (don't go below 5% or above 95%)
      b1_hat <- base_rate - adjustment
      b1_hat <- max(0.05, min(0.95, b1_hat))

      # If adjustment makes base rate too extreme, use observed rate
      if (abs(b1_hat - base_rate) > 0.4) {
        b1_hat <- base_rate
        showNotification("Base rate adjustment too extreme - using observed rate", type = "message")
      }
    } else {
      # Use observed base rate for small amounts of previous coding
      b1_hat <- base_rate
    }

    # Calculate maximum accuracy using equation from the paper
    # numerator <- b1_hat * tau_kappa
    # denominator <- 1 - tau_kappa + tau_kappa * b1_hat + b1_hat * (1 - tau_kappa)

    # if (denominator <= 0) {
    #   return(list(cais_n = Inf, a_max = NA, b1_adjusted = b1_hat))
    # }

    # fraction_term <- numerator / denominator
    # a_max <- 1 - b1_hat + b1_hat * tau_kappa - (1 - tau_kappa) * (1 - 2 * b1_hat) * fraction_term

    # Carl's note: I commented the block above
    # and uses the following line
    # to corrrectly compute a_max
    a_max<-max_accuracy(b1_hat,tau_kappa)
    # Check if a_max is reasonable
    if (a_max <= 0.5 || a_max >= 1) {
      showNotification(
        paste("Calculated a_max is extreme:", round(a_max, 3),
              "- May indicate base rate issues"),
        type = "warning"
      )
      return(list(cais_n = Inf, a_max = a_max, b1_adjusted = b1_hat))
    }

    # Check log value
    if (log(a_max) >= 0) {  # log(a_max) should be negative
      return(list(cais_n = Inf, a_max = a_max, b1_adjusted = b1_hat))
    }

    # Calculate Cai's N
    cais_n <- ceiling(log(alpha) / log(a_max))

    # Cap Cai's N at reasonable maximum (based on available data size)
    # max_reasonable_n <- min(1000, nrow(coded_data_values$coded_data) * 0.1)  # Cap at 10% of dataset
    max_reasonable_n <- min(1000, data_size * 0.1)  # Cap at 10% of dataset

    if (cais_n > max_reasonable_n) {
      showNotification(
        paste("Cai's N too large:", cais_n, "- Capped at", max_reasonable_n,
              ". Consider refining classifiers further."),
        type = "warning"
      )
      cais_n <- max_reasonable_n
    }

    return(list(
      cais_n = max(1, cais_n),
      a_max = a_max,
      b1_adjusted = b1_hat,
      original_base_rate = base_rate
    ))
  }

  # Initialize validation pool (create fresh test set for new cycle)
  initialize_validation_cycle <- function() {
    req(coded_data_values$coded_data, current_code())

    # Reset validation state at start of cycle
    validation_values$validation_complete <- FALSE
    validation_values$cycle_failed <- FALSE
    validation_values$current_validation_item <- NULL
    validation_values$current_pool_index <- NULL

    coded_data <- coded_data_values$coded_data
    code_name <- current_code()
    new_col_name <- paste0(code_name, ".Positive")

    # Exclude items already used in training
    excluded_indices <- c()
    if (nrow(training_values$training_results) > 0) {
      excluded_indices <- training_values$training_results$.row_id
    }

    if (length(excluded_indices) > 0) {
      available_data <- coded_data[-excluded_indices, ]
      available_indices <- setdiff(1:nrow(coded_data), excluded_indices)
    } else {
      available_data <- coded_data
      available_indices <- 1:nrow(coded_data)
    }

    if (nrow(available_data) == 0) {
      showNotification("No more data available for validation", type = "error")
      return(NULL)
    }

    # Estimate base rate from available data
    estimated_baserate <- sum(available_data[[new_col_name]] == 1) / nrow(available_data)

    # Calculate previously coded items for base rate adjustment
    previously_coded <- nrow(training_values$training_results)

    # Show base rate information
    showNotification(
      paste("Validation cycle setup - Base rate:", round(estimated_baserate, 3),
            "| Previously coded:", previously_coded),
      type = "message", duration = 3
    )

    # Check if base rate is reasonable for validation
    if (estimated_baserate < 0.03 || estimated_baserate > 0.95) {
      showModal(modalDialog(
        title = "Extreme Base Rate Detected",
        div(
          p(paste("Your classifiers predict a base rate of", round(estimated_baserate * 100, 1), "%")),
          p("This may lead to very large required sample sizes."),
          p("Consider:"),
          tags$ul(
            tags$li("Refining your classifiers to be less extreme"),
            tags$li("Adding more diverse keywords"),
            tags$li("Checking if your code definition is too narrow/broad")
          )
        ),
        footer = tagList(
          modalButton("Continue Anyway"),
          actionButton("go_back_to_classifiers", "Refine Classifiers", class = "btn-primary")
        )
      ))
    }

    # Calculate Cai's N
    cais_result <- calculate_cais_n(
      tau_kappa = 0.80,
      base_rate = estimated_baserate,
      alpha = 0.025,
      previously_coded = previously_coded,
      data_size = nrow(coded_data_values$coded_data)
    )

    if (is.infinite(cais_result$cais_n)) {
      showNotification(
        paste("Cannot calculate Cai's N. Base rate:", round(estimated_baserate, 3),
              "Previously coded:", previously_coded),
        type = "error"
      )
      return(NULL)
    }

    # Create validation pool for this cycle (will sample randomly when selecting items)
    validation_pool <- available_data
    validation_pool$original_indices <- available_indices

    return(list(
      validation_pool = validation_pool,
      cais_n = cais_result$cais_n,
      estimated_baserate = estimated_baserate,
      adjusted_baserate = cais_result$b1_adjusted,
      a_max = cais_result$a_max
    ))
  }

  # Handle going back to classifiers from base rate modal
  observeEvent(input$go_back_to_classifiers, {
    removeModal()
    updateNavbarPage(session, "tabs", selected = "create_classifiers")
    showNotification("Please refine your classifiers to adjust the base rate", type = "message", duration = 5)
  })

  # Initialize validation when moving to validation tab
  observeEvent(input$tabs, {
    if (input$tabs == "validation") {
      if (is.null(validation_values$validation_pool)) {
        val_setup <- initialize_validation_cycle()
        if (!is.null(val_setup)) {
          validation_values$validation_pool <- val_setup$validation_pool
          validation_values$cais_n <- val_setup$cais_n
          validation_values$estimated_baserate <- val_setup$estimated_baserate
          validation_values$adjusted_baserate <- val_setup$adjusted_baserate
          validation_values$a_max <- val_setup$a_max
          validation_values$current_cycle <- 1
          validation_values$perfect_agreements_current_cycle <- 0
          validation_values$validation_complete <- FALSE
          validation_values$cycle_failed <- FALSE
          validation_values$total_items_coded <- 0

          # AUTO-LOAD FIRST ITEM
          load_next_validation_item()
        }
      }
    }
  })

  # Handle validation responses - YES with auto-save
  observeEvent(input$validate_yes, {
    req(validation_values$current_validation_item)
    process_validation_response(user_coding = 1)
    auto_save()  # Auto-save after validation response
  })

  # Handle validation responses - NO with auto-save
  observeEvent(input$validate_no, {
    req(validation_values$current_validation_item)
    process_validation_response(user_coding = 0)
    auto_save()  # Auto-save after validation response
  })

  # Process validation response using Perfect Sampling Cycling Logic
  process_validation_response <- function(user_coding) {
    code_name <- current_code()
    new_col_name <- paste0(code_name, ".Positive")

    current_item <- validation_values$current_validation_item
    auto_coding <- current_item[[new_col_name]]

    # Record the response
    new_validation_row <- data.frame(
      pool_index = validation_values$current_pool_index,
      original_.row_id = current_item$original_indices,
      TextData = current_item$TextData,
      auto.coding = auto_coding,
      user.coding = user_coding,
      cycle = validation_values$current_cycle,
      stringsAsFactors = FALSE
    )

    validation_values$current_cycle_results <- rbind(validation_values$current_cycle_results, new_validation_row)
    validation_values$all_validation_results <- rbind(validation_values$all_validation_results, new_validation_row)
    validation_values$total_items_coded <- validation_values$total_items_coded + 1

    # Clear current item
    validation_values$current_validation_item <- NULL
    validation_values$current_pool_index <- NULL
    shinyjs::hide("validate_yes")
    shinyjs::hide("validate_no")

    # PERFECT SAMPLING RULE: Check for agreement
    if (auto_coding == user_coding) {
      # AGREEMENT - increment perfect agreement count for current cycle
      validation_values$perfect_agreements_current_cycle <- validation_values$perfect_agreements_current_cycle + 1

      # Check if we've reached Cai's N (perfect sampling complete)
      if (validation_values$perfect_agreements_current_cycle >= validation_values$cais_n) {
        # SUCCESS! Perfect sampling achieved
        validation_values$validation_complete <- TRUE
        shinyjs::hide("validation_controls")
        shinyjs::show("validation_complete")

        showNotification(
          paste0("✓ Perfect sampling achieved! κ > 0.80 validated with ",
                 validation_values$perfect_agreements_current_cycle, " consecutive perfect agreements in cycle ",
                 validation_values$current_cycle),
          type = "message", duration = 10
        )

        output$validation_text <- renderUI({
          div(
            class = "alert alert-success",
            style = "text-align: center; padding: 35px;",
            h3("✓ Perfect Sampling Complete!", style = "color: #28a745; margin-bottom: 25px;"),
            h5("Your automated classifier has been validated!", style = "margin-bottom: 20px;"),
            div(
              style = "background-color: #d4edda; padding: 20px; border-radius: 8px; margin: 20px 0;",
              div(
                style = "display: grid; grid-template-columns: 1fr 1fr; gap: 15px; text-align: left;",
                div(
                  p("✓ Perfect agreements:", strong(validation_values$perfect_agreements_current_cycle, "/", validation_values$cais_n),
                    style = "margin: 5px 0;"),
                  p("✓ Validation cycle:", strong(validation_values$current_cycle), style = "margin: 5px 0;"),
                  p("✓ Total items coded:", strong(validation_values$total_items_coded), style = "margin: 5px 0;")
                ),
                div(
                  p("✓ κ > 0.80 validated (α = 0.025)", style = "margin: 5px 0; font-weight: bold;"),
                  p("✓ Statistical significance achieved", style = "margin: 5px 0; font-weight: bold;"),
                  p("✓ Classifier ready for use", style = "margin: 5px 0; font-weight: bold;")
                )
              )
            ),
            p("Your automated classifier has passed perfect sampling validation and is ready for use on the full dataset!",
              style = "font-size: 16px; font-weight: bold; color: #155724; margin-top: 15px;")
          )
        })

        return()
      } else {
        # Continue current cycle - need more perfect agreements
        # AUTO-LOAD NEXT ITEM
        Sys.sleep(0.2)  # Small delay for smooth transition
        load_next_validation_item()
      }
    } else {
      # DISAGREEMENT - Cycle failed, must refine classifiers and start new cycle
      validation_values$cycle_failed <- TRUE

      # Move disagreed item to training data
      failed_training_data <- data.frame(
        .row_id = current_item$original_indices,
        TextData = current_item$TextData,
        auto.coding = auto_coding,
        user.coding = user_coding,
        stringsAsFactors = FALSE
      )
      training_values$training_results <- rbind(training_values$training_results, failed_training_data)

      showNotification(
        paste("Disagreement in cycle", validation_values$current_cycle,
              "- refinement required. Disagreed item moved to training data."),
        type = "warning", duration = 8
      )

      output$validation_text <- renderUI({
        div(
          class = "alert alert-warning",
          style = "text-align: center;",
          h4("❌ Disagreement Found - Cycle Failed", style = "color: #856404;"),
          h5("Next Steps:"),
          p("1. Go back to 'Create Classifiers' and refine your keywords"),
          p("2. Return here and restart validation for a new cycle"),
          hr(),
          actionButton("go_refine_classifiers",
                       "Go Refine Classifiers",
                       icon = icon("arrow-left"),
                       class = "btn-warning btn-lg")
        )
      })
    }
  }

  # Navigate back to classifier refinement
  observeEvent(input$go_refine_classifiers, {
    updateNavbarPage(session, "tabs", selected = "create_classifiers")
    showNotification("Returned to classifier refinement", type = "message", duration = 3)
  })

  # Start new validation cycle (after refinement)
  start_new_validation_cycle <- function() {
    shinyjs::show("get_validation_item")
    # Reset current cycle data
    validation_values$current_cycle_results <- data.frame(
      pool_index = integer(), original_.row_id = integer(),
      TextData = character(), auto.coding = integer(), user.coding = integer(),
      cycle = integer(), stringsAsFactors = FALSE
    )
    validation_values$perfect_agreements_current_cycle <- 0
    validation_values$cycle_failed <- FALSE
    validation_values$current_cycle <- validation_values$current_cycle + 1

    # Create new validation pool (fresh test set)
    val_setup <- initialize_validation_cycle()
    if (!is.null(val_setup)) {
      validation_values$validation_pool <- val_setup$validation_pool
      validation_values$cais_n <- val_setup$cais_n
      validation_values$estimated_baserate <- val_setup$estimated_baserate
      validation_values$adjusted_baserate <- val_setup$adjusted_baserate
      validation_values$a_max <- val_setup$a_max

      showNotification(
        paste("New validation cycle", validation_values$current_cycle,
              "started with refined classifiers. Cai's N:", val_setup$cais_n),
        type = "message", duration = 5
      )

      output$validation_text <- renderUI({
        div(
          class = "alert alert-info",
          style = "text-align: center; padding: 20px;",
          h5("✓ New Validation Cycle Started", style = "color: #0c5460; margin-bottom: 15px;"),
          p("Loading next validation item...",
            style = "color: #6c757d; font-style: italic; text-align: center;")
        )
      })

      load_next_validation_item()

      return(TRUE)
    }
    return(FALSE)
  }

  # Restart validation (manual restart)
  observeEvent(input$restart_validation, {
    if (start_new_validation_cycle()) {
      shinyjs::show("validation_controls")
      shinyjs::hide("validation_complete")
      load_next_validation_item()
      auto_save()  # Auto-save the restart
    }
  })

  # Auto-restart when returning to validation tab after refinement
  observeEvent(input$tabs, {
    if (input$tabs == "validation" && validation_values$cycle_failed) {
      # Check if classifiers have been modified since last cycle failure
      # For simplicity, always offer to start new cycle when returning
      if (start_new_validation_cycle()) {
        # Cycle successfully restarted
      }
    }
  })

  # Output overall validation progress (Perfect Sampling with Cycles)
  output$validation_overall <- DT::renderDataTable({
    progress_df <- data.frame(
      Metric = c(
        "Total Cycles Attempted",
        "Total Items Coded (All Cycles)",
        "Estimated Base Rate"
      ),
      Value = c(
        max(1, validation_values$current_cycle),
        validation_values$total_items_coded,
        paste0(round(validation_values$estimated_baserate * 100, 1), "%")
      ),
      stringsAsFactors = FALSE
    )

    datatable(
      progress_df,
      rownames = FALSE,
      colnames = NULL,
      class = "cell-border stripe",
      options = list(dom = 't', ordering = FALSE)
    )
  })

  # Output current cycle validation metrics (Perfect Sampling Status)
  output$validation_current_cycle <- DT::renderDataTable({
    if (validation_values$validation_complete) {

      # Calculate progress_pct for completed validation
      progress_pct <- round((validation_values$perfect_agreements_current_cycle / max(1, validation_values$cais_n)) * 100, 1)

      metrics_df <- data.frame(
        Status = c("Current Cycle",
                   "Required Sample Size",
                   "Progress",
                   "Validation Complete"),
        Result = c(
          validation_values$current_cycle,
          validation_values$cais_n,
          paste0(progress_pct,
                 "% (",
                 validation_values$perfect_agreements_current_cycle,
                 "/",
                 validation_values$cais_n,
                 ")",
                 "<br>",
                 validation_values$total_items_coded,
                 " items coded"),
          "✅ κ > 0.80  α = 0.05"
        ),
        stringsAsFactors = FALSE
      )
    } else if (validation_values$cycle_failed) {

      # Calculate progress_pct for completed validation
      progress_pct <- round((validation_values$perfect_agreements_current_cycle / max(1, validation_values$cais_n)) * 100, 1)

      metrics_df <- data.frame(
        Status =  c("Current Cycle",
                    "Required Sample Size",
                    "Progress"),
        Result = c(
          paste(validation_values$current_cycle, "failed ❌"),
          validation_values$cais_n,
          paste0(progress_pct,
                 "% (",
                 validation_values$perfect_agreements_current_cycle,
                 "/",
                 validation_values$cais_n,
                 ")",
                 "<br>",
                 validation_values$total_items_coded,
                 " items coded")
        ),
        stringsAsFactors = FALSE
      )
    } else {

      # Calculate progress_pct for completed validation
      progress_pct <- round((validation_values$perfect_agreements_current_cycle / max(1, validation_values$cais_n)) * 100, 1)

      metrics_df <- data.frame(
        Status = c(
          "Current Cycle",
          "Required Sample Size",
          "Progress"
        ),
        Result = c(
          validation_values$current_cycle,
          validation_values$cais_n,
          paste0(#progress_pct,
            #"% (",
            validation_values$perfect_agreements_current_cycle,
            "/",
            validation_values$cais_n,
            #")",
            "<br>",
            #validation_values$perfect_agreements_current_cycle,
            "items coded")
        ),
        stringsAsFactors = FALSE
      )
    }

    datatable(
      metrics_df,
      rownames = FALSE,
      colnames = NULL,
      class = "cell-border stripe",
      escape = FALSE,
      options = list(dom = 't', ordering = FALSE)
    )
  })

  # Download handlers for validation
  output$download_validation_results <- downloadHandler(
    filename = function() {
      code_name <- if (!is.null(current_code())) current_code() else "validation"
      paste0(code_name, "_perfect_sampling_cycles_", Sys.Date(), ".csv")
    },
    content = function(file) {
      if (nrow(validation_values$all_validation_results) == 0) {
        write.csv(
          data.frame(Message = "No validation data available yet."),
          file, row.names = FALSE
        )
      } else {
        export_data <- validation_values$all_validation_results
        export_data$Agreement <- ifelse(
          export_data$auto.coding == export_data$user.coding,
          "Perfect Agreement", "Disagreement"
        )
        export_data$Cycle_Outcome <- ifelse(
          export_data$Agreement == "Disagreement", "Cycle Failed", "Cycle Continued"
        )

        write.csv(export_data, file, row.names = FALSE)
      }
    }
  )

  output$download_validation_current_cycle <- downloadHandler(
    filename = function() {
      code_name <- if (!is.null(current_code())) current_code() else "validation"
      paste0(code_name, "_perfect_sampling_efficiency_", Sys.Date(), ".csv")
    },
    content = function(file) {
      metrics_export <- data.frame(
        Metric = c(
          "Perfect Sampling Status",
          "Total Cycles Attempted",
          "Current/Final Cycle",
          "Cai's N",
          "Perfect Agreements Achieved",
          "Total Items Coded (All Cycles)",
          "Items Moved to Training",
          "Estimated Base Rate",
          "Adjusted Base Rate",
          "Statistical Test",
          "Alpha Level"
        ),
        Value = c(
          ifelse(validation_values$validation_complete, "Complete", "In Progress"),
          max(1, validation_values$current_cycle),
          validation_values$current_cycle,
          validation_values$cais_n,
          validation_values$perfect_agreements_current_cycle,
          validation_values$total_items_coded,
          nrow(training_values$training_results),
          round(validation_values$estimated_baserate, 4),
          round(validation_values$adjusted_baserate, 4),
          ifelse(validation_values$validation_complete, "Kappa > 0.80 validated", "Not yet achieved"),
          "0.025"
        ),
        Description = c(
          "Whether perfect sampling validation completed successfully",
          "Number of validation cycles attempted (includes failed cycles)",
          "Current cycle number (or final cycle if complete)",
          "Required consecutive perfect agreements",
          "Perfect agreements achieved in current/final cycle",
          "Total validation items coded across all cycles (efficiency measure)",
          "Items with disagreements automatically moved to training data",
          "Base rate estimated from validation pool",
          "Conservative base rate used in Cai's N calculation",
          "Statistical significance Cohen's Kappa test result",
          "Significance level for two tests"
        ),
        stringsAsFactors = FALSE
      )

      write.csv(metrics_export, file, row.names = FALSE)
    }
  )

  # Control validation complete flag for UI
  output$validation_complete_flag <- reactive({
    validation_values$validation_complete
  })
  outputOptions(output, "validation_complete_flag", suspendWhenHidden = FALSE)

  # Code entire dataset and provide download
  observeEvent(input$code_entire_dataset, {
    req(coded_data_values$coded_data, current_code())

    withProgress(message = 'Coding entire dataset...', value = 0, {
      tryCatch({
        # Get original dataset and coding info
        original_data <- dataset()
        coded_data <- coded_data_values$coded_data
        code_name <- current_code()
        new_col_name <- paste0(code_name, ".Positive")

        incProgress(0.3, detail = "Applying classifier...")

        # Create final dataset with original columns plus coding
        final_dataset <- original_data
        final_dataset[[new_col_name]] <- coded_data[[new_col_name]]

        incProgress(0.4, detail = "Adding metadata...")

        # Add metadata columns
        final_dataset$Coding_Method <- "Perfect_Sampling_Validated"
        final_dataset$Validation_Kappa <- "Above_0.80"
        final_dataset$Coding_Date <- as.character(Sys.Date())
        final_dataset$Validation_Cycles <- validation_values$current_cycle
        final_dataset$Cais_N_Required <- validation_values$cais_n

        incProgress(0.2, detail = "Preparing download...")

        # Calculate summary stats
        total_items <- nrow(final_dataset)
        coded_positive <- sum(final_dataset[[new_col_name]] == 1)
        coded_negative <- total_items - coded_positive
        final_base_rate <- round((coded_positive / total_items) * 100, 1)

        incProgress(0.1, detail = "Complete!")

        # Store for download
        session$userData$final_coded_dataset <- final_dataset
        session$userData$coding_complete <- TRUE
        auto_save()  # Auto-save completion status

        # Show success modal with download option
        showModal(modalDialog(
          title = "✓ Dataset Coding Complete!",
          size = "l",
          div(
            div(
              class = "alert alert-success",
              style = "text-align: center; margin-bottom: 20px;",
              h4("Your entire dataset has been coded!", style = "color: #28a745;")
            ),

            # Summary statistics
            div(
              style = "background-color: #f8f9fa; padding: 20px; border-radius: 8px; margin-bottom: 20px;",
              h5("Coding Summary:", style = "margin-bottom: 15px;"),
              div(
                style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px;",
                div(
                  p("📊 Total items:", strong(format(total_items, big.mark = ",")), style = "margin: 5px 0;"),
                  p("✅ Coded positive:", strong(format(coded_positive, big.mark = ",")), style = "margin: 5px 0;"),
                  p("❌ Coded negative:", strong(format(coded_negative, big.mark = ",")), style = "margin: 5px 0;")
                ),
                div(
                  p("📈 Final base rate:", strong(paste0(final_base_rate, "%")), style = "margin: 5px 0;"),
                  p("🔬 Code applied:", strong(code_name), style = "margin: 5px 0;"),
                  p("⚡ Method:", strong("Perfect Sampling"), style = "margin: 5px 0;")
                )
              )
            ),

            # Validation details
            div(
              style = "background-color: #d4edda; padding: 15px; border-radius: 5px; margin-bottom: 20px;",
              h6("Validation Details:", style = "color: #155724; margin-bottom: 10px;"),
              p("✓ Perfect sampling validation completed", style = "margin: 3px 0; color: #155724;"),
              p(paste("✓ Achieved", validation_values$perfect_agreements_current_cycle, "of", validation_values$cais_n, "required perfect agreements"), style = "margin: 3px 0; color: #155724;"),
              p(paste("✓ Validation cycles:", validation_values$current_cycle), style = "margin: 3px 0; color: #155724;"),
              p("✓ Statistical significance: κ > 0.80 (α = 0.025)", style = "margin: 3px 0; color: #155724;")
            )
          ),
          footer = tagList(
            modalButton("Close"),
            downloadButton("download_final_coded_dataset",
                           "Download Coded Dataset",
                           icon = icon("download"),
                           class = "btn-success btn-lg")
          )
        ))

        showNotification(
          paste("Success! Coded", format(total_items, big.mark = ","), "items -",
                format(coded_positive, big.mark = ","), "positive,",
                format(coded_negative, big.mark = ","), "negative"),
          type = "message", duration = 8
        )

      }, error = function(e) {
        showNotification(paste("Error coding dataset:", e$message), type = "error")
      })
    })
  })

  # Download handler for final coded dataset
  output$download_final_coded_dataset <- downloadHandler(
    filename = function() {
      code_name <- if (!is.null(current_code())) current_code() else "coded"
      paste0(code_name, "_perfect_sampling_dataset_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(session$userData$final_coded_dataset)
      write.csv(session$userData$final_coded_dataset, file, row.names = FALSE)
    }
  )
} # end of server

# Run the application
shinyApp(ui = ui, server = server)
