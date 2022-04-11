div( 
  selectInput("age", h3("Age"),
              choices = list("18-22" = 1, "22-30" = 2,
                             ">30" = 3), selected = 2),
    selectInput("gender", h3("Select gender"),
                choices = list("Male" = 1, "Female" = 2,
                               "Other" = 3), selected = 2),
    textInput("nationality", h3("Nationality"), value = ""),
    textInput("course", h3("Current course")),
    textInput("experience", h3("What kind of experience do you have with this kind of search engines"))
  )
