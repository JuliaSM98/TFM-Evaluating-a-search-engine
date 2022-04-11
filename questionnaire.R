textInput("age", h3("Age"), value = "")
selectInput("gender", h3("Select gender"),
            choices = list("Male" = 1, "Female" = 2,
                           "Other" = 3), selected = 2)
textInput("nationality", h3("Nationality"), value = "")
textInput("course", h3("Current course"), value = "")
textInput("experience", h3("What kind of experience do you have with this kind of search engines"), value = "")