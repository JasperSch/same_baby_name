library(shiny)

TEST_STRING = 'same_name_checker'

encrypt <- function(x){
	digest::digest(tolower(x), algo='md5')
}

ui <- fluidPage(
	tags$script("
					Shiny.addCustomMessageHandler('txt', function (txt) {
					navigator.clipboard.writeText(txt);
					});
					"),
	h1("Same Baby Name !?"),
	helpText("Test if you have the same baby name in mind as your friends. Without revealing it!"),
	helpText("The tool will compare encrypted versions of your names and only reveal the name if an exact match is found."),
	helpText("Ensure to include all different ways of writing and abbreviating your name(s)."),
	helpText("Note that your friends could abuse this tool by trying to guess your name until they get a match."),
	helpText("You can add some additional 'distraction names' to avoid this."),
	imageOutput("demoImage"),
	textAreaInput('ownNames',
			label = '1. Fill in your own names, seperated by ", " ',
			placeholder = 'Jim, Jimmy, Jimbo, Dwight'),
	tags$b('2. Copy your codes and send to your friends:'),
	verbatimTextOutput('ownCodes'),
	actionButton("copy_link", "Copy to clipboard"),
	textAreaInput(
			'friendCodes',
			label = '3. Enter the codes you recieved:',
			placeholder = 'a898dab114ee49ace0aa3dc9a834935a bee9393a99948667107e0f5cb545b759 f07bb844ebd0430622d0a7bed6c331ad ea47243aa25a5b4d2b4d384451e06240'),
	textOutput('matchMessage')
)

server <- function(input,output,session){
	encodedNames <- reactive({
				ownNames <- strsplit(input$ownNames, ', ')[[1]]
				names <- c(TEST_STRING, ownNames)
				codes <- unlist(lapply(names, function(x){
							encrypt(x)
						}))
				
				unique(data.frame(
						name = names,
						code = codes
				))
			})
		
	observeEvent(input$copy_link, {
				session$sendCustomMessage("txt", ownCodes())
			})

	output$demoImage <- renderImage( 
			{ 
				list(src = "www/demo.png") 
			}, 
			deleteFile = FALSE 
	) 
	
	ownCodes <- reactive({paste(unique(encodedNames()$code), collapse = ' ')})
	
	output$ownCodes <- renderText({
				ownCodes()
			})
	
	output$matchMessage <- renderText({
				friendCodes <- strsplit(gsub('\n', '', input$friendCodes), ' ')[[1]]
				
				if(length(friendCodes) == 0){
					message = ''
				} else if (!encrypt(TEST_STRING) %in% friendCodes){
					message = 'Something is wrong with the codes. Please add your friend codes again.'
				}
				else {
					matches <- encodedNames()[encodedNames()$code %in% friendCodes,]
					matches <- matches[matches$code != encrypt(TEST_STRING),]
					n_matches = (nrow(matches))
					
					if(n_matches == 0){
						message = 'No Match found!' # Only control sequence
					} else {
						message = sprintf('Bad news... Found conflicting names: %s', paste(matches$name, collapse = ', '))
					}			
				}

				message 
			})
}
	
# Create Shiny app ----
shinyApp(ui = ui, server = server)



