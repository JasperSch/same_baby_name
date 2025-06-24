library(shiny)

# Default encoded code to ensure same application version is used.
TEST_STRING = 'same_name_checker'

# Some salt to ensure it's not easy to use md5 reference tables
# https://en.wikipedia.org/wiki/Salt_(cryptography)
# If you've reached this code trying to crack your friends name, please do not cross this ethical boundary.
SALT = paste(rep('thatswhatshesaid', 100), collapse='')


#' Encrypt a string 
#' @param x character
encrypt <- function(x){
	digest::digest(paste0(tolower(x), SALT), algo='md5')
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
			placeholder = sapply(c('Meredith', 'Angela', 'Pam'), encrypt)),
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
	
shinyApp(ui = ui, server = server)



