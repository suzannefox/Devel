
x1 <- c(1,2,3)
x2 <- c("one","two","three")
xdf <- data.frame(x1,x2)

x <- Burrow(xdf, "jim")

x1 <- TRUE
class(x1)

# ==============================================
# create an S3 class
# ==============================================
Burrow <- function(sourcedata, sourcedescription, diagnostics=FALSE) {

  # =================================================================
  # INPUT VALIDITY CHECKING
  # =================================================================
  # check sourcedata is a dataframe
  if(class(sourcedata) != "data.frame")  {
    stop("First argument must be the source data supplied as a dataframe")
  }
  
  # check sourcedescription is a character
  if(class(sourcedescription) != "character")  {
    stop("Second argument must be a character variable describing the data")
  }

  # check diagnostics is logical
  if(class(diagnostics) != "logical")  {
    stop("Third argument must be a logical variable")
  }
  
  # =================================================================
  # RECORD CREATION TIME
  # =================================================================
  myCreation <- format(Sys.time(), "%a %b %d %Y, %X")
  
  # =================================================================
  # ASSEMBLE THE LIST OF OBJECTS TO RETURN TO THE CALLER
  # =================================================================
  # class can be set using class() or attr() function
  value <- list(sourcedata = sourcedata, 
                sourcedescription = sourcedescription, 
                myCreation = myCreation)
  
  attr(value, "class") <- "Burrow"
  value

}