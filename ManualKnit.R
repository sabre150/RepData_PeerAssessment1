# For some reason my knitr button produces the following error:

# Error in tools::file_path_as_absolute(input) : 
#     file 'PA1_template.Rmd' does not exist
# Calls: <Anonymous> -> setwd -> dirname -> <Anonymous>
#     Execution halted

# So sourcing this file does the same thing as the knitr button
# Assume working directory is already set

file2Knit <- "./PA1_template.Rmd"

# Make output file names
fileNoExt = substr(file2Knit, 1, nchar(file2Knit) - 4)
fileHtml <- paste(fileNoExt, "html", sep = ".")
# filePdf <- paste(fileNoExt, "pdf", sep = ".")

library(knitr)

knit2html(file2Knit, fileHtml)
browseURL(fileHtml)
