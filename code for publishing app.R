#install.packages('rsconnect')
#rsconnect::setAccountInfo(name='jrushikesh', token='XXXXXXXXXXXXXXXXXXXX', secret='XXXXXXXXXXXXXXXXXXXX/XXXXXX')

library(rsconnect)
rsconnect::deployApp("Project Path") #Path of your appliction i.e. ui.R and server.R file
