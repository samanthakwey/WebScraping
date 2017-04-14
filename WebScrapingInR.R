#libraries

library(rvest)

##################
#                #
#    Practice    #
#                #
##################

####################
#    Exercise 1    #
####################

# Consider the url 'http://statbel.fgov.be/en/statistics/figures/economy/indicators/prix_prod_con/'
# Extract all the information load on table 'Third Quarter 2016'.

ex1 = read_html("http://statbel.fgov.be/en/statistics/figures/economy/indicators/prix_prod_con/") %>%
  html_nodes("table") %>% html_table() %>% data.frame() 
ex1 = ex1[[1]]

####################
#    Exercise 2    #
####################

# Consider the url 'http://www2.sas.com/proceedings/sugi30/toc.html'
# Extract all the papers names, from 001-30 to 268-30

ex2 = read_html("http://www2.sas.com/proceedings/sugi30/toc.html") %>%
  html_nodes("cite") %>% html_text()

####################
#    Exercise 3    #
####################

# Consider the url 'http://www.gibbon.se/Retailer/Map.aspx?SectionId=832'
# Extract all the options (countries) availables on select button.

ex3 = read_html("http://www.gibbon.se/Retailer/Map.aspx?SectionId=832") %>%
  html_nodes("#ctl00_ContentPlaceHolder1__countries") %>% html_children() %>% html_text()

####################
#    Exercise 4    #
####################

# Consider the url 'http://r-exercises.com/start-here-to-learn-r/'
# Extract all the topics available on the url.

ex4 = read_html("http://r-exercises.com/start-here-to-learn-r/") %>%
  html_nodes("a") %>% html_text()
ex4 = ex4[22:112]

####################
#    Exercise 5    #
####################

# Consider the url 'http://www.immobiliare.it/Roma/agenzie_immobiliari_provincia-Roma.html'
# Extract all inmobiliaries names published on first page.

ex5 = read_html("http://www.immobiliare.it/Roma/agenzie_immobiliari_provincia-Roma.html") %>% 
  html_nodes("a") %>% html_text()
ex5 = unique(ex5[min(grep("Building&Money",ex5)):max(grep("Affiliato",ex5))])

ex5  = ex5[-2]; ex5  = ex5[-2]

####################
#    Exercise 6    #
####################

# Consider the url 'http://www.gibbon.se/Retailer/Map.aspx?SectionId=832'.
# Extract the links to the detailed information of each row on the table.

ex6 = read_html("http://www.gibbon.se/Retailer/Map.aspx?SectionId=832") %>% 
  html_nodes("a") %>% html_attrs()
ex6 = unlist(ex6)
ex6 = paste("http://www.gibbon.se/Retailer/",unique(ex6)[grep("Retailer.aspx",unique(ex6))],sep="")

####################
#    Exercise 7    #
####################

# Consider the url 'https://www.bkk-klinikfinder.de/suche/suchergebnis.php?next=1'
# Extract the links to the detailed information of each hospital. 

ex7 = read_html("https://www.bkk-klinikfinder.de/suche/suchergebnis.php?next=1") %>% 
  html_nodes("a") %>% html_attrs()
ex7 = unlist(ex7)
ex7 = as.vector(ex7)
ex7 = paste("https://www.bkk-klinikfinder.de",ex7[grep("/krankenhaus/index.php?",ex7)],sep="")

####################
#    Exercise 8    #
####################

# Consider the url scraped in Exercise 7.
# Extract the links to 'Details' for each hospital display on the first 4 pages.

ex8 = c()
for(i in c(1,11,21,31)){
  temp = read_html(paste("https://www.bkk-klinikfinder.de/suche/suchergebnis.php?next=",i,sep="")) %>% 
    html_nodes("a") %>% html_attrs()
  temp = unlist(temp)
  temp = as.vector(temp)
  ex8 = c(ex8,paste("https://www.bkk-klinikfinder.de",temp[grep("/krankenhaus/index.php?",temp)],sep=""))
}

####################
#    Exercise 9    #
####################

# Consider the url='http://www.dictionary.com/browse/' and the words 'handy','whisper','lovely','scrape'.
# Build a data frame, where the first variables is "Word" and the second variables is "definitions". 
# Scrape the definitions from the url.

word = c("handy","whisper","lovely","scrape")
defs = c()

for(i in 1:4){
  temp = read_html(paste("http://www.dictionary.com/browse/",word[i],sep="")) %>% 
    html_nodes('.def-content,#source-word-origin')%>%html_text()
  temp = temp[1:(grep("Origin of",temp)-1)]
  temp = unique(unlist(strsplit(unlist(strsplit(temp,"\r")),"\n")))
  temp = paste(temp,rep('/',length(temp)))
  temp = toString(temp)
  defs = c(defs,temp)
}

ex9 = data.frame(word = word, defs = defs)


####################
#    Exercise 10   #
####################

# Consider the url 'http://www.gibbon.se/Retailer/Map.aspx?SectionId=832'.
# Build a data frame with all the information available for each row.

ex10.links = read_html("http://www.gibbon.se/Retailer/Map.aspx?SectionId=832") %>% 
  html_nodes("a") %>% html_attrs()
ex10.links = unlist(ex10.links)
ex10.links = paste("http://www.gibbon.se/Retailer/",unique(ex10.links)[grep("Retailer.aspx",unique(ex10.links))],sep="")

details = c()

for(link in ex10.links){
  nameOfComp = read_html(link)%>%html_nodes("h1")%>%html_text()
  nameOfComp = nameOfComp[2]
  span = read_html(link)%>%html_nodes("span")%>%html_text()
  Tel = span[(grep("Telefon:",span)+1)]
  email = span[(grep("Mail-adress:",span)+1)]
  website = span[(grep("Hemsida:",span)+1)]
  details = rbind(details,c(nameOfComp,Tel,email,website))
}

details = data.frame(details)
names(details) = c("Nmae of Store","Phone Number","Email","Website URL")

##################################
#                                #
#    Marketing Mix Model Case    #
#                                #
##################################

###############################################################
#    Extract Official DMA Name and Code from Nelson Website   #
###############################################################

DMA210 = read_html("https://support.google.com/richmedia/answer/2745487?hl=en") %>%
  html_nodes("table") %>%
  .[[1]] %>%
  html_table()
DMA210 = DMA210[,-2]
names(DMA210) = c("DMA","DMA_Name")

##################################################################
#    Extract US Holiday and make holiday weekly dummy variable   #
##################################################################

library(data.table)

getHoliday = function(startYear=2010,endYear=2016){
  data = data.frame(stringsAsFactors = F)
  for(i in seq(startYear,endYear)){
    session = html_session("http://www.timeanddate.com/holidays/us/")
    form = html_form(session)[[3]]
    form = set_values(form, year = i)
    USHDay =  read_html(submit_form(session,form))
    USHDayTable = USHDay %>%
      html_nodes("table") %>%
      .[[1]] %>%
      html_table()
    USHDayTable = USHDayTable[-1,]
    USHDayTable$Date = paste(i,USHDayTable$Date)
    data = rbind(data,USHDayTable)
  }
  data$Date = as.Date(data$Date,"%Y %b %d")
  weekMonday = function(date)
    date - (setNames(c(6,0:5),0:6) [strftime(date,'%w')])
  data$Week = weekMonday(data$Date)
  data = unique(data[,c(6,3)])
  data$`Holiday Name` = gsub("\\s|'s|'","",data$`Holiday Name`)
  data$var = 1
  data = dcast(data = data,formula = Week ~ `Holiday Name`,fun.aggregate = sum,value.var = "var")
  data$PresidentsDay = ifelse(month(data$Week)==11,0,data$PresidentsDay)
  return(data)
}

USHDayTable = getHoliday(startYear=2015,endYear=2016)

