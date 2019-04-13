######## scrap

hmtl0 = 'https://www.laopinionpergamino.com.ar/mas/servicios/funebres'

######## set date range to scrap

days = seq(as.Date("2015/01/01"), as.Date("2015/12/31"), "days")
daysName = weekdays(days)
daysE = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
daysS = c('lunes', 'martes', 'miercoles', 'jueves', 'viernes', 'sabado', 'domingo')
for (dd in 1:7) daysName[daysName == daysE[dd]] = daysS[dd]
days = format(days, "%d-%m-%Y")
daysP = paste(daysName,days,sep='-')
daysP2 = paste(daysName,days,sep='-')

######## save data

avisos_fun = data.frame(date='pepe', aviso=';')
library(rvest)
for (page in 1:length(daysP)){
  # page=daysP[1]
                wpage = try(read_html(paste0(hmtl0, '/funebres-', daysP[page])))
                if(isTRUE(class(wpage)=="try-error")){
                  avisos_fun=rbind(avisos_fun, cbind(date = daysP[page], aviso = ''))
                  next
                }
                p_nodes = wpage %>% 
                html_nodes("p") %>% 
                html_text()
    avisos_fun=rbind(avisos_fun, cbind(date = daysP[page], aviso = p_nodes))
    print(i)
}
summary(avisos_fun$date)
table(avisos_fun$date)
    
####### get useful rows
avisos_fun_ok = avisos_fun[substr(avisos_fun$aviso,1,1)=='†',]
write.table(avisos_fun_ok, 'example_avisos.txt', sep=';')

####### get names
avisos_fun_ok$names = substr(avisos_fun$aviso, 3,
                             regexpr(pattern = "Q.", 
                                     avisos_fun$aviso)[1]-2)

####### get age - 4 caracteres antes de anios

####### get death date - 5 strings after "D.)."

####### get sex - dictionary with names and sex
