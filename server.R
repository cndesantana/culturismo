
function(input, output, session) {
  output$route2booksPlot <- renderPlot({
     invalidateLater(1*60*1000)
     allcidades <- input$Destination
     mycidades <- database %>% filter(cidade %in% allcidades) %>% select(cidade)
     mypaises <- database %>% filter(cidade %in% allcidades) %>% select(país)
     mylocais <- database %>% filter(cidade %in% allcidades) %>% select(local)
#     locationscidade <- sapply(stringr::str_c(mycidades$cidade,mypaises$país,sep=", "),geocode)
#     locationslatlon <- sapply(stringr::str_c(mylocais$local, mycidades$cidade, mypaises$país,sep=", "),geocode)
     #consertar a base de dados, para incluir o nome real das localidades, e não o nome informal como "casas coloridas de Cartagena"
     
###Plotar mapa 
     #plotar um mapa para o locationscidade
     
     #adicionar marcadores (tiles, pop-ups, boxes, etc) nas localidades indicadas em locationslatlon
  
  })

  output$books2routePlot <- renderPlot({
     invalidateLater(1*60*1000)
     #to take a list of books and to return a map with the locations of places present in the listed books
     #we return one map for each book
     allbooks <- input$Books
     mybooks <- database %>% filter(livro %in% allbooks) %>% select(livro)
     mycidades <- database %>% filter(livro %in% allbooks) %>%  select(cidade)
     mypaises <- database %>% filter(livro %in% allbooks) %>% select(país)
     mylocais <- database %>% filter(livro %in% allbooks) %>%  select(local)
     locationscidade <- sapply(stringr::str_c(mycidades$cidade,mypaises$país,sep=", "),geocode)
     locationslatlon <- sapply(stringr::str_c(mylocais$local, mycidades$cidade, mypaises$país,sep=", "),geocode)

     
     ###Plotar mapa 
     #plotar um mapa para cada locationscidade
     
     #adicionar marcadores (tiles, pop-ups, boxes, etc) nas localidades indicadas em locationslatlon
     
  })


}


