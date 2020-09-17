## Save Graph Objects

library(webshot)

# webshot::install_phantomjs()

webshot("chikv_network.html",
        "chikv_network.pdf",
        vwidth = 960,
        vheight = 475,
        zoom = 2)


library(exportwidget)
library(htmltools)

# html_print(tagList(graph,
#                    export_widget()),
#            viewer = utils::browseURL)

html_print(tagList(graph), viewer = utils::browseURL) %>%
  normalizePath(.,winslash="/") %>%
  gsub(x=.,pattern = ":/",replacement="://") %>%
  paste0("file:///",.) %>%
  webshot(file = "chikv_network.png", delay = 2)



visNetwork::visExport(graph,
                      type = "png")

visNetwork::visSave(graph, file = "chikv_network.html")
