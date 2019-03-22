treepreview <- ape::read.tree("../data/chikv_westernafrica.phy")
md <- read_csv("../data/chikv_westernafrica_metadata.csv")
#input$columnselection_row_last_clicked

colorby <- colnames(md)[2] %>%
  as.character()

# colorby <- md[1]
# colnames(colorby) <- "Legend"

t1 <- ggtree(treepreview, ladderize = F) %<+% md +
  geom_point(aes_string(color = colorby, size = 3)) +
  geom_text(aes(label = label),
            hjust = 0,
            position = position_nudge(x = 0.2)) +
  scale_fill_brewer(palette="Spectral") +
  scale_x_continuous(expand = c(.1, .1))

plotly::ggplotly(t1)

gr <- makeTransNet(treeFileName = "../data/chikv_westernafrica.phy",
                   csvFileName = "../data/chikv_westernafrica_metadata.csv",
                   columnSelection = 1,
                   centralityMetric = 2)

visSave(gr, file = "grfile.html", background = "transparent")
rmarkdown::pandoc_convert("grfile.html", to = "pdf")
