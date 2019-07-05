## Baysian Example
listStates(treeFileName = "../data/batRABV.mcc.tree",
           treeType = "bayesian")

graph <- makeTransNet(treeFileName = "../data/batRABV.mcc.tree",
                      columnSelection = "state",
                      centralityMetric = 6,
                      threshold = 0.9,
                      treeType = "bayesian")
print(graph)

## Parsimonsious Example
listStates(treeFileName = "../data/chikv_westernafrica.phy",
           csvFileName = "../data/chikv_westernafrica_metadata.csv",
           treeType = "parsimonious")

graph <- makeTransNet(treeFileName = "../data/chikv_westernafrica.phy",
                      csvFileName = "../data/chikv_westernafrica_metadata.csv",
                      columnSelection = "Host",
                      centralityMetric = 6,
                      treeType = "parsimonious")
print(graph)



########################

treepreview <- OutbreakTools::read.annotated.nexus("../data/batRABV.mcc.tree")

#colorby <- treepreview$root.annotation[["state"]]

t1 <- ggtree(treepreview, ladderize = F) +
  geom_point(aes_string(size = 3)) +
  geom_text(aes(label = label),
            hjust = 0,
            position = position_nudge(x = 0.2)) +
  scale_fill_brewer(palette="Spectral") +
  scale_x_continuous(expand = c(.1, .1))

plotly::ggplotly(t1)

########################

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
