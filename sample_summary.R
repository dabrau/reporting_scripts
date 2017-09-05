library(ggplot2)
library(readxl)
library(dplyr)

fingerprintPopulations <- read_excel(paste0(getwd(),'/fingerprint.xlsx'), sheet = 1)
populationTable <- read.table(paste0(getwd(),'/populations.tsv'), header = TRUE, sep = '\t')

fingerprintTable <- left_join(fingerprintPopulations, populationTable, by = c("population" = "name", "ancestry" = "ancestry", "stain" = "stain")) %>%
  left_join(., ., by = c("parent_ratio" = "id.x"), suffix = c(".child", ".parent")) %>%
  mutate(ratio = count.child / count.parent) %>%
  mutate(ratio_name = paste0(name.child, ' / ', name.parent, ' (', stain.child, ')')) %>%
  filter(!is.na(ratio))

fingerprintTable$ratio_name <- factor(filtered$ratio_name, levels = filtered$ratio_name)

p <- ggplot(fingerprintTable, aes(ratio_name, ratio)) + 
  geom_bar(stat="identity", position = "dodge", fill = "steelblue") + 
  theme(axis.text.x=element_text(angle=90,hjust=1)) +
  xlab('populations') + 
  ggtitle('fingerprint', subtitle = NULL) +
  theme(plot.title = element_text(hjust = 0.5))
print(p)

mfiPopulationHeatmap <- function(df) {
  p <- ggplot(df, aes(mfi_gate, name.child)) + 
    geom_tile(aes(fill = ratio), colour = "white") + 
    scale_fill_gradient(low = "white", high = "steelblue") +
    xlab('mfi gate') +
    ylab('population') +
    ggtitle(paste(unique(df$stain), 'stain', sep = " "), subtitle = NULL) +
    theme(plot.title = element_text(hjust = 0.5))
  
  print(p)
  return(df)
}

mfiPopulations <- filter(populationTable, grepl('^.* hi', name))
parentPopulations <- mutate(filtered, parent_name = paste(population.child, ancestry.child, sep = '\t'))

inner_join(mfiPopulations, parentPopulations, by = c("ancestry" = "parent_name", "stain" = "stain.child"), c(".mfi", ".pop")) %>%
  mutate(ratio = count / count.child) %>%
  mutate(mfi_gate = substr(name,1,nchar(as.character(name))-3)) %>%
  group_by(stain) %>% 
  do(mfiPopulationHeatmap(.))


