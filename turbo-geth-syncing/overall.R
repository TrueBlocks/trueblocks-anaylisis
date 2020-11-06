library(tidyjson)
library(dplyr)   
library(httr)
library(ggplot2)

par(las=2)

file <- "/Users/jrush/Development/trueblocks-analysis/turbo-geth-syncing/sorted.csv"
df <- read.csv(file, TRUE, ",", "\"", ".", TRUE, "#", stringsAsFactors = FALSE)
df <- df %>% mutate(observation = 1:n())
df <- df %>% select(observation,id,longname,contains("bytes"))
df <- df %>% filter(row_number() %% 3 == 1)
df <- df %>% mutate(bytes_br = as.numeric(stat_branch_bytes))
df <- df %>% mutate(bytes_lf = as.numeric(stat_leaf_bytes))
df <- df %>% mutate(bytes_ov = as.numeric(stat_overflow_bytes))
df <- df %>% mutate(bytes = bytes_br + bytes_lf + bytes_ov)
df <- df %>% mutate(gb = bytes / 1024 / 1024 / 1024)

graphed <- df %>% filter(
  (
    (longname == 'AccountHistoryBucket')
    |
    (longname == 'BlockBodyPrefix')
    |
    (longname == 'Log')
    |
    (longname == 'LogTopicIndex')
    |
    (longname == 'PlainAccountChangeSetBucket')
    |
    (longname == 'PlainStorageChangeSetBucket')
    |
    (longname == 'Senders')
    |
    (longname == 'StorageHistoryBucket')
    |
    (longname == 'TxLookupPrefix')
  )
)
#df <- df %>% filter(df$gb <= 1)

jpeg("overall.jpg", width = 640, height = 480)
ggplot(graphed, aes(x = observation, y = gb, fill=longname))+
  geom_bar(stat="identity", colour=c("snow3"), width=20, position="dodge")+
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
  labs(title="Size of tables during sync (bytes, large tables (>1GB), sampled every 15 minutes)", size="20pt", y="Size in GB")+
  geom_line(stat="identity", linetype=3)
dev.off()
