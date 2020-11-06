require('tidyverse')
require('jsonlite')

## select an address!
address <- "0xd10e6c4b44ee5557df0f11239ca4be5025791e8e"

## this is how we get json and turn it into a tibble.
test.data <- paste0("http://localhost:8080/export?address=", address) %>%
  fromJSON(simplifyVector = TRUE) %>%
  as_tibble()

## take a look at the data.
test.data %>% View()

## let's massage the data, getting eth values.
test.data <- test.data %>%
  mutate(ether.val = value / 10^18) 

## let's do a rudimentary chart showing value transferred for this address.
test.data %>%
  mutate(status = ifelse(from == address, "from", ifelse(to == address, "to", "other"))) %>%
  ggplot(aes(x=ether.val, fill = status)) +
  geom_histogram() +
  labs(caption = paste("This chart shows that address ", address, " was on the receiving end of a number of tx,",
    " but did not send any. You can also see the trace volume in blue, although it's not the most helpful ",
    " representation", sep = "\n"))

test.data %>%
  mutate(status = ifelse(from == address, "from", ifelse(to == address, "to", "other"))) %>%
  ggplot(aes(x=blockNumber, y = value, color = status)) +
  geom_point()