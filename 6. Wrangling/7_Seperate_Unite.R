# import data
path <- system.file("extdata", package = "dslabs")
list.files(path)
file_name <- file.path(path,"life-expectancy-and-fertility-two-countries-example.csv")
raw_data <- read_csv(file_name)
head(raw_data)

# gather all columns except country

dat <- raw_data %>% gather(key, value, -country)
head(dat)
dat$key[1:5]

# separate on underscores

dat %>% separate(key, c("year","variable_name"),sep="_")
dat %>% separate(key, c("year", "variable_name"))#_ is the default separator so no need mention it

# split on all underscores, pad empty cells with NA

dat %>% separate(key, c("year","first_variable_name", "Second_variable_name"), fill = "right"  )

# split on first underscore but keep life_expectancy merged

dat %>% separate(key,c("year","variable_name"), fill = "right", extra = "merge")

# separate then spread

dat %>% separate(key,c("year","variable_name"), sep = "_", extra = "merge") %>%
  spread(variable_name, value)

# separate then unite

dat %>% separate(key,c("year","first_variable_name","second_variable_name"), sep = "_", fill = "right") %>%
  unite(variable_name,first_variable_name,second_variable_name, sep = "_") %>%
  spread(variable_name, value) %>%
  rename(fertility=fertility_NA)

