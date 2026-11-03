
# Install packages
packages <- c(
  "tidyverse",
  "janitor",
  "skimr",
  "corrplot",
  "GGally",
  "caret",
  "randomForest",
  "DataExplorer"
)
installed <- packages %in% installed.packages()

if(any(!installed))
  install.packages(packages[!installed])
install.packages("naniar")

library(tidyverse)
library(janitor)
library(skimr)
library(corrplot)
library(GGally)
library(caret)
library(randomForest)
library(DataExplorer)
library(dplyr)
library(stringr)
library(naniar)


# Import Dataset
df_cacoa1 <- read_csv(
  "flavors_of_cacao.csv",
  locale = locale(encoding = "Latin1")
)

df_cacoa <- df_cacoa1 %>%
  clean_names()

names(df_cacoa)

df_cacoa <- df_cacoa |>
  rename(
    company = company_a_maker_if_known,
    specific_origin = specific_bean_origin_or_bar_name,
    review_year = review_date,
    broad_origin = broad_bean_origin
  )


df_cacoa$cocoa_percent <- as.numeric(str_remove(df_cacoa$cocoa_percent, "%"))

#function to clean text columns
clean_text <- function(x){
  
  #remove non breaking spaces
  x <- stringr::str_replace_all(x, "\u00A0", "")
  
  #remove all non ASCII characters
  x <- iconv(x, from= "UTF-8", to = "ASCII", sub = "")
  
  #trim white space
  x[x=="" | is.na(x)] <- "unknown"
  
  return(x)
}

#apply to all character columns
df_cacoa <- df_cacoa %>%
  mutate(
    across(
      c(company,specific_origin,
        company_location,
        bean_type, 
        broad_origin),
      clean_text)
  )

#convert to numeric
df_cacoa <- df_cacoa %>%
  mutate(
    cocoa_percent = as.numeric(cocoa_percent),
    rating = as.numeric(rating)
  )
# Data quality checks
print(colSums(is.na(df_cacoa)))
print(sum(duplicated(df_cacoa)))
summary(df_cacoa)

# Outliers
# Distribution of Cocoa Percentage

ggplot(df_cacoa, aes(x = cocoa_percent)) +
  geom_histogram(
    aes(y = after_stat(density)),
    binwidth = 0.05,
    fill = "#8B4513",      # Chocolate brown
    colour = "white",
    linewidth = 0.4,
    alpha = 0.85
  ) +
  geom_density(
    colour = "#2E4057",    # Dark blue
    linewidth = 1.3
  ) +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0.4, 1.0, by = 0.1)
  ) +
  labs(
    title = "Distribution of Cocoa Percentage",
    subtitle = "Distribution of cocoa content across all reviewed chocolate bars",
    x = "Cocoa Percentage",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 16),
    plot.subtitle = element_text(hjust = 0.5, colour = "grey40"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(colour = "grey85"),
    panel.grid.major.y = element_line(colour = "grey90")
  )

# Rating Distribution
ggplot(df_cacoa, aes(x = rating)) +
  geom_histogram(binwidth = 0.25,
                 fill = "skyblue",
                 colour = "black") +
  geom_density(aes(y = after_stat(count * 0.25)),
               colour = "darkred",
               linewidth = 1) +
  labs(
    title = "Distribution of Chocolate Ratings",
    x = "Rating",
    y = "Frequency"
  ) +
  theme_minimal()

# Missingness
gg_miss_var(df_cacoa)

# Correlation
num <- df_cacoa|>select(cocoa_percent,review_year,rating)
corrplot(cor(num),method="color",addCoef.col="black")


# Feature Engineering
df_cacoa <- df_cacoa |>
  mutate(
    cocoa_group=case_when(
      cocoa_percent<0.60~"Low",
      cocoa_percent<0.75~"Medium",
      cocoa_percent<0.85~"High",
      TRUE~"Very High"
    ),
    Elite=ifelse(rating>=3.75,1,0)
  )
# Create Rating Groups
df_cacoa <- df_cacoa %>%
  mutate(
    rating_group = case_when(
      rating < 2.5 ~ "Low",
      rating < 4.0 ~ "Medium",
      TRUE ~ "High"
    )
  )

# Scatter
ggplot(df_cacoa,aes(cocoa_percent,rating))+
  geom_point(alpha=.4)+
  geom_smooth(method="lm")+
  theme_minimal()



# Histograms
# Histogram of Chocolate Ratings

ggplot(df_cacoa, aes(x = rating)) +
  geom_histogram(
    bins = 15,
    fill = "#2C7FB8",      # Professional blue
    colour = "white",
    linewidth = 0.4
  ) +
  labs(
    title = "Distribution of Chocolate Ratings",
    subtitle = "Frequency of ratings in the Flavors of Cacao dataset",
    x = "Rating",
    y = "Number of Reviews"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

ggplot(df_cacoa, aes(x = cocoa_percent)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 20,
    fill = "#8B4513",
    colour = "white",
    alpha = 0.8
  ) +
  geom_density(
    colour = "#4E342E",
    linewidth = 1.2
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Distribution of Cocoa Percentage",
    subtitle = "Histogram with Density Curve",
    x = "Cocoa Percentage",
    y = "Density"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )


# Density
ggplot(df_cacoa,aes(rating))+geom_density(fill="steelblue",alpha=.4)

#Summarise the Groups
rating_summary <- df_cacoa %>%
  group_by(rating_group) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  )


# Distribution of Chocolate Ratings by Rating Group

rating_summary$rating_group <- factor(
  rating_summary$rating_group,
  levels = c("Low", "Medium", "High")
)

ggplot(rating_summary,
       aes(x = rating_group,
           y = Count,
           fill = rating_group)) +
  geom_col(width = 0.65, colour = "white") +
  geom_text(aes(label = Count),
            vjust = -0.4,
            size = 5,
            fontface = "bold") +
  scale_fill_manual(values = c(
    "Low" = "#D73027",
    "Medium" = "#F4A261",
    "High" = "#2A9D8F"
  )) +
  labs(
    title = "Distribution of Chocolate Ratings",
    subtitle = "Number of chocolate bars in each rating category",
    x = "Rating Group",
    y = "Number of Chocolate Bars"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(colour = "black"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  )


# Country summaries
country_summary <- df_cacoa |>
  group_by(company_location) |>
  summarise(Reviews=n(),Average_Rating=mean(rating),.groups="drop")



# Top 10 Countries by Average Chocolate Rating

top_countries <- country_summary %>%
  arrange(desc(Average_Rating)) %>%
  slice_head(n = 10)

ggplot(top_countries,
       aes(x = reorder(company_location, Average_Rating),
           y = Average_Rating,
           fill = Average_Rating)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = round(Average_Rating, 2)),
            hjust = -0.15,
            size = 4) +
  coord_flip() +
  scale_fill_gradient(
    low = "#66C2A5",
    high = "#006D2C"
  ) +
  labs(
    title = "Top 10 Countries by Average Chocolate Rating",
    subtitle = "Average ratings based on chocolate reviews",
    x = "Company Location",
    y = "Average Rating"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  expand_limits(y = max(top_countries$Average_Rating) + 0.2)



# Top 10 Countries by Number of Reviews

top_reviews <- country_summary %>%
  arrange(desc(Reviews)) %>%
  slice_head(n = 10)

ggplot(top_reviews,
       aes(x = reorder(company_location, Reviews),
           y = Reviews,
           fill = Reviews)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = Reviews),
            hjust = -0.2,
            size = 4) +
  coord_flip() +
  scale_fill_gradient(
    low = "#74A9CF",
    high = "#045A8D"
  ) +
  labs(
    title = "Top 10 Countries by Number of Chocolate Reviews",
    subtitle = "Countries with the highest number of reviewed chocolate bars",
    x = "Company Location",
    y = "Number of Reviews"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    legend.position = "none",
    panel.grid.minor = element_blank()
  ) +
  expand_limits(y = max(top_reviews$Reviews) + 20)

# Trends
yr <- df_cacoa|>group_by(review_year)|>
  summarise(avg=mean(rating),n=n(),.groups="drop")
ggplot(yr,aes(review_year,avg))+geom_line()+geom_point()
ggplot(yr,aes(review_year,n))+geom_line()+geom_point()


# Cocoa Distribution

ggplot(df_cacoa,aes(cocoa_percent))+
  geom_histogram(
    bins=20,
    fill="darkred",
    colour="black"
  )+
  theme_minimal()


company_reviews <- df_cacoa %>%
  count(company, sort = TRUE) %>%
  slice(1:15)

ggplot(company_reviews,
       aes(reorder(company, n), n)) +
  geom_col(fill = "darkorange") +
  coord_flip() +
  labs(
    title = "Top 15 Chocolate Companies by Number of Reviews",
    x = "Company",
    y = "Number of Reviews"
  ) +
  theme_minimal()



## Average Rating by Cocoa Percentage Group

avg_rating <- df_cacoa %>%
  group_by(cocoa_group) %>%
  summarise(
    Average_Rating = mean(rating),
    Reviews = n(),
    .groups = "drop"
  )

ggplot(avg_rating,
       aes(x = cocoa_group,
           y = Average_Rating,
           fill = cocoa_group)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = round(Average_Rating, 2)),
            vjust = -0.4,
            size = 4) +
  labs(
    title = "Average Chocolate Rating by Cocoa Percentage Group",
    x = "Cocoa Percentage Group",
    y = "Average Rating"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 3. Average Rating by Bean Type (Top 15)
bean_summary <- df_cacoa %>%
  group_by(bean_type) %>%
  summarise(
    Average_Rating = mean(rating),
    Reviews = n()
  ) %>%
  filter(Reviews >= 5) %>%
  arrange(desc(Average_Rating))

ggplot(head(bean_summary,15),
       aes(reorder(bean_type, Average_Rating),
           Average_Rating,
           fill = Average_Rating)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title="Average Rating by Bean Type",
    x="Bean Type",
    y="Average Rating"
  )


## Average Rating by Review Year

year_summary <- df_cacoa %>%
  group_by(review_year) %>%
  summarise(
    Average_Rating = mean(rating),
    .groups = "drop"
  )

ggplot(year_summary,
       aes(x = review_year,
           y = Average_Rating)) +
  geom_line(linewidth = 1, colour = "steelblue") +
  geom_point(size = 3, colour = "darkred") +
  labs(
    title = "Average Chocolate Rating by Review Year",
    x = "Review Year",
    y = "Average Rating"
  ) +
  theme_minimal()

## Number of Chocolates by Bean Type

bean_counts <- df_cacoa %>%
  count(bean_type, sort = TRUE) %>%
  slice_head(n = 10)

ggplot(bean_counts,
       aes(x = reorder(bean_type, n),
           y = n,
           fill = bean_type)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 10 Most Common Bean Types",
    x = "Bean Type",
    y = "Number of Chocolates"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 6. Heatmap of Reviews by Year and Cocoa Group

heat_data <- df_cacoa %>%
  count(review_year, cocoa_group)

ggplot(heat_data,
       aes(review_year,
           cocoa_group,
           fill = n)) +
  geom_tile() +
  scale_fill_gradient(low="white",
                      high="darkred") +
  theme_minimal() +
  labs(
    title="Review Frequency by Year and Cocoa Group",
    x="Review Year",
    y="Cocoa Group"
  )

# 7. Pie Chart of Cocoa Groups

ggplot(df_cacoa,
       aes(x="",
           fill=cocoa_group)) +
  geom_bar(width=1) +
  coord_polar("y") +
  labs(title="Distribution of Cocoa Groups") +
  theme_void()

# 8. Pie Chart of Elite vs Non-Elite Chocolate
ggplot(df_cacoa,
       aes(x="",
           fill=as.factor(Elite))) +
  geom_bar(width=1) +
  coord_polar("y") +
  labs(
    title="Elite Chocolate Distribution",
    fill="Elite"
  ) +
  theme_void()

# 9. Violin Plot of Ratings
avg_rating <- df_cacoa %>%
  group_by(cocoa_group) %>%
  summarise(
    Average_Rating = mean(rating),
    .groups = "drop"
  )

ggplot(avg_rating,
       aes(x = cocoa_group,
           y = Average_Rating,
           fill = cocoa_group)) +
  geom_col() +
  geom_text(aes(label = round(Average_Rating, 2)),
            vjust = -0.3,
            size = 4) +
  labs(
    title = "Average Chocolate Rating by Cocoa Percentage Group",
    x = "Cocoa Percentage Group",
    y = "Average Rating"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# 10. Bean Type Frequencies
df_cacoa %>%
  count(bean_type, sort=TRUE) %>%
  slice(1:15) %>%
  ggplot(aes(reorder(bean_type,n),
             n,
             fill=n)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title="Most Common Bean Types",
    x="Bean Type",
    y="Count"
  )

# 11. Broad Bean Origin Frequencies
df_cacoa %>%
  count(broad_origin, sort=TRUE) %>%
  slice(1:15) %>%
  ggplot(aes(reorder(broad_origin,n),
             n,
             fill=n)) +
  geom_col() +
  coord_flip() +
  theme_minimal() +
  labs(
    title="Top Broad Bean Origins",
    x="Origin",
    y="Reviews"
  )

# 12. Scatter Plot with Regression by Elite Status
avg_rating <- df_cacoa %>%
  group_by(cocoa_group) %>%
  summarise(Average_Rating = mean(rating))

ggplot(avg_rating,
       aes(x = cocoa_group,
           y = Average_Rating,
           fill = cocoa_group)) +
  geom_col() +
  geom_text(aes(label = round(Average_Rating, 2)),
            vjust = -0.4) +
  labs(
    title = "Average Rating by Cocoa Percentage Group",
    x = "Cocoa Group",
    y = "Average Rating"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(cor.test(df_cacoa$cocoa_percent,df_cacoa$rating))
print(shapiro.test(df_cacoa$rating))
print(summary(aov(rating~company_location,data=df_cacoa)))
print(summary(aov(rating~broad_origin,data=df_cacoa)))

glm_model <- glm(Elite~cocoa_percent+review_year,
                 family=binomial,data=df_cacoa)
summary(glm_model)


set.seed(123)
idx <- createDataPartition(df_cacoa$Elite,p=.8,list=FALSE)
train<-df_cacoa[idx,]
test<-df_cacoa[-idx,]

rf_model<-randomForest(as.factor(Elite)~cocoa_percent+review_year,
                       data=train,ntree=500,importance=TRUE)


pred<-predict(rf_model,test,type="class")
print(confusionMatrix(pred,as.factor(test$Elite)))
importance(rf_model)
varImpPlot(rf_model)

prob<-predict(glm_model,type="response")
roc_obj<-pROC::roc(df_cacoa$Elite,prob)
plot(roc_obj)
print(pROC::auc(roc_obj))

ctrl<-trainControl(method="cv",number=10)
cv<-train(as.factor(Elite)~cocoa_percent+review_year,
          data=df_cacoa,method="rf",trControl=ctrl)


print(cv)

write_csv(df_cacoa,"Chocolate_Cleaned.csv")

