# Business  Myntra Retail Data
# There are 2 csv files that are shared here.
#A. Product Details
    #ProductID – ID assigned to the product
    # ProductName – Name of the Product
    # ProductBrand – Brand Name of the Product
#B. Products Catalog
    # Gender – gender to which specific products that have been designed
    #Price (INR) – Price of the products
    #NumImages – Number of images that have been clicked for specific product
    #ID - ID assigned to the product
    #Description – full details of the product
    #PrimaryColor – Color of the product

#######Scenario : 1
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
  
#1. (Observation Of Data What are the problem might be occured of 2 Data set)

getwd()
product_details= read.csv('product details.csv')
dim(product_details)
product_catalog= read.csv('products catalog.csv')
dim(product_catalog)
head(product_catalog)
head(product_details)

library(dplyr)
library(ggplot2)

str(product_catalog)
head(product_catalog)

str(product_details)
head(product_details)

summary(product_catalog)
summary(product_details)

mean(product_catalog$Price..INR.)
median(product_catalog$Price..INR.)
var(product_catalog$Price..INR.)
sd(product_catalog$Price..INR.)
range(product_catalog$Price..INR.)

quartile= quantile(product_catalog$Price..INR.)
print(quartile)

IQR= IQR(product_catalog$Price..INR.)
print(IQR)


product_catalog%>%
  summarise(
    mean_price=mean(Price..INR.),
    median_price= median(Price..INR.),
    min_price= min(Price..INR.),
    max_price= max(Price..INR.),
    sd_price= sd(Price..INR.),
  )

gender_group = product_catalog%>%
  group_by(Gender) %>%
  summarise('Total Cust' = n())
gender_group

---------------------------------------------------------------------
#1. Create a new dataframe “df” by joining the 2 datasets
#2. Drop the duplicate data
#3. Check for missing values


head(product_catalog)
head(product_details)
  
df = product_catalog %>%
  inner_join(product_details,by= c('ID'='ProductID'))
head(df)
View(df)
dim(df)

# Column has the highest count of unique values
unique_values = df %>%
  summarise_all(~length(unique(.)))
print(unique_values)

summary(df)


df%>%
  distinct(ID,all.equal=TRUE)
head(df)
dim(df)


#for overall df checking missing value if any
missing_value = any(is.na(df))
print(missing_value)

#for column wise 
missing_colvalue=colSums(is.na(df))
print(missing_colvalue)

---------------------------------------------------------------------
#### Scenario:  2
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------

#1. There is a column that needs string strip operation. We will identify that 
# and apply it.(removing space,bar)
#2. Filling the missing value by ‘Others’ in the column containing it
#3. Since all the column names are single word so we can convert the 
#‘Price (INR)’ also to single name as ‘Price’.
#4. Analysing the Gender column and include your viewpoints how to make it useful.
  
install.packages('stringr')
library(stringr)
library(dplyr)

df%>% 
  mutate(PrimaryColor=str_trim(PrimaryColor)) 


missing_colvalue=colSums(is.na(df))
print(missing_colvalue)
head(df)
dim(df)

colSums( is.na(df) )

df %>% 
  group_by(PrimaryColor) %>%
  summarise(totalRecords = n())

df$PrimaryColor[df$PrimaryColor %in% ""] = 'Others'
View(df)

 
df= df%>%
  rename(Price=Price..INR.)
View(df)

#Analyse the Gender column 
gender_group
select(df,Gender)

df%>%
  count(Gender)

#Observatio1:::
#The gender column distribution is skewed more towards men and women, with the
#highest frequency. 

#Observatio2:::
#However, there is a unnecessarily created of data related to indivisual unisex
#items, which could be included in both the men's and women's categories. 

#Similarly, unisex kids' products could be associated with either boys or girls.

#Observatio3:::
#To enhance understanding, we can propose creating a new category for unisex 
#items that contains products associated with unisex for both men, women, 
#and kids.



---------------------------------------------------------------------
  #### Scenario:  3- In deepth analysis
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
  
#1. Univariate analysis of each variable
#2. Bivariate Analysis of categorical vs numerical variables (Take target 
#variable as fixed variable here)
#3. Multivariate Analysis of categorical and numerical variables
#4. Check distribution of variables
  
#Univariate analysis of each variable::::
  
  
View(df)
head(df)

gender_group = df%>%
  group_by(Gender) %>%
  summarise('Total Cust' = n())
gender_group

#Gender (Categorical Variable)
ggplot(df, aes(x = Gender)) +
  geom_bar(fill = rainbow(6),color='black') +
  xlab('------Gender-------') +
  ylab('-----Total Count-------') +
  labs(title = "Distribution of Gender")+
  theme_classic()

---------------------------------
#Price Continues Variable

df%>% 
  select(Price)%>% 
  summary()
ggplot(data = df, aes(x = Price)) +
  geom_histogram(bins = 30,fill= 'orange') +
  xlab('------Price-------') +
  ylab('-----Frequency-------') +
  labs(title = 'Histogram of Price') +
  theme_bw()
#as per the graph seen maximum price range < 10000,we can split further
#5000 & 10000

#Less than 5000 range
df%>%
  filter(df$Price<5000)%>%
  dim()
# Observation::90% records are < 5000 (compare df with df$price)

#price < = 5000

df %>%
  filter(Price <=5000) %>%
  group_by(Gender) %>%
  summarise(total_records = n(),
            total_price= sum(Price))


df%>%
  filter(df$Price<5000)

result_of=df%>%
  filter(df$Price<5000)
ggplot(data = result_of, aes(x = Price)) +
  geom_histogram(bins = 30,fill= 'green') +
  xlab('------Price-------') +
  ylab('-----Frequency-------') +
  labs(title = 'Histogram of Price') +
  theme_bw()

df%>%
  filter(df$Price<5000)
  ggplot(data = df,aes(x=Price)) +
  geom_histogram(bins = 30 ,fill='yellow',color='black')+
  xlab('------Price-------') +
  ylab('-----Frequency-------') +
  labs(title = 'Histogram of Price') +
  theme_bw()


  
---------------------------------

df$PrimaryColor
  
  df %>%
    mutate(PrimaryColor = str_trim(PrimaryColor)) %>% 
    ggplot(aes(x = PrimaryColor)) +
    geom_bar(fill = 'red') +
    xlab('------Primary Colour-------') +
    ylab('-----Count-------') +
    labs(title = 'Count of Primary Colour') +
    theme()

             
---------------------------------
View(df)
str(df$ProductBrand)
    
    ggplot(data = df, aes(x = ProductBrand)) +
      geom_bar(fill = 'white', color = 'red') +
      xlab('------Product Name-------') +
      ylab('-----Count-------') +
      labs(title = 'Distribution of Product Brands') +
      theme_classic()

    
    ggplot(data = df, aes(x = factor(NumImages))) +
      geom_bar(fill = "yellow", color = "black") +
      xlab("Number of Images") +
      ylab("Frequency") +
      labs(title = "Distribution of Number of Images") +
      theme_minimal()
    
    
---------------------------------     
#Bivariate Analysis

View(df)
df$Gender
head(df)

ggplot(data = df, aes(x = Gender, y = Price)) +
  geom_boxplot( fill=rainbow(6) )
  xlab('------Gender-------') +
  ylab('------Price-------') +
  labs(title = 'Bivariate Analysis of Gender and Price')


summary_table= df %>%
  group_by(Gender) %>%
  summarise(
    count = n(),
    avg_price = mean(Price),
    Median_Price = median(Price),
    Min_Price = min(Price),
    Max_Price = max(Price) )
print(summary_table)

# NumImages by Gender::::

ggplot(data = df, aes(x = as.factor(NumImages), fill = Gender)) +
  geom_bar() +
  xlab('Num of images clicked') +
  ylab('Count') +
  labs(title = 'Bar Plot: Count of each NumImages by Gender') +
  theme_classic()

#Numimage by price

  ggplot(data=df,aes(x = NumImages, y = Price)) +
  geom_point(color='blue') +
  xlab("Num of images clicked") +
  ylab("Price") +
  labs(title = "Scatter Plot: NumImages vs. Price") +
  theme_classic()


#Top  Product 

#total records
df %>% 
  group_by(ProductBrand) %>%
  summarise(totalRecords = n()) %>% 
  arrange( desc(totalRecords)  ) %>%
  head(10)

#avrg price-top 5 based on total records

  df %>%
  group_by(ProductBrand) %>%
  summarise(totalRecords = n(), Average_product_Price = mean(Price)) %>%
  arrange(desc(totalRecords)) %>%
  head(5)%>%
    ggplot(aes(x = ProductBrand, y = Average_product_Price) )+
    geom_bar(stat = 'identity',fill = rainbow(5)) +
    xlab('Product Brand') +
    ylab('Average_product_Price') +
    labs(title = 'Top 5 Product Brands vs Average Price') +
    theme_light()
  

#Least one under performed product based on total records

df %>% 
  group_by(ProductBrand) %>%
  summarise(totalRecords = n(),min_product_price= min(Price)) %>% 
  arrange( totalRecords  ) %>%
  head(10)%>%
  ggplot(aes(x = ProductBrand, y = min_product_price) )+
  geom_bar(stat = 'identity',fill = rainbow(10)) +
  xlab('Product Brand') +
  ylab('Avrg_Product_Price') +
  labs(title = 'Bottom 10 Product Brands vs Average Price') +
  theme_light()

# Average Price of Products by Color
df %>%
  group_by(PrimaryColor) %>%
  summarise(avg_price = mean(Price)) %>%
  filter(avg_price < 1500) %>%
  ggplot(aes(x = PrimaryColor, y = avg_price)) +
  geom_bar(stat = "identity") +
  xlab("Primary Color") +
  ylab("Average Price") +
  labs(title = "Average Price of Products by Color (Avg Price < 1500)") +
  theme_classic()

#Above average scenario of 

df %>%
  group_by(ProductBrand) %>%
  summarise(totalRecords = n()) %>%
  filter(totalRecords > mean(totalRecords)) %>%
  arrange(totalRecords)

#below average scenario of 
df %>% 
  group_by(ProductBrand) %>%
  summarise(totalRecords = n()) %>% 
  filter( totalRecords > mean(totalRecords) )%>%
  arrange( desc(totalRecords)   ) 

# brand have an avg price more than 10000
df %>%
  group_by(ProductBrand) %>%
  summarise(totalRecords = n(), Average_product_Price = mean(Price)) %>%
  arrange((Average_product_Price)) %>%
  filter(Average_product_Price > 10000) %>% 
  ggplot(aes(x = ProductBrand, y = Average_product_Price) )+
  geom_bar(stat = 'identity',fill= rainbow(8)) +
  xlab('Product Brand') +
  ylab('Avrg_Product_Price') +
  labs(title = 'Product more than 10000 price') +
  theme_light()

# brand have an avg price more than 10000
approx_average_price_less_10000=df %>%
  filter(Price < 10000) %>%
  summarise(approx_average_price_less_10000 = mean(Price))
print(approx_average_price_less_10000)

# brand have an avg price less than 10000
approx_average_price_above_10000 = df %>%
  filter(Price > 10000) %>%
  summarise(approx_average_price_above_10000 = mean(Price))

print(approx_average_price_above_10000)

# Gender that buys most of the products having price less than 10000
df %>%
  filter(Price < 10000) %>%
  group_by(Gender) %>%
  summarise(total_products = n()) %>%
  arrange(desc(total_products)) %>%
  ggplot(aes(x = Gender,y=total_products,fill=Gender)) +
  geom_bar(stat = "identity") +
  xlab("Gender") +
  ylab("Total Products") +
  labs(title = "Bar Plot: Audience Buying Most Products (Price < 10000)") +
  theme_classic()

# Gender that buys most of the products having price more than 10000

df %>%
  filter(Price > 10000) %>%
  group_by(Gender) %>%
  summarise(total_products = n()) %>%
  arrange(desc(total_products)) %>%
  ggplot(aes(x = Gender,y=total_products,fill=Gender)) +
  geom_bar(stat = "identity") +
  xlab("Gender") +
  ylab("Total Products") +
  labs(title = "Bar Plot: Audience Buying Most Products (Price >10000)") +
  theme_classic()


---------------------------------
# Multivariate Analysis
  
  head(df)
df$NumImages
head(df$NumImages)


df$NumImages=factor(df$NumImages)
df %>%
  ggplot(aes(x = Price, y = NumImages, color = Gender)) +
  geom_point() +
  xlab("Price") +
  ylab('Num of images clicked') +
  labs(title = 'Scatter Plot->Gender vs. Price vs. NumImages') +
  theme_light()

summary_table2= df %>%
  group_by(Gender,NumImages) %>%
  summarise(
    count = n(),
    Avg_price = mean(Price),
    Median_Price = median(Price),
    Min_Price = min(Price),
    Max_Price = max(Price) )
print(summary_table2)



df %>%
  ggplot(aes(x = Gender, y = Price, fill = Gender)) +
  geom_boxplot() +
  xlab("Gender") +
  ylab('Price') +
  labs(title = 'Box Plot-> Gender vs. Price') +
  theme_gray()

--------------------------------- 
# Check distribution of variables

  View(df)

#Density Plot
ggplot(data = df, aes(x = Price, fill = Gender)) +
  geom_density() +
  xlab("Price") +
  ylab("Density") +
  labs(title = "Density Plot: Distribution of Price by Gender") +
  theme_classic()

# Boxplot for Price by Gender

ggplot(data = df, aes(x = Gender, y = Price)) +
  geom_boxplot(fill=rainbow(6)) +
  xlab("Gender") +
  ylab("Price") +
  labs(title = "Boxplot: Distribution of Price by Gender") +
  theme_classic()

# Boxplot for Numimages by Gender

ggplot(data = df, aes(x = Gender, y = NumImages,fill=Gender)) +
  geom_boxplot() +
  xlab("Gender") +
  ylab("Num of images clicked") +
  labs(title = "Boxplot: Distribution of NumImages by Gender") +
  theme_classic()


#  Price Distribution 
ggplot(data = df, aes(x = Price)) +
  geom_histogram(bins = 10, fill = 'blue', color = 'black') +
  xlab("Price") +
  ylab("Frequency") +
  labs(title = "Histogram: Distribution of Price") +
  theme_light()
  
str(df)
head(df)
---------------------------------------------------------------------
---------------------------------------------------------------------
---------------------------------------------------------------------
#1. Create a new Column “NewGender” to analyse further its distribution. 
#Going forward we will consider this group for tasks

  #Logic Applied
#i. Include Boys & Men as Men
#ii. Include Girls & Women as Women
#iii. Include Unisex & Unisex Kids as Unisex

#2. Complete the analysis of NewGender along with other categorical cols.


View(df)
unique(df$Gender)
gender_group

df =df %>%
  mutate(New_Gender = case_when(
    Gender %in% c('Boys', 'Men') ~ 'Men',
    Gender %in% c('Girls', 'Women') ~ 'Women',
    Gender %in% c('Unisex Kids', 'Unisex') ~ 'Unisex',
    TRUE ~ 'others')
    )
unique(df$New_Gender)


# analysis of NewGender by count


ggplot(data = df, aes(x = New_Gender, fill = New_Gender)) +
  geom_bar(color = 'black') +
  xlab('-------New Gender Group--------') +
  ylab('------------Count----------') +
  labs(title = 'Bar Plot Analysis: Gender By Count') +
  theme_classic()

# Statistics:::::Count of each category in New_Gender

summary_New_Gender= df%>%
  group_by(New_Gender) %>%
  summarise('Total Cust' = n())
print(summary_New_Gender)

# analysis of NewGender by Average Price

df %>%
  group_by(New_Gender) %>%
  summarise(avg_price = mean(Price)) %>%
  arrange(desc(avg_price))%>%
  ggplot(aes(x = New_Gender, y = avg_price,fill=New_Gender)) +
  geom_bar(stat = "identity") +
  xlab("---New Gender---") +
  ylab("--- Average Price ----") +
  labs(title = "Bar Plot: Avrg Price of New Gender") +
  theme_classic()

# analysis of brand on which men category has highest amount on average

df %>%
  filter(New_Gender == "Men") %>%
  group_by(ProductBrand) %>%
  summarise(average_amount = mean(Price, na.rm = TRUE)) %>%
  arrange(desc(average_amount)) %>%
  head(5) %>%
  ggplot(aes(x = ProductBrand, y = average_amount,fill=ProductBrand)) +
  geom_bar(stat = "identity") +
  xlab("---Brand Name---") +
  ylab("--- Average Price ----") +
  labs(title = "Bar Plot: Top 5 Avrg Price of Men Catagories") +
  theme_classic()

# Top 5 colored products for Unisex

df %>%
  filter(New_Gender == "Unisex") %>%
  group_by(PrimaryColor)%>%
  summarise(count = n()) %>%
  arrange(desc(count))%>%
  head(5)%>%
  ggplot(aes(x = PrimaryColor, y = count,fill=PrimaryColor)) +
  geom_bar(stat = "identity") +
  xlab('-------Primary Color--------') +
  ylab('------------Count----------') +
  labs(title = 'Bar Plot: Top 5 Colored Products for Unisex') +
  theme_classic()

#Top 5 colored products for Women

df %>%
  filter(New_Gender == "Women") %>%
  group_by(PrimaryColor)%>%
  summarise(count = n()) %>%
  arrange(desc(count))%>%
  head(5)%>%
  ggplot(aes(x = PrimaryColor, y = count,fill=PrimaryColor)) +
  geom_bar(stat = "identity") +
  xlab('-------Primary Color--------') +
  ylab('------------Count----------') +
  labs(title = 'Bar Plot: Top 5 Colored Products for Womens') +
  theme_classic()

---------------------------------------------------------------------
#Create a new Column “DescriptionLength” to analyse further its distribution.

#Logic Applied:::::::::

#a. Each record of DescriptionLength is equal to the number of chars in Description
#b. Complete the analysis of DescriptionLength along with other categorical cols.
#c. Isn’t it important to check if attribute information is also included in Description?
    #Complete this task before answering it.
#d. Also need to check if attribute information is also included in ProductName
  
  
### #DescriptionLength = number of chars in existing Description column

df$DescriptionLength= nchar(df$Description)

View(df$DescriptionLength)
View(df)

# Distribution of Description Length by Gender
ggplot(df, aes(x = New_Gender, y = DescriptionLength, fill = New_Gender)) +
  geom_boxplot() +
  xlab('Gender') +
  ylab('Description Length') +
  labs(title = 'Distribution of Description Length by Gender') +
  theme_minimal()

#Group have the highest average description length for the products
avrg_desc_length = df %>%
  group_by(New_Gender) %>%
  summarise(avrg_desc_length = mean(DescriptionLength)) %>%
  arrange(desc(avrg_desc_length))%>%
  ggplot(aes(x = New_Gender, y = avrg_desc_length,fill=New_Gender)) +
  geom_bar(stat = "identity") +
  xlab('-------New Gender--------') +
  ylab('------------Avg Desc Lenghth----------') +
  labs(title = 'Bar Plot: Group have the highest average description length') +
  theme_classic()

avrg_desc_length

  
df %>%
  group_by(ProductBrand) %>%
  summarise(NumRecords = n(), AvgDescLength = mean(DescriptionLength))

  
df %>%
  group_by(ProductBrand, Price) %>%
  summarise(AvgDescLength = mean(DescriptionLength))

df %>%
  group_by(ProductBrand, NumImages) %>%
  summarise(AvgDescLength = mean(DescriptionLength))


View(df)
str(df$ProductBrand)
is.vector(df$ProductBrand) 


# Percetage of of products that have mentioned brand name in Description 
#This will help us to analyse the Marketing team strtegies for SEO/SEM
df$Brandname_mentioned= 
apply(df, 1, function(row) 
  grepl(as.character(row["ProductBrand"]),row["Description"])) 

Total_number_Brandname_mentioned= sum(df$Brandname_mentioned)
Total_rows = nrow(df)
percentage_brand_mentioned= (Total_number_Brandname_mentioned/Total_rows)*100

print(percentage_brand_mentioned)


# Percetage of products that have mentioned color in Description
df$colorname_mentioned=
  apply(df, 1, function(row) 
    grepl(as.character(row["PrimaryColor"]),row["Description"])) 

Total_number_Brandname_mentioned= sum(df$colorname_mentioned)
Total_rows = nrow(df)
percentage_color_mentioned=(Total_number_Brandname_mentioned/Total_rows)*100

print(percentage_color_mentioned)


#Name the group for which you can find the maximum number of records having 
#color mentioned in Description

grouped_color_mentioned = df %>%
  group_by(New_Gender) %>%
  summarise(num_records_with_color_mentioned = sum(colorname_mentioned))%>%
  
  filter(num_records_with_color_mentioned == 
           max(num_records_with_color_mentioned))
grouped_color_mentioned

#Percentage of products that have mentioned color in ProductName

df$color_in_product = grepl("color", tolower(df$ProductName))

percentage_color_in_product= mean(df$color_in_product) *100
percentage_color_in_product

#Group for which you can find the maximum number of records having 
#color mentioned in product name

grouped_color_in_product= df%>%
  group_by(New_Gender)%>%
  summarise(num_records_with_color_in_product=sum(color_in_product))%>%
  filter(num_records_with_color_in_product == max(num_records_with_color_in_product))
grouped_color_in_product

#Group for which you can find the maximum number of records having 
#brand mentioned in product name

grouped_brand_mentioned <- df %>%
  filter(grepl(as.character(ProductBrand), ProductName)) %>%  
  group_by(New_Gender) %>%
  count()

max_records_brand_mentioned <- grouped_brand_mentioned %>%
  filter(n == max(n))

max_records_brand_mentioned


---------------------------------------------------------------------
#1.Create a new Column “AgeGroup” to analyse further its distribution.

#Logic Applied
  #i. Include Boys, Girls & Unisex Kids as Kids
  #ii. Include Men, Women & Unisex as Adults

#2. Complete the analysis of NewGender along with other categorical cols.

  
  df =df %>%
  mutate(AgeGroup = case_when(
    Gender %in% c("Boys", "Girls", "Unisex Kids") ~ "Kids",
    Gender %in% c("Men", "Women", "Unisex") ~ "Adults",
    TRUE ~ "Others")
    )
unique(df$AgeGroup)

  
#Unique Product By Age Group

unique_products_by_agegroup = df %>%
  group_by(AgeGroup) %>%
  summarise(num_unique_products = n_distinct(ProductName))

ggplot(unique_products_by_agegroup, aes(x = AgeGroup, y = num_unique_products, fill = AgeGroup)) +
  geom_bar(stat = "identity", color = "black") +
  xlab("--Age Group--") +
  ylab("--Number of Unique Products--") +
  labs(title = "Number of Unique Products by Age Group") +
  theme_minimal()


#Group has wide range of products:
widest_range_group = df %>%
  group_by(AgeGroup) %>%
  summarise(num_unique_products = n_distinct(ProductName)) %>%
  filter(num_unique_products == max(num_unique_products))
widest_range_group

#Avg price > of the products for adults as compared to kids
avg_price_comparison_agegroup = df %>%
  group_by(AgeGroup) %>%
  summarise(avg_price = mean(Price))
avg_price_comparison_agegroup


ggplot(data=avg_price_comparison_agegroup, aes(x = AgeGroup, y = avg_price, fill = AgeGroup)) +
  geom_bar(stat = "identity", color = "yellow") +
  xlab("--Age Group--") +
  ylab("--Avg Price--") +
  labs(title = "Avg price Comparison of Age Group") +
  theme_classic()

# Avg images of the products for Kids:

str(df$NumImages)

df$NumImages = as.numeric(as.character(df$NumImages))

avg_image_kids =df %>%
  filter(AgeGroup == "Kids") %>%
  summarise(avg_images = mean(NumImages))
avg_image_kids

# Avg images of the products for adults:
str(df$NumImages)

df$NumImages = as.numeric(as.character(df$NumImages))

avg_image_adults =df %>%
  filter(AgeGroup == "Adults") %>%
  summarise(avg_images = mean(NumImages))
avg_image_adults

# Visuallization of avg_image_comparison_by_agegroup
avg_images_by_agegroup <- df %>%
  mutate(NumImages = as.numeric(as.character(NumImages))) %>%
  group_by(AgeGroup) %>%
  summarise(avg_images = mean(NumImages))

ggplot(avg_images_by_agegroup, aes(x = AgeGroup, y = avg_images, fill = AgeGroup)) +
  geom_bar(stat = 'identity') +
  xlab('--Age Group--') +
  ylab('--- Avg Images of the Products ---') +
  labs(title = 'Comparison of Average Number of Images by Age Group') +
  theme_classic()
  
## Top brand among kids and adults
   #for Kids analysis
  top_brand_kids = df %>%
    filter(AgeGroup == "Kids") %>%
    group_by(ProductBrand) %>%
    summarise(number_products = n()) %>%
    arrange(desc(number_products))%>%
    head(10)%>%
    ggplot(aes(x = ProductBrand, y = number_products, fill = ProductBrand)) +
    geom_bar(stat = "identity") +
    xlab("Top Brand Among Kids") +
    ylab("Number of Products") +
    labs(title = "Top 10 Brand Among Kids by Number of Products") +
    theme_minimal()
  top_brand_kids
    
  #for Adults analysis
  
  install.packages('cowplot')
  library(cowplot)
  
  top_brand_adults = df %>%
    filter(AgeGroup == "Adults") %>%
    group_by(ProductBrand) %>%
    summarise(number_products = n()) %>%
    arrange(desc(number_products))%>%
    head(10)%>%
    ggplot(aes(x = ProductBrand, y = number_products, fill = ProductBrand)) +
    geom_bar(stat = "identity") +
    xlab("Top Brand Among Adults") +
    ylab("Number of Products") +
    labs(title = "Top 10 Brand Among Adults by Number of Products") +
    theme_minimal()
  top_brand_adults
  
  plot_grid(top_brand_kids, top_brand_adults, ncol = 2)



# Kids >brand that is having the highest price on average
highest_avg_price_kidsbrand = df %>%
  filter(AgeGroup == "Kids") %>%
  group_by(ProductBrand) %>%
  summarise(avg_price = mean(Price)) %>%
  arrange(desc(avg_price)) %>%
  head(5)
highest_avg_price_kidsbrand %>%
ggplot(aes(x = ProductBrand, y = avg_price, fill = ProductBrand)) +
  geom_bar(stat = "identity") +
  xlab("-- Product Brand --") +
  ylab("------Average Price------") +
  labs(title = "Kids brand with highest price on average") +
  theme_minimal()



# Adults brand that is having the least price on average among all

lowest_brand_adults = df %>%
  filter(AgeGroup == "Adults") %>%
  group_by(ProductBrand) %>%
  summarise(avrg_price = mean(Price)) %>%
  arrange(avrg_price)%>%
  head()
lowest_brand_adults%>%
  ggplot(aes(x = ProductBrand, y = avrg_price, fill = ProductBrand)) +
  geom_bar(stat = "identity") +
  xlab("Top Brand Among Adults") +
  ylab("Number of Products") +
  labs(title = "Adult brand with least price on average") +
  theme_minimal()


#3rd most shown color for the kids products

Top_most_shown_color_kids =df %>%
  filter(AgeGroup == "Kids") %>%
  group_by(PrimaryColor) %>%
  summarise(num_color = n()) %>%
  arrange(desc(num_color))%>%
  head(5)
Top_most_shown_color_kids %>%
    ggplot(aes(x = PrimaryColor, y = num_color, fill = PrimaryColor)) +
    geom_bar(stat = "identity") +
    xlab("Color Name") +
    ylab("Number of Colors") +
    labs(title = "Top 5 Color for Kids") +
    theme_minimal()
  





