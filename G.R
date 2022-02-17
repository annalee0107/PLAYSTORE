
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(formatR)) install.packages("formatR", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(data.table)
library(lubridate)
library(ggplot2)
library(formatR)


loc <- "D:/R/GOOGLE STORE/Playstore_final.csv/Playstore_final.csv"
if(!exists("d1")) d0<-read.csv2(loc ,header=TRUE, sep=",",quote='"',  encoding="UTF-8")

 d0a <- d0[ !is.na(as.numeric(d0$Rating) ),]
   d0b <- d0a[ !is.na(as.numeric(d0a$Reviews) )  ,]
    d0c <- d0b[!is.na(as.numeric(d0b$Rating.Count) ),] 
    d0d <- d0c[!is.na(as.numeric(d0c$Price) ),] 
    
# the dataset summarized mean Rating per applications
# the rating total of every application is calculated 
 d0e<- d0d %>% mutate(ratingtotal = as.numeric(Rating) * as.numeric(Rating.Count))
   d0f <- d0e[!is.na(as.numeric(d0e$ratingtotal) ),]

d0g<-d0f %>% select (App.Id,Category, Content.Rating, Rating, Rating.Count,ratingtotal, Developer.Id, Developer)
d1 <- d0g %>% mutate (Rating = as.numeric(Rating),Rating.Count= as.numeric(Rating.Count),ratingtotal = as.numeric(ratingtotal) )

#Preview d1 dataset
head(d1)

#Create Train and test dataset
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = d1$Rating, times = 1, p = 0.1, list = FALSE)

store <- d1[-test_index,]
temp <- d1[test_index,]

# Make sure Category and Developer.Id in validation set are also in store set
validation <- temp %>% 
      semi_join(store , by = "Developer.Id")%>% 
      semi_join(store , by = "Category")

# Add rows removed from validation set back into store set

removed <- anti_join(temp, validation)
store <- rbind(store, removed)

temp0<- store %>% group_by(Category) %>% summarise (Num_of_App=n() , sum_rating = sum(as.numeric(ratingtotal)), sum_ratecount = sum(as.numeric(Rating.Count  ))) 
temp0a <- temp0 %>% mutate( Avg_rating = as.numeric(sum_rating) /  as.numeric(sum_ratecount) ) 
temp0a %>% arrange (desc (Num_of_App))
rm(temp0)

#Plot of Number of Apps, Number of Ratings, Rating and Mean Rating by Category:

top_n(temp0a, n=50, Num_of_App) %>% mutate(name = fct_reorder(Category, Num_of_App))%>%  ggplot( aes(x=name, y=Num_of_App)) +  geom_bar(stat="identity", fill="#ff0000", alpha=.6, width=.4) + coord_flip() + xlab("Category") + theme_bw()  + ggtitle("Plot of Number of Apps by Category")+  theme( axis.text = element_text(size = 6)  )

top_n(temp0a, n=50, sum_ratecount) %>% mutate(name = fct_reorder(Category, sum_ratecount))%>%  ggplot( aes(x=name, y=sum_ratecount)) +  geom_bar(stat="identity", fill="#ff00ff", alpha=.6, width=.4) + coord_flip() + xlab("Category") + theme_bw()  + ggtitle("Plot of Number of Rating by Category")+  theme( axis.text = element_text(size = 6)  )
 
ggplot(store ,aes(factor(Category),as.numeric(Rating) ))+ geom_boxplot() + xlab("Category") + ylab("Rating") + coord_flip()  + ggtitle("Plot of Rating by Category") 

top_n(temp0a, n=50, Avg_rating) %>% mutate(name = fct_reorder(Category, Avg_rating))%>%  ggplot( aes(x=name, y=Avg_rating)) + geom_point()  +geom_line()+geom_smooth() + xlab("Category ")+  theme_bw()+ coord_flip()  + ggtitle("Plot of Mean Rating by Category")+ theme( axis.text = element_text(size = 6)  )

#Plot of Number of Apps, Number of Ratings, Rating and Mean Rating by Developer:


temp1<- store %>% group_by(Developer = str_sub(Developer,1,20) ) %>% summarise (Num_of_App=n() , sum_rating = sum(as.numeric(ratingtotal)), sum_ratecount = sum(as.numeric(Rating.Count ))) 
temp1a <- temp1 %>% mutate( Avg_rating = as.numeric(sum_rating) /  as.numeric(sum_ratecount) ) 
temp1a %>% arrange (desc (Num_of_App))
rm(temp1)

top_n(temp1a, n=25, Num_of_App) %>% mutate(name = fct_reorder(Developer, Num_of_App))%>%  ggplot( aes(x=name, y=Num_of_App)) +  geom_bar(stat="identity", fill="#ff0000", alpha=.6, width=.4) + coord_flip() + xlab("Developer (top 25)") + theme_bw()  + ggtitle("Plot of Number of Apps by Developer")

top_n(temp1a, n=25, sum_ratecount) %>% mutate(name = fct_reorder(Developer, sum_ratecount))%>%  ggplot( aes(x=name, y=sum_ratecount)) +  geom_bar(stat="identity", fill="#ff00ff", alpha=.6, width=.4) + coord_flip() + xlab("Developer (top 25)") + theme_bw()  + ggtitle("Plot of Number of Rating by Developer")
 
#ggplot(store,aes(factor(Developer),as.numeric(Rating) ))+ geom_boxplot() + xlab("Developer") + ylab("Rating") + coord_flip()  + ggtitle("Plot of Rating by Developer") 

top_n(temp1a, n=25, sum_ratecount) %>% mutate(name = fct_reorder(Developer, Avg_rating))%>%  ggplot( aes(x=name, y=Avg_rating)) + geom_point()  +geom_line()+geom_smooth() + xlab("Developer (top 25)")+  theme_bw()+ coord_flip()  + ggtitle("Plot of Mean Rating by Developer")


#Plot of Number of Apps, Number of Ratings, Rating and Mean Rating by Content.Rating:

temp2<- store %>% group_by(Content.Rating) %>% summarise (Num_of_App=n() , sum_rating = sum(as.numeric(ratingtotal)), sum_ratecount = sum(as.numeric(Rating.Count  ))) 
temp2a <- temp2 %>% mutate( Avg_rating = as.numeric(sum_rating) /  as.numeric(sum_ratecount) ) 
temp2a %>% arrange (desc (Num_of_App))
rm(temp2)


temp2a %>% mutate(name = fct_reorder(Content.Rating, Num_of_App))%>%  ggplot( aes(x=name, y=Num_of_App)) +  geom_bar(stat="identity", fill="#ff0000", alpha=.6, width=.4) + coord_flip() + xlab("Content Rating") + theme_bw()  + ggtitle("Plot of Number of Apps by Content Rating")

temp2a %>% mutate(name = fct_reorder(Content.Rating, sum_ratecount))%>%  ggplot( aes(x=name, y=sum_ratecount)) +  geom_bar(stat="identity", fill="#ff00ff", alpha=.6, width=.4) + coord_flip() + xlab("Content Rating") + theme_bw()  + ggtitle("Plot of Number of Rating by Content Rating")
 
ggplot(store ,aes(factor(Content.Rating),as.numeric(Rating) ))+ geom_boxplot() + xlab("Content.Rating") + ylab("Rating") + coord_flip()  + ggtitle("Plot of Rating by Content Rating") 

temp2a %>% mutate(name = fct_reorder(Content.Rating, Avg_rating))%>%  ggplot( aes(x=name, y=Avg_rating)) + geom_point()  +geom_line()+geom_smooth() + xlab("Content Rating")+  theme_bw()+ coord_flip()  + ggtitle("Plot of Mean Rating by Content Rating")

## RMSE 
##Predict using mean rating

s1 <- store%>% summarize (sum_rating =sum(as.numeric(ratingtotal)), sum_ratecount = sum(as.numeric(Rating.Count ))  )
store_mean <- s1$sum_rating / s1$sum_ratecount
store_mean




##Predict using :

##1. Mean rating

```{r RMSE, warning = FALSE,echo=FALSE}
RMSE <- function(true_ratings, predicted_ratings){
     sqrt(mean((true_ratings - predicted_ratings)^2))
}

#naive_rmse <- RMSE(as.numeric(validation$ratingtotal), store_mean *as.numeric( validation$Rating.Count) )
naive_rmse <- RMSE(as.numeric(validation$Rating), store_mean  )

predictions <- rep(round(store_mean,1) , nrow(validation))

rmse_results <- data_frame(method = "Using Mean Rating", RMSE = naive_rmse)


##2. Mean rating and Category effect :


Category_avgs <- store %>% 
     group_by(Category) %>%  summarize(b_i = mean(as.numeric(Rating) - store_mean))

Category_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- store_mean + validation %>% 
     left_join(Category_avgs, by='Category') %>%
     .$b_i

model_1_rmse <- RMSE(predicted_ratings, as.numeric(validation$Rating))
rmse_results2 <- bind_rows(rmse_results,
                          data_frame(method="Category Effect Model",
                                     RMSE = model_1_rmse ))

rmse_results2 %>% knitr::kable()

##3. Category + Developer.Id effect :



Developer.Id_avgs <- store %>% select (Category, Developer.Id, Rating ) %>%
     left_join(Category_avgs, by='Category') %>%
     group_by(Developer.Id) %>%
     summarize(b_u = mean(as.numeric(Rating) - store_mean - b_i))
Developer.Id_avgs %>% qplot(b_u, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- validation %>% select (Category, Developer.Id) %>%
     left_join(Category_avgs, by='Category') %>%
     left_join(Developer.Id_avgs, by='Developer.Id') %>%
     mutate(pred = store_mean + b_i + b_u) %>%
     .$pred

model_2_rmse <- RMSE(predicted_ratings, as.numeric(validation$Rating) )
rmse_results3 <- bind_rows(rmse_results2,
      data_frame(method="Category + Developer.Id Effects Model",   RMSE = model_2_rmse ))
rmse_results3 %>% knitr::kable()

##4. Content.Rating effect

Content.Rating_avgs <- store %>% select (Category, Developer.Id, Content.Rating, Rating ) %>%
     left_join(Category_avgs, by='Category') %>%
      left_join(Developer.Id_avgs, by='Developer.Id') %>%
     group_by(Content.Rating) %>%
     summarize(b_m = mean(as.numeric(Rating) - store_mean - b_i - b_u))
Content.Rating_avgs %>% qplot(b_m, geom ="histogram", bins = 10, data = ., color = I("black"))

predicted_ratings <- validation %>% select (Category,  Developer.Id, Content.Rating) %>%
     left_join(Category_avgs, by='Category') %>%
     left_join(Developer.Id_avgs, by='Developer.Id') %>%
      left_join(Content.Rating_avgs, by='Content.Rating') %>%
     mutate(pred = store_mean + b_i + b_u +b_m) %>%
     .$pred
model_3_rmse <- RMSE(predicted_ratings, as.numeric(validation$Rating) )
rmse_results4 <- bind_rows(rmse_results3,
      data_frame(method="Category + Developer.Id + Content.Rating Effects Model",   RMSE = model_3_rmse ))
rmse_results4 %>% knitr::kable()

##5. Regularized Category Effect

lambda <- 3
mu <- mean(as.numeric(store$Rating))
Category_reg_avgs <- store %>% 
    group_by(Category) %>% 
    summarize(b_i = sum(as.numeric(Rating) - mu)/(n()+lambda), n_i = n()) 
data_frame(original = Category_avgs$b_i, 
           regularlized = Category_reg_avgs$b_i, 
           n = Category_reg_avgs$n_i) %>%
    ggplot(aes(original, regularlized, size=sqrt(n))) + 
    geom_point(shape=1, alpha=0.5)

store %>%
     dplyr::count(Category) %>% 
     left_join(Category_reg_avgs) %>%
     arrange(desc(b_i)) %>% 
     select( b_i, n) %>% 
     slice(1:10) %>% 
     knitr::kable()
validation %>%
     dplyr::count(Category) %>% 
     left_join(Category_reg_avgs) %>%
     arrange(b_i) %>% 
     select( b_i, n) %>% 
     slice(1:10) %>% 
     knitr::kable()
predicted_Ratings <- validation %>% 
     left_join(Category_reg_avgs, by='Category') %>%
     mutate(pred = mu + b_i) %>%
     .$pred
model_5_rmse <- RMSE(predicted_Ratings, as.numeric(validation$Rating) )
rmse_results5 <- bind_rows(rmse_results,
       data_frame(method="Regularized Category Effect Model",  
         RMSE = model_5_rmse ))

rmse_results5 %>% knitr::kable()

rm(Category_reg_avgs)


##6. optimise lamdas for Category effect

lambdas <- seq(0, 10, 0.25)
mu <- mean(as.numeric (store$Rating) )
just_the_sum <- store %>% 
    group_by(Category) %>% 
    summarize(s = sum(  as.numeric(Rating) - mu), n_i = n())
rmses5a <- sapply(lambdas, function(l){
    predicted_Ratings <- validation %>% 
        left_join(just_the_sum, by='Category') %>% 
        mutate(b_i = s/(n_i+l)) %>%
        mutate(pred = mu + b_i) %>%
        .$pred
    return(RMSE(predicted_Ratings, as.numeric(validation$Rating)  ))
})
qplot(lambdas, rmses5a)  
l1 <- lambdas[which.min(rmses5a)]
l1
rmses5 <- validation %>% 
        left_join(just_the_sum, by='Category') %>% 
        mutate(b_i = s/(n_i+l1)) %>%
        mutate(pred = mu + b_i) %>%
        .$pred
model_5_rmse <- RMSE(rmses5, as.numeric(validation$Rating)  )
rmse_results5 <- bind_rows(rmse_results,
       data_frame(method="Regularized Category Effect Model",  
         RMSE = model_5_rmse ))

rmse_results5 %>% knitr::kable()

 
##7. optimise lambdas for Developer.Id effect



lambdas <- seq(0, 10, 0.25)

 Category_avgs2e <- store %>% select(Category) %>%  left_join(just_the_sum, by='Category') %>%     mutate(b_i = s/(n_i+l1)) %>%
       select (Category , b_i)
  Category_avgs2v <- validation %>% select(Category) %>%  left_join(just_the_sum, by='Category') %>%     mutate(b_i = s/(n_i+l1)) %>%
       select (Category , b_i)

u1 <- store %>% select ( Developer.Id, Rating) %>% cbind (Category_avgs2e$b_i) %>% set_names ("Developer.Id","Rating","b_i") %>% group_by(Developer.Id) %>%      summarize(s = sum( as.numeric(Rating) - mu - b_i ), n_i = n()) %>% select(Developer.Id,s,n_i)


rmses6a <- sapply(lambdas, function(l){
    predicted_Ratings <- validation %>% select ( Developer.Id)  %>% 
      cbind (rmses5)%>%   set_names ("Developer.Id","mu_b_i")%>%
      left_join(u1, by='Developer.Id') %>% 
        mutate(b_u = s/(n_i+l)) %>%
        mutate(pred = mu_b_i +b_u) %>%
        .$pred
    return(RMSE(predicted_Ratings,  as.numeric(validation$Rating)   ))
})
qplot(lambdas, rmses6a)  
l2<- lambdas[which.min(rmses6a)]
l2

rmses6 <- validation %>% select ( Developer.Id)  %>% 
      cbind (rmses5)%>%   set_names ("Developer.Id","mu_b_i")%>%
      left_join(u1, by='Developer.Id') %>% 
        mutate(b_u = s/(n_i+l2)) %>%
        mutate(pred = mu_b_i +b_u) %>%
        .$pred
model_6_rmse <- RMSE( rmses6 ,  as.numeric(validation$Rating) )
rmse_results6 <- bind_rows(rmse_results5,
       data_frame(method="Regularized Developer.Id Effect Model",  
         RMSE = model_6_rmse ))

rmse_results6 %>% knitr::kable()



##8. optimise lambdas for Content.Rating effect



lambdas <- seq(0, 10, 0.25)

 Developer.Id_avgs2 <- store %>% select(Developer.Id) %>%  left_join(u1, by='Developer.Id') %>%     mutate(b_u = s/(n_i+l2)) %>%  select (Developer.Id , b_u)

 rm(u1)

g1 <- store %>% select ( Content.Rating, Rating) %>% cbind (Category_avgs2e$b_i,Developer.Id_avgs2$b_u) %>% set_names ("Content.Rating","Rating","b_i","b_u") %>% group_by(Content.Rating) %>%      summarize(s = sum( as.numeric(Rating) - mu - b_i - b_u ), n_i = n()) %>% select(Content.Rating,s,n_i)


rmses7a <- sapply(lambdas, function(l){
    predicted_Ratings <- validation %>% select ( Content.Rating)  %>% 
      cbind (rmses5)%>%   set_names ("Content.Rating","mu_b_i_bu")%>%
      left_join(g1, by='Content.Rating') %>% 
        mutate(b_g = s/(n_i+l)) %>%
        mutate(pred = mu_b_i_bu +b_g) %>%
        .$pred
    return(RMSE(predicted_Ratings, as.numeric(validation$Rating)  ))
})
qplot(lambdas, rmses7a)  
l3<- lambdas[which.min(rmses7a)]
l3

rmses7 <- validation %>% select (Content.Rating)  %>% 
      cbind (rmses6)%>%   set_names ("Content.Rating","mu_b_i_bu")%>%
      left_join(g1, by='Content.Rating') %>% 
        mutate(b_g = s/(n_i+l3)) %>%
        mutate(pred = mu_b_i_bu +b_g) %>%
        .$pred

model_7_rmse <- RMSE( rmses7 , as.numeric(validation$Rating)  )
rmse_results7 <- bind_rows(rmse_results6,
       data_frame(method="Regularized Content.Rating Effect Model",  
         RMSE = model_7_rmse ))

rmse_results7 %>% knitr::kable()

#Conclusion
v2 <- validation %>% mutate (diff = validation$Rating - rmses7)
ggplot(v2 ,aes(factor(Category),as.numeric(diff) ))+ geom_boxplot() + xlab("Category") + ylab("Prediction Variance") + coord_flip()  + ggtitle("Prediction Variance by Category") 

ggplot(v2 ,aes(factor(Content.Rating),as.numeric(diff) ))+ geom_boxplot() + xlab("Content.Rating") + ylab("Prediction Variance") + coord_flip()  + ggtitle("Prediction Variance by Content Rating") 

