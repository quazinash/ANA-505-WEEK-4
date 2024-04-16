##### load and create new dataframe
#Task: Write the function to get a dataset from Base R: Titanic
#and give the dataframe a new name of your choice
#(hint: you will want your data to be a dataframe. Use the function: as.data.frame(Titanic))

data("Titanic")

load_titanic_dataset <- function(titanic_nf1) {
  # Load the Titanic dataset
  Titanic_df <- as.data.frame(Titanic)
  
  # Assign the new name to the dataframe
  assign(titanic_nf1, Titanic_df, envir = .GlobalEnv)
  
  # Return the name of the dataframe
  return(titanic_nf1)
}

new_dataset_name <- load_titanic_dataset("titanic_nf1")

###fucntion for top view for row###
#See the top rows of the data
#TASK: Write the function to see the top rows of the data

view_top_rows <- function(titanic_nf1, n = 10) {
  
  # Return the top 'n' rows of the dataframe
  return(head(titanic_nf1,n))
}

view_top_rows(titanic_nf1)

head(titanic_nf1,10)

### Function to install dplyr package###
#Install and call the package dplyr
#TASK: Write the functions to install and call dplyr
install_dplyr <- function()
  
  # Function to call dplyr package
  call_dplyr <- function()
    
  {
    if (requireNamespace("dplyr", quietly = TRUE)) {
      library(dplyr)
      message("dplyr package is now loaded.")
    } else {
      message("dplyr package is not installed. Please install it first.")
    }
  }

call_dplyr()

### select Survived and Sex columns
#Let's just see the Survived and Sex columns
#Task: Write the function to 'select' the Survived and Sex columns 
#(hint: use the 'select' function)

library(dplyr)
select_columns <- function(titanic_nf1, Survived,Sex) {
  selected_columns <- dplyr::select(titanic_nf1, Survived,Sex)
  return(selected_columns)
}

selected_columns <- select_columns(titanic_nf1, c("Survived", "Sex"))
head(selected_columns)

###create new dataset from the existing dataframe with selected columns###
#Let's name the dataset with just the two columns, Survived and Sex
#TASK: Write the function to save the two columns as one new dataset
#and give it a name

create_new_dataset <- function(titanic_nf1, new_tita_1, Survived,Sex) {
  
  selected_columns <- dplyr::select(titanic_nf1, Survived,Sex)
  
  assign(new_tita_1, selected_columns, envir = .GlobalEnv)
  
  return(new_tita_1)
}

new_dataset <- create_new_dataset(titanic_nf1, "new_tita_1", c("Survived", "Sex"))


###remove column from the new dataset###
#Let's get rid of the Sex column in the new dataset created above
#TASK: Write the function that deselects the sex column
#(hint: use the 'select' function to not select a -column


remove_column <- function(new_tita_1, Sex) {
  
  deselected_columns <- dplyr::select(new_tita_1, -Sex)
  
  return(deselected_columns)
}

new_tita1 <- remove_column(new_tita_1, "Sex")
print(new_tita_1)

###rename column name###
#Let's rename a column name
#TASK: Write the function that renames 'Sex' to 'Gender'


rename_column <- function(new_tita_1, Sex,Gender){

  renamed_column<- dplyr::rename(new_tita_1, !!Gender:= !!Sex)
  
  return(renamed_column)
}

rename_new_tita_1 <- rename_column(new_tita_1, "Sex", "Gender")
print(rename_new_tita_1)

###create new dataset with new column name###

#Let's make a new dataframe with the new column name
#TASK: Write the function that names a new dataset that includes the 'gender' column

create_dataset_with_gender <- function(titanic_nf1, new_tita_2) {
  
  renamed_data <- dplyr::rename(titanic_nf1, Gender = Sex)
  
  assign(new_tita_2, renamed_data, envir = .GlobalEnv)
  
  return(new_tita_2)
}
 
new_dataset_name <- create_dataset_with_gender(titanic_nf1, "new_tita_2")

#filter
#Let's 'filter' just the males from our dataset
#TASK: Write the function that includes only rows that are 'male'

filter_male <- function(new_tita_2) {
  

  filtered_data <- dplyr::filter(new_tita_2, Gender == "Male")
  
  return(filtered_data)
}

male_data <- filter_male(new_tita_2)

##arrange by gender
#Let's 'arrange' our data by gender (not the data you just filtered)
#TASK: Write the function to group the data by gender (hint: arrange()

arrange_by_gender <- function(new_tita_2) {

  arranged_data <- dplyr::arrange(new_tita_2, Gender)

  return(arranged_data)
}

arranged_data <- arrange_by_gender(new_tita_2)

##sum the freq column##
#Let's see how many people were examined in the dataset (total the frequency in the original dataframe)
#TASK: Sum the Freq column
#TASK: After you run it, write the total here:2201

sum_freq <- function(new_tita_2)
  {
    total_freq <- sum(new_tita_2$Freq)
    return(total_freq)
}

total_frequency <- sum(new_tita_2$Freq)
print(total_frequency)

#total frequency=2201

#filter
#Since we have a males dataset, let's make a females dataset
#TASK: Write the function that includes only rows that are 'female'


female_data <- function(new_tita_2) {
  filtered_data <- dplyr::filter(new_tita_2, Gender == "Female")
  
  # Return the filtered dataframe
  return(filtered_data)
}

Femaldata <- female_data(new_tita_2)


#join male and female
#And now let's join the males and females
#TASK: Write the function that joins the male and female rows 
#(hint: try using 'union' or 'bind_rows')

library(dplyr)

join_male_female <- function(male_data, Femaldata) {
  
  joined_data <- bind_rows(male_data, Femaldata)

  return(joined_data)
}

joined_dataset <- join_male_female(male_data, Femaldata)

