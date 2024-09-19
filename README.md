# fctutils: Advanced Factor Manipulation Utilities for R
The fctutils package provides a collection of utility functions for manipulating and analyzing factor vectors in R. It offers tools for filtering, splitting, combining, and reordering factor levels based on various criteria. The package is designed to simplify common tasks in data preprocessing and categorical data analysis, making it easier to work with factors in a flexible and efficient manner.

Key Features:

## Factor Level Manipulation: Functions to remove, replace, and lump factor levels.
_Advanced Sorting: Sort factor levels based on character frequencies, positions, patterns, or associated vectors.
_Splitting and Reordering: Split factor levels using patterns or positions and reorder based on extracted parts.
_Grouping: Group factor levels by prefixes or within other factor groups.
_Combining Factors: Functions to intersect or unite multiple factor vectors.

## Introduction
Factors are a fundamental data type in R, used to represent categorical data. Managing and manipulating factor levels efficiently is crucial in data analysis workflows. The fctutils package provides a suite of functions to simplify these tasks, allowing for more expressive and concise code.

### Installation
```
devtools::install_github("guokai8/fctutils")
```
#### Usage Examples
##### 1. Splitting and Reordering Factor Levels
The fct_split function allows you to split factor levels using specified patterns and reorder them based on extracted parts.
```
library(fctutils) 
# Example factor vector
factor_vec <- factor(c('item1-sub1', 'item2_sub2', 'item3|sub3', 'item1-sub4'))

# Split by multiple patterns and reorder based on the first part
factor_vec <- fct_split(factor_vec, split_pattern = c('-', '_', '\\|'), part = 1)

# View reordered levels
levels(factor_vec)
```
##### 2. Combining Factors  
Use fct_union and fct_intersect to combine multiple factor vectors.
```
# Factor vectors
factor_vec1 <- factor(c('apple', 'banana'))
factor_vec2 <- factor(c('banana', 'cherry'))

# Union of factor levels
union_factor <- fct_union(factor_vec1, factor_vec2)
levels(union_factor)

# Intersection of factor levels
intersect_factor <- fct_intersect(factor_vec1, factor_vec2)
levels(intersect_factor)
```
##### 3. Grouping and Reordering
Group factor levels by a common prefix and reorder within groups.
```
# Factor vector
factor_vec <- factor(c('apple_red', 'apple_green', 'banana_yellow', 'banana_green', 'cherry_red'))

# Group by prefix
grouped_factor <- fct_group_by_prefix(factor_vec, prefix_length = 5)
levels(grouped_factor)

# Reorder within groups
data <- data.frame(
  item = grouped_factor,
  group = factor(c('Group1', 'Group1', 'Group2', 'Group2', 'Group3')),
  value = c(10, 20, 15, 25, 5)
)
data$item <- fct_reorder_within(data$item, data$group, data$value)
```

##### 4. Lump Rare Levels
Lump together all but the most frequent levels using fct_lump_n.
```
# Factor vector
factor_vec <- factor(c('apple', 'banana', 'cherry', 'date', 'fig', 'grape', 'apple', 'banana', 'apple'))

# Lump rare levels
lumped_factor <- fct_lump_n(factor_vec, n = 2)
levels(lumped_factor)
```
### Conclusion
The fctutils package provides versatile functions to handle various factor manipulation needs. By incorporating these tools into your workflow, you can streamline the preprocessing and analysis of categorical data in R.

