# Homework 1
# Chayton Rhodes


# Problem 1

# Setup code
student_id <- c("S01", "S02", "S03", "S04", "S05", "S06")
section <- c("A", "B", "A", "B", "A", "B")
quiz1 <- c(82, 91, 76, 88, 95, 69)
quiz2 <- c(85, 89, 80, 92, 94, 74)
passed <- c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE)


# Part A

section <- factor(section, levels = c("A", "B"))

students <- data.frame(
  student_id = student_id,
  section = section,
  quiz1 = quiz1,
  quiz2 = quiz2,
  passed = passed
)

score_matrix <- matrix(
  c(quiz1, quiz2),
  nrow = length(student_id),
  ncol = 2,
  dimnames = list(student_id, c("quiz1", "quiz2"))
)

course_record <- list(
  course = "R Programming",
  scores = students,
  cutoffs = c(pass = 70, excellent = 90)
)

# Inspect the objects
typeof(section)
class(section)
length(section)
str(section)

typeof(students)
class(students)
length(students)
str(students)
dim(students)

typeof(score_matrix)
class(score_matrix)
length(score_matrix)
str(score_matrix)
dim(score_matrix)

typeof(course_record)
class(course_record)
length(course_record)
str(course_record)

# A vector contains elements of one atomic type
# A list can contain objects of different types and structures
# A matrix is a two-dimensional atomic structure, while a factor represents categorical data using levels
# A data frame is a rectangular table whose columns can have different data types


# Part B

# S04's second quiz score
score_matrix["S04", "quiz2"]

# First two rows, preserving matrix dimensions
score_matrix[1:2, , drop = FALSE]

# Extract course three different ways
course_record["course"]
course_record[["course"]]
course_record$course

# [ returns a subset while preserving the container structure when appropriate
# [[ extracts one individual element from a list-like object
# $ extracts a named element by name from a list or data frame


# Part C

students$average <- rowMeans(students[, c("quiz1", "quiz2")])

students$excellent <- students$average >= 90

section_A_80 <- students[
  students$section == "A" & students$average >= 80,
  c("student_id", "section", "average")
]

section_A_80

student_averages <- students$average
names(student_averages) <- students$student_id
student_averages

# These calculations are vectorized because they operate on whole vectors or rows at once rather than processing one observation at a time with a loop


# Problem 2

# Setup code
csv_text <- "sample_id,site,temp_c,ph,status
M01,North,18.2,7.1,ok
M02,South,20.5,,ok
M03,North,NA,6.8,review
M04,East,22.1,7.4,ok
M05,South,19.7,7.0,review
M06,East,23.0,NA,ok
M07,North,17.8,6.9,ok
M08,South,21.2,7.2,ok"


# Part A

measurements <- read.csv(
  text = csv_text,
  na.strings = c("", "NA")
)

head(measurements)
str(measurements)
dim(measurements)
names(measurements)

# Missing values in each column
sum(is.na(measurements$sample_id))
sum(is.na(measurements$site))
sum(is.na(measurements$temp_c))
sum(is.na(measurements$ph))
sum(is.na(measurements$status))

# Keep only complete rows
measurements_complete <- measurements[complete.cases(measurements), ]
measurements_complete

# Report sample IDs removed by the complete-case filter
measurements$sample_id[!complete.cases(measurements)]

# x == NA is not a valid missing-value test because comparisons with NA return NA not TRUE or FALSE
# Missing values should be tested with is.na()


# Part B

measurements$site <- factor(measurements$site)
measurements$status <- factor(measurements$status)

levels(measurements$site)
levels(measurements$status)

measurements$temp_f <- measurements$temp_c * 9 / 5 + 32

measurements$ph_below_7 <- measurements$ph < 7

selected_measurements <- measurements[
  complete.cases(measurements) &
    measurements$site %in% c("North", "South") &
    measurements$status == "ok",
  c("sample_id", "site", "temp_c", "temp_f", "ph")
]

selected_measurements

overall_mean_temp <- mean(measurements$temp_c, na.rm = TRUE)
overall_mean_temp

south_mean_temp <- mean(
  measurements$temp_c[measurements$site == "South"],
  na.rm = TRUE
)
south_mean_temp


# Part C

A <- matrix(1:4, nrow = 2)
B <- matrix(5:8, nrow = 2)

elementwise_result <- A * B
matrix_result <- A %*% B

elementwise_result
matrix_result

dim(elementwise_result)
dim(matrix_result)

# A * B multiplies corresponding matrix entries element by element
# A %*% B performs matrix multiplication using row-by-column dot products


# Problem 3

# Setup code
student_id <- paste0("P", sprintf("%02d", 1:8))
scores <- c(95, 82, NA, 67, 74, 88, 59, 91)


# Part A

grade_one <- function(
    score,
    a_min = 90,
    b_min = 80,
    c_min = 70,
    d_min = 60
) {
  if (is.na(score)) {
    return(NA_character_)
  } else if (score >= a_min) {
    return("A")
  } else if (score >= b_min) {
    return("B")
  } else if (score >= c_min) {
    return("C")
  } else if (score >= d_min) {
    return("D")
  } else {
    return("F")
  }
}

grade_one(NA)
grade_one(90)
grade_one(80)
grade_one(85)
grade_one(74)


# Part B

grades <- rep(NA_character_, length(scores))

for (i in seq_along(scores)) {
  grades[i] <- grade_one(scores[i])
}

names(grades) <- student_id
grades

# During the loop i represents the current position/index in the scores vector


# Part C-1

summarize_scores <- function(x, na.rm = TRUE, digits = 1) {
  total_count <- length(x)
  missing_count <- sum(is.na(x))
  
  stats <- c(
    mean = mean(x, na.rm = na.rm),
    sd = sd(x, na.rm = na.rm),
    min = min(x, na.rm = na.rm),
    max = max(x, na.rm = na.rm)
  )
  
  stats <- round(stats, digits = digits)
  
  c(
    total = total_count,
    missing = missing_count,
    stats
  )
}

# Call with defaults
summarize_scores(scores)

# Call with named arguments
summarize_scores(x = scores, na.rm = TRUE, digits = 2)



# Part C-2

plot_scores <- function(x, ...) {
  plot(seq_along(x), x, ...)
}

plot_scores(
  scores,
  type = "b",
  pch = 19,
  xlab = "Position",
  ylab = "Score",
  main = "Student Scores"
)

# The ... argument passes additional plotting arguments from plot_scores() to plot()

