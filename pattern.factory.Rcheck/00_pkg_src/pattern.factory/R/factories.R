# frequently used pattern
library(magrittr)
library(stringr)
create_number_pattern <- function(
  is_integer = FALSE,
  is_signed = TRUE,
  allow_scientific_notation = TRUE,
  allow_leading_zero = TRUE,
  allow_omit_whole_zero = TRUE
){

  # Generate a number pattern for matching
  #
  # Args:
  # is_integer: boolean. only match the whole number part
  # is_signed: boolean. include the sign '+' or '-'
  # allow_scientific_notation: boolean. support the format like 1.23e-21
  # allow_leading_zero:boolean. allow the number begin with one or multiple zero
  # allow_omit_whole_zero: boolean. allow real number which their absolute value
  #   is less than 1 omits their whole number part
  # Returns:
  # the result pattern

  # check sign
  if(is_signed){
    sign_pattern <- "[+-]?"
  }else{
    sign_pattern <- ""
  }
  # check leading zero
  if(allow_leading_zero){
    leading_zero_pattern = "0*"
  }else{
    leading_zero_pattern = ""
  }
  # check integer
  if(is_integer){
    whole_pattern = "[0-9]+"
    decimal_pattern = ""
  }else{
    # check omit zero
    if(allow_omit_whole_zero){
      whole_pattern = "[0-9]*"
    }else{
      whole_pattern = "[0-9]+"
    }
    decimal_pattern = "[\\.]?[0-9]*"
  }
  # check scientific notation
  if(allow_scientific_notation){
    scientific_notation_pattern = "[eE]?-?[0-9]*"
  }else{
    scientific_notation_pattern = ""
  }
  # connect parts
  paste(
    sign_pattern,
    leading_zero_pattern,
    whole_pattern,
    decimal_pattern,
    scientific_notation_pattern,
    sep = "")
}

create_word_pattern <- function(
  allow_space = TRUE,
  allow_leading_space = TRUE,
  allow_end_space =TRUE,
  allow_number = TRUE,
  allow_letter = TRUE,
  first_letter_upcase = FALSE,
  allowed_symbols = "",
  min_length = 0,
  max_length = 0
){
  # Generate a string pattern for matching
  #
  # Args:
  # allow_space: string can contain space, has higher priority over other space related setting
  # allow_leading_space: string can have leading space
  # allow_end_space: string can have end space
  # allow_number: string can have number
  # allow_letter: string can have letter
  # first_letter_upcase: the first (non-space, if leading space is enable) letter has to be upcase
  #   higher priority over allow letter
  # allowed_symbols: extra symbols allowed to be appeared inside string goes here, like "+@#"
  # min_length: the minimal length of the string, 0, if no limit
  # max_length: the maximum length of the string, 0, if no limit or less than min_length
  # Returns:
  # the result pattern
  if(allow_letter){
    body_set <- "A-Za-z"
  }else{
    body_set <- ""
  }

  if(allow_number){
    body_set <- body_set %>% paste(., "0-9", sep = "")
  }

  body_set <- body_set %>% paste(., allowed_symbols, sep = "")

  if(first_letter_upcase){
    first_letter_pattern <- "[A-Z]"
  }else{
    first_letter_pattern <- body_set %>% paste("[", ., "]", sep = "")
  }

  if(allow_space){
    body_set <- body_set %>% paste(., " ", sep = "")
  }

  if(max_length <= 0 || max_length < min_length){
    body_pattern <- body_set %>% paste("[", ., "]*", sep = "")
  }else if(max_length == 1){
    body_pattern <- ""
  }else if(min_length <= 1){
    body_pattern <- body_set %>% paste("[", ., "]{0,", max_length - 1, "}", sep = "")
  }else{
    body_pattern <- body_set %>% paste("[", ., "]{", min_length - 1, ",", max_length - 1, "}", sep = "")
  }

  if(allow_leading_space){
    leading_space_pattern = "[ ]*"
  }else{
    leading_space_pattern = "[ ]{0}"
  }

  if(allow_end_space){
    end_space_pattern = "[ ]*"
  }else{
    end_space_pattern = "[ ]{0}"
  }

  paste(leading_space_pattern,
        first_letter_pattern,
        body_pattern,
        end_space_pattern,
        sep = ""
  )
}

seal_pattern <- function(pattern){

  # add begin and end sign(^ and $) to the pattern
  #
  # Args:
  # pattern: the target pattern
  # Returns:
  # the result pattern

  paste(
    "^",
    pattern,
    "$",
    sep = ""
  )
}

box_pattern <- function(pattern, bound = "", left_bound = "\"", right_bound = "\""){
  # add parenthesis or other boundaries to the pattern
  #
  # Args:
  # bound: boundary symbol for both side.
  #  if the bound is set, it will overwrite  left_bound and right_bound
  # left_bound: the boundary symbol of left side
  # right_bound: the boundary symbol of right side
  # Returns:
  # the result pattern
  if(bound != ""){
    left_bound <- bound
    right_bound <- bound
  }
  paste(
    left_bound,
    pattern,
    right_bound,
    sep = ""
  )
}

connect_pattern <- function(patterns, connector = '-'){
  # connect a series of pattern with connector
  #
  # Args:
  # patterns: a vector of patterns
  # connector: the symbol of connector
  # Returns:
  # the result pattern
  result <- ""
  for(pattern in patterns){
    result <- paste(result, pattern, sep = connector)
  }
  substring(result, 2)
}



# usage example

# my_pattern<- create_number_pattern(
#   is_integer = FALSE,
#   is_signed = FALSE,
#   allow_scientific_notation = FALSE,
#   allow_leading_zero = FALSE,
#   allow_omit_whole_zero = FALSE
#   ) %>%
#   seal_pattern() %>%
#   print
#
# print("no sign whole number")
# grepl(pattern = my_pattern, "1") %T>% print
# print("signed number")
# grepl(pattern = my_pattern, "-1") %T>% print
# grepl(pattern = my_pattern, "+1") %T>% print
# print("leading 0")
# grepl(pattern = my_pattern, "01") %T>% print
# print("decimal and whole number")
# grepl(pattern = my_pattern, "1.1") %T>% print
# print("decimal without whole number")
# grepl(pattern = my_pattern, ".2") %T>% print
# print("scientific notation")
# grepl(pattern = my_pattern, "1e12") %T>% print
# grepl(pattern = my_pattern, "1e-12") %T>% print

# my_pattern <- create_word_pattern(
#   allow_leading_space = TRUE,
#   allow_end_space = TRUE,
#   allow_space = FALSE,
#   allow_number = TRUE,
#   first_letter_upcase = TRUE
# ) %>%
#   seal_pattern() %>%
#   print
#
# print("plain text")
# grepl(pattern = my_pattern, "Hello") %T>% print
# print("plain text with leading space")
# grepl(pattern = my_pattern, "  Hello") %T>% print
# print("plain text with end space")
# grepl(pattern = my_pattern, "Hello ") %T>% print
# print("plain text with inside space")
# grepl(pattern = my_pattern, "Hello  world") %T>% print
# print("plain text with number")
# grepl(pattern = my_pattern, "Hello2018") %T>% print
# print("start with lowercase letter")
# grepl(pattern = my_pattern, "hello") %T>% print
#
#
# my_pattern <- create_word_pattern() %>%
#   box_pattern(., bound = "'") %>%
#   box_pattern(., left_bound = '(', right_bound = ')') %>%
#   # seal_pattern() %>%
#   print
# print("simple boxing")
# str_match_all("'''d''s'", my_pattern) %>% print

# net_digits <- create_word_pattern(allow_number = TRUE,
#                     allow_space = FALSE,
#                     allow_leading_space = FALSE,
#                     allow_end_space = FALSE,
#                     allow_letter = FALSE,
#                     min_length = 3,
#                     max_length = 3
# )
# hlr_digits <- create_word_pattern(allow_number = TRUE,
#                                   allow_space = FALSE,
#                                   allow_leading_space = FALSE,
#                                   allow_end_space = FALSE,
#                                   allow_letter = FALSE,
#                                   min_length = 4,
#                                   max_length = 4
# )
#
# personal_digits <- create_word_pattern(allow_number = TRUE,
#                                   allow_space = FALSE,
#                                   allow_leading_space = FALSE,
#                                   allow_end_space = FALSE,
#                                   allow_letter = FALSE,
#                                   min_length = 4,
#                                   max_length = 4
# )
# tele_pattern <- connect_pattern(c(net_digits, hlr_digits, personal_digits)) %>%
#   seal_pattern()
# print("standard cell phone format")
# grepl(tele_pattern, "139-4418-6354") %>% print
# print("mess up with letter")
# grepl(tele_pattern, "1a9-4418-6354") %>% print
# print("mess up with length")
# grepl(tele_pattern, "139-4418-63546") %>% print
# grepl(tele_pattern, "139-481-6354") %>% print
