# Basically a copy of Wordle. Picks a word and guesses it at random
# Uses the wordle helper function


# Function to help solve wordle
function_wordle <- function(df,e,is,ns){

  
  # Exclude letters not in the word
  if (length(e) > 0) {
    ptrn_excl = paste0(e, collapse="|")
    df = df[!grepl(ptrn_excl, word, perl = TRUE)]}
  
  # Keep words with letters already guessed in only the right spot
  is1 = is[!(is.na(is))]
  if (length(is1) > 0){
    m = match(is1, is)
    for (j in m){
      ptrn = paste0("^.{", j - 1,"}[",is[j],"]", collapse="")
      df = df[grepl(ptrn, word, perl=TRUE)]
  
      # Cant do this as repeated words in the spot 
      # would not always be be figured out
      # for (z in seq(1:5)[!(seq(1:5) %in% m)]){
      #   ptrnz = paste0("^.{", z - 1,"}[",is[j],"]", collapse="")
      #   df = df[!(grepl(ptrnz, word, perl=TRUE))]
      # }
    }
  }


  # keep words with letters already guessed but not at the right spot
  # eliminate words with letter on the wrong spot
  ns1 = ns[!is.na(ns)]
  if (length(ns1) > 0){
    ptrn2 = paste0("(?=.*",ns1,")", collapse="")
    df = df[grepl(ptrn2, word, perl = TRUE)]  #keep only words with letters
    
    m1 = match(ns1, ns)
    for (k in m1){
      ptrn3 = paste0("^.{", k - 1,"}[",ns[k],"]", collapse="")
      df = df[!(grepl(ptrn3, word, perl=TRUE))]
    }
  }

  return(df)
}


word_guess = function(wl){
  word_of_the_day = unlist(wl[sample(nrow(wl),1)]) #word to guess
    
  # Set a counter and make the first attempt
  # Guess at random
  counter = 1
  attempt = unlist(wl[sample(nrow(wl),1)])
  attempted_words = c()
   
  # Set some variable to keep guessing the word until you get it right
  # Utilize the wordle_function created
  n = nchar(word_of_the_day)
  in_seq = rep(NA, n)
  in_notseq = rep(NA,n)
  excl = c()
  
  # Keep trying until you gess the word
  while (word_of_the_day != attempt){
    # print(paste(paste0("Attempt ", counter), attempt, sep = " = "))
    
    # For the word guessed, check whether there are any matched words
    # Either in the right spot or the wrong spot
    # Generate words to be excluded and words in seq and not in seq

    for (k in 1:n){
     
      subst_word = substr(word_of_the_day,k,k)
      substr_attempt = substr(attempt,k,k)
      atmpt_wrd_ptrn = paste0("(?=.*",substr_attempt,")", collapse="")
      
      if (subst_word == substr_attempt){
        in_seq[k] = substr_attempt
      } else if (subst_word != substr_attempt & 
          grepl(atmpt_wrd_ptrn, word_of_the_day, perl = TRUE)){
            in_notseq[k] = substr_attempt
          } else {
            excl = c(excl, substr_attempt)
          }
        
    }

    # Run the World function to eliminate words
    wl = function_wordle(wl, excl, in_seq, in_notseq)
    
    # Keep a lit of attempted words
    attempted_words = c(attempted_words, attempt)
    wl = wl[!(wl$word %in% attempted_words),]
    
    # Try another attempt
    attempt = unlist(wl[sample(nrow(wl),1)])
    counter = counter + 1
  }
  
   print("SUCCESS!")
   print(paste0("Attempt ", counter))
   print(paste0("Word of the Day is ", attempt))
   
   return(counter)
  #return(list(counter, attempted_words))
}


library(data.table)
library(qdapDictionaries)


# Get a list of acceptable words
# For this grab a dictionary for all 5 letter words
n = 5
prm = rbind(data.table(DICTIONARY)[,1], data.table(word = GradyAugmented))
prm = prm[nchar(word) == n]
prm = unique(prm, by = "word")


word_guess(prm)