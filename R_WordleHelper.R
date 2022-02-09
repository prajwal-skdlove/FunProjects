library(data.table)
library(qdapDictionaries)


# Get a list of acceptable words
# For this grab a dictionary for all 5 letter words
n = 5
prm = rbind(data.table(DICTIONARY)[,1], data.table(word = GradyAugmented))
prm = prm[nchar(word) == n]
prm = unique(prm, by = "word")


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


excl = c("r","i", "n", "e", "u", "d") #letters that aren't in
in_seq = c("a","l","o",NA, NA) #letters in the right spot
in_notseq = c(NA, "a",NA, NA, NA) #letter not in the right spot
wordle_list = function_wordle(prm, excl, in_seq, in_notseq)

