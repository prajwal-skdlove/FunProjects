# Function to help solve wordle
function_wordle <- function(n,e,is,ns){
  library(data.table)
  library(qdapDictionaries)
  
  # Create a list of n letter word to figure out
  prm = rbind(data.table(DICTIONARY)[,1], data.table(word = GradyAugmented))
  prm = prm[nchar(word) == n]
  prm = unique(prm, by = "word")
  
  # Exclude letters not in the word
  ptrn_excl = paste0(e, collapse="|")
  prm = prm[!grepl(ptrn_excl, word, perl = TRUE)]
  
  # Keep words with letters already guessed in only the right spot
  is1 = is[!(is.na(is))]
  if (length(is1) > 0){
    m = match(is1, is)
    for (j in m){
      ptrn = paste0("^.{", j - 1,"}[",is[j],"]", collapse="")
      prm = prm[grepl(ptrn, word, perl=TRUE)]
  
      # Cant do this as repeated words in the spot 
      # would not always be be figured out
      # for (z in seq(1:5)[!(seq(1:5) %in% m)]){
      #   ptrnz = paste0("^.{", z - 1,"}[",is[j],"]", collapse="")
      #   prm = prm[!(grepl(ptrnz, word, perl=TRUE))]
      # }
    }
  }


  # keep words with letters already guessed but not at the right spot
  # eliminate words with letter on the wrong spot
  ns1 = ns[!is.na(ns)]
  if (length(ns1) > 0){
    ptrn2 = paste0("(?=.*",ns1,")", collapse="")
    prm = prm[grepl(ptrn2, word, perl = TRUE)]  #keep only words with letters
    
    m1 = match(ns1, ns)
    for (k in m1){
      ptrn3 = paste0("^.{", k - 1,"}[",ns[k],"]", collapse="")
      prm = prm[!(grepl(ptrn3, word, perl=TRUE))]
    }
  }

  return(prm)
}


excl = c("r","a", "i", "n", "l", "p") #letters that aren't in
in_seq = c(NA,NA,"o",NA, "e") #letters in the right spot
in_notseq = c("s", NA,NA, NA, NA) #letter not in the right spot
no = 5
wordle_list = function_wordle(no, excl, in_seq, in_notseq)

