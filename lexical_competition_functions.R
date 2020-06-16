# Functions for the lexical competition model described in
# Karjus et al 2020, "Communicative need modulates competition in language change"
# Includes functions for the topical advection model from
# Karjus et al 2020, "Quantifying the dynamics of topical fluctuations in language"
# Code: Andres Karjus, https://andreskarjus.github.io/

# These packages are required:
library(text2vec)
library(compiler)
library(grr)
library(parallel)
library(Matrix)
library(tidyverse)
library(fastmatch)
#library(ggbeeswarm)
library(ggrepel)
#library(shadowtext)
#library(plotly)
library(patchwork)
library(scales)
library(colorspace)
#library(Rtsne)
library(rms)
library(stringdist)
#library(party)
library(stringr)
library(tibble)
library(data.table)
library(MASS)
library(caret)
#library(igraph)
#library(car)


year=function(x){seq(1810,2009,1)[x]}

yearger=function(x){seq(1800,1919,1)[x]}


coha_forpos = function(filepath,w, minlen=3, 
                       fixnums = T,
                       fixhyphens = T ){
  
  ## load data
  words = read.csv(filepath, sep="\t", quote="", header=F, skipNul=T, 
                   stringsAsFactors=F, fileEncoding="latin1")
  words = words[which(grepl("^nn|^vv|^j|^r", words[,3])),]  
  
  if(fixhyphens){
    words[,2] = gsub("-", "", words[,2])
  }
  if(fixnums){  # replace all numbers with generic 000; numbers separated by ., considered single number
    words[,2] = gsub("[0-9]+[0-9.,]*", "000",  words[,2])
  } # importantly, numbers like 000-000 becomes one single 000 if hyphens are removed too
  words = words[ which(words[,2] %fin% w), ]
  words = words[,2:3]
  return(words)
}

coha_posfinder = function(files,w){
  library(fastmatch)
  wlist = vector("list", length(files))
  for(f in seq_along(files)){
    try({
      wtmp = coha_forpos(files[f],w)
      wlist[[f]] = wtmp
      if(f %in% c(seq(1000,20000,1000))){print(paste(Sys.time(), f) )}})
  }
  wframe = do.call(rbind, wlist)
  wframe[,2] = gsub("^(.).*" ,"\\1", wframe[,2])
  wframe = table(wframe[,1], wframe[,2])
  pos=colnames(wframe)
  wtops = apply(wframe, 1, function(x) pos[which.max(x)]  )
  return(wtops)
}




coha_csv = function(filepath, minlen=3, 
                    markS=F,rmstopwords=T,removenp,
                    stopliteral=T,
                    fixnums = F,
                    fixhyphens = T,
                    docborders= "^##[0-9]|@@[0-9]|^@",
                    untouchables = NULL,
                    domultiword=F,
                    phrases=NULL
                    
){
  # docborders:
  # "^##[0-9]|@@[0-9]|^@|<p>"
  # ##=coca, @@=coha  
  # |^@$ = censorship holes, leave out to not split docs. But bad if need to filter single-document words
  # <p> paragraph tag appears in more recent coha, might as well make use of it. or not, to keep consistent?
  # if <p> left in, should be filtered by minlen and/or stopwords
  # coca spoken: @!xxx speaker tags; now @ counts; if not: their 2nd column (lemma) is zero-length, minlen would filter out
  
  
  # define grep to match stopword pos tags:
  stopgrep = "^ge|^a|^c|^d|^ex$|^i|^r[egpa]|^rrq|^mc|^to|^xx|^z|^y|null|\"|^nnu|np|\\.\\.\\.|!|^p|^v[^v]|^u"
  # Note: all others rely on the primary tag, except for NP proper noun which is matched in subsequent
  # tags as well (in case of multitag like nn1_np1), since it's such a common error in coha.
  # Note that the tag for some punctuation is the actual punctuation itself: ! ...
  
  # define list of additional stopwords - in case they escape the pos filter (e.g. the weird hyphenated ones that will have hyphens filtered down the line) and the max length filter (current default 3)
  stopliterals = unique( c("about","above","after","again","against","because","been","before","being","below","between","both","could","does","doing","down","each","from","further","have","having","he'd","he'll","he's","here","here's","hers","how's","i'll","i've","into","it's","let's","more","most","once","only","other","ought","ours","over","same","she'd","she'll","she's","should","some","such","than","that","that's","their","theirs","them","then","there","there's","these","they","they'd","they'll","they're","they've","this","those","through","under","until","very","we'd","we'll","we're","we've","were","what","what's","when","when's","where","where's","which","while","who's","whom","why's","with","would","you'd","you'll","you're","you've","your","yours","don't","ain't","can't","the", "but", 
                           "i", "be", "is", "was", "we", "me", "his", "her", "you", "our", "us", "my", "she", "he", "they", "or", "of", "to", "are", "has","all", "and", "did", "it", "for", "any", "who", "so", "do","by", "thi", "go", "got", "get", "also", "maybe", "no", "yes", "if", "how", "as", "at", "didn't",
                           "i", "me", "my", "myself", "we", "our", "ours", "ourselves", "you", "your", "yours", "yourself", "yourselves", "he", "him", "his", "himself", "she", "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", "theirs", "themselves", "what", "which", "who", "whom", "this", "that", "these", "those", "am", "is", "are", "was", "were", "be", "been", "being", "have", "has", "had", "having", "do", "does", "did", "doing", "would", "should", "could", "ought", "i'm", "you're", "he's", "she's", "it's", "we're", "they're", "i've", "you've", "we've", "they've", "i'd", "you'd", "he'd", "she'd", "we'd", "they'd", "i'll", "you'll", "he'll", "she'll", "we'll", "they'll", "isn't", "aren't", "wasn't", "weren't", "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't", "won't", "wouldn't", "shan't", "shouldn't", "can't", "cannot", "couldn't", "mustn't", "let's", "that's", "who's", "what's", "here's", "there's", "when's", "where's", "why's", "how's", "a", "an", "the", "and", "but", "if", "or", "because", "as", "until", "while", "of", "at", "by", "for", "with", "about", "against", "between", "into", "through", "during", "before", "after", "above", "below", "to", "from", "up", "down", "in", "out", "on", "off", "over", "under", "again", "further", "then", "once", "here", "there", "when", "where", "why", "how", "all", "any", "both", "each", "few", "more", "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", "so", "than", "too", "very", "will"
  ) 
  )
  
  
  # for compatability with earlier run-scripts, add missing prefix parameters
  fixmarks = setdiff( c("N","V","A","D","M","P"), names(markS))
  if(length(fixmarks)>0){
    tmp = as.list(rep(F, length(fixmarks))); names(tmp) = fixmarks
    markS = c(markS, tmp)
  }
  
  ## load data
  words = read.csv(filepath, sep="\t", quote="", header=F, skipNul=T, 
                   stringsAsFactors=F, fileEncoding="latin1")
  #print(head(words))
  doctags = grep(docborders, words[,1])
  words[doctags, 2] = "<doc>" 
  words[doctags, 3] = "<doc>"  # to avoid filters on borders (some tagged as mc for some reason in corpus)
  # NB: COHA lemma column is always lowercase!
  
  
  stopwords=NULL; stopwords2=NULL; nps=NULL;badocr=NULL; rmnums=NULL
  
  if(rmstopwords){
    stopwords = grep(stopgrep, words[,3]) 
    # "^ge|^a|^c|^d|^ex$|^i|^r[egpa]|^rrq|^mc|^to$|^xx$|^z|^y$|null|\"|^nnu|\\.\\.\\.|^p|^v[^v]"
    # "^ge|^a|^c|^d|^ex$|^i|^r[egpa]|^rrq|^mc|^to$|^xx$|^z|^y$|null|\"|^nnu|^np[12]*|\\.\\.\\.|^p|^v[^v]" # current, np as stopwords
    # coha <p> tag is null, " tag is actual ", ! tag is !
    # ... tag is literally ...
    # nnu is units of measurement eg $1000 but sometimes lemma field empty
    # fo is formula, mostly bad ocr eg &+++ BUT ALSO doc borders, which are needed
    # z is -- dash, zz is letters (for some reason)
    # u = interjection
  }
  
  if(removenp){
    #nps = grep("^np", words[,3]) # also under stopwords
    nps = grep("^[A-Z]", words[,1])   # all capitalized considered np, hard filter.----
  }
  
  
  badocr = grep(
    "^doesn|weren[t]*$|^could[nta]*$|^should[nta]*$|^isn[t]*$|^would[nta]*$|^aren[t]*$|^hadn[t]*$|^wasn[t]*$|^hasn[t]*$|^didn[t]*$|^befor$|^dieir|^diat|^diose|^diis|^diere|^diese|^dius|\\+|[%;©®™@.*/#=$]|^p[0-9]+|^[^a-z]*$|^[01][a-z]+|^ther$|^-|-$", 
    words[,2] )
  # ^- -$ --  broken compounds, but maybe still keep? especially now if removing hyphens
  #  ---- , &+++++ ; p[0-9]+ are page numbers
  # doesn weren aren  ->  't short forms! but sometimes tagged as nouns...
  # ^[^a-z]*$ - to catch if no (English) letters present but still tagged as content word class
  # ^[0-9][a-z]+ - mostly bad ocr, single number instead of capital (O -> 0)
  # |^[[:punct:]]|[[:punct:]]$ would remove words starting with any punctuation, 
  # maybe just remove ^-|-$ which is mostly broken hypenation (eg gets "ing" as word), an then remove rest of punctuation below
  # new, for COCA: remove words ending with .!? (unsegmented punctuation)
  
  alldocs = words[,2]  # keep only lemmas after this
  ## add POS prefixes is applicable
  # (need to do this before removing stopwords and such so indices match)
  
  # match for the literal stopwords once before adding tags
  if(stopliteral){
    stopwords21 = which(alldocs %fin% stopliterals) # fastmatch
  }
  
  if(any(unlist(markS))){
    # problem in coha/coca: double tags ( _ ) and @-signs...!
    if(markS$N){
      ispos = grep("^nn([^u]|$)", words[,3])
      alldocs[ispos] = paste0("N:", alldocs[ispos])
    }
    if(markS$V){
      ispos = grep("^vv", words[,3])
      alldocs[ispos] = paste0("V:", alldocs[ispos])
    }
    if(markS$A){
      ispos = grep("^j", words[,3])
      alldocs[ispos] = paste0("A:", alldocs[ispos])
    }
    if(markS$D){
      ispos = grep("^d", words[,3])
      alldocs[ispos] = paste0("D:", alldocs[ispos])
    }
    if(markS$M){
      ispos = grep("^mc([^gm]|$)", words[,3])
      alldocs[ispos] = paste0("M:", alldocs[ispos])
    }
    if(markS$P){  # proper nouns   # see stopwords above
      ispos = grep("^np", words[,3])
      alldocs[ispos] = paste0("P:", alldocs[ispos])
    }
  }
  rm(words) # no need for the full csv anymore, just lemmas
  
  ### only these following two modify the actual words:
  # fix hyphens and nums here before removing, to be able to do newwords-filtering
  if(fixhyphens){
    # now also cleans other punctuation that sometimes is lemma field
    # but words ending/starting with "-" get filtered out regardless by badocr list
    oktogsub = setdiff(seq_along(alldocs), doctags)
    alldocs[oktogsub] = gsub("[[:punct:] ]", "", alldocs[oktogsub]) # also erronous whitespaces
    # notably, <doc> tags contain punctuation
  }
  if(fixnums){  # replace all numbers with generic 0; numbers separated by ., considered single number
    alldocs = gsub("[0-9]+[0-9.,]*", "000", alldocs)
    # importantly, numbers like 1-2 becomes one single 000 if hyphens are removed too
  } else {
    rmnums = grep("[0-9]", alldocs)  
    # if no fixing then add all number-containing words to remove list
  } 
  
  ###
  
  
  # match for the literal stopwords here once more (now that the hyphens are gone)
  if(stopliteral){
    stopwords22 = which(alldocs %fin% stopliterals) # fastmatch
  }
  
  # don't touch targets (likely mistagged if on the remove list)
  # and don't touch doc tags!
  # if targets have tags - tags were already applied to current data above
  if(is.null(untouchables)){
    dontremove=doctags
  } else {
    dontremove = unique( c(doctags,          # shouldn't need unique but anyway
                           which(alldocs %fin% untouchables) ) )  # fastmatch
  }
  
  # new: search for shorties here, now that hyphens etc are gone
  shorts = which(nchar(alldocs) < minlen) #
  
  # concatenate remove indices
  toremove = setdiff( c(stopwords, stopwords21, stopwords22, nps, shorts, badocr, rmnums ), 
                      dontremove) 
  # but keep if on the list
  
  # If doing multiword unit concatenation, here's the place to do it
  if(domultiword){
    if(length(toremove) > 0){
      alldocs[ toremove ] = "?"  # can't be an empty string, quanteda as.tokens would lose this
    }
    alldocs = tokens_compound(as.tokens(list(alldocs)), 
                              pattern = phrases, concatenator = "" 
    ) %>% 
      unlist(use.names = F, recursive = F) # compound all matches, then return to character vector form
    alldocs = alldocs[nchar(alldocs)>1]  # last step, purge the toremove list items
    # this is a bit hacky; assumes minlen is at least 2
  } else {
    if(length(toremove) > 0){
      alldocs = alldocs[ -toremove ]
    }
  }
  return(alldocs)
}



# docorpus_parallel > fixcohaperiod > coha_csv
fixcohaperiod = function(period1, 
                         path,
                         minlen=3, 
                         markS, rmstopwords, removenp, saveyear=T, byyear,
                         onlyvocab=F,
                         untouchables=NULL,
                         FOLDER=NULL, periodfolder=NULL,
                         domultiword=F, phrases=NULL
){
  gc()
  print(paste(Sys.time(), period1))
  #Magazine/wlp_mag_1996.txt
  
  if(byyear){
    pfiles = file.path(path,period1) # vector of filenames with path from main folder
  }
  if(!byyear){
    pfiles = list.files(file.path(path,period1), full.names = T)
  }
  
  period = list();
  for(y in pfiles){
    #print(y)
    tryCatch({
      filepath = y #paste0(path, period1,"/", y) # now uses fullnames that has path
      
      alldocs = coha_csv(filepath, minlen=minlen, markS = markS, rmstopwords=rmstopwords, removenp=removenp, untouchables=untouchables, domultiword=domultiword, phrases=phrases) 
      #alldocs = coha_csv("/Users/pplsuser/Desktop/korpused/COHA/wlp/wlp_1920s_bsq/fic_1929_14181.txt", minlen=minlen, markS = markS, rmstopwords=rmstopwords, removenp=removenp) # debug
      if(length(alldocs)>2){
        s = which(alldocs == "<doc>")
        fi = findInterval(seq_along(alldocs), s)
        fi[s] = max(fi)+1
        periodlist2 = split(alldocs, fi)
        if(length(s) > 0) {periodlist2[length(periodlist2)] = NULL} # lause/dokivahede vektor minema
        periodlist2[which(sapply(periodlist2, length) < 2)] = NULL # ühesõnalised laused/dokid (sh @-tükid)
        #names(periodlist2) = rep(y, length(periodlist2)) # rohkem debugi jaoks, võib pärast maha võtta
        period = c(period, periodlist2)
      }
    }, error = function(e) {print(e)})
  }
  
  # rdata filename
  if(byyear){
    #subperiod=gsub( "[^_]*_([0-9]{4}).*","\\1",gsub("[^/]+/(.*)", "\\1", period1[1]) ) # gets year
    #subperiod=yearname
    subperiod=names(period1)[1] # hacky: name is passed as the name attribute of the first filepath
  }
  if(!byyear){
    subperiod = ifelse(saveyear, gsub("[^0-9]","", period1), period1) # year from foldername
  }
  
  if(!onlyvocab){
    tryCatch({
      save(period, file=file.path(FOLDER, periodfolder, paste0(subperiod,".RData" ) )) 
    }, error = function(e) {print(e)})
    #print(Sys.time())
    return(NULL) #  parallel, just saves into folder
  }
  if(onlyvocab){
    vocab = itoken(period, progressbar = FALSE) %>% 
      create_vocabulary(ngram = c(ngram_min = 1L, ngram_max = 1L))
    tmp=vocab$term
    vocab = vocab$term_count; names(vocab) = tmp 
    #try(save(vocab, file=file.path(FOLDER, periodfolder, paste0(subperiod,".RData" ) )))
    return(vocab)
  }  
}


docorpus_parallel = function(theperiods, 
                             nfree=0, # how many cpu cores to leave unused (increase if RAM issues)
                             markS = list(N=F,V=F,A=F,D=F,M=F,P=F), 
                             minlen=3, 
                             path, 
                             saveyear=T,    # use folder decade as filename
                             rmstopwords=T, # remove stopwords?
                             removenp=F,    # remove proper nouns by removing all Capitalized?
                             byyear=F,      # do by year instead of decade; theperiods needs to be list where each element is a vector of year files, with full paths
                             onlyvocab=F, # only collects lexicon frequencies, does not save period files
                             untouchables=NULL, # optional vector of words to bypass filtering
                             FOLDER,
                             periodfolder="cohaperiods",
                             yearnames=NULL,
                             skipperiods=F, # debug
                             domultiword=F, # concatenate multiword units? if so, must provide list
                             phrases=NULL   # phrases must be a quanteda phrase(c("a b", "x y)) type object
){
  require("parallel")
  require("grr")
  require("compiler") # pre-compile functions for faster execution in the cluster:       
  setCompilerOptions("suppressAll" = T)
  fixcohaperiod = cmpfun(fixcohaperiod, options=list(optimize=3))
  coha_csv = cmpfun(coha_csv, options=list(optimize=3))
  try(dir.create(file.path(FOLDER, periodfolder) )) # try to create folder in case doesn't exist
  
  if(!skipperiods){
    # prep cluster
    nc = detectCores() - nfree
    cl = makeCluster(nc)#, outfile=file.path(FOLDER, periodfolder, "log.txt"))
    print(paste(Sys.time(), "start"))
    tryCatch({
      clusterExport(cl, c("fixcohaperiod", "coha_csv", "markS", "minlen", "path", "saveyear","rmstopwords", "removenp", "byyear", "onlyvocab", "FOLDER", "periodfolder", "domultiword", "phrases"),envir = environment()) # exports params and compiled functions to cluster
      clusterEvalQ(cl, c(library(text2vec), library(magrittr), library(fastmatch), library("quanteda") ) ) # exports packages to cluster
      
      tmp = parLapply(cl, theperiods, fixcohaperiod, 
                      markS = markS, minlen=minlen, path=path, saveyear=saveyear, 
                      rmstopwords=rmstopwords, removenp=removenp,byyear=byyear, 
                      onlyvocab=onlyvocab, untouchables=untouchables,
                      FOLDER=FOLDER,periodfolder=periodfolder,
                      domultiword=domultiword, phrases=phrases
      )
    }, error=function(e){print(e)},  finally = stopCluster(cl) )
    
    # if(onlyvocab){   # not functional in this version
    #countmat = fixvocablist(tmp)
    #countmat = countmat[rowSums(countmat)>1, ] # remove hapaxes (single occurrence in entire corpus)
    #print(paste(Sys.time(), "corpus files done"))
    #return(countmat)
    #return(NULL)
    #} else {
    # }
  }
  
  # go with new faster counter function; although maybe not so fast, and takes RAM
  # print(paste(Sys.time(), "corpus done, doing counts"))
  # fast_countmat = function(paths, rm_global_hapaxes=T){
  #   # read and treat year as doc, then dtm ~ word counts matrix over time, useful
  #   reader_years = function(f) {
  #     load(f)
  #     period = unlist(period, use.names = F, recursive = F)  # flatten; otherwise itoken won't work; year==doc for word counting purposes
  #     return( period )
  #   }
  #   notokenizer = function(x){return(list(x))}   # apparently need to cast as list for it to work
  #   
  #   it_files  = ifiles(file_paths=paths, reader = reader_years)
  #   it = itoken(it_files, tokenizer = notokenizer, progressbar = FALSE, xptr=T)
  #   rm(it_files)
  #   v = create_vocabulary(it) %>% 
  #     prune_vocabulary(term_count_min = ifelse(rm_global_hapaxes,2,1)) %>% 
  #     vocab_vectorizer()
  #   countmat = create_dtm(it, v, type = "dgCMatrix") %>% Matrix::t()
  #   return(countmat)
  # }
  # paths = list.files(file.path(FOLDER, periodfolder), full.names = T )
  # freqmat = fast_countmat(paths, rm_global_hapaxes=T)  # rows=words
  # 
  # colnames(freqmat) = stringr::str_extract(paths, "[12][0-9]{3}\\.RData") %>% gsub("\\.RData", "", .) 
  
  freqmat = dotrends(foldr=periodfolder, mincount=1, nfree=nfree, rm_global_hapaxes=T,  FOLDER=FOLDER)
  print(paste(Sys.time(), "corpus and counts done"))
  
  return(freqmat)
}




dotrends = function(foldr, mincount=NULL, smooth0=NULL, nfree=0, rm_global_hapaxes=T, FOLDER=NULL, normd=F, listgiven=F, freqs=NULL){
  files = list.files( file.path(FOLDER, foldr), full.names = F, pattern="RData$")
  if(listgiven){ files = names(freqs) }
  
  print(paste(Sys.time(), "Start counting words"))
  library(parallel)
  nc = detectCores() - nfree
  cl = makeCluster(nc)
  clusterExport(cl, c( "files"),envir = environment())
  clusterEvalQ(cl, c(library(text2vec),library(fastmatch)) )
  tryCatch({
    if(!listgiven){
      freqs = parLapply(cl, files, function(f){
        load(file.path(FOLDER, foldr, f) )
        #period=period[1:2]# DEBUGGING
        it = itoken(period, progressbar = FALSE) # period list from load()
        rm(period); gc() # clean periods - iterator envir now contains all data
        vocab2 <- create_vocabulary(it, ngram = c(ngram_min = 1L, ngram_max = 1L))
        #vocab2 <- prune_vocabulary(vocab2, term_count_min = mincount)
        #docdisp = vocab2$doc_count/attr(vocab2,"document_count"); names(docdisp)=vocab2$term
        
        # now doing normalization in the end!
        # sm=sum(vocab2$term_count)
        freqs=vocab2$term_count
        names(freqs)=vocab2$term 
        return(freqs  )
      })
    }
    gc(verbose=F)
    allwords = unique(unlist(lapply(freqs, function(x) names(x) ),use.names = F ) )
    #print(paste(Sys.time(), "Period counts done"))
    
    clusterExport(cl, c("allwords", "freqs"),envir = environment())
    lexorder = parLapply(cl, 1:length(files), function(x){ 
      return(fmatch(allwords, names(freqs[[x]]) ) )
    })
    
    stopCluster(cl) 
    print(paste(Sys.time(), "Aggregating counts"))
    #clusterExport(cl, c("lexorder"),envir = environment())
    # countmat1 = parSapply(cl, 1:length(files), function(x){
    #   freqvec = freqs[[x]]
    #   tmp = freqvec[lexorder[[x]] ]; tmp[is.na(tmp)] = 0 # if no occurrence, then 0
    #   return(tmp)
    # } )
    countmat1 = matrix(0, nrow=length(allwords), ncol=length(files))
    for(x in 1:length(files)){
      tmp = freqs[[x]][lexorder[[x]]]
      tmp[is.na(tmp)] = 0
      countmat1[,x] = tmp
      if(x%%100==0){gc(verbose=F);print(x)}
    }
    
    colnames(countmat1) = gsub(".RData$","", files, ignore.case = T)
    rownames(countmat1) = allwords
    
  }, error=function(e){print(e)},  finally = try(stopCluster(cl),silent = T) )
  gc(verbose=F)
  ## Fix the counts matrix, normalize, save info ##
  
  # remove useless words that occur just once per entire corpus
  if(rm_global_hapaxes){ 
    if(listgiven){  # for twitter hashtag spread; should not use if doing day by day!
      countmat1 = countmat1[apply(countmat1, 1, function(x) sum(x>0)>1 ), ]
    } else {   # the usual word count matrix
      countmat1 = countmat1[rowSums(countmat1)>1, ]
    }
  }
  
  # save list of words that occur frequently enough to be included in future topic models
  #lexicon = rownames(countmat1)[apply(countmat1, 1, function(x) any(x>=minc) ) ]
  
  # normalize matrix columns to per-million
  thesums = colSums(countmat1)
  
  if(normd){
    for(i in 1:ncol(countmat1)){
      countmat1[,i] =  (countmat1[,i]/thesums[i])*1000000
    }
    ones = (1/thesums)*1000000 # vector of (normalized) values that correspond to 1 occurrence   
    countmat=list(countmat=countmat1, thesums=thesums, ones=ones )
  } else {
    countmat=countmat1
    attr(countmat, "thesums") = thesums
  }
  # for smoothing in log() frequency difference calculations
  
  #try({ save("countmat", file=file.path(FOLDER, "countmat.RData")) })
  #print(paste(Sys.time(), "Done with counting words."))
  return(countmat)
}





dofreqdif = function(countmat, ones, uselog=T){
  countmat = round(countmat, 10) # to avoid any chance of floating point errors when looking for 0s
  freqdifmat = countmat
  freqdifmat[] = NA
  for(i in 2:ncol(countmat)){
    fr1 = countmat[,i-1] 
    fr2 = countmat[,i]   
    if(uselog){
      fr1[fr1==0] = ones[i-1]  # smoothing by pm value that equals 1 occurrence in that subcorpus
      fr2[fr2==0] = ones[i]
      x = log(fr2/fr1)  
      # Super important: 
      # return changes from-0-to-0 back into zeroes! - The smoothing above
      # makes all zeroes non-zero, which would lead to weird numbers, so this fixes it:
      x = ifelse( rowSums(countmat[,c(i-1,i), drop=F])==0, 0, x) # fixed
      # x = ifelse( (fr1+fr2)<0.00000001, 0, x) # this was wrong
      #
      freqdifmat[,i] = x
    } else {  # raw freq difference
      freqdifmat[,i] = fr2-fr1
    }
  }
  gc()
  return(freqdifmat)
}; dofreqdif = cmpfun(dofreqdif, options=list(optimize=3))





fullppmi = function(pmat, N, colp,rowp, positive=T){
  library(Matrix, quietly = T)
  # throws error is supplied vectors don't make sense:
  if(length(colp) != nrow(pmat) | length(rowp) != ncol(pmat) ) { 
    stop("mismatching norm vector length(s)")
  } 
  # note colp~nrow comparison - because the matrix is transposed for efficiency below
  
  pmat = Matrix::t(pmat)
  pp = pmat@p+1
  ip = pmat@i+1
  tmpx = rep(0,length(pmat@x))
  for(i in 1:(length(pmat@p)-1) ){
    #for(i in 1:100 ){
    #not0 = which(pmat[, i] > 0)
    ind = pp[i]:(pp[i+1]-1)
    not0 = ip[ind]
    icol = pmat@x[ind]
    #print(icol)
    #tmp = log( (pmat[not0,i]/N) / (rowp[not0] * colp[i] ))
    tmp = log2( (icol/N) / (rowp[not0] * colp[i] ) )
    tmpx[ind] = tmp
    #print(tmp)
    # tmp = ifelse(tmp < 0, 0, tmp)
    #pmat2[not0, i] = tmp
  }
  if(positive){
    tmpx[tmpx<0] = 0
  }
  pmat2 = pmat
  pmat2@x = tmpx
  pmat2 = Matrix::t(pmat2)
  pmat2 = drop0(pmat2,is.Csparse=T)
  return(pmat2)
}; fullppmi = cmpfun(fullppmi, options=list(optimize=3))




doppmimat = function(years, winsize=5,minc = 100, ppmi=T,
                     foldr=NULL, 
                     preloaded=F, weightedwin=T
){
  require(text2vec)
  require(Matrix)
  
  if(preloaded){
    plist=years
  } else {
    toload = years
    files=list.files(foldr, full.names = T,pattern = "RData$")
    plist = list()
    for(f in toload){
      load(files[f]) # load period
      plist = c(plist, period) # could optimize (but need to keep it 1-deep list!)
    }
    rm(period) # remove last
  }
  it = itoken(plist, progressbar = FALSE); rm(plist)
  voc0 = prune_vocabulary(create_vocabulary(it), term_count_min = minc)
  vectorizer = vocab_vectorizer(voc0)
  if(weightedwin){  tcm = create_tcm(it, vectorizer, skip_grams_window = winsize )}
  if(!weightedwin){ tcm = create_tcm(it, vectorizer, skip_grams_window = winsize, 
                                     weights=rep(1,winsize ))
  }
  tcm = tcm + Matrix::t(Matrix::triu(tcm)) ; Matrix::diag(tcm) = 0 
  if(ppmi){
    normvoc = voc0$term_count/sum(voc0$term_count)
    rm(it,vectorizer, voc0) # free some memory
    N=sum(tcm@x)
    ptcm = fullppmi(pmat=tcm, N=N, colp=normvoc,rowp=normvoc)
    return(ptcm)
  } else {
    return(tcm)
  }
}

dodispersion = function(years, minc,foldr="/Users/pplsuser/Desktop/korpused/lexcom_model/cohaperiods"){
  require(text2vec)
  require(Matrix)
  
  toload = years
  files=list.files(foldr, full.names = T,pattern = "RData$")
  plist = list()
  for(f in toload){
    load(files[f]) # load period
    plist = c(plist, period) # could optimize (but need to keep it 1-deep list!)
  }
  rm(period) # remove last
  it = itoken(plist, progressbar = FALSE); rm(plist)
  voc0 = create_vocabulary(it) %>% prune_vocabulary(term_count_min = minc)
  disp = log2(voc0$term_count/voc0$doc_count)
  names(disp) = voc0$term
  # 
  return(disp)
}



dorelevants = function(ppmimat,relevancy_threshold){
  library(grr)
  relevants = list()
  #print(paste(Sys.time(), "do relevance vectors"))
  #excludeself = match(rnames, colnames(ppmimat))  # might not have match in comlex ->NA
  #excludeself[is.na(excludeself)] = ncol(ppmimat)+100  # idea: those with no match get index > length of vector, so they can't be -indexed but won't be NA when -indexed, no need for ifelse
  
  # if(paral){
  # library(parallel)
  # nc = detectCores() - nfree
  # cl = makeCluster(nc)
  # clusterEvalQ(cl, library(Matrix)) 
  # clusterExport(cl, c("ppmimat","relevancy_threshold"),envir = environment()) 
  # print(paste(Sys.time(), "export ready, start"))
  # tryCatch({
  #   relevants = parLapply(cl, 1:nrow(ppmimat), 
  #       function(x){
  #         #y=sort(ppmimat[x,-(union(which(ppmimat[x,]==0), excludeself[x]))], decreasing=T)
  #         # self-ppmi is always zero due to how text2vec handles the tcm
  #         y=sort(ppmimat[x,-which(ppmimat[x,]==0)], decreasing=T) 
  #         return(names(y[1:min(length(y),relevancy_threshold)])) 
  #         }
  #       )
  # }, error=function(e){print(e)}, finally = stopCluster(cl))
  # }
  # 
  # if(!paral){
  relevants = list(); length(relevants) = nrow(ppmimat)
  names(relevants) = rownames(ppmimat)
  ppmimat = Matrix::as.matrix(ppmimat)
  for(x in 1:nrow(ppmimat)){
    tmp = ppmimat[x, which(ppmimat[x,]>0) ]
    y=tmp[rev( .Call("ordercpp",tmp) )] # sort2 is decreasing=F BUT MESSES UP NAMES, USE order2 -> now this also doesn't work, but directly calling the internal cpp function works.
    y=y[1:min(length(y),relevancy_threshold) ] # but this need top ones, so rev
    relevants[[x]] = y; names(relevants[[x]]) = names(y)
  }
  #print(paste(Sys.time(),"relevance vectors done"))
  return(relevants)
}; dorelevants = cmpfun(dorelevants, options=list(optimize=3))


dorels_tcm = function(tcm, w, th=1){
  if(!(all(rownames(tcm) == w))) {tcm = tcm[w,]}
  tcm = as.matrix(tcm>th)
  rels=vector("list", length(w)); names(rels)=w
  for(i in 1:nrow(tcm)){
    rels[[i]] = names(which(tcm[i,]))
  }
  return(rels)
}; dorels_tcm = cmpfun(dorels_tcm, options=list(optimize=3))



doadvection = function(relevants, difvec, threshold){ 
  words = names(relevants)
  #advmat=matrix(NA, nrow=length(words),ncol=ncol(difmat)); rownames(advmat) = words
  adv = rep(NA, length(words)); names(adv)=words
  #diffs = difvec[words]
  diffs = difvec
  # if(!(all(
  #   unique( c(names(relevants), unlist(sapply(relevants, names), use.names = F)))
  #   %in% names(diffs)) ) ){ stop("not all rel words in difvec") }
  
  # advection from current values of present relevants
  for(i in 1:length(relevants)){ 
    nrelevants = 1:(min(threshold, length(relevants[[i]])))
    adv[i] = weighted.mean(diffs[ names(relevants[[i]][nrelevants] ) ],
                           w = relevants[[ i ]][nrelevants] , na.rm = T) 
  }
  return(adv)
}

binfreqmat = function(freqmat, xperiods, permillion=T,smooth1=F,sums=NULL){
  # if freqmat/corpus column sums provided, use those (eg in targets finder), if not,
  #  calculate and attach (if using entrire freqmat!)
  newmat = matrix(0, nrow = nrow(freqmat), ncol=length(xperiods), 
                  dimnames = list(rownames(freqmat), c() ))
  localsums=rep(NA, length(xperiods))
  newsums=rep(NA, length(xperiods))
  if(!is.null(sums)){
    # if using provided sums, double-check:
    if(length(sums)!=ncol(freqmat) | max(unlist(xperiods))>length(sums)){
      stop("error in binfreqmat: sums vector and freqmat and/or xperiods do not match")
    } 
    for(i in seq_along(xperiods)){
      newsums[i] = sum(sums[xperiods[[i]] ]) # sum totals of periods-to-be-binned into one
    }
  }
  for(i in seq_along(xperiods)){
    newmat[,i] = rowSums(freqmat[,xperiods[[i]],drop=F ])
    if(is.null(sums)){
      s=sum(newmat[,i])
      localsums[i] = s
    } else {
      s = newsums[i]
    }
    if(permillion) {
      if(smooth1){
        newmat[ newmat[,i]<0.000001 ,i] = 1  # make all 0s 1s 
        # (only use if immediately doing freq change and discarding the binned freqmat right away)
        # (check is against very small non-0, to avoid any possible floating point issues)
      }
      newmat[,i] = (newmat[,i]/s ) * 1000000
    }
  }
  if(is.null(sums)) {
    attr(newmat, "sums") = localsums
    attr(newmat, "ones") = (1/localsums)*1000000 
  } else {
    attr(newmat, "ones") = (1/newsums)*1000000 
  }
  return(newmat)
}

dosim =  function(relr, relc){
  #if(length(relr)>0 & length(relc)>0){   # doesn't seem to occur, always some context
  # requires fastmatch
  r1 = fmatch(relc, relr, nomatch = 0) # the intersection part; remove nomatch 0s below
  r2 = fmatch(relr[r1], relc, nomatch = 0)
  return(sum( 1/(colMeans(rbind(r1[r1>0],r2) ) ) )) #
  #   } else return(NA)
};dosim = cmpfun(dosim, options=list(optimize=3))


dotopsims = function(rels){
  
  require(parallel)
  nc = detectCores()
  cl = makeCluster(nc)
  
  tryCatch({
    clusterEvalQ(cl, c(library(fastmatch)) )
    clusterExport(cl, c("rels"),envir = environment())
    clusterExport(cl, c("dosim"))
    
    tops = parSapply(cl, 1:length(rels), function(x){
      sims=rep(NA,length(rels)); names(sims)=names(rels)
      xseq = c(1:length(rels))[-c(x, which(names(rels) %in% names(rels[[x]] ) )) ]
      # skip target and contexts of target
      for(i in xseq ){
        sims[i] = dosim(names(rels[[x]]), names(rels[[i]]) )
      }
      return(sort(sims,decreasing = T)[1])
      
    } )
  }, error=function(e){print(e)},  finally = stopCluster(cl) )
  return(tops)
}


do_probmass_neibs = function(w, lsamatbig, rels, freq3, suffix="_t1$", rand=F){
  tmp = lsamatbig[grep(suffix, rownames(lsamatbig)),]
  rownames(tmp) = gsub(suffix, "", rownames(tmp))
  probdiffs = freq3[rownames(tmp),2] - freq3[rownames(tmp),1]
  wdiffs = freq3[w,2] - freq3[w,1]
  
  pmsims = rep(NA, length(w)); names(pmsims)=w
  pmranks = rep(NA, length(w)); names(pmranks)=w
  pmposranks = rep(NA, length(w)); names(pmposranks)=w
  topsims = rep(NA, length(w)); names(topsims)=w
  ndown = rep(NA, length(w)); names(ndown)=w
  leftover = rep(NA, length(w)); names(leftover)=w
  losers = rep(NA, length(w)); names(losers)=w
  
  rpmsims =  rep(NA, length(w)); names(rpmsims)=w
  rpmranks =  rep(NA, length(w)); names(rpmranks)=w
  rpmposranks = rep(NA, length(w)); names(rpmposranks)=w
  
  pmsimsnorm =  rep(NA, length(w)); names(pmsimsnorm)=w
  rpmsimsnorm =  rep(NA, length(w)); names(rpmsimsnorm)=w
  
  sims = sim2(tmp, lsamatbig[w,,drop=F])        # will not work with many k words!!!
  rm(tmp)
  for(i in which(w %in% rownames(lsamatbig)) ){
    #x = sort(sim2(tmp, lsamatbig[w[i],,drop=F])[,1], decreasing = T)
    x = sort(sims[,w[i] ] , decreasing = T)
    x = x[which(  !(names(x) %fin% c(w[i], names(rels[[ w[i] ]]) ) ) ) ] # fastmatch
    # x0=x # original for randomization part
    
    # if(norm){
    #   x=x/max(x) # makes 1st neighbor sim=1
    # }
    px = probdiffs[names(x)]
    px[px>0] = 0   # only count decreases
    xc = abs(cumsum(px)) # abs of sum of decreases
    wxc = which(xc >= wdiffs[ w[i] ] )[1] # index where prob mass gets equalized
    leftover[i] = abs(xc[wxc]-wdiffs[ w[i] ]  )/wdiffs[ w[i] ] # % of w change
    
    pmsims[i]  = x[ wxc ]  # sim of nth neib
    pmsimsnorm[i] = (x/max(x))[wxc]
    pmranks[i] = wxc   # nth neib
    pmposranks[i] = length(which(px[1:wxc] == 0) ) # how many increases (all pos->0 above) before index where equalized - this way both one-one and one-many competition processes are covered
    topsims[i] = x[1] # proxy to density
    ndown[i] =  length(which(px[1:wxc] < 0 )) # number of words that go down to equalize prob mass
    losers[i] = paste( names( which(px[1:wxc]<0)), collapse=" ")
    
    if(rand){
      names(x) = names(x)[sample(1:length(x), length(x))] # randomize
      # if(norm){
      #   x=x/max(x) # makes 1st neighbor sim=1
      # }
      px = probdiffs[names(x)]
      px[px>0] = 0   # only count decreases
      xc = abs(cumsum(px)) # abs of sum of decreases
      wxc = which(xc >= wdiffs[ w[i] ] )[1] # where prob mass gets equalized
      rpmsims[i]  = x[ wxc ]  # sim of nth neib
      rpmsimsnorm[i] = (x/max(x))[wxc]
      rpmranks[i] = wxc   # nth neib
      rpmposranks[i] = length(which(px[1:wxc] == 0) )
      
    }
    
    if(i %in% seq(1,15000,1000)){print(paste(i, Sys.time()))}
  }
  return(list(pmsims=pmsims, pmsimsnorm=pmsimsnorm, pmranks=pmranks, pmposranks=pmposranks,
              topsims=topsims, ndown=ndown, leftover=leftover, losers=losers, 
              rpmsims=rpmsims, rpmranks=rpmranks, rpmsimsnorm=rpmsimsnorm, rpmposranks=rpmposranks)) # randomized
}



findperiods = function(wy, l1=10, l2=20){
  ys = sort(unique(wy$y))
  ylist = vector("list", length(ys))
  for(i in seq_along(ys)){
    sublist = list()
    sublist$t0count = (ys[i]-l1) :(ys[i]-1)
    sublist$t1count = (ys[i])    :(ys[i]+(l1-1))
    sublist$t0sem   = (ys[i]-l2) :(ys[i]-1)
    sublist$t1sem   = (ys[i])    :(ys[i]+(l2-1))
    sublist$tadv    = (ys[i]-l1) :(ys[i]+(l1-1))
    sublist$targets = wy$word[wy$y %in% ys[i]]
    ylist[[i]] = sublist
  }
  names(ylist) = ys
  return(ylist)
}

yearloader = function(files){
  plist = list()
  for(f in files){
    load(f) # load period
    plist = c(plist, period) # could optimize (but need to keep it 1-deep list!)
  }
  rm(period) # remove last
  return(plist)
}



dodens=function(neibs0, neibs1, ns){
  dens0 = mean(neibs0[ns])
  dens1 = mean(neibs1[ns])
  dc = dens1-dens0
  dc_raw=dc
  dc = ((dc) / ( (0^0^dc) - dens0)) * sign(dc) # normalize - possible dif is limited by value of dens0
  return(c("dif"=dc, "d0"=dens0, "d1"=dens1, "raw"=dc_raw))
}

do_ylist_neibs = function(ylist, WINSIZE=2, minc=100,denslength=2:10,dodens=T,smoothsem=T,
                          foldr="/Users/pplsuser/Desktop/korpused/lexcom_model/cohaperiods", corpusbounds=c(1,200)
){
  
  #dir.create(lsafolder, showWarnings = FALSE) # in case it doesn't exist yet
  
  # check if within corpus bounds
  if(smoothsem){
    y = unique(unlist(lapply(ylist, function(x) x[c("t0sem", "t1sem")])))
  } else {
    y = unique(unlist(lapply(ylist, function(x) x[c("t0count", "t1count")])))
  }
  if(any(y<corpusbounds[1] | y>corpusbounds[2]) ) { stop("time window(s) out of corpus bounds") }
  
  files=list.files(foldr, full.names = T,pattern = "RData$")
  neiblist = list()
  denslist = list()
  semchange = vector("list", length=length(ylist)); names(semchange)=names(ylist)
  
  if(smoothsem){
    t0="t0sem"; t1="t1sem"
  } else {
    t0="t0count"; t1="t1count"
  }
  for(i in seq_along(ylist)){
    print(paste(i, Sys.time()))
    try(rm(tcmboth,tmp0, tmp1), silent = T)
    gc()
    
    # t0
    tcm0 = yearloader(files[ ylist[[i]][[t0]] ]) %>% 
      doppmimat(winsize=WINSIZE, minc = minc, ppmi=T, preloaded = T)
    # t1
    tcm1 = yearloader(files[ ylist[[i]][[t1]] ]) %>% 
      doppmimat(winsize=WINSIZE, minc = minc, ppmi=T, preloaded = T)
    # will need these for semchange
    
    lsa = LatentSemanticAnalysis$new(100) # use lsa of t0 below for transforms
    tmp0 = lsa$fit_transform(tcm0)
    lsa1 = LatentSemanticAnalysis$new(100) # 
    tmp1 = lsa1$fit_transform(tcm1)
    
    for(w in ylist[[i]]$targets){
      tryCatch({ 
        xrow = tcm1[w,,drop=T] %>% .[colnames(tcm0)] %>% replace(is.na(.), 0)
        neibs0 = sort(sim2(tmp0, 
                           transform(matrix(xrow,nrow=1), lsa))[,1],
                      decreasing = T)
        neibs0 = neibs0[which(!(names(neibs0)%in% w) )] # remove possible older self
        neiblist[[w]] = neibs0
        neibs1 = sort(sim2(tmp1, tmp1[w,,drop=F])[,1], decreasing = T)[-1] # top is always self
        if(dodens){
          denslist[[w]] = dodens(neibs0, neibs1, denslength)
        }
        
      }, error = function(e) print(e) )
    }
    
    # compound lsa for semchange
    cols = intersect(colnames(tcm0), colnames(tcm1))
    rows = intersect(rownames(tcm0), rownames(tcm1))
    tcm0 = tcm0[rows,cols]
    tcm1 = tcm1[rows,cols]
    rownames(tcm1) = paste0(rownames(tcm1), "_t1")
    tcmboth = rbind(tcm0, tcm1)
    rm(tcm0, tcm1)
    lsa = LatentSemanticAnalysis$new(100)
    biglsa = lsa$fit_transform(tcmboth)
    if(!(all( rownames(biglsa)[1:length(rows)] == 
              gsub("_t1","", rownames(biglsa)[(length(rows)+1):nrow(biglsa) ] )) )){
      stop("compound LSA model has name mismatch")
    }
    sims = psim2(biglsa[1:length(rows),], biglsa[(length(rows)+1):nrow(biglsa),])
    names(sims) = rows
    semchange[[i]] = sims
    
  }
  return(list(neiblist=neiblist, denslist=denslist, semchange=semchange))
}



reader_tweets = function(f) {
  load(f)
  rt=attr(period, "retweets")+1
  rtcum=cumsum(rt)
  res = rep(NA, sum(rt))
  for(i in 1:length(period) ){
    p = period[[i]][which( !grepl(" |\n", period[[i]]) )] # should have removed empties in parsing
    if(length(p)>0) res[ (rtcum[ max(i-1,1) ]+1):(rtcum[i]) ]  = stringr::str_c(p,collapse=" ")
  }
  return( res )
}
reader_tweets_countmat = function(f) {
  load(f)
  rt=attr(period, "retweets")+1
  rtcum=cumsum(rt)
  res = rep(NA, sum(rt))
  for(i in 1:length(period) ){
    p = period[[i]][which( !grepl(" |\n", period[[i]]) )] # should have removed empties in parsing
    if(length(p)>0) res[ (rtcum[ max(i-1,1) ]+1):(rtcum[i]) ]  = stringr::str_c(p,collapse=" ")
  }
  rm(period)
  res = unlist(res, recursive = F, use.names = F)
  res = res[which(!is.na(res))]
  res = stringr::str_c(res, collapse=" " )
  names(res) = stringr::str_extract(f, "[0-9]{4}-[0-9]{2}-[0-9]{2}")
  return( res )
}

reader_tweets_countmat_noretweets = function(f) {
  load(f)
  period = unlist(period, recursive = F, use.names = F)
  period = period[which( !grepl(" |\n", period) )] # should have removed empties in parsing
  period = stringr::str_c(period, collapse=" " )
  names(period) = stringr::str_extract(f, "[0-9]{4}-[0-9]{2}-[0-9]{2}")
  return( period )
}

tweetstcm=function(paths, minc=100){
  require(text2vec)
  it_files  = ifiles(file_paths=paths, reader = reader_tweets)
  print("done reading files")
  it = itoken(it_files, tokenizer = space_tokenizer, progressbar = FALSE, xptr=T)
  rm(it_files)
  print("doing vocab")
  v = create_vocabulary(it, stopwords=c("\n", "\n\n",NA,"'s") ) # should also have removed those
  v = prune_vocabulary(v, term_count_min = minc) 
  vec = vocab_vectorizer(v)
  print("doing tcm")
  tcm = create_tcm(it, vec, skip_grams_window = 100,  weights=rep(1,100) )
  tcm = tcm + Matrix::t(Matrix::triu(tcm)) ; Matrix::diag(tcm) = 0 
  normvoc = v$term_count/sum(v$term_count)
  N=sum(tcm@x)
  rm(it,v, vec) # free some memory
  print("doing ppmi transformation")
  ptcm = fullppmi(pmat=tcm, N=N, colp=normvoc,rowp=normvoc)
  return(ptcm)
  
}

fast_countmat = function(paths, rm_global_hapaxes=T, retweets=F){
  # read and treat year as doc, then dtm ~ word counts matrix over time, useful
  if(retweets)  it_files  = ifiles(file_paths=paths, reader = reader_tweets_countmat)
  if(!retweets) it_files  = ifiles(file_paths=paths, reader = reader_tweets_countmat_noretweets)
  it = itoken(it_files, tokenizer = space_tokenizer, progressbar = FALSE, xptr=T)
  rm(it_files)
  v = create_vocabulary(it) %>%   # stopwords=c("\n", "\n\n",NA,"'s") no need for stopwords when parser fixed
    prune_vocabulary(term_count_min = ifelse(rm_global_hapaxes,2,1)) %>% 
    vocab_vectorizer()
  countmat = create_dtm(it, v, type = "dgCMatrix") %>% Matrix::t()
  return(countmat)
  
  # problem? Error in tolower(completions) : invalid input [gibberish] in 'utf8towcs'
  # this is probably from datatable which voc uses, but it still runs...
}


# current one
do_ylist_winners = function(ylist, freqmat, WINSIZE=2, WINSIZE_ADV=10,WINSIZE_DISS=2, minc=100, smoothsem=F, foldr=NULL, corpusbounds=NULL, twitter=F, singlemodel=F, ntopicwords=75, skiplsa=F
){
  if(twitter) { weightedwin = F} # don't weigh context windows if twitter posts
  if(!twitter){ weightedwin = T}
  
  targets = unlist(sapply(ylist, function(x) x$targets), use.names = F)
  
  # check if within corpus bounds
  if(smoothsem){
    y = unique(unlist(lapply(ylist, function(x) x[c("t0sem", "t1sem")])))
  } else {
    y = unique(unlist(lapply(ylist, function(x) x[c("t0count", "t1count")])))
  }
  if(any(y<corpusbounds[1] | y>corpusbounds[2]) ) { stop("time window(s) out of corpus bounds") }
  if( !all(targets %in% rownames(freqmat)) ) { stop("not all targets in freqmat, something is wrong") }
  
  files=list.files(foldr, full.names = T,pattern = "RData$")
  neiblist = vector("list", length=length(targets)); names(neiblist)=targets
  #denslist = list()
  #semchange = vector("list", length=length(ylist)); names(semchange)=names(ylist)
  # advres=c()
  adv    = setNames(rep(NA, length(targets)),targets)
  advres = setNames(rep(NA, length(targets)),targets)
  diss   = setNames(rep(NA, length(targets)),targets)
  xrels  = vector("list", length=length(targets)); names(xrels)=targets
  xmeans = setNames(rep(NA, length(targets)),targets)
  xsds   = setNames(rep(NA, length(targets)),targets)
  
  # not used in this version
  if(smoothsem){
    t0="t0sem"; t1="t1sem"
  } else {
    t0="t0count"; t1="t1count"
  }
  
  # do all target years/periods, some have multiple targets
  if(!singlemodel){
    for(i in seq_along(ylist)){
      print(paste0(i,"/", length(ylist), " ", Sys.time()))
      if(i>1) try({rm(tcm1,tcm0,tmp1);gc(verbose=F)}, silent = T )
      
      # load years
      #
      y0files = yearloader(files[ ylist[[i]]$t0count ])
      y1files = yearloader(files[ ylist[[i]]$t1count ])
      
      ### adv  ###
      #
      tcma = c(y0files, y1files) %>% 
        doppmimat(winsize=WINSIZE_ADV, minc = minc, ppmi=T, preloaded = T, weightedwin=weightedwin)  
      # - bigger window for topics
      countmat = binfreqmat(freqmat, 
                            xperiods=list(ylist[[i]]$t0count, ylist[[i]]$t1count),
                            permillion = T, smooth1=F, sums=attr(freqmat, "thesums")
      )
      diffs = dofreqdif(countmat, ones = attr(countmat, "ones"))[,2] # get vec only
      
      rels = dorelevants( tcma[ ylist[[i]]$targets, ,drop=F] , 75)  # 1000 for excluding from sem neighbors - or just 75 if using the ntopicwords as the filter limit as well
      #rels = dorelevants( tcma , 75)  # or do all, to be able to do residuals approach (slow)
      # but also the adv~diffs correlation is pretty low, residuals not reliable?
      # also if not using adv residuals, can forego full lists and just do rels for targets, faster
      a = doadvection(rels, diffs, ntopicwords)  # but use only 75 for nice topics
      adv[ylist[[i]]$targets] = a[ylist[[i]]$targets]
      r = rels[ylist[[i]]$targets]
      r = lapply(r, names)
      xrels[ylist[[i]]$targets] = r
      #d2 = (abs(countmat[names(a),2] - countmat[names(a),1])) # abslute magnitude of change
      #advres[ylist[[i]]$targets] = residuals(lm(a ~ diffs[names(a)] * d2))[ylist[[i]]$targets]
      try({rm(rels,diffs,tcma,a,r);gc()}, silent = T )
      
      # d =  data.frame(a,  
      #                 d=diffs[names(a)], 
      #                 d2 = (abs(countmat[names(a),2] - countmat[names(a),1])) ,
      #                 f=log(rowMeans(countmat[names(a),])+0.01)
      #                 )
      #  ggplot(d, aes(x=a,y=d, color=d2<1.3) ) +geom_point() + geom_smooth(method="lm") +geom_smooth(method="lm", aes(color=NULL))
      # ggplot(d, aes(x=d,y=d2) ) +geom_point() + geom_smooth(method="lm")
      # summary( lm(a~d*d2, d))
      # plot(residuals( lm(a~d*d2, d)), residuals( lm(a~d, d)))
      
      
      ### t1 counts for dissemination (winners, so t1) ###
      #
      if(F){   # doesn't add much to model though ------------  OFF --- -
        tcm = y1files %>% 
          doppmimat( winsize=WINSIZE_DISS, minc = 2, ppmi=F, preloaded = T, weightedwin=weightedwin)  
        # just count tcm; excluding only hapaxes, need everything else for dissemination model
        
        cocount = log(rowSums(tcm>0)); cocount[is.infinite(cocount)] = NA
        lf = log(countmat[rownames(tcm), 2]); lf[is.infinite(lf)] = NA       # use t0, column1
        #cocount=cocount[exp(lf)>= (100/attr(countmat, "sums")[2])*1000000 ]
        #lf=lf[exp(lf)>= (100/attr(countmat, "sums")[2])*1000000 ]
        lf2 = lf^2
        # summary(lm((cocount)~ lf+lf2 ))
        # tm=seq(min(lf, na.rm=T), max(lf,na.rm=T), 0.1)
        # predictedcounts <- predict(lm((cocount)~ lf+lf2 ),list(lf= tm, lf2=tm^2))
        # plot( (cocount) ~lf, col=rgb(1,1,1,0.1))
        # lines( tm, predictedcounts, col = "darkgreen", lwd = 3)
        # points( lf[ylist[[i]]$targets] , (cocount)[ylist[[i]]$targets], col="red" )
        diss[ylist[[i]]$targets]  = residuals(lm((cocount)~ lf+lf2 ))[ylist[[i]]$targets] 
        try({rm(tcm, countmat, cocount, lf, lf2);gc()}, silent=T )
      }
      
      
      
      ### neibs ###
      #
      # if at any point filtering tcm before lsa: lsa assumes dtm, meaning it will compress the rows, word subsets should be by column I guess
      # but the xrow thing below works for now because the tcms are symmetric, no difference.
      #
      if(!skiplsa){
        # t0
        tcm0 = y0files %>% 
          doppmimat(winsize=WINSIZE, minc = minc, ppmi=T, preloaded = T, weightedwin=weightedwin)
        # t1
        tcm1 = y1files %>% 
          doppmimat(winsize=WINSIZE, minc = minc, ppmi=T, preloaded = T, weightedwin=weightedwin)
        
        #lsa = LatentSemanticAnalysis$new(100) # 
        #tmp0 = lsa$fit_transform(tcm0)
        
        lsa1 = LatentSemanticAnalysis$new(100) # for winners use this for transform
        tmp1 = lsa1$fit_transform(tcm0)        # tcm1 if losers
        
        
        ## calculate embedding model mean and sd estimate for use in scaling later
        #
        # note: fast removal of self-similarities by matching 1's: technically sim with 
        # some other word *could* be 1, but in practise this is nigh impossible on large'ish corpora 
        # (would require exact same usage/ppmi vector, i.e. 100+ usages of both words
        # that would always have the exact same co-occurring words)
        # use same number of words for all spaces to estimate mean and sd
        #
        # this doesn't seem to improve anything and is harder to explain, not sure if any point
        if(FALSE){   # ----------- turning off for now -------------
          x = as.matrix(sim2(tmp1, tmp1[sample(1:nrow(tmp1), min(5000,nrow(tmp1)) ),]))
          x=round(x,10) # treats any tiny floating point errors in sim2: now can match 1's below:
          x[x==1]=-1 # selfsim
          if(any(is.na(x))){stop("sim2 matrix should not have NA's, but does, something is off")}
          xm = apply(x,2,max)
          xmeans[ylist[[i]]$targets] = mean(xm)
          xsds[ylist[[i]]$targets]  = sd(xm)
          rm(x,xm)
        }
        
        
        #### find neighbours in LSA ###
        #
        for(w in ylist[[i]]$targets){
          tryCatch({ 
            xrow = tcm1[w,,drop=T] %>% .[colnames(tcm0)] %>% replace(is.na(.), 0)  # winners
            #xrow = tcm0[w,,drop=T] %>% .[colnames(tcm1)] %>% replace(is.na(.), 0) # losers, so tcm0->tcm1
            neibs0 = sort(sim2(tmp1, 
                               transform(matrix(xrow,nrow=1), lsa1))[,1],
                          decreasing = T)
            neibs0 = neibs0[which(!(names(neibs0)%in% w) )] # remove possible older self
            neiblist[[w]] = neibs0
            #neibs1 = sort(sim2(tmp1, tmp1[w,,drop=F])[,1], decreasing = T)[-1] # top is always self
            # if(dodens){
            #   denslist[[w]] = dodens(neibs0, neibs1, denslength)
            # }
            
          }, error = function(e) print(e) )
        }
        
        # compound lsa for semchange    ----------- off, not sure how to pick params -----
        # cols = intersect(colnames(tcm0), colnames(tcm1))
        # rows = intersect(rownames(tcm0), rownames(tcm1))
        # tcm0 = tcm0[rows,cols]
        # tcm1 = tcm1[rows,cols]
        # rownames(tcm1) = paste0(rownames(tcm1), "_t1")
        # tcmboth = rbind(tcm0, tcm1)
        # rm(tcm0, tcm1)
        # lsa = LatentSemanticAnalysis$new(100)
        # biglsa = lsa$fit_transform(tcmboth)
        # if(!(all( rownames(biglsa)[1:length(rows)] == 
        #           gsub("_t1","", rownames(biglsa)[(length(rows)+1):nrow(biglsa) ] )) )){
        #   stop("compound LSA model has name mismatch")
        # }
        # sims = psim2(biglsa[1:length(rows),], biglsa[(length(rows)+1):nrow(biglsa),])
        # names(sims) = rows
        # semchange[[i]] = sims
        
      }
    }
  }
  
  if(singlemodel){
    print(paste("doing twitter model with single tcm", Sys.time()) )
    
    # load ALL years
    # y0files = yearloader(files)
    # 
    # # one single tcm for both adv and neibs, since same windowsize anyway
    # tcm = y0files %>%
    #   doppmimat(winsize=WINSIZE, minc = minc, ppmi=T, preloaded = T,
    #             weightedwin=weightedwin)
    # rm(y0files)
    
    ### incremental loader here with multiplied retweets
    tcm = tweetstcm(files, minc=100)
    
    
    ### adv  ###
    #
    # loop across targets, but keep using same tcm to get topics
    print(paste("doing twitter model advection", Sys.time()) )
    freqmat = as.matrix(freqmat) # is Matrix, expensive subsetting
    for(i in seq_along(ylist)){
      countmat = binfreqmat(freqmat, 
                            xperiods=list(ylist[[i]]$t0count, ylist[[i]]$t1count), 
                            permillion=T,smooth1=F, sums=attr(freqmat, "thesums"))
      diffs = dofreqdif(countmat, ones = attr(countmat, "ones"))[,2] # get vec only
      
      rels = dorelevants( tcm[ ylist[[i]]$targets, ,drop=F] , 1000)  # 1000 for excluding from sem neighbors
      # also if not using adv residuals, can forego full lists and just do rels for targets, faster
      a = doadvection(rels, diffs, 75)  # but use only 75 for nice topics
      adv[ylist[[i]]$targets] = a[ylist[[i]]$targets]
      r = rels[ylist[[i]]$targets]
      r = lapply(r, names)
      xrels = c(xrels, r)
      try({rm(countmat, freqmat, rels,diffs,a,r);gc()}, silent = T )
    }
    
    
    ### neibs ##
    #
    # using same tcm, no loop
    print(paste("doing twitter model LSA", Sys.time()) )
    lsa1 = LatentSemanticAnalysis$new(300)
    tmp1 = lsa1$fit_transform(tcm)        
    try({rm(tcm);gc()}, silent = T )
    
    # if twitter hashtag model: remove all other words since comparing only #s ?
    #if(twitter){
    #tmp1 = tmp1[grep("^#", rownames(tmp1)), ]
    #}
    
    ## calculate embedding model mean and sd estimate for use in scaling later
    ws = unlist(sapply(ylist, function(x) x$targets), use.names = F)
    x = as.matrix(sim2(tmp1, tmp1[sample(1:nrow(tmp1), min(5000,nrow(tmp1)) ),])) 
    # use same number of words for all spaces to estimate mean and sd
    x=round(x,10) # treats tiny floating point errors in sim2: now can match 1's below:
    x[x==1]=-1 # selfsim
    if(any(is.na(x))){
      stop("sim2 matrix should not have NA's, but does, something is off")}
    xm = apply(x,2,max)
    xmeans[ws] = mean(xm)
    xsds[ws]  = sd(xm)
    rm(x,xm)
    
    #### find neighbours in LSA ###
    #
    print(paste("doing twitter model neighbours", Sys.time()) )
    ws = unlist(sapply(ylist, function(x) x$targets), use.names = F)
    sm = sim2(tmp1, tmp1[ws,,drop=F]) # columns
    
    for(w in ws){
      neibs0 = sort(sm[,w], decreasing = T)
      neibs0 = neibs0[which(!(names(neibs0) %in% w) )] # remove self
      neiblist[[w]] = neibs0
    } 
    # since this now uses entire corpus, should do post-filtering of neighbors
    # as some of them might not actually even occur (at all)
    # in the period of observation of the target!
    try({rm(tmp1,lsa1,sm,neibs0);gc()}, silent = T )
    
    
    ### counts for dissemination  ###
    #
    # runs out of mem whatever scrap it (or rewrite with incremental iterators)
    #
    # print(paste("doing twitter model polysemy", Sys.time()) )
    # y0files = yearloader(files)
    # tcm = y0files %>% # all files
    #   doppmimat( winsize=WINSIZE, minc = 2, ppmi=F, preloaded = T, weightedwin=weightedwin)  
    # rm(y0files)
    # ws = unlist(sapply(ylist, function(x) x$targets), use.names = F) # all targets
    # #
    # cocount = log(rowSums(tcm>0)); cocount[is.infinite(cocount)] = NA
    # lf = log(countmat[rownames(tcm), 2]); lf[is.infinite(lf)] = NA       # use t0, column1
    # lf2 = lf^2
    # diss[ws]  = residuals(lm((cocount)~ lf+lf2 ))[ws]
    
  } # end if-twitter
  
  return(list(neiblist=neiblist, diss=diss, adv=adv,advres=advres, xrels=xrels, xmeans=xmeans, xsds=xsds )) # denslist=denslist, semchange=semchange))
}



# if words in lists, precomputed neighbors, the current model:
do_probmass_neibs_list = function(ylist, neibslist, rels, freqmat, semchange=NULL, rand=F, losermodel=F, ntopsim=2, xmeans=NULL, xsds=NULL, twitter=F, ntopicwords=75, minneibyears){
  
  if( grepl("Matrix", class(freqmat))) freqmat=as.matrix(freqmat) # if sparse make nonsparse for faster subsetting
  w = unlist(sapply(ylist, function(x) x$targets), use.names = F)
  
  pmsims          =  setNames(rep(NA, length(w)),w)
  pmsimsnorm      =  setNames(rep(NA, length(w)),w)  
  pmsimsscaled    =  setNames(rep(NA, length(w)),w)
  
  pmranks         =  setNames(rep(NA, length(w)),w)  
  pmposranks      =  setNames(rep(NA, length(w)),w)    
  ndown           =  setNames(rep(NA, length(w)),w)
  
  topsims         =  setNames(rep(NA, length(w)),w)  
  topneib         =  setNames(rep(NA, length(w)),w)
  leftover        =  setNames(rep(NA, length(w)),w)  
  losers          =  setNames(rep(NA, length(w)),w)
  
  maxsemchange    =  setNames(rep(NA, length(w)),w)
  meansemchange   =  setNames(rep(NA, length(w)),w)
  minstringd      =  setNames(rep(NA, length(w)),w)
  topeditneib     =  setNames(rep(NA, length(w)),w)
  spellcomm       =  setNames(rep(NA, length(w)),w)
  
  
  freqf1          =  setNames(rep(NA, length(w)),w)
  freqf2          =  setNames(rep(NA, length(w)),w)
  freqchange      =  setNames(rep(NA, length(w)),w)
  freqchangeabs   =  setNames(rep(NA, length(w)),w)
  percentchange   =  setNames(rep(NA, length(w)),w)
  isnew           =  setNames(rep(NA, length(w)),w)
  freqsd          =  setNames(rep(NA, length(w)),w)
  minpercentlosers=  setNames(rep(NA, length(w)),w)
  stringcontained =  setNames(rep(NA, length(w)),w)
  
  
  # occurrence sd in window
  tmp = binfreqmat(freqmat, as.list(1:ncol(freqmat)), 
                   permillion = T, sums=attr(freqmat, "thesums")) %>% 
    .[unlist(sapply(ylist, function(x) x[["targets"]])), ]
  if(losermodel){tx="t0count"}else{tx="t1count"}
  for(i in seq_along(ylist)){
    freqsd[ylist[[i]]$targets] =
      apply(tmp[ylist[[i]]$targets, ylist[[i]][[tx]], drop=F], 1, sd)
  }
  rm(tmp)
  
  
  if(rand){
    allsims = unlist(neibslist, use.names = F)
    #allw = unique(unlist(sapply(neibslist, function(y) names(y) ), use.names = F))
    neibslist2=vector("list", length(neibslist)); names(neibslist2)=names(neibslist)
    n=sapply(neibslist, length) # these have different lengths from different lsa models
    for(i in seq_along(neibslist2)){
      neibslist2[[i]] = rev(sort2(sample(allsims, n[i] )))
      names(neibslist2[[i]]) = sample(names(neibslist[[i]]), n[i])
    }
    neibslist=neibslist2; rm(neibslist2)
  }
  
  # prep data for each timespan
  for(i in seq_along(ylist) ){
    tmptarg = ylist[[i]]$targets
    
    freq3 = binfreqmat(freqmat, 
                       xperiods=list(ylist[[i]]$t0count, 
                                     ylist[[i]]$t1count),
                       permillion=T, smooth1=F, sums=attr(freqmat, "thesums") 
    )
    probdiffs = freq3[,2] - freq3[,1]
    percentdiffs = (probdiffs/ 
                      pmax(freq3[,1], attr(freq3,"ones")[1]) # avoid Inf 
    )*100
    percentchange[tmptarg] = percentdiffs[tmptarg]
    freqchange[tmptarg] = dofreqdif(freq3[tmptarg,,drop=F], 
                                    ones=attr(freq3,"ones"))[,2] # log change
    freqf1[tmptarg] = freq3[tmptarg,1]
    freqf2[tmptarg] = freq3[tmptarg,2]
    
    # for each word in this timespan
    for(x in tmptarg ){
      cat(" "); cat(x)
      isnew[x] = all(freqmat[x, 1:(ylist[[i]]$t0count[1]-1) ] < 0.00000001)
      
      freqchangeabs[x] = freq3[x,2] - freq3[x,1] 
      pw = probdiffs[x]    # how much target freq has changed, absolute
      nbs = neibslist[[x]]
      
      #twitter single lsa model fix; if doing randoms+twitter, should rethink
      # if(twitter){
      #   freqtmp = binfreqmat(freqmat,
      #                      xperiods=list(ylist[[i]]$t0count,
      #                                    ylist[[i]]$t1count),
      #                      permillion=F,  # !
      #                      smooth1=F, sums=attr(freqmat, "thesums") 
      #   )
      #   # keep only neibs that actually occur in t1; exclude hapaxes
      #   nbs = nbs[ which(freqtmp[names(nbs),1] > 1  ) ]
      #   rm(freqtmp)
      # }
      # if(twitter){ 
      #   nbs = nbs[grep("^#", names(nbs))]
      # }
      
      # keep only neibs that actually occur in t0:
      ok = rowSums(freqmat[names(nbs),ylist[[i]]$t0count,drop=F] > 0) >= minneibyears
      nbs = nbs[ok ]
      
      
      ## how far probmass equalized
      nbs = nbs[which(!(names(nbs) %in% c(rels[[x]][1:ntopicwords], x))) ] 
      # exclude topic words (and self to make sure - also needed if using randomization test)
      topneib[x] = names(nbs)[1]
      px = probdiffs[ names(nbs) ]
      
      # nullify pos or neg changes depending on model 
      if(losermodel){
        px[px<0] = 0   # only count increases
      } else {
        px[px>0] = 0   # only count decreases
      }
      xc = abs(cumsum(px)) # abs of sum of increases
      wxc = which(xc >= abs(pw) )[1] # index where prob mass gets equalized
      if(is.na(wxc)){ break}
      
      leftover[x] = abs(xc[wxc]-abs(pw)  )/abs(pw) # % of w change
      pmsims[x]     = nbs[ wxc ]  # sim of nth neib
      pmsimsnorm[x] = (nbs/max(nbs))[wxc]
      topsims[x] = mean(nbs[ntopsim]) 
      # - proxy to density; autocorrelates with pmsimsnorm if 1 ->does it?
      
      
      ### z-scored dist using mean&sd (estimate) from full lsa model of t0
      # converting to distance right here, so no need to do it later
      if(!is.null(xmeans)){
        pmsimsscaled[x] = ( (1-pmsims[x]) - (1-xmeans[x]) ) / xsds[x]
      }
      
      
      if(!losermodel){
        pmranks[x] = wxc   # nth neib
        pmposranks[x] = length(which(px[1:wxc] == 0) ) # how many increases (above made all pos->0) before index where equalized - this way both one-one and one-many competition processes are covered
        ndown[x] =  length(which(px[1:wxc] < 0 )) # %number of words that go down to equalize prob mass (ie if one-to-many process)
        los=names( which(px[1:wxc]<0))
        minpercentlosers[x] = min(percentdiffs[los] )
      } else {
        los=names( which(px[1:wxc]>0))
        ndown[x] =  length(which(px[1:wxc] > 0 )) # number of winners in loser model
      }
      losers[x] = paste( los, collapse=" ") # or winners, if losermodel
      
      #minstringd[x] = min(stringdist(x, names(neibslist[[x]])[1:10])/
      #                      pmax(nchar(x),nchar(names(neibslist[[x]])[1:10]))  )
      #minstringd[x] = min(stringdist(x, names(nbs[1:wxc]))/
      #                      pmax(nchar(x),nchar(names(nbs[1:wxc])))  )
      #minstringd[x] = min(stringdist(x, los)/
      #                      pmax(nchar(x),nchar( los ) )  )
      #losd= 1-(stringdist(x, names(nbs[1:wxc]))/pmax(nchar(x),nchar(names(nbs[1:wxc]))) )
      #minstringd[x] = max(losd)/which.max(losd)
      #topeditneib[x] = names(nbs[1:wxc])[which.max(losd)] # is multiple takes closest/first!
      #losd= which(stringdist(x, names(nbs)) == 1 )[1]
      #minstringd[x] = ifelse(!is.na(losd), losd, length(nbs))
      #topeditneib[x] = ifelse(is.na(losd), NA, names(nbs)[losd])
      
      # losd= which.min(stringdist(x, names(nbs))) # first of the smallest dist
      # minstringd[x] = losd
      # topeditneib[x] = names(nbs)[losd]
      try({  # notutf8 still crash
        if(twitter  ){
          nbs = nbs[ which(!grepl("[^a-z0-9öäüõó]", names(nbs))) ] 
        } # exclude entities, broken or not, to be safe         ---------fix later?---
        #  now won't crash if weird stuff ends here, wrapped in try()
        # spelling
        periodw = setdiff(names(nbs) ,x)
        spellcomm[x] = mean(stringdist(x, periodw)/pmax(nchar(x),nchar(periodw)) ) 
        
        # editneibs
        #normalized restricted-damerau-levenshtein, min of nearest 20
        losd = stringdist(x, names(nbs)[1:20])/pmax(nchar(x),nchar(names(nbs)[1:20]))
        topeditneib[x] = (names(nbs)[1:20])[which.min(losd)] # the closest in 20 words
        minstringd[x] = min(losd[1:20])
        # or: all words, but "weighted" by normalized similarity by addition
        #losd = (stringdist(x, names(nbs))/pmax(nchar(x),nchar(names(nbs))))+(nbs/max(nbs))
        #topeditneib[x] = (names(nbs))[which.min(losd)]
        # not sure though
        
        #
        # another measure, more relevant if using multiword units: how is a word that
        #  contains the target, or vice versa, which is contained in the target:
        stringcontained[x] = any(
          grepl(paste0("^", x, "|",x,"$"), names(nbs)[1:100]),     # target within neibs
          sapply(names(nbs)[1:100], function(y) grepl(paste0("^", y, "|",y,"$"), x ))  # neibs within target
        )
        # could also do as distance... but some words would be Inf, no perfect overlaps either direction.
        #  }
      })
      # losd = (1-(stringdist(x, names(nbs))/pmax(nchar(x),nchar(names(nbs)))))*nbs
      # minstringd[x] = max(losd)
      # topeditneib[x] = (names(nbs))[which.max(losd)]
      # losd = (1-(stringdist(x, names(nbs))/pmax(nchar(x),nchar(names(nbs)))) )
      # minstringd[x] = max(losd)
      # topeditneib[x] = ((nbs))[which.max(losd)] # only makes sense with this interaction
      
      # semantic change - difficult, not clear how far to look for change
      # somehow weigh by distance...?
      if(!is.null(semchange)){
        maxsemchange[x] = 1-min(semchange[[names(ylist)[i] ]][names(nbs)[1:10]])
        meansemchange[x] = 1-weighted.mean(semchange[[names(ylist)[i] ]][names(nbs)[1:10]], na.rm = T, w = nbs[1:10])
      }
      
    }
    
  }
  if(!rand){ neibslist = NULL} # returns shuffled neighbors list if random model
  return(list(pmsims=pmsims, pmsimsnorm=pmsimsnorm, pmranks=pmranks, pmposranks=pmposranks,
              topsims=topsims, ndown=ndown, leftover=leftover, losers=losers, 
              freqchange=freqchange, freqchangeabs=freqchangeabs, freqf1=freqf1, freqf2=freqf2,
              minstringd=minstringd,topeditneib=topeditneib, meansemchange=meansemchange, maxsemchange=maxsemchange, pmsimsscaled=pmsimsscaled,
              isnew=isnew, spellcomm=spellcomm,topneib=topneib, freqsd=freqsd, minpercentlosers=minpercentlosers,
              neibslist=neibslist,stringcontained=stringcontained, percentchange=percentchange)) 
}




peakdetection = function(x,lim){
  d=rep(NA,length(x))
  for(i in seq_along(x)){d[i]=scale(x[i],mean(x[-i]), sd(x[-i]) )[1,1] }
  return(max(d, na.rm=T))
}; peakdetection=cmpfun(peakdetection, options=list(optimize=3))

peakdetection2 = function(x,lim=Inf,val=F){
  d=rep(NA, nrow(x))
  rms = x; rms[]=NA
  rsd = rms
  d=rms
  for(i in 1:ncol(x)){
    rms[,i] = rowMeans(x[,-i,drop=F])
    rsd[,i] = apply(x[,-i,drop=F],1, sd)
  }
  rsd[rsd==0] = 1  # smoothing: if constant vector, then sd==0, and z-scoring would fail/Inf
  x2=t(x)
  for(i in 1:ncol(x)){
    d[,i] = scale( x2[i,,drop=F], rms[,i,drop=F], rsd[,i,drop=F] )
  }
  if(val) return(max(d, na.rm=T))
  if(!val){
    checks = !(rowSums(d>=lim) > 0) # returns T if is ok, ie no peaks
    return(checks)
  }
}; peakdetection2=cmpfun(peakdetection2, options=list(optimize=3))

peakdetection3 = function(d,lim){
  mx=apply(d,1,max)
  me=apply(d,1,median)
  return(mx < (lim*me))
}; peakdetection3=cmpfun(peakdetection3, options=list(optimize=3))


do_competition_model = function(
  FOLDER="",  # folder containing folder of parsed periods
  fpath=file.path(FOLDER, "freqmat.RData"),   # location of the RData containing freqmat
  periodfolder = "cohaperiods",   # name of folder of parsed periods
  savefile = "cohamodel.RData",          # name RData file to save results into (incl RData suffix)
  minc=100,     # minimum frequency in t2
  minc2=100,    # minimum frequency for other words (minc >= minc2 !)
  miny=81    ,  # lowest year/leftmost column in freqmat to use; 81=1890 for coha
  plen=10    ,     # length of target period in years
  minchange=2,   # min inter-decade log change for word to be considered target
  min_non0=3 ,   # min n of years where target occurs; must be <= plen
  maxsd=Inf  ,   # max sd in yearly counts; Inf=disabled; small value=less spiky ones
  topsimrange=2, # which top neighbors to use to calculate density proxy
  corpusbounds=c(1,200), # number of years in corpus; vector of length 2, c(1, max)
  ntopicwords=75,        # how many topic words to use for the advection model
  minneibyears=5,    # how many years neighbour must occur to be considered
  stages = c("targets", "sem", "prob"),  # allows running only parts of the model (needs objects in global)
  save=T,         # save to RData? if T, will pull from and save to Global env; for interactive use
  skiplsa=F,       # skip semantics (if already present)
  minraw=NULL,     # extra parameter for twitter target finder, since larger corpus
  # all these are for the twitter model:
  twitter=F,     # much larger corpus and longer timespans in units, use simpler peak detection; maxsd param is used for median multiplier instead
  spread=NULL,    # matrix of daily dispersion among users
  spreadmin=10,    # normalized median daily users threshold
  spreadmax=50,    # must have at least one day with that many (normalized) unique users
  singleoccurrence = 1,  # what counts as minimal occurrence; could increase for twitter
  round_y = F           # optimize the number of models a bit to speed up, round start of series to nearest 10, makes little difference since just a few days off in what is usually the low-freq part of the series anyway
){
  if(length(stages)<3) save=F  # doesn't save/overwrite model if doing partial tasks
  if(!is.null(fpath) | exists("freqmat")) load(fpath)    # load freqmat if needed
  
  # in case doing part of the model only:
  #wy=NULL; ylist=NULL; neibs_adv_diss=NULL; problist=NULL
  
  if("targets" %in% stages){
    print(paste( Sys.time(), "starting; looking for target words"))
    # do changes matrix; new: do all first, and subset later, instead of messing with subsets in loop
    
    yearsfreqs=freqmat[rowSums(freqmat)>=minc,,drop=F]
    if(twitter){
      yearsfreqs = yearsfreqs[rownames(yearsfreqs) %in% rownames(spread),] # first filter this
      spread = spread[rownames(yearsfreqs),]  # then align both
      spread[yearsfreqs==0] = NA # only check spread on days where actually occurs
      spread = as.matrix(spread) # make dense, will be doing lots of subsetting and 0s now NAs anyway
      # normalize spread matrix - daily user count varies! -> now using ratio
      #   cs = colSums(spread, na.rm = T)
      #   css = median(cs)
      #   spread = spread %*% diag(1/cs)
      #   spread = spread*css  
      spreadratio = spread / yearsfreqs
    }
    decmat = yearsfreqs; decmat[] = NA
    for(i in miny:(ncol(yearsfreqs)-(plen-1)) ){
      y1 = yearsfreqs  %>% binfreqmat(list( (i-plen):(i-1) ), 
                                      smooth1 = F, permillion = T, sums=attr(freqmat, "thesums"))
      y2 = yearsfreqs  %>% binfreqmat(list(  i:(i+plen-1)  ), 
                                      smooth1 = F, permillion = T, sums=attr(freqmat, "thesums"))
      decmat[,i] = dofreqdif(cbind(y1,y2), ones=c(attr(y1, "ones"),attr(y2, "ones")))[,2]
      #decmat[,i] = log( (y2) /(y1) )    
    }
    #if(!all(rownames(decmat)==rownames(yearsfreqs))) stop("mismatching matrices in target finder")
    
    # winners model ##
    print(paste( Sys.time(), "filtering target words"))
    wy = data.frame(y=rep(NA,nrow(decmat)), n=rep(0,nrow(decmat)), nth=rep(0,nrow(decmat)), change=rep(0,nrow(decmat)), row.names = rownames(decmat))
    
    for(i in miny:(ncol(decmat)-(plen-1)) ){ 
      ytmp = yearsfreqs[,i:(i+plen-1),drop=F]
      tmp=which(decmat[,i] >= minchange & 
                  ytmp[,1] >= singleoccurrence &     # ---> start t of increase period must be >0 too
                  rowSums(ytmp) >= minc &              # enough occurrences
                  rowSums(ytmp >= singleoccurrence) >= min_non0   # enough non-zero years
                # Do filtering here instead - if below, might exclude good examples
      ) 
      # no massive outlier peaks (uses raw counts though!)
      if(twitter){ 
        if(is.infinite(maxsd)){
          pks = 1:nrow(ytmp)
        } else {
          pks = which(peakdetection3(ytmp, lim=maxsd))
        }
        #spreadok = which(apply(spread[,i:(i+plen-1)],1, function(x) max(x)>=spreadmin ))
        #spreadok = which(apply(spread[,i:(i+plen-1)],1, function(x) median(x)>=spreadmin ))
        # spreadok = which(apply(spread[,i:(i+plen-1)],1, function(x) max(x,na.rm=T)>=spreadmax ) &
        #   apply(spread[,i:(i+plen-1)],1, function(x) median(x,na.rm=T)>=spreadmin ) )
        spreadok = which(apply(spread[,i:(i+plen-1)],1, function(x) max(x,na.rm=T)>=spreadmax ) &
                           apply(spreadratio[,i:(i+plen-1)],1, function(x) median(x,na.rm=T)>=spreadmin ) )
        pks = intersect(pks, spreadok )
      } else {
        pks = which(peakdetection2(ytmp, lim=maxsd))
      }
      tmp = intersect(tmp, pks)
      f = ifelse(decmat[tmp,i] > wy$change[tmp], T,  F ) # keep biggest change
      wy$y[tmp][f] = i
      wy$n[tmp] = wy$n[tmp]+1
      wy$nth[tmp][f] = wy$nth[tmp][f]+1
      wy$change[tmp][f] = decmat[tmp,i][f]
    }
    wy=wy[wy$nth>0 ,]
    wy$f2 = NA; wy$f2sd=NA; wy$not0 = NA;wy$peaks=NA
    for(i in 1:nrow(wy)){
      wy$f2[i] = sum( yearsfreqs[rownames(wy)[i],(wy$y[i]):(wy$y[i]+ (plen-1) ) ]  )
      #wy$f2sd[i] = sd( yearsfreqs[rownames(wy)[i],(wy$y[i]):(wy$y[i]+ (plen-1) ) ]  )
      wy$not0[i] = length(which( yearsfreqs[rownames(wy)[i],(wy$y[i]): (wy$y[i]+(plen-1) ) ] >= singleoccurrence ))
      wy$peaks[i] = yearsfreqs[rownames(wy)[i],(wy$y[i]):(wy$y[i]+(plen-1) ),drop=F] %>% peakdetection2(val=T)
    }
    # further filters: (-> now done in the frequency matrix loop instead)
    # wy=wy[
    #   wy$not0>=min_non0 & 
    #     wy$f2>=minc & 
    #     wy$y>=miny #&
    #    # wy$f2sd<maxsd
    #   ,]
    wy$word = rownames(wy)
    if(twitter){
      wy$spread = NA
      for(s in 1:nrow(wy)){
        wy$spread[s] = median(spreadratio[wy$word[s],wy$y[s]:(wy$y[s]+plen-1)],na.rm=T)
      }
    }
    wy = wy[order(wy$y),]
    if(round_y){
      wy$yactual = wy$y
      wy$y = round(wy$y,-1)
      wy$y = ifelse( wy$y<miny, ceiling(miny/10)*10,  wy$y)
      wy$y = ifelse( wy$y>(corpusbounds[2]-plen-1), floor((corpusbounds[2]-plen-1)/10)*10,  wy$y)
    }
    if(!save) wy <<- wy
    print(paste( Sys.time(), "found", nrow(wy), "targets; starting models"))
  }
  
  if("sem" %in% stages){
    ylist = findperiods(wy, plen, plen)
    neibs_adv_diss = do_ylist_winners(ylist, freqmat,WINSIZE=2, WINSIZE_ADV=10, WINSIZE_DISS=2, minc=minc2, smoothsem=F, foldr=file.path(FOLDER, periodfolder), corpusbounds=corpusbounds, ntopicwords=ntopicwords, skiplsa=skiplsa)
    if(!save) {neibs_adv_diss <<- neibs_adv_diss; ylist <<- ylist}
    print(paste( Sys.time(), "semantic models done, starting competition model"))
  }
  
  if("prob" %in% stages){
    problist = do_probmass_neibs_list(ylist, neibslist=neibs_adv_diss$neiblist, rels=neibs_adv_diss$xrels, freqmat=freqmat, semchange=NULL, rand=F, losermodel=F, ntopsim = topsimrange, xmeans = neibs_adv_diss$xmeans, xsds = neibs_adv_diss$xsds, ntopicwords=ntopicwords, minneibyears=minneibyears, twitter=twitter )
    if(!save) problist <<- problist
  }
  print(paste( Sys.time(), "all done"))
  if(save){
    save(wy,ylist,neibs_adv_diss, problist, file=file.path(FOLDER, savefile) )
  } else{
    return(NULL)
  }
}; do_competition_model=cmpfun(do_competition_model, options=list(optimize=3))




fixfreqs_est = function(periodlist, splitlow=F, someH=F, ssplit=T, ssep="</s>"){
  library(text2vec)
  alldocs = unlist(periodlist, use.names = F, recursive = F)
  if(someH){
    # perioodi top pärisnimed sisse, muu siluda
    h = grep("^[hH]:", alldocs, value = F) # et kui lowercase siis ikkagi töötaks
    hl = length(h)
    h1 = sort(table(alldocs[h]), decreasing = T)[101:hl] 
    #[1:round(length(h)*1/100)] # 1% on liig suure peal 
    alldocs[which(alldocs %in% names(h1))] = "H:PN.."
  }
  
  if(splitlow){
    # Ühe korra vaatab välja madala sagedusega liitsõnad ja haagib need lahti, lõpp alles:
    it = itoken(alldocs, progressbar = FALSE)
    vocab <- create_vocabulary(it, ngram = c(ngram_min = 1L, ngram_max = 1L)) 
    #vocab <- prune_vocabulary(vocab, term_count_min = 1L, doc_proportion_max=1) 
    comp = grep("^[^Hh]:[^_]*_", vocab$vocab$terms)
    low = which(vocab$vocab$terms_counts < 10) 
    lowcomp = vocab$vocab$terms[intersect(comp, low)] 
    prunedcomp = gsub("^(.:)([^_]*[_])*(.*)", "\\1\\3", lowcomp) # asendused
  }
  
  if(splitlow){
    alldocs = factorreplace(lowcomp, prunedcomp, alldocs)
  }
  # lausestamine: iga lause = 1 doc, akna piirid ei lähe üle lause
  if(ssplit){
    s = which(alldocs == ssep)
    fi = findInterval(seq_along(alldocs), s)
    fi[s] = max(fi)+1
    periodlist2 = split(alldocs, fi)
    periodlist2[length(periodlist2)] = NULL # lausevahede vektor minema
    periodlist2[which(sapply(periodlist2, length) < 2)] = NULL # ühesõnalised laused
  } else {
    periodlist2 = list( alldocs)
  }
  return(periodlist2)
}


fixcorpus_cz = function(
  y=2003, 
  path="",
  wordclasses=""
){
  # proper nouns not tagged but retain capitalization in lemma
  
  words = fread(file.path(path, paste0(y, ".txt")), sep="\t", quote = "",header = F,encoding = "UTF-8")
  isdoc = grep("^<doc id", words[[1]])
  words[[2]][isdoc] = "<doc>"
  words[[3]][isdoc] = "<doc>"
  ok = grep(wordclasses, words[[3]], value=F)
  ok = setdiff(ok, 
               grep("^[AÁBCČDĎEÉĚFGHCIÍJKLMNŇOÓPQRŘSŠTŤUÚŮVWXYÝZŽ]",words[[2]])
  )
  words = words[[2]][ok]
  words = 
    words %>%
    gsub("[01234567890_'.,= -]", "", .)%>%  
    tolower() 
  
  shorts = which(nchar(words)<3)   # kui markereid pole siis lihtsalt pikkus
  if(length(shorts) > 0) {
    words = words[-shorts] # lühikesed sõnad (selle miinusega peab ettevaatlik olema)
  }
  
  # 
  stopwrds = c(
    "a", "aby", "aj", "ale", "ani", "aniž", "ano", "asi", "až", "bez", "bude", "budem", "budeš", "by", "byl", "byla", "byli", "bylo", "být", "co", "což", "cz", "či", "článek", "článku", "články", "další", "dnes", "do", "ho", "i", "já", "jak", "jako", "je", "jeho", "jej", "její", "jejich", "jen", "jenž", "ještě", "ji", "jiné", "již", "jsem", "jseš", "jsme", "jsou", "jšte", "k", "kam", "každý", "kde", "kdo", "když", "ke", "která", "které", "kterou", "který", "kteři", "ku", "ma", "máte", "me", "mě", "mezi", "mi", "mít", "mně", "mnou", "můj", "může", "my", "na", "ná", "nad", "nám", "napište", "náš", "naši", "ne", "nebo", "nechť", "nejsou", "není", "než", "ní", "nic", "nové", "nový", "o", "od", "ode", "on", "pak", "po", "pod", "podle", "pokud", "pouze", "práve", "pro", "proč", "proto", "protože", "první", "před", "přede", "přes", "při", "pta", "re", "s", "se", "si", "sice", "strana", "své", "svůj", "svých", "svým", "svými", "ta", "tak", "také", "takže", "tato", "te", "tě", "tedy", "těma", "ten", "tento", "této", "tím", "tímto", "tipy", "to", "to", "tohle", "toho", "tohoto", "tom", "tomto", "tomuto", "toto", "tu", "tuto", "tvůj", "ty", "tyto", "u", "už", "v", "vám", "váš", "vaše", "ve", "více", "však", "všechen", "vy", "z", "za", "zda", "zde", "ze", "zpět", "zprávy", "že",
    "about","above","after","again","against","because","been","before","being","below","between","both","could","does","doing","down","each","from","further","have","having","hed","hell","hes","here","heres","hers","hows","ill","ive","into","its","lets","more","most","once","only","other","ought","ours","over","same","shed","shell","shes","should","some","such","than","that","thats","their","theirs","them","then","there","theres","these","they","theyd","theyll","theyre","theyve","this","those","through","under","until","very","wed","well","were","weve","were","what","whats","when","whens","where","wheres","which","while","whos","whom","whys","with","would","youd","youll","youre","youve","your","yours","dont","aint","cant","the","but","be","is","was","we","me","his","her","you","our","us","my","she","he","or","of","to","are","has","all","and","did","it","for","any","who","so","do","by","thi","go","got","get","also","maybe","no","yes","if","how","as","at","didnt","rate","agr","i","myself","ourselves","yourself","yourselves","him","himself","herself","its","itself","themselves","am","had","im","id","isnt","arent","wasnt","werent","hasnt","havent","hadnt","doesnt","wont","wouldnt","shant","shouldnt","cannot","couldnt","mustnt","a","an","during","up","in","out","on","off","why","few","nor","not","own","too","will")
  isstop = which(words %fin% stopwrds) # fastmatch
  if(length(isstop)>0){
    words=words[-isstop]
  }
  
  return(words)
}

fixcorpus_est = function(
  y=2003, 
  path="",
  wordclasses="_[ACDGHIKPSUVNO]_|</s>", 
  nodash = T,
  splitall=F,
  lns=-1, # loe kõik?
  nomarkers =T
){
  
  # miski sööb sõna (algustest?) katustega tähti???? samas ka ehhi_maa ja ijoone, samas S:stažeerimine_60 ok
  
  library(text2vec)
  library(magrittr)
  
  words = readLines(file.path(path, paste0(y, ".txt")), lns, encoding = "UTF-8")
  words = grep(wordclasses, words, value=T) # _[JYZ]_
  # tundub et varasemates, ise morfitud failides on olnud -> ol+nud, uuemas ole+nud...
  words = gsub("ol=nud.0 //_A_ //", "ole+nud //_V_ nud, //", words) # vanemad/isemorfitud on halvemini ühestatud, seega häkk
  #    lisaks see ol= vs ole+ on mõttetu eristus nagunii, ja allpool kaotan sõnaklassid ära
  words = gsub("    ", "@@@@", words)
  words = gsub("[= ]", "", words) # tuletusmärgid ära, tühikud maha (kaheosalised pärisnimed!)
  
  #2019:
  nums=grep("_[NO]_ \\?", words) # mittesõnalistel on küsimärk käände slotis!
  if(length(nums)>0){
    words = words[-nums]  # numbrid numbritena välja; sõnalised numbrid jätan sisse
  }
  #
  
  words = gsub("^[^@]*@@@@([^+]*)+[^/]*//_(.)_.*", "\\2:\\1", words) # urgib lemma ja klassi välja
  #######    peaks üldse eemaldama liitsõnamärgid? samas pärast mudelis läheks tarvis teada, et kas esineb liitsõna osana...
  
  
  #for(word in 1:length(words)){
  #  if(length(grep("^[NO]:|perc[e]*nt", words[word]))>0){
  #    words[word] = "N:NO.."
  words = 
    words %>%
    gsub("[0-9]+_*", "", .)     %>%  # number-sõna kombodest number välja
    gsub("^(V:.*)", "\\1ma", .) %>%  # verbidele inf otsa (ei on juba maha võetud)
    tolower()                   %>%  # kõik väiketähestada; all matchib ^.: seega ok
    gsub("zh", "ž", .)          %>%
    gsub("sh", "š", .)          %>%  # nagunii kirjavead, sama hästi võib ühtlustada
    gsub("[öõ]", "ó", .)     # homogeniseerida õ/ö, ports kirjavigu kus lihtsalt õ asemel ö
  
  # gsub("^[^S]:", "", .)   # if(onlyS){ # muud klassid vs S, võtab klassimarkeri muudelt ära
  #gsub("^[NO]:|perc[e]*nt", "N:NO...", .) %>%  # kõik numbrid ja arvsõnad -> hoopis nüüd: arvud välja, aga sõnalised arvud sisse
  
  if(nomarkers){  # 2019: variant üldse ilma sõnaklassimarkeriteta
    words = gsub("^.:", "", words)   # 
  }
  
  
  
  if(nodash){
    words = gsub("[-_'.,]", "", words) # üldse maha
  } else {
    words = gsub("-", "_", words)   # liitsõnamärgid samaks, varieerub nagunii
  }
  
  #shorts = grep("^(.:)*.{0,2}$", words)
  shorts = which(nchar(words)<3)   # kui markereid pole siis lihtsalt pikkus
  if(length(shorts) > 0) {
    words = words[-shorts] # lühikesed sõnad (selle miinusega peab ettevaatlik olema)
  }
  if(splitall){
    words = gsub("^(.:)([^_]*[_])*(.*)", "\\1\\3", words) #  võtab liitsõnade esimese poole ära
  }
  
  # 2019:
  # lisaks võtab välja inglisekeelsed, mida on ka natuke korpuses:
  stopwrds = c( "eima","olema","saama","kui","kus","kas","ära","mitte","üle","juba","siis","isegi","ikka","jälle","ette","seal","läbi","enam","küll","siin","veel","vóima","nagu","nii","üks","pidama","või","tóttu","vói","kóik","vóib","näi","sónu","about","above","after","again","against","because","been","before","being","below","between","both","could","does","doing","down","each","from","further","have","having","he'd","he'll","he's","here","here's","hers","how's","i'll","i've","into","it's","let's","more","most","once","only","other","ought","ours","over","same","she'd","she'll","she's","should","some","such","than","that","that's","their","theirs","them","then","there","there's","these","they","they'd","they'll","they're","they've","this","those","through","under","until","very","we'd","we'll","we're","we've","were","what","what's","when","when's","where","where's","which","while","who's","whom","why's","with","would","you'd","you'll","you're","you've","your","yours","don't","ain't","can't","the","but","be","is","was","we","me","his","her","you","our","us","my","she","he","or","of","to","are","has","all","and","did","it","for","any","who","so","do","by","thi","go","got","get","also","maybe","no","yes","if","how","as","at","didn't","umber","bin","abu","septembr","rate","agr","nm/p/min","i","myself","ourselves","yourself","yourselves","him","himself","herself","its","itself","themselves","am","had","i'm","i'd","isn't","aren't","wasn't","weren't","hasn't","haven't","hadn't","doesn't","won't","wouldn't","shan't","shouldn't","cannot","couldn't","mustn't","a","an","during","up","in","out","on","off","why","few","nor","not","own","too","will")
  isstop = which(words %fin% stopwrds) # fastmatch
  if(length(isstop)>0){
    words=words[-isstop]
  }
  
  
  # t3 = gsub("^.:[A-ZÜÕÖÄŽŠ]{3,}$", "H:PN..", t3)
  #t3 = gsub("^.:","", t3) # sõnaklassid minema (pole väga informatiivne, ja morf ajab sassi vahel)
  #t3 = t3[-grep("^.:.{0,2}$", t3)] # lühikesed sõna sh ei+0 (samas pron. on pikemas vormis lemmatiseeritud!)
  ## [_-=]|mine$|ja$|lisus$|likkus$|
  
  return(words)
}



modelstats = function(
  # cite Model Comparisons and R2 Anderson 1994
  response= "simequalnorm",
  interest = "advection",
  controls = "" ,
  dat2=dat
){
  m1 = lm(as.formula(paste(response, "~", interest)),data=dat2)
  mf = lm(as.formula(paste(response,"~",interest,controls)) , data = dat2)
  mr = lm(as.formula(paste(response,"~",controls)) , data = dat2)
  r2plus =  
    1-(
      (1-summary(mf)$adj.r.squared) /
        (1-summary(mr)$adj.r.squared)
    )
  r21=summary(m1)$adj.r.squared
  r2full=summary(mf)$adj.r.squared
  p = anova(mr,mf)$`Pr(>F)`[2]
  n = paste0(nobs(mf),"/",nrow(dat2))
  coe = as.data.frame(summary(mf)$coefficients[,4,drop=F] %>% round(3))
  coe$s = ifelse(coe[,1]<0.05,"*", " ")
  print(coe)
  print(paste0("R2univ=",round(r21,3), 
               "  R2full=",round(r2full,3),
               "  R2+=",round(r2plus,3), 
               "  p=", round(p,3), 
               "  n=",n),quote=F)
  return(round(r2plus,2))
}

dodat=function(corp="coha-multiword"){
  dat = data.frame(
    word      = wy$word,
    not0      = wy$not0,
    freqraw   = wy$f2,
    peaks     = wy$peaks,
    #isnew     = as.factor(problist$isnew),
    #rankequal = problist$pmranks,
    simequal  = problist$pmsims,
    simequalnorm = 1-problist$pmsimsnorm,   # sim->dist
    simequalscaled = problist$pmsimsscaled, # already dist
    advection = neibs_adv_diss$adv,
    #advres  = neibs_adv_diss$advres,
    lexdissem = neibs_adv_diss$diss,
    topsim    = problist$topsims,
    ndown     = problist$ndown,
    nup       = problist$pmposranks,
    leftover  = problist$leftover,
    minloserpercent = problist$minpercentlosers,
    freqchange= problist$freqchange,
    freqchangeabs= problist$freqchangeabs,
    percentchange = problist$percentchange,
    freqsd    = problist$freqsd,
    wordage   = {(wy$y - apply(freqmat[wy$word,], 1, function(x) which(x>0)[1])) %>% replace(.<0, 0)},
    logfreqt1 = log(problist$freqf1),
    logfreqt2 = log(problist$freqf2),
    minstringd= problist$minstringd,
    stringcontained=problist$stringcontained,
    topeditneib=problist$topeditneib,
    spellcomm  =problist$spellcomm,
    wordlen   = nchar(names(problist$pmranks)),
    # denschange= sapply(neibs_denss[[2]], function(x) x["dif"]),
    # dens0 = sapply(neibs_denss[[2]], function(x) x["d0"]),
    # denschangeraw = sapply(neibs_denss[[2]], function(x) x["raw"]),
    losers = problist$losers,
    #maxsemchange=problist$maxsemchange,
    #meansemchange=problist$meansemchange,
    topneib=problist$topneib,
    stringsAsFactors = F
  ); 
  # "coha-multiword", "twitter", "est", "dta","czech"
  if(corp=="coha-multiword"){ dat$year = year(wy$y) } # coha
  if(corp=="est"){ dat$year =  as.numeric(colnames(freqmat)[wy$y])   }
  if(corp=="dta"){ dat$year =  yearger(wy$y) }
  if(corp=="czech"){ dat$year =  as.numeric(colnames(freqmat)[wy$y])   }
  if(corp=="twitter"){  
    dat$year = as.Date(colnames(freqmat)[wy$y])
    dat$spreadratio=wy[rownames(dat), "spread"]
    dat = dat[dat$advection<1.5,] # remove 2 outliers (both suspiciously polical hashtags anyways)
  }
  dat = dat[which(dat$leftover<1  
                  # & dat$lexdissem<0.15
                  #& dat$not0>=9
                  #& dat$year>=1800
  ),]; print(nrow(dat))
  return(dat)
}




resultsplot = function(dat, main=T, titl, formula=""){
  if(main){
    d2 = dat[c("airplane","funding", "aids", "famed", "radio", "internet","bomber"), ]
    d2$nx=c(0.12,0.08,0.1, rep(0.1,3),0)
    d2$ny=c(-0.14,-0.19,-0.1, rep(0.2,2),0.5,0.4); 
  }
  if(grepl("witter", titl)){
    frm= paste0(formula, "+ spreadratio")
  } else {
    frm= formula
    #"+ topsim + freqchange + leftover + minloserpercent + minstringd + wordlen + peaks + as.numeric(year)"
  }
  r2 = modelstats( response= "simequalnorm", interest = "advection",
                   controls = frm, 
                   dat2 = dat
  )
  
  g = ggplot(dat, aes(y=advection, x=simequalnorm
                      #, text=paste0(word," ",year, "\n",topneib,"\n", losers) # for interactive
  )) + 
    geom_point(size=ifelse(main,2.5,1), color="#C4B56CFF", alpha=1,shape=16) + # scales::viridis_pal(option="E")(10)[8]
    #    geom_text(aes(label=word)) +
    geom_smooth( method="lm",color="black", size=ifelse(main,1,0.5), alpha=0.1) +
    scale_color_viridis_c(option="E", direction = -1, end=1, name="full model\nresiduals",
                          # limits=c(0,0.31)
    ) +
    geom_label(data=data.frame(x=-Inf,y=Inf,l=titl), mapping=aes(x=x,y=y,label=l), vjust=1,label.size = NA, size=ifelse(main,5,4)  ,
               hjust=0, label.padding = unit(0.15, "lines"), inherit.aes = F) +
    annotate("label", x=Inf,y=-Inf,  
             label=as.character(as.expression(substitute(italic(R)^2~"="~r2))), 
             vjust=0,label.size = NA,label.padding = unit(0.1, "lines"), size=ifelse(main,4,3.5) , 
             hjust=1, parse=T) +
    #coord_cartesian(x=c(-0.1,1.25)) +
    scale_size(guide=F) +
    scale_x_continuous(expand=c(0.01,0)) +
    scale_y_continuous(expand=c(0.05,0)) +
    theme_bw() +
    theme(legend.position = c(0.5,0.5)) +    # c(0.1,0.8)
    theme(axis.title = element_text(hjust = 0, vjust=0.5)) +
    theme(
      panel.border = element_rect(color="darkgray",fill=NA), 
      axis.ticks = element_line(color="darkgray"), 
      strip.background = element_blank()
      #strip.text = element_blank(),
    ) 
  # theme(axis.ticks.length=unit(-0.15, "cm"), 
  #       axis.text.x = element_text(margin=unit(c(0.5,0.5,0.5,0.5)*0.6, "cm"), hjust=0), 
  #       axis.text.y = element_text(margin=unit(c(0.5,0.5,0.5,0.5)*0.6, "cm"))  )
  if(main){
    g = g+
      geom_label_repel(data=d2, aes(label=paste0(word, "\n(", year,")") ),
                       color="black", hjust=1, vjust=1, size=4, lineheight=0.8, fill=rgb(1,1,1,0.6),
                       min.segment.length = 0, nudge_y = d2$ny,nudge_x = d2$nx ,
                       label.size = NA, segment.color="gray50") +
      labs(y="Advection (mean topic change for target)",
           x="Equalization range (normalized cosine distance from target)") +
      annotate("text", x=c(0, 0.25), y=c(-0.25), color="gray30", hjust=0,vjust=1,
               label=c("< clear competition",
                       " no clear competition >") ) +
      annotate("text", x=c(-0.06), y=c(-0.15,0.5), color="gray30", hjust=0,vjust=1,angle=90,
               label=c("Neutral topic",
                       "Increasing topic >\n(increased comm.need)") ) 
  } else {
    if(max(dat$advection>3)){  # german, different values
      yl=c(0,3.5)
      xl=c(0,0.5)
      br=c(0,1,2,3)
    } else {
      yl = c(-0.2,1.25); br=c(0,0.5,1)
      xl = c(0,0.4)
    }
    g = g + theme(axis.title = element_blank(),
                  axis.text = element_text(size=rel(0.8)) 
    ) +
      scale_x_continuous(expand=c(0.02,0), limits=xl) +
      scale_y_continuous(expand=c(0,0), limits=yl, breaks = br) 
  }
  g
  
} # %>% ggplotly()


