# Script for the lexical competition model described in
# Karjus et al 2020, "Communicative need modulates competition in language change"
# Code: Andres Karjus, https://andreskarjus.github.io/


# Install any missing packages as needed:
install.packages("text2vec")
install.packages("compiler")
install.packages("grr")
install.packages("parallel")
install.packages("Matrix")
install.packages("tidyverse")
install.packages("fastmatch")
install.packages("ggrepel")
install.packages("patchwork")
install.packages("scales")
install.packages("colorspace")
install.packages("rms")
install.packages("stringdist")
install.packages("stringr")
install.packages("data.table")
install.packages("MASS")
install.packages("caret")


#### Define paths ####
SCRIPTPATH = ""  # full path to lexical_competition_functions.R
FOLDER = ""      # full path to a folder where intermediate steps will be saved
CORPUS = ""      # full path to the corpus, ...COHA/wlp in the case of COHA
PHRASES = ""     # optional, full path the an RData file with phrases for multiword concatenation (as a list called "phrases" where each element is a character vector of length 2)
FREECORES = 3    # how many cores to leave free in parallel processing; adjust according to number of your cores; with 4 cores and 16GB RAM 2-3 works; if lots of RAM, can be 0.


#### Parse a corpus; here on the example of COHA ####

source(SCRIPTPATH) # load functions

if(PHRASES!=""){load(PHRASES)}else{phrases=NULL}

# Find COHA corpus files and collapse into years
# theperiods needs to be list, elements are vectors of file paths, by year, 
# so list of length 200 for COHA
allfiles =  list.files(CORPUS, full.names = F, recursive = T) #116614
theperiods=list()
for(i in seq(1810,2009,by = 1)){
  theperiods[[as.character(i)]] = grep(paste0("_",i,"_"), allfiles, value = T)
  names(theperiods[[as.character(i)]])[1] = as.character(i)
}
rm(allfiles)

# this may take a few hours:
freqmat = docorpus_parallel(theperiods,
                            nfree=FREECORES,      
                            markS = list(N=F,V=F, A=F,D=F,M=F,P=F), 
                            minlen=3, 
                            path=CORPUS, 
                            saveyear=T,    # use folder decade/file year as filename
                            rmstopwords=T, # remove stopwords?
                            removenp=F,    # remove proper nouns by removing all Capitalized?
                            byyear=T,      # do by year instead of decade; theperiods needs to be list where each element is a vector of year files, with full paths
                            #onlyvocab=F, # only frequencies (not functional)
                            untouchables = NULL,
                            FOLDER = FOLDER,
                            periodfolder="cohaperiods",
                            domultiword = ifelse(is.null(phrases),F,T),  # concatenate multiwords?
                            phrases = phrases, # quanteda list of phrases
                            skipperiods = F #
)
save("freqmat", file=file.path(FOLDER, "freqmat.RData" ))


#### Run model ####

# this may take a few hours to a day:
do_competition_model(
  FOLDER=FOLDER,  # folder containing folder of parsed periods
  fpath=file.path(FOLDER, "freqmat.RData"),   # location of the RData containing freqmat
  periodfolder = "cohaperiods",   # name of folder of parsed periods
  savefile = "cohamodel.RData",   # name RData file to save results into (incl RData suffix)
  minc=200,      # minimum frequencyfor target in t2
  minc2=100,     # min for other words
  miny=81    ,  # lowest year/leftmost column in freqmat to use; 81=1890 for coha
  plen=10  ,     # length of target period(s) in years
  minchange=2,   # min inter-decade log change for word to be considered target
  min_non0=8 ,   # min n of years in second period where target occurs; must be <= plen
  maxsd=10 ,     # max peak sd
  corpusbounds=c(1,200), # number of years in corpus; vector of length 2, c(1, max)
  ntopicwords=75,        # how many topic words to use for the advection model
  minneibyears=5,    # how many years neighbour must occur to be considered
  topsimrange = 1,
#  stages="prob",     # to redo any phase on its own, see source
  save=T
)


#### Results ####

load(file.path(FOLDER,  "cohamodel.RData")); load(file.path(FOLDER, "freqmat.RData"))

dat = dodat(corp = "coha-multiword")

modelstats( response= "simequalnorm", interest = "advection",
            controls = "+ topsim + freqchangeabs + leftover + minloserpercent + minstringd + wordlen + peaks + as.numeric(year)"
)

resultsplot(dat, main=T, titl="COHA results", 
            formula =  "+ topsim + freqchangeabs + leftover + minloserpercent + minstringd + wordlen + peaks + as.numeric(year)")



