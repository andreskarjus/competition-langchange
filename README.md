Code to run the lexical competition model (and topical advection model) on a diachronic corpus (like COHA), as described in Karjus et al 2020, "Communicative need modulates competition in language change".

Import functions are defined for all the corpora discussed in the paper. To run the model on your own corpus, just make sure the input files follow a similar format - data from a chosen unit of time (e.g. year for most diachronic corpora) in a single RData file, consisting of a list called "period" where each element is a character vector (e.g. a document).