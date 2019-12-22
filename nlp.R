#install.packages("openNLP")
library(rstudioapi)
library(rJava)
.jinit(parameters="-Xmx4g")
library(NLP)
library(openNLP)
library(openNLPmodels.en)
library(tm)
library(rJava)


#dyn.load('/Library/Java/JavaVirtualMachines/jdk-12.0.1.jdk/Contents/Home/jre/lib/server/libjvm.dylib')
source.pos = DirSource("./pos", encoding = "UTF-8")
corpus = Corpus(source.pos)
# I only import my document (cv493_12839.txt) to speed up the process.
length(corpus)


.libPaths()

# •       Add word, sentence and part-of-speech annotations
# •       Evaluate whether the POS annotation is correct
# –      Perform the evaluation manually
# –      Choose the document that matches the last three numbers of your identity card
# –      Analyse only the two first sentences of the document
# –      Only take into account complete and proper sentences
# –      Use the precision and recall metrics



#inspect(corpus[[1]])

meta(corpus[[1]])

tdm = TermDocumentMatrix(corpus)
tdm
freq=rowSums(as.matrix(tdm))
head(freq,10)




#doc=corpus[1]
#doc = tm_map(doc,removeWords,stopwords())
#doc = tm_map(doc,removePunctuation)
#doc = tm_map(doc,removeNumbers)
#doc = tm_map(doc,stripWhitespace)



getAnnotationsFromDocument = function(doc){
  x=as.String(doc)
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  y1 <- annotate(x, list(sent_token_annotator, word_token_annotator))
  y2 <- annotate(x, pos_tag_annotator, y1)
  parse_annotator <- Parse_Annotator()
  y3 <- annotate(x, parse_annotator, y2)
  return(y3)  
}

getAnnotatedMergedDocument = function(doc,annotations){
  x=as.String(doc)
  y2w <- subset(annotations, type == "word")
  tags <- sapply(y2w$features, '[[', "POS")
  r1 <- sprintf("%s/%s", x[y2w], tags)
  r2 <- paste(r1, collapse = " ")
  return(r2)  
}

getAnnotatedPlainTextDocument = function(doc,annotations){
  x=as.String(doc)
  a = AnnotatedPlainTextDocument(x,annotations)
  return(a)  
}


annotations = lapply(corpus, getAnnotationsFromDocument)

head(annotations[[1]], 20)

tail(annotations[[1]])

typeof(corpus)

tags  = Map(getAnnotatedPlainTextDocument, corpus, annotations)
tags[[1]]

taggedText = Map(getAnnotatedMergedDocument, corpus, annotations)
taggedText[[1]] 

doc = tags[[1]]

as.character(doc)

head(words(doc), 20)
head(sents(doc),3)
head(tagged_words(doc), 10)
head(tagged_sents(doc),2)
head(parsed_sents(doc),2)