## targeter

## targeter 1.0.0 

### Some informtion 

* This is a new release: first public version

* No specific references available for that package, I might add some generic 
  references about Information Value usage if required.
  The variant for numeric targets is a creation of mine, work in progress 
  to analyze it (hopefully a student's master thesis so not for tomorrow...) 


### Notes review 
> checking CRAN incoming feasibility ... NOTE
Maintainer: 'Eric Lecoutre <eric.lecoutre@welovedatascience.com>'

--> aka ericlecoutre@gmail.com, author R2HTML package

> checking R code for possible problems ... NOTE

--> all due to usage of data.table syntax, for instance dt[,..selvars]


(r-hub)>  Possibly misspelled words in DESCRIPTION:
    EDA (11:51)

--> acronym explained in DESCRIPTION.

(r-hub)❯ checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

--> no idea what it is about

(r-hub)❯ checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

--> no idea what it is about

<rhub>> checking examples ... [37s/52s] NOTE
Examples with CPU (user + system) or elapsed time > 5s
                  user system elapsed
summary.targeter 9.451  0.057  13.799
focus            9.231  0.205  13.599
targeter         8.588  0.019  12.220

--> occured only once and seems acceptable for me, I could pass them as \dontrun