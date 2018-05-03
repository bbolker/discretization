%.pdf: %.rmd
	echo "rmarkdown::render(\"$<\")" | R --slave

%.html: %.rmd
	echo "rmarkdown::render(\"$<\",output_format=\"html_document\")" | R --slave


## assumes in spinnable format?
%.html: %.R
	echo "rmarkdown::render(\"$<\")" | R --slave


