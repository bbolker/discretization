## Hooks

-include target.mk

###################################################################

# stuff

Sources += Makefile .gitignore
Ignore += .ignore

## You can change the location of makestuff in local.mk
msrepo = https://github.com/dushoff
ms = ./makestuff
Ignore += local.mk
-include local.mk
-include $(ms)/os.mk

Makefile: $(ms)

$(ms):
	cd $(dir $(ms)) && git clone $(msrepo)/$(notdir $(ms)).git
 
Ignore += $(ms)

######################################################################

## Content

Sources += $(wildcard *.R)
mmasim.html: mmasim.R

Sources += discrete.bib discrete.rmd
discrete.pdf: discrete.rmd

Sources += isec_abstract.md

Sources += README.md TODO.md

######################################################################

## Bolker rules

%.pdf: %.rmd
	echo "rmarkdown::render(\"$<\")" | R --slave

%.html: %.rmd
	echo "rmarkdown::render(\"$<\",output_format=\"html_document\")" | R --slave


## assumes in spinnable format?
%.html: %.R
	echo "rmarkdown::render(\"$<\")" | R --slave


######################################################################

-include $(ms)/visual.mk
-include $(ms)/git.mk

