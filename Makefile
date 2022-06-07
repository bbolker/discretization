## Hooks

-include target.mk

###################################################################

stockholm_nofreelunch.pdf: stockholm_nofreelunch.rmd discrete.bib
	Rscript -e "rmarkdown::render('stockholm_nofreelunch.rmd')"
	bash ./skip2 stockholm_nofreelunch

ares_sims.rda:
	R CMD BATCH --vanilla ares_sims.R

colloq_discretization.pdf:
	Rscript -e "rmarkdown::render('colloq_discretization.rmd')"


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

RCMD = Rscript --vanilla
######################################################################

## Content

Sources += $(wildcard *.R R/*.R)
mmasim.html: mmasim.R simdata/c.rds simdata/f.rds simdata/f20.rds simdata/f20.rds simdata/fzc.rds simdata/fnc.rds ## simdata/frc.rds 

## params: n_true n_reps avg_method n_full pcor ctype
simtest.rds: R/mmasim_batch.R
	$(RCMD) R/mmasim_batch.R test 20 10 mma 10 0.3 compsym 

simdata/c.rds: R/mmasim_batch.R
	$(RCMD) R/mmasim_batch.R c 20 300 mma 10 0.3 compsym

simdata/f.rds: R/mmasim_batch.R
	$(RCMD) R/mmasim_batch.R f 20 300 full 10 0.3 compsym

simdata/fnc.rds: R/mmasim_batch.R
	$(RCMD) R/mmasim_batch.R fnc 20 300 full 10 -0.05 compsym

simdata/f20.rds: R/mmasim_batch.R
	$(RCMD) R/mmasim_batch.R f20 20 300 full 20 0.3 compsym

simdata/fzc.rds: R/mmasim_batch.R
	$(RCMD) R/mmasim_batch.R fzc 20 300 full 10 0.0 zero

## simdata/frc.rds: R/mmasim_batch.R
##	$(RCMD) R/mmasim_batch.R frc 20 300 full 10 0.02 unif

Sources += discrete.bib discrete.rmd
discrete.pdf: discrete.rmd

Sources += isec_abstract.md

Sources += README.md TODO.md

######################################################################

## Bolker rules

%.pdf: %.rmd
	echo "rmarkdown::render(\"$<\",output_format=\"pdf_document\")" | R --slave

%.html: %.rmd
	echo "rmarkdown::render(\"$<\",output_format=\"html_document\")" | R --slave

%.slides.html: %.rmd
	echo "rmarkdown::render(\"$<\",output_format=\"ioslides_presentation\")" | R --slave

%.slides.pdf: %.rmd
	echo "rmarkdown::render(\"$<\",output_format=\"beamer_presentation\")" | R --slave

## assumes in spinnable format?
%.html: %.R
	echo "rmarkdown::render(\"$<\")" | R --slave


######################################################################

-include $(ms)/visual.mk
-include $(ms)/git.mk

