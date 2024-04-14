
-include target.mk

Sources += Makefile .gitignore
Ignore = makestuff

texclean:
	rm -f *.aux *.log *.bbl *.blg

###################################################################

stockholm_nofreelunch.pdf: stockholm_nofreelunch.rmd discrete.bib header.tex ares_sims.rda
	Rscript -e "rmarkdown::render('stockholm_nofreelunch.rmd')"
	bash ./skip2 stockholm_nofreelunch

ares_sims.rda:
	R CMD BATCH --vanilla ares_sims.R

colloq_discretization.pdf:
	Rscript -e "rmarkdown::render('colloq_discretization.rmd')"

######################################################################

RCMD = Rscript --vanilla
RRun = Rscript --vanilla $<

Sources += $(wildcard *.R R/*.R)
mmasim.html: mmasim.R simdata/c.rds simdata/f.rds simdata/f20.rds simdata/f20.rds simdata/fzc.rds simdata/fnc.rds ## simdata/frc.rds 

## params: n_true n_reps avg_method n_full pcor ctype
simtest.rds: R/mmasim_batch.R
	$(RRun) test 20 10 mma 10 0.3 compsym 

simdata/c.rds: R/mmasim_batch.R
	$(RRun) c 20 300 mma 10 0.3 compsym

simdata/f.rds: R/mmasim_batch.R
	$(RRun) f 20 300 full 10 0.3 compsym

simdata/fnc.rds: R/mmasim_batch.R
	$(RRun) fnc 20 300 full 10 -0.05 compsym

simdata/f20.rds: R/mmasim_batch.R
	$(RRun) f20 20 300 full 20 0.3 compsym

simdata/fzc.rds: R/mmasim_batch.R
	$(RRun) fzc 20 300 full 10 0.0 zero

## simdata/frc.rds: R/mmasim_batch.R
##	$(RRun) frc 20 300 full 10 0.02 unif

Sources += $(wildcard *.bib *.rmd)
Ignore += discrete.pdf
discrete.pdf: discrete.rmd body.md abstract.md discrete.bib

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

# stuff

msrepo = https://github.com/dushoff

Makefile: makestuff/00.stamp
makestuff/%.stamp:
	- $(RM) makestuff/*.stamp
	(cd makestuff && $(MAKE) pull) || git clone --depth 1 $(msrepo)/makestuff
	touch $@

-include makestuff/os.mk

-include makestuff/git.mk
-include makestuff/visual.mk
