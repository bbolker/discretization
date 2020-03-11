# To do

- add: @betini_why_2017, *Why are we not evaluating multiple competing hypotheses in ecology and evolution?*
- from @dormann_model_2018 :

> We also investigate the quality of the confidence intervals calculated for model‐averaged predictions, showing that they differ greatly in behaviour and seldom manage to achieve nominal coverage. Our overall recommendations stress the importance of non‐parametric methods such as cross‐validation for a reliable uncertainty quantification of model‐averaged predictions.

- CrossValidated post on "no free lunch" principle for coverage of shrinkage estimators?
    - early results suggest no CI advantage for e.g. ridge regression
	- Turek results show undercoverage
	- ... but Burnham et al. get 'adequate' coverage
- More sims?
- Why would we expect different (better/worse) performance from penalized regression vs MA?  (What are their loss functions?)
- check Kabaila et al quote

- ideas for the talk:
    - what do we want to do? (predict, infer, test, quantify importance ...)
	- importance measures: AIC weights, variance explained, R^2, scaled variance, ...
	- examples of people using and misusing discrete comparisons: Luttbeg et al., Ponciano and Taper model averaging, evidentiary stats, etc.
	- examples of people taking discrete models seriously?
	- thinking about *what* before thinking about *how*

## Sims

- Check the intercept is percolating correctly (there's a place where we do cbind(0, …) which is probably insufficiently principled
- Embed sims in pipeline
- Improve colors, add more shapes
- 

## ISEC notes/thoughts

- say something/more about "what should we do then?"
- clarify prediction/inference dichotomy if necessary
- toot Harrell's horn
- maybe try out B&A sims
- some discussion of p-value/CI results under sparsity, tapering???
- correct anti-Galipaud ref is Giam and Olden 2015 MEE "Quantifying variable importance ..." also see more recent Galipaud et al paper ...


notes from twitter on model expansion: https://onlinelibrary.wiley.com/doi/abs/10.1111/j.2044-8317.2011.02037.x (Gelman and Shalizi B J Math Stat Phil 2012); https://onlinelibrary.wiley.com/doi/abs/10.1002/jwmg.891 (Ver Hoef and Boveng J Wild Man ? 2015); from F Rodriguez-Sanchez @frod_san on twitter)


From Cade 2015:

"Given the availability of valid statistical procedures for obtaining
shrinkage estimates, there is little reason to rely
on AIC model-averaged regression coefficients to provide
something similar to shrinkage estimates (Burnham
and Anderson 2002, Lukacs et al. 2010, Giudice et al. 2012),
especially given that these model-averaged estimates have no
defined units in the presence of multicollinearity"

@schomaker_when_2018
@fletcher_model_2018
@fletcher_model-averaged_2011
