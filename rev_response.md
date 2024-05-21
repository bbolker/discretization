
1) expose how information criteria are related to entropy.
2) express how information criteria relate to statistical evidence (sensu Lele 2004).
3) be aware of Ponciano & Taper (2019), which deals with entropy , statistical evidence, and model averaging. 



An interesting paper with some useful insights. I share a few thoughts and
critiques below.
Lines 11-12: Perhaps some rewording required here – (1) aren’t all models
artificial? Are you trying to say models of little interest?
Lines 13-16: Isn’t all shrinkage going to be dependent on sample size relative to
number of parameters in the model? Larger sample sizes relative to number of
parameters resulting in less shrinkage.
Lines 31-44: It seems as if you are equating multiple processes with multiple
variables in a model. Do your arguments still apply in a situation where multiple
variables are used to characterize one encompassing process? Or at least where
there is not a one-one mapping of processes and variables?
Line 66: Insert word “it”, “unequivocally that it has”.
Lines 72-77: One issue with only fitting the full model with all factors
simultaneously even with very large sample sizes, is that the magnitude of the
parameter estimates for each factor (variable) will reflect the multicollinearity
among all the factors (variables) which alters their interpretation compared to a
model with no collinearity among the factors (variables). See Cade (2015). There
almost always is substantial multicollinearity among factors (predictor variables)
in ecological models. One approach to help understand this better might be to not
only consider the full model with all factors that has parameter estimates that
reflect multicollinearity among factors (predictor variables), but to consider single
factor (predictor) models where there is no collinearity possible. This could be
used as a device to help understand how much rates of change associated with
factor parameters have been altered by multicollinearity with other factors in the
full model. It is easy to forget that regression coefficients estimated in a multiple
regression model only provide an interpretation for the part of a factor (predictor
variable) that is not linearly related to the other factors (predictor variables) as
Cade (2015) points out. If we really want to know about changes in outcomes
associated with the linearly correlated parts of multiple factors, then it seems like
something else must be done. It seems to me that the crucial exploration of models
with multiple factors that have some degree of collinearity (like precipitation and
temperature in the example on lines 104-105), is to understand changes in
outcomes with changes in factors that occur simultaneously. This will usually
require more than just simple interpretation of individual parameter estimates in a
multifactorial model when there is some degree of multicollinearity among the
factors.
2
Line 88: Seems like Cade (2015) critique about MMA of coefficients is principally
about issues associated with collinear predictor variables. The critique of Walker
(2017) relies on fraught causal interpretations of regression models that is
inconsistent with conventional statistical theory associated with regression models.
I would eliminate this citation.
Lines 89-90: Seems like this needs to be reorganized a bit. The issue with variable
collinearity (multicollinearity) is directly related to issues with model averaging
estimates of predictor parameters (regression coefficients).
Lines 90-91: Again, Cade (2015) critique also applies to problems with summing
model weights to assess relative importance of predictors.
Line 109: We could fit all five of these models and NOT average their parameters.
Line 149-150: There are several built in approaches to obtaining shrinkage
estimates for quantile regression. The lasso is an option in Roger Koenker’s
quantreg package and other shrinkage type estimators are available in Fasiolo’s
qgam package.
Lines 155-161: I think I recall reading somewhere that with lasso shrinkage
estimates that it may be appropriate to reestimate a model that excludes all the
parameters that were shrunk to zero to obtain better standard errors. I would
guess this might have something to do with correctly reflecting the
multicollinearity among predictors. Can you offer any comments or insights on
this model reestimation approach after shrinkage?
Line 199: Disagree. Some things we can imagine have no influence on the
outcomes we observe. They may sometimes be weakly correlated with other
relevant processes.


There’s a lot to like about this paper. Who doesn’t enjoy a polemic? Who doesn’t want a simple answer to a complicated problem. Ultimately, however, I don’t think this paper works as a standalone paper. Either, it could serve as the basis of a read paper with rejoinders and further discussion. Or it could be expanded under the more honest title: “There is no best way to understand multifactorial systems”.

 

Doing statistics is hard. Learning from data is hard. To deny this is to show that one has never had to analyse real observational data and that one thinks generations of statisticians have been severely misguided as to what they should be doing.

 

The view that the best approach is “to use full (maximal) statistical models” is, as the author is no doubt fully aware, completely misleading. It fails spectacularly in two very common scenarios: (1) when there are correlated explanatory variables, and (2) when the effect on the outcome of interest is nonlinear. To take the author’s own example: precipitation and temperature are correlated variables whose effect is often nonlinear. When they are correlated, the full model can easily be too complicated and inferred effects can have the wrong sign and/or inflated CIs; when their effect is nonlinear, the full model is too simple.

 

And this raises another issue with the paper: the class of models the author seems to be contemplating is rather limited. Why not use generalised additive models? Random forests? There is a whole raft of approaches to prevent overfitting, to determine the relative importance of explanatory variables, etc that has real relevance to the question of how to understand multifactorial systems.

 

Finally, there is no discussion (or criticism) in the paper of the Gelman & Hill approach to model building. I think this has particular relevance to the ecological applications that the author has in mind.

 

 

Minor comments:

 

* An actual working example of the author’s claims would have aided this paper enormously. 

 

* Line 34: Please can we stop advocating that computing R^2 is a measure of whether our model “describes the data reasonably well”. Perhaps more important here is to stress the importance of the checking of *model assumptions*.

 

* Line 103: I am not sure what is meant by “unnecessarily discretizing a continuous world” in this context. Is the author suggesting model space is continuous? But isn’t that what MMA is trying to recover? Or is the author saying that it is a category error to imagine the components of a multifactorial system have independent effects? But in that case a multifactorial model can only ever be a reflection of the imaginative power of the researcher. Who is to say what the “full statistical model” is?

 

Miscellaneous:

 

* Line 193: The author needs to state when this Google Scholar search was performed.

The manuscript discusses the limitations of multi-model approaches in dealing with multifactorial
systems. The author presents several interesting observations that merit publication. Particularly
noteworthy is the discussion regarding the potential pitfalls of model averaging.
However, I believe the paper could benefit from improved precision in certain statements and
increased accessibility for non-ecologists.
- The paragraph starting at line 43: Some of the philosophical statements in this paragraph
may be subject to scrutiny. Specifically, it appears as though the author is asserting that
only the "full model" is reasonable to consider. The notion of a full model is also referenced
later in line 135, where the author seems to assume that analysts typically have a full model
that closely approximates truth. I am uncertain of the author's intended meaning, but it
seems these statements require clarification. It may be beneficial to express these ideas with
greater precision, or if not possible, to soften them. It is my understanding that constructing
a complete model for complex multifactorial phenomena is often unattainable. We operate
under the assumption that some factors may explain observations, yet inevitably overlook
others. Thus, all models can be regarded as subsets of others that may better explain the
data, unrecognized to the analyst. Consequently, it may be prudent to explore models that
encompass only a subset of factors present in the largest model conceivable by the analyst.
Firstly, parsimony is crucial to mitigate overfitting – this is somehow mentioned by the
author. Secondly, selecting appropriate models is largely empirical, and I am reluctant to
dissuade scientists from considering simpler models if they adequately explain
observations compared to more complex alternatives.
- Does the paper address the possibility of correlations between different factors in certain
applications? This consideration seems significant and warrants discussion.
- Line 78: Stepwise regression is described as deprecated, although I guess it may still be
widely utilized in certain contexts—perhaps not so much within ecology? While I
acknowledge that data constraints can severely impact the efficacy of stepwise regression,
many criticisms of this method presuppose knowledge of the correct model. However, this
is rarely the case when proposing models to explain a dataset. Consequently, what purpose
is served by employing a more complex model if the available data can be explained by a
simpler one? The simpler model may exhibit bias compared to a superior model fitted to a
larger dataset. Nonetheless, such a comparison is only feasible when a larger dataset and a
more appropriate model are available.
- In relation to the previous point, does the author consider stepwise regression based on
combinations of multiple factor subsets or only one factor at a time? For instance, Hastie et
al.'s "The Elements of Statistical Learning" advocate for exploring multiple subsets.
- Incidentally, adding a citation to Hastie et al.'s book may be appropriate regarding
penalized approaches.
- I am concerned that the paper's key messages may not be fully appreciated by many
readers. While there is a discernible emphasis on ecology, readers outside this domain may
struggle to extract relevant information. Accordingly, I propose several modifications and
additions:
o Introducing the example from the paragraph starting at line 103 earlier in the paper
and utilizing it throughout to elucidate various concepts.
o Providing a straightforward illustration of multi-model averaging would help.
o A diagram or table summarizing the discussed methods and their respective
strengths and weaknesses could prove beneficial.
