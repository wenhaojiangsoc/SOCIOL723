<h1 align="center">SOCIOL 723: Social Statistics II</h1>

<p align="center"><b>Estimation, Causal Inference, and Machine Learning</b></p>

<p align="center"><b>Taught by <a href="https://wenhaojiangsoc.github.io">Wenhao Jiang</a> · Department of Sociology · Duke University · Fall 2026</b></p>

---

This is the second course in the graduate statistics sequence for sociology
doctoral students. It builds on the model-comparison treatment of regression and
ANOVA in *Social Statistics I* and extends into three pillars: **estimation**
(OLS and maximum likelihood), **causal inference** (potential outcomes, DAGs, and
the standard research designs), and **machine learning** (prediction,
regularization, and double/debiased ML).

Tuesday sessions are lectures; Thursday sessions are hands-on `R` labs.

- [Syllabus](./syllabus/SOCIOL723_syllabus.pdf)
- Lab data: [`Data/gss_earnings.rds`](./Data) — a GSS 2010–2022 extract of
  full-time workers aged 25–64 (*n* = 3,509), built by
  [`Data/build_gss_extract.R`](./Data/build_gss_extract.R).

> **A note on the slides.** Frames marked ★ contain material included for
> understanding rather than for evaluation — matrix derivations, projection
> geometry, and asymptotic arguments. Everything examined is developed first in
> scalar form.

---

<details>
  <summary><h2>Week 1 &nbsp;·&nbsp; The Linear Model and OLS</h2></summary>

We open by looking *inside* the estimator. Where do $\hat\beta_0$ and
$\hat\beta_1$ come from, what exactly does "holding constant" do, and what
happens when a relevant variable is left out? The entire argument is developed in
scalar form first, then rewritten in matrix form — which is the language every
later week speaks.

### Roadmap
- Recast the model-comparison framework of *Social Statistics I* as least squares,
  and derive the normal equations from the first-order conditions.
- Solve for $\hat\beta_0 = \bar Y - \hat\beta_1\bar X$ and
  $\hat\beta_1 = \widehat{\mathrm{Cov}}(X,Y)/\widehat{\mathrm{Var}}(X)$, and read
  the slope formula three ways.
- Establish what the normal equations imply mechanically about residuals — and
  why those implications are not evidence of anything.
- Introduce **partialling out** as a three-step recipe, the scalar form of the
  Frisch–Waugh–Lovell theorem, and use it to say precisely what "controlling for"
  costs.
- Derive **omitted variable bias**, $\tilde\beta_1 = \beta_1 + \beta_2\delta_1$,
  sign it, and verify the identity numerically on GSS data.
- Step back to the population: the **conditional expectation function**, the
  **best linear predictor**, and the three justifications for OLS when the CEF is
  not a line. Saturated models as the case where regression *is* the CEF.
- Rewrite everything in matrix form; the **geometry** of OLS as projection onto
  $\mathrm{col}(\mathbf{X})$; the hat matrix, the residual maker, leverage, and
  the sum-of-squares decomposition. ★
- Statistical properties: the sampling-error decomposition, unbiasedness,
  $\mathrm{Var}(\hat\beta_1) = \sigma^2/\sum_i(X_i-\bar X)^2$, what drives
  precision, $s^2$, and the Gauss–Markov theorem. ★
- Close on regression as **adjustment**: what OLS reports when treatment effects
  are heterogeneous, and the distinction between good and bad controls.

### Materials
- [Slides: Week 1 — The Linear Model and OLS](./Week%201%20The%20Linear%20Model%20and%20OLS/slides.pdf)
- [Lab 1: Matrix Algebra and OLS by Hand](./Week%201%20The%20Linear%20Model%20and%20OLS/lab1.pdf)

### Reading
*Required*: MHE Ch. 3; CCI Ch. 6. &nbsp;·&nbsp; *Additional*: ISL Ch. 2–3.

---
</details>

<details>
  <summary><h2>Week 2 &nbsp;·&nbsp; Regression Inference and Robust Standard Errors</h2></summary>

We have $\hat\beta$; now we need to know how uncertain it is. The organizing idea
is that every standard error in this course — classical, robust, clustered, HAC —
is the *same* variance formula with the error variance estimated differently.

### Roadmap
- What a sampling distribution *is*, built by simulation rather than asserted.
- Two routes to it: exact inference under normality, and the large-sample route
  via the law of large numbers and the central limit theorem, which needs no
  distributional assumption at all.
- Hypothesis testing: $t$-tests, and the $F$-test as the model comparison you
  already know — with the warning that the sums-of-squares form is valid *only*
  under homoskedasticity. The Wald statistic and the delta method. ★
- **Heteroskedasticity**: what it is, why it does not bias $\hat\beta$, and the
  **scalar sandwich**
  $\mathrm{Var}(\hat\beta_1) = \sum_i d_i^2\sigma_i^2 / (\sum_i d_i^2)^2$.
  White's estimator is what you get by substituting $\hat e_i^2$ for
  $\sigma_i^2$ — that is the whole idea.
- Finite-sample corrections HC0–HC3 and when they matter; why you should not
  pre-test for heteroskedasticity; why we fix the standard errors rather than
  reweight the estimator.
- **Clustering**: where the scalar formula breaks, the Moulton factor, the
  cluster-robust sandwich, choosing the level from the *design*, the few-clusters
  problem, and CR2 / wild cluster bootstrap remedies. Bertrand, Duflo, and
  Mullainathan (2004) as the cautionary tale.
- **The bootstrap**: the plug-in principle, pairs / residual / wild / cluster
  variants, percentile vs. BCa vs. bootstrap-*t*, and when the bootstrap fails.
- Close with Freedman's critique and a scoreboard of what robust standard errors
  do and do not fix.

### Materials
- [Slides: Week 2 — Regression Inference and Robust Standard Errors](./Week%202%20Regression%20Inference%20and%20Robust%20Standard%20Errors/slides.pdf)
- [Lab 2: Calculus, Simulation, and Robust Inference](./Week%202%20Regression%20Inference%20and%20Robust%20Standard%20Errors/lab2.pdf)
- [Problem Set 1](./Homework/homework1/homework1.pdf) — assigned Sep 1, due Mon Sep 14

### Reading
*Required*: MHE Ch. 8. &nbsp;·&nbsp; *Additional*: Freedman (2006); Bertrand,
Duflo, and Mullainathan (2004).

---
</details>

<details>
  <summary><h2>Week 3 &nbsp;·&nbsp; Maximum Likelihood: Theory</h2></summary>

Least squares asks which line is closest to the data. Likelihood asks which
parameter values make the data we actually saw most probable. It is the general
engine behind almost every model sociologists use, and — as we show at the
outset — OLS under normal errors is already a special case of it.

### Roadmap
- Design-based versus model-based inference, and why the normal linear model is
  not enough for binary, count, categorical, ordered, or censored outcomes.
- **Probability versus likelihood**: the same formula read with the data fixed
  and the parameter varying. Worked from a binomial example.
- Build likelihoods and log-likelihoods for the Bernoulli, Poisson, and normal
  cases; see least squares fall out of the normal log-likelihood.
- The **score function** as the derivative you set to zero, with the MLE derived
  by hand in all three examples — including the fact that
  $\hat\sigma^2_{\text{MLE}}$ divides by $n$ and is therefore biased.
- Numerical maximization: Newton–Raphson, BFGS, Fisher scoring; starting values,
  convergence codes, and the sign of the Hessian. **Identification** as a flat
  likelihood — the analogue of perfect collinearity.
- **Curvature is information**: Fisher information, observed information, and
  $\widehat{\mathrm{Var}}(\hat\theta) = \mathcal{I}(\hat\theta)^{-1}$,
  verified against the closed-form variances for the Bernoulli and Poisson.
- Consistency, asymptotic normality, efficiency, invariance — and the regularity
  conditions that fail in practice (boundaries, separation, support depending on
  the parameter). The quasi-MLE **sandwich** when the model is wrong. ★
- **Likelihood ratio, Wald, and score tests** as three ways of measuring the same
  distance, plus AIC and BIC for non-nested comparison.

### Materials
- [Slides: Week 3 — Maximum Likelihood: Theory](./Week%203%20Maximum%20Likelihood%20Theory/slides.pdf)
- [Lab 3: Coding and Maximizing a Likelihood](./Week%203%20Maximum%20Likelihood%20Theory/lab3.pdf)

### Reading
*Required*: Pawitan Ch. 2 and 4. &nbsp;·&nbsp; *Additional*: Pawitan Ch. 3.

---
</details>

<details>
  <summary><h2>Week 4 &nbsp;·&nbsp; Maximum Likelihood: Applications</h2></summary>

Now we put covariates inside the likelihood. The generalized linear model keeps
the linear predictor $X'\beta$ and pushes all the nonlinearity into a link
function — which means everything from Weeks 1–2 still applies, but
interpretation becomes the hard part.

### Roadmap
- The **GLM recipe**: random component, linear predictor, link. Why `glm()` is
  iteratively reweighted least squares. ★
- **Binary outcomes**: the case for and against the linear probability model; the
  latent-variable motivation for logit and probit; deriving the logit
  log-likelihood and score, and noticing that the score equations are the OLS
  normal equations with $\hat p_i$ in place of $\hat Y_i$.
- Why logit and probit are the same model in different units, and why the
  **scale is not identified** — so logit coefficients cannot be compared across
  nested models or across groups (Allison 1999; Mood 2010).
- **Interpretation on the probability scale**: odds ratios and their traps;
  average marginal effects versus marginal effects at the mean; predicted
  probabilities via the observed-value approach (Hanmer and Kalkan 2013);
  uncertainty by delta method, simulation, or bootstrap.
- **Interactions in nonlinear models**: why a product term is neither necessary
  nor sufficient for interaction on the probability scale (Ai and Norton 2003;
  Berry, DeMeritt, and Esarey 2010).
- **Multinomial** logit and IIA; **ordered** logit and proportional odds.
- **Counts**: Poisson and the log link; exposure offsets; overdispersion and the
  quasi-Poisson / negative binomial / robust-SE remedies; zero-inflation and
  hurdle models.
- **Diagnostics**: separation and Firth's penalized likelihood (Zorn 2005);
  deviance, pseudo-$R^2$, ROC, and predictive checks.

### Materials
- [Slides: Week 4 — Maximum Likelihood: Applications](./Week%204%20Maximum%20Likelihood%20Applications/slides.pdf)
- [Lab 4: Limited Dependent Variable Models](./Week%204%20Maximum%20Likelihood%20Applications/lab4.pdf)

### Reading
*Required*: ISL Ch. 4; Hanmer and Kalkan (2013). &nbsp;·&nbsp; *Additional*:
Berry, DeMeritt, and Esarey (2010); Zorn (2005); Mood (2010).

---
</details>

<details>
  <summary><h2>Week 5 &nbsp;·&nbsp; Potential Outcomes and DAGs</h2></summary>

The pivot of the course. Weeks 1–4 asked how to estimate a parameter; from here
on the question is *what are we trying to estimate, and what would have to be
true about the world for our estimate to recover it?* Two complementary
languages answer it.

### Roadmap
- **Potential outcomes**: $Y_i(1)$, $Y_i(0)$, the switching equation, and
  Holland's fundamental problem — causal inference as a missing data problem.
- **Estimands**: ATE, ATT, ATU, CATE, and why they differ whenever effects are
  heterogeneous and correlated with selection.
- The decomposition
  $\E[Y|D{=}1] - \E[Y|D{=}0] = \text{ATT} + \text{selection bias}$,
  derived line by line; what randomization buys and why.
- **SUTVA** — no interference, no hidden versions — and how routinely
  sociological settings violate it.
- Identification under **conditional ignorability**, **positivity**, and SUTVA;
  why positivity fails silently as covariates accumulate, and why regression
  never warns you.
- **DAGs**: from structural equations to graphs; chains, forks, and colliders;
  $d$-separation and the **back-door criterion** as conditional ignorability read
  off a picture.
- **Collider bias**: Berkson's paradox, the birth-weight paradox, and the
  recognition that sample selection *is* collider bias. A good-and-bad-controls
  table, including bias amplification (Cinelli, Forney, and Pearl 2024).
- The **front-door criterion** and why to be sceptical of it.
- **Defining a well-posed estimand** (Lundberg, Johnson, and Stewart 2021):
  theoretical estimand → empirical estimand → estimation strategy.

### Materials
- [Slides: Week 5 — Potential Outcomes and DAGs](./Week%205%20Potential%20Outcomes%20and%20DAGs/slides.pdf)
- [Lab 5: Simulating Confounding, Colliders, and DAGs](./Week%205%20Potential%20Outcomes%20and%20DAGs/lab5.pdf)

### Reading
*Required*: MHE Ch. 2; CCI Ch. 1–3; Lundberg, Johnson, and Stewart (2021).
&nbsp;·&nbsp; *Additional*: Holland (1986); Greenland and Pearl (2017).

---
</details>

<details>
  <summary><h2>Week 6 &nbsp;·&nbsp; Matching, Propensity Scores, and Weighting</h2></summary>

We take Week 5's identification result as given and ask a purely practical
question: **how should the adjustment actually be done?** Matching, regression,
weighting, and doubly robust estimation all target the *same* identified
quantity. They differ in what they model, what they extrapolate, and how they
fail.

### Roadmap
- Exact matching as "compare like with like," and the recognition that the ATE
  and the ATT differ only in **the weights on covariate cells** — so choose them
  deliberately rather than letting OLS choose for you.
- The curse of dimensionality; coarsened exact matching, Mahalanobis distance,
  calipers, matching with and without replacement, and bias correction.
- **Matching is preprocessing, not estimation** (Ho, Imai, King, and Stuart
  2007): match without ever looking at the outcome, check balance, *then* model.
  And never use a balance $t$-test.
- The **propensity score theorem** (Rosenbaum and Rubin 1983), proved: a single
  number suffices. Plus the *propensity score paradox* — a model that predicts
  treatment well is a model with poor overlap.
- **IPW** derived from first principles, stabilization, trimming, effective
  sample size, and a table of the weights that produce the ATE, ATT, ATU, and
  overlap estimands.
- **G-computation** as the mirror image: model the outcome, impute both potential
  outcomes, average.
- **AIPW** and double robustness, with the cancellation shown line by line — and
  the recognition that this cancellation *is* Neyman orthogonality, the same
  property that made FWL work in Week 1 and that underwrites Week 13.
- **Sensitivity analysis**: since ignorability is untestable, report how strong an
  unobserved confounder would have to be (Cinelli and Hazlett 2020).

### Materials
- [Slides: Week 6 — Matching, Propensity Scores, and Weighting](./Week%206%20Matching%20Propensity%20Scores%20and%20Weighting/slides.pdf)
- [Lab 6: Matching, Weighting, and Doubly Robust Estimation](./Week%206%20Matching%20Propensity%20Scores%20and%20Weighting/lab6.pdf)

### Reading
*Required*: CCI Ch. 4, 5, 7, 8. &nbsp;·&nbsp; *Additional*: Rosenbaum and Rubin
(1983); Sekhon (2009); Ho et al. (2007).

---
</details>

<details>
  <summary><h2>Week 7 &nbsp;·&nbsp; Instrumental Variables</h2></summary>

A change of strategy. Instead of controlling for confounders, find a source of
variation in the treatment that is *as good as randomly assigned* and use only
that. We throw away most of the variation in $D$ and keep a sliver we can
defend: precision falls, credibility rises, and the estimand changes.

### Roadmap
- Endogeneity in scalar form: $\hat\beta \to \beta + \Cov(D,\epsilon)/\Var(D)$,
  a bias that does not shrink with $n$. Omitted variables, simultaneity,
  measurement error, selection.
- **Relevance** (testable) and **exclusion** (not testable), drawn as a DAG.
- The **Wald estimator** as reduced form ÷ first stage, then **2SLS** with
  covariates — and why you must never run the two stages by hand.
- 2SLS as partialling out: the same projection matrix as Week 1, now applied to
  the regressors. ★
- **What IV actually estimates**: compliers, always-takers, never-takers,
  defiers; monotonicity; the **LATE theorem** and why different instruments give
  different — and equally correct — answers.
- Canonical applications: quarter of birth (Angrist and Krueger 1991), settler
  mortality (Acemoglu, Johnson, and Robinson 2001).
- **Weak instruments**: why they are worse than no instrument; why the
  first-stage $F > 10$ rule is far too lax (Lee, McCrary, Moreira, and Porter
  2022 put the threshold near 104); Anderson–Rubin inference that survives
  arbitrary weakness; and why a non-rejected overidentification test proves
  nothing.
- A **reader's checklist** for any IV paper (after Sovey and Green 2011).

### Materials
- [Slides: Week 7 — Instrumental Variables](./Week%207%20Instrumental%20Variables/slides.pdf)
- [Lab 7: Two-Stage Least Squares and Its Diagnostics](./Week%207%20Instrumental%20Variables/lab7.pdf)

### Reading
*Required*: MHE Ch. 4; CCI Ch. 9. &nbsp;·&nbsp; *Additional*: Angrist, Imbens,
and Rubin (1996); Sovey and Green (2011).

---
</details>

<details>
  <summary><h2>Week 8 &nbsp;·&nbsp; Fall Break / In-Class Midterm</h2></summary>

No class Tuesday, October 13. **In-class midterm Thursday, October 15**, covering
Weeks 1–7. Open book and open note, timed; no coding.

---
</details>

<details>
  <summary><h2>Week 9 &nbsp;·&nbsp; Regression Discontinuity Designs</h2></summary>

Many treatments are assigned by a rule — a test-score threshold, an income
cut-off, a vote share above 50%. Just below and just above the cutoff, units are
essentially identical except that one group got the treatment. The identifying
variation is visible in a scatterplot, which is why RD is often the most credible
observational design there is.

### Roadmap
- Sharp RD and the **continuity assumption**: no functional-form claim away from
  the cutoff, and no ignorability anywhere else.
- Why the estimand is **local to the cutoff**, and Lee (2008)'s reading of RD as
  a local experiment.
- **Local linear regression** with a triangular kernel and separate slopes on each
  side; why high-order global polynomials should not be used (Gelman and Imbens
  2019).
- The **bias–variance tradeoff in the bandwidth**, MSE-optimal selection, and why
  the optimal bandwidth leaves a bias that invalidates the conventional interval
  — hence the CCT **robust bias-corrected** interval.
- **Fuzzy RD** as local IV, inheriting the whole Week 7 apparatus including LATE
  and weak-instrument worries.
- Validity: **density/manipulation tests** (McCrary; Cattaneo–Jansson–Ma),
  covariate smoothness, placebo cutoffs, donut holes, bandwidth sensitivity,
  heaping, and the effective sample size.

### Materials
- [Slides: Week 9 — Regression Discontinuity Designs](./Week%209%20Regression%20Discontinuity%20Designs/slides.pdf)
- [Lab 9: Estimating and Stress-Testing an RD](./Week%209%20Regression%20Discontinuity%20Designs/lab9.pdf)

### Reading
*Required*: MHE Ch. 6; Imbens and Lemieux (2008). &nbsp;·&nbsp; *Additional*:
Lee and Lemieux (2010); McCrary (2008); Calonico, Cattaneo, and Titiunik (2014).

---
</details>

<details>
  <summary><h2>Week 10 &nbsp;·&nbsp; Panel Data and Difference-in-Differences</h2></summary>

A third source of variation: **time within the same unit**. Differencing removes
every time-invariant confounder, including ones we never measured and could not
name. The price is precision, and a new and demanding assumption about
counterfactual trends — plus, as the last decade has shown, a serious problem
with the standard implementation.

### Roadmap
- Unobserved heterogeneity and the error-components model; **within, first
  differences, and unit dummies as the same estimator** (FWL again).
- What fixed effects costs: precision, amplified measurement error, Nickell bias,
  and the fixed-effects-versus-lagged-dependent-variable choice.
- The canonical **2×2 DID**, **parallel trends** (untestable, and not scale-free),
  no anticipation, and **event studies** — with Roth (2022)'s warning that
  pre-trend tests have low power and conditioning on them distorts inference.
- **The trouble with TWFE under staggered adoption.** Goodman-Bacon (2021)'s
  decomposition and the *forbidden comparison* in which already-treated units
  serve as controls; negative weights (de Chaisemartin and D'Haultfœuille 2020);
  contaminated event studies (Sun and Abraham 2021). With dynamic effects TWFE
  can have the wrong sign.
- The modern estimators — Callaway–Sant'Anna, Sun–Abraham, Borusyak–Jaravel–Spiess,
  stacked DID — all built on one principle: never use an already-treated unit as
  a control. Conditional parallel trends brings Week 6 back inside a DID.
- **Honest inference about pre-trends** (Rambachan and Roth 2023) and a DID
  checklist.

### Materials
- [Slides: Week 10 — Panel Data and Difference-in-Differences](./Week%2010%20Panel%20Data%20and%20Difference-in-Differences/slides.pdf)
- [Lab 10: Fixed Effects, DID, and the Staggered-Adoption Problem](./Week%2010%20Panel%20Data%20and%20Difference-in-Differences/lab10.pdf)

### Reading
*Required*: MHE Ch. 5; CCI Ch. 11; Mixtape (DID chapter). &nbsp;·&nbsp;
*Additional*: Goodman-Bacon (2021); Roth et al. (2023).

---
</details>

<details>
  <summary><h2>Week 11 &nbsp;·&nbsp; Synthetic Control</h2></summary>

What if the treated unit is California, or East Germany, or a single firm?
Comparative case studies have always constructed a comparison informally.
Synthetic control makes that construction a transparent, data-driven, and
checkable procedure.

### Roadmap
- The estimator: choose donor weights on a **simplex** so that the weighted
  donors reproduce the treated unit's pre-treatment trajectory and predictors.
- The **optimization problem written out**, including the nested choice of
  predictor-importance weights $V$ — and where the researcher degrees of
  freedom hide.
- Why the constraints $w_j \ge 0$, $\sum w_j = 1$ matter: **no extrapolation**,
  sparsity, transparency, and a design that *tells you* when no good comparison
  exists.
- Relation to DID: synthetic control *searches* for a weighting under which
  pre-trends coincide, rather than assuming one. The **factor-model
  justification** (Abadie, Diamond, and Hainmueller 2010), and why the length of
  the pre-period does the identification work.
- **Permutation inference** with one treated unit: in-space placebos, the
  post/pre RMSPE ratio, exact $p$-values and their hard floor of $1/(J{+}1)$,
  in-time placebos, and leave-one-donor-out.
- The **California Proposition 99** application, read as a design a reader can
  check by eye.
- Extensions: augmented and ridge-augmented SC, generalized SC for multiple
  treated units, and synthetic DID.

### Materials
- [Slides: Week 11 — Synthetic Control](./Week%2011%20Synthetic%20Control/slides.pdf)
- [Lab 11: Synthetic Control](./Week%2011%20Synthetic%20Control/lab11.pdf)

### Reading
*Required*: Abadie, Diamond, and Hainmueller (2010). &nbsp;·&nbsp;
*Additional*: Abadie (2021); Xu (2017); Arkhangelsky et al. (2021).

---
</details>

<details>
  <summary><h2>Week 12 &nbsp;·&nbsp; Machine Learning: Prediction and Regularization</h2></summary>

For one week we set identification aside entirely and ask a different question:
how do we predict well out of sample? The answer requires giving up the thing
Weeks 1–2 prized most — unbiasedness.

### Roadmap
- **Prediction versus inference**, and why the two goals conflict.
- The **bias–variance decomposition** of prediction error, and the recognition
  that every tuning parameter in the course — penalty size, tree depth,
  bandwidth (Week 9!) — is the same dial.
- Why in-sample fit is not evidence: **optimism**, and $K$-fold
  **cross-validation** as an honest estimate of test error. Everything
  data-dependent must happen *inside* the fold; split on the independent unit.
- **Ridge**: the $\ell_2$ penalty, why $(X'X+\lambda I)$ is invertible
  when $X'X$ is not, and when dense signals favour it.
- **Lasso**: the $\ell_1$ penalty, why it yields exact zeros, approximate
  sparsity, and elastic net. Choosing $\lambda$ by CV, the one-standard-error
  rule, and plug-in penalties.
- **Why lasso's selected set is *not* a variable-importance ranking**, and
  why naive post-selection $p$-values are invalid.
- **Trees**, and then **bagging, random forests, and boosting** — with the clean
  summary that bagging and forests attack variance while boosting attacks bias.
- What ML buys sociology, and what it emphatically does not.

### Materials
- [Slides: Week 12 — Machine Learning: Prediction and Regularization](./Week%2012%20Machine%20Learning%20Prediction%20and%20Regularization/slides.pdf)
- [Lab 12: Regularization, Trees, and Cross-Validation](./Week%2012%20Machine%20Learning%20Prediction%20and%20Regularization/lab12.pdf)

### Reading
*Required*: ISL Ch. 5, 6, 8; Molina and Garip (2019). &nbsp;·&nbsp;
*Additional*: ISL Ch. 10; Kleinberg et al. (2015).

---
</details>

<details>
  <summary><h2>Week 13 &nbsp;·&nbsp; Causal Machine Learning</h2></summary>

The two halves of the course meet. Every design in Weeks 5–11 ended with a
nuisance function to estimate; Week 12 gave us flexible estimators for exactly
such functions. The obvious move — plug one into the other — fails, and
understanding why is the point of the week.

### Roadmap
- The **partially linear model**, and **regularization bias**: because every ML
  estimator is deliberately biased and converges slower than $\sqrt n$, the naive
  plug-in bias *diverges* rather than vanishing.
- The fix, part one: **partial out both sides**, so the bias becomes the
  *product* of two estimation errors and vanishes if both converge at
  $n^{-1/4}$.
- Recognizing this as **Neyman orthogonality** — already derived in Week 6 via
  the Gateaux derivative, and already met in Week 1 as FWL.
  **The thread of the whole course: FWL → AIPW → DML.**
- The fix, part two: **cross-fitting**, which removes the overfitting bias that
  orthogonality alone leaves behind.
- The DML recipe, what the theory requires, and — emphatically — what it does
  not: **DML identifies nothing**. It is estimation technology for a
  parameter already identified.
- Where DML plugs in: partially linear regression, AIPW/ATE, IV, RD, and
  conditional DID.
- **Heterogeneous effects**: the CATE, the doubly robust score as a
  *pseudo-outcome* whose conditional mean is $\tau(x)$, **causal forests**,
  honesty and local centering, and how to report heterogeneity without
  fooling yourself (calibration tests and sorted group ATEs).

### Materials
- [Slides: Week 13 — Causal Machine Learning](./Week%2013%20Causal%20Machine%20Learning/slides.pdf)
- [Lab 13: Double Machine Learning and Causal Forests](./Week%2013%20Causal%20Machine%20Learning/lab13.pdf)

### Reading
*Required*: CML Ch. 4 and 10; Athey and Imbens (2017). &nbsp;·&nbsp;
*Additional*: Chernozhukov et al. (2018); Wager and Athey (2018).

---
</details>

<details>
  <summary><h2>Week 14 &nbsp;·&nbsp; Synthesis and Final Project Workshop</h2></summary>

### Roadmap
- **Three questions, in order**: what am I estimating; what would have to be
  true; how do I estimate it and how uncertain am I. Most methodological disputes
  are disagreements about the first two conducted as if they were about the third.
- **One idea recurring**: FWL → AIPW → DML, and partialling out as the
  through-line.
- Every design in one table — assumption, estimand, main threat — and every
  inference choice in another.
- A **decision guide** you can run on your own data, and a list of things that
  are never a solution (more controls without a DAG; robust standard errors for a
  bias problem; a non-significant specification test as confirmation; machine
  learning for an identification problem).
- The **final project**: expectations, common failure modes, and the workshop
  format.

### Materials
- [Slides: Week 14 — Synthesis](./Week%2014%20Synthesis/slides.pdf)

No Thursday lab (Thanksgiving recess). **Final project due December 14.**

---
</details>

---

### Software

`R` and `RStudio`, with `Quarto` or `RMarkdown` for assignments. `Python` appears
as a supplement in the machine-learning weeks. Packages used so far:
`dplyr`, `ggplot2`, `broom`, `modelsummary`, `sandwich`, `lmtest`,
`clubSandwich`, `boot`, `fixest`, `marginaleffects`, `MASS`, `AER`, `brglm2`,
`pROC`, `nnet`, `dagitty`, `ggdag`, `MatchIt`, `cobalt`, `sensemakr`, `ivreg`,
`rdrobust`, `rddensity`, `did`, `bacondecomp`.

### Textbooks

| | |
|:--|:--|
| **[MHE]** | Angrist and Pischke (2009), *Mostly Harmless Econometrics* ([online](https://www.dsecoaching.com/pdf/2008%20Angrist%20Pischke%20MostlyHarmlessEconometrics.pdf)) |
| **[CCI]** | Morgan and Winship (2015), *Counterfactuals and Causal Inference*, 2nd ed. |
| **[ISL]** | James et al. (2021), *An Introduction to Statistical Learning* ([online](https://www.statlearning.com/)) |
| **[Pawitan]** | Pawitan (2013), *In All Likelihood* |
| **[Mixtape]** | Cunningham (2021), *Causal Inference: The Mixtape* ([online](https://mixtape.scunning.com/)) |
| **[CML]** | Chernozhukov et al. (2025), *Applied Causal Inference Powered by ML and AI* ([online](https://causalml-book.org/)) |
