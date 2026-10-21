# SOCIOL 723: Social Statistics II (Duke Sociology, Fall 2026)

Project memory for any Claude session that opens this folder, on any machine.
Read this first; then `.claude/napkin.md` for the running log of decisions.

## What this repo is
Graduate stats sequence, second course. Tuesday = lecture (beamer, `Week N .../slides.tex`),
Thursday = R lab (`Week N .../labN.qmd` -> PDF). Real GSS data for labs:
`Data/gss_earnings.rds` (n = 3,509, built by `Data/build_gss_extract.R`; `gssr` is not on
CRAN, never make a lab depend on it). Public GitHub repo; lab answer keys are tracked on purpose.

## Build commands
- Slides: `pdflatex -interaction=nonstopmode slides.tex` twice, in the week folder.
  Check `grep Overfull slides.log`; the log line number is the `\end{frame}`.
- Labs: `/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto render labN.qmd`
  (quarto is NOT on PATH). Renders take 2-10 min; run in the background.
- Syllabus: `cd syllabus && pdflatex main.tex` twice, then `cp syllabus/main.pdf Syllabus.pdf`.
- Eyeball a slide (no poppler): `gs -dNOPAUSE -dBATCH -sDEVICE=png16m -r70 -dFirstPage=N -dLastPage=N -sOutputFile=out.png slides.pdf`
- Slide numbers in `.nav`: `grep framepages slides.nav`.
- Every number on a slide comes from a saved script: `Week 3 .../figs/make_testing_figs.R`,
  `figs/sim_reg_rare.R`, `Week 4 .../figs/w4_examples.R`, `Week 4 .../figs/w4_multilevel.R`.
  Rerun those, do not retype.

## Conventions (locked in)
- Fonts: Latin Modern, serif body (`\usefonttheme{serif}`), NOT newpx. Reuse Week 1's preamble.
- `\core` blue star = examinable; `\adv` orange star = for understanding only. The user decides stars.
- `mathtools` is not installed; use amsmath/amsfonts/amssymb/bm only.
- Column vectors, prime for transpose, `X_i'\beta` for the linear predictor.
- Causal weeks: one effect letter, `\tau`; `\tau(x)` = CATE(x), `\tau_R` = regression
  estimand, `\tau_{\text{AIPW}}`; `\text{ATE}`/`\text{ATT}` spelled out; `\pi = \Pr(D_i=1)`;
  `e(x)` propensity score, `m_d(x)` outcome model; plain `\epsilon`, indicator `\mathds{1}`.
- Never add `Co-Authored-By` lines to commits. Push only when the user says so.

## How the user wants slides written
- Succinct. No chatty asides, no "next frame will..." narration, no metaphors that overstate math.
- Goal first, then a numbered plan, then one step per frame; every equality in an `align*`
  annotated with `&& \text{\footnotesize reason}`.
- Every non-obvious claim needs proof ON the slide: a derivation, a worked number, or a
  simulation figure/table. Prefer real GSS numbers over made-up ones.
- No shorthand variables; no latent-variable motivations (user dislikes "Y* > 0"); one
  framework per section; delete over hedge.
- The user edits `slides.tex` between turns: always re-grep exact text before replacing.
- Lab code chunks carry `##` explanatory notes on every step.
- Labs must not run off the right margin (user 2026-09-30): every lab YAML carries the
  `fvextra` header from lab6.qmd (breaklines for `Highlighting` and `verbatim`), `options(width = 76)`
  in setup, and long outputs are `round()`ed; check `Overfull \\hbox` in the kept .tex's log.

## State of the course (as of 2026-09-16)
- Weeks 1-2: done. Week 3 (MLE theory): testing section rebuilt; score test dropped from the
  course except as the third line of the rulers picture; LR-for-a-block + AIC/BIC + BIC-in-
  practice + "large n rejects everything / where BIC came from" close the deck (72 pp).
- Week 4 (MLE applications, 58 pp): X inside theta -> logit (two ways, S-curve, fitted GSS
  logit, Newton by hand, score = normal equations) -> interpretation (three scales, predicted
  probabilities first, Pager 2003 read slowly, AME person by person, AME/MEM, uncertainty,
  probit, recommendation, interactions with real output) -> several categories (multinomial on
  four-region residence, derivation, computation, ordered logit on degree, cutpoints as gates,
  proportional odds) -> counts (Poisson, overdispersion: three beliefs + where the numbers come
  from, excess zeros, hurdle) -> MULTILEVEL (replaced event history 2026-09-14; HSB data via
  nlme, lme4; opens with Coleman 1966 / Catholic-school motivation and "why not OLS +
  clustered SE", u_j tied to Week 2's error components model; no likelihood derivation frames,
  the user cut them; NO null model anywhere, everything from the full model; no shrinkage frame; "When You Need a
  Random Intercept" = Tomaskovic-Devey 2020 between-workplace share + few-groups case, "otherwise
  clustered OLS is enough"; no multilevel literature frame; "Two Assumptions About u_j" frame; RI logit frame has a
  predicted-probability figure from figs/w4_multilevel.R) -> Looking Ahead -> References.
  EVENT HISTORY dropped again 2026-09-16 (restorable from commit 9ee5254); Assessing Fit and
  Practical Protocol frames dropped. Lab 4 follows the same order and adds multinomial (region4)
  and an AIC/BIC table Poisson vs NB vs hurdle. "In the Literature" frames: Pager 2003, Kuo & Raley 2016, Breen &
  Jonsson 2000, Zajacova et al. 2017, Olzak 2021, Ajrouch et al. 2016,
  Tomaskovic-Devey et al. 2020; all web-verified. No "(Thursday's session)" tags in titles.
- Week 5 (PO + DAGs, 55 pp, rebuilt 2026-09-17): identification vs estimation (Manski) ->
  potential outcomes (estimands, two decompositions, randomization, SUTVA/consistency,
  selection on observables, positivity) -> path analysis to DAGs (Wright, Blau & Duncan
  diagram, structural equations) -> reading a DAG (blocks, d-separation, intervention graph +
  do, back-door with adjustment formula, bias amplification, college-and-earnings example) ->
  colliders (admissions T/A/C, algebra, ancestors, birth-weight paradox) -> controls (taxonomy +
  two three-panel DAG frames from 690S) -> front
  door (formula, APC via Winship & Harding 2008, critiques) -> estimands (Lundberg). Every DAG
  uses the shared `dag` tikz style; no frame spills to a "(cont.)" page.
- Week 5 (PO + DAGs, 64 pp): PO section expanded 2026-09-21 (Brand & Xie 2010, regression =
  naive comparison, Neyman/HC2, multi-valued treatments, CIA proof; user dropped the simulation
  and bounds frames). d-separation and do-operator slowed 2026-09-23 (worked paths table,
  seeing vs doing, adjustment formula derived from the intervention graph). Lab 5 rebuilt
  2026-09-23: PO simulations, Baron-Kenny mechanism test on GSS, Blau & Duncan replication from the
  correlation matrix, dagitty, and a TikZ-in-Quarto section using the slides' tikzset.
- Week 6 (82 pp, restructured 2026-09-28/30): opening (slide 4 regression = CEF + gap;
  slide 5 ATE/ATT/ATU as weighted tau(x); slide 6 OLS returns tau_R) -> Matching FIRST (exact
  matching for the ATE, unmatched cells, checks, ATT, GSS worked example from
  `figs/w6_matching_gss.R`, three SE frames, Mazrekaj et al. 2020 CEM in 6 frames with the
  paper's figures) -> Propensity Score (Cheng et al. 2021 Science Advances in 7 frames with the
  paper's Figures 1-4) -> Weighting (why after matching; ATT, ATE, weighted regression, ATU +
  table, overlap weights cell by cell, why it works)
  -> Doubly Robust (g-computation = matching written as a regression, continuous X fills the cells,
  extrapolation simulation, AIPW, two cases,
  honest warning, sensitivity + Cinelli-Hazlett then Oster frames (user-written), choosing, workflow)
  -> Anatomy at the END (goal-and-plan opener, covariance frame, 'Continuous X: What OLS Does
  Instead' + 'Reading It: Two Implicit Models' from `figs/w6_fuzzy.R`, Angrist 1998) -> Looking
  Ahead. 'G-Computation Extrapolates' (same simulation, one panel) sits in the DR section. HECKMAN DROPPED 2026-09-30 (lecture and
  lab). All literature frames web-verified; paper figures live in `figs/` as PNG/JPG.
  Lab 6: exact matching on cells by hand + MatchIt, PS, NN/CEM, weighting, AIPW, sensitivity.
- Week 7 (IV, 84 pp as of 2026-10-02): endogeneity -> 2SLS -> LATE -> FOUR CANONICAL APPLICATIONS
  (goal + overview table; AJR 2001, Angrist-Krueger 1991, Angrist 1990 draft lottery, Card 1995
  proximity on the lab data via `figs/w7_card.R`) -> APPLICATIONS IN SOCIOLOGY (overview table;
  Sharkey, Torrats-Espinosa & Takyar 2017 ASR nonprofits; de Vaan & Stuart 2019 ASR opioid
  contagion with the ED physician's leave-one-out prescribing rate, an examiner design; Aksoy &
  Gambetta 2022 ASR Ramadan daylength) -> WEAK IV: TWO REMEDIES (opener; Anderson-Rubin in four
  titled steps + Card/weak-draw figure from `figs/w7_weakiv.R`; LIML/Fuller k-class frame + simulation
  table from `figs/w7_liml.R`, 2026-10-07) -> judge IV -> shift-share -> Practice.
  Sharkey & Torrats-Espinosa 2017 JUE was DROPPED 2026-10-02 (user). Every paper = THREE frames in a fixed order
  (user 2026-10-02): 'In the Literature: Authors (Year)' = The question / The data / The
  instrument; 'Authors: the Three Conditions' = Relevance / Independence / Exclusion table
  (threat + evidence, testable or argued) + one or two bullets; 'Authors: Main Results' = OLS /
  first stage / reduced form / IV table + two reading bullets. AJR, AK, and de Vaan & Stuart
  numbers verified from the papers (AJR Table 4; AK Table III; dV&S Tables 4-6)
  -> 2SLS -> LATE -> weak IV -> JUDGE AND EXAMINER DESIGNS (leave-one-out leniency with
  `figs/w7_judge.R`, worked monotonicity failure, MTW/FLL repairs, Harding et al. 2017/2018/2019)
  -> SHIFT-SHARE (Bartik/Card, GPSS vs BHJ, `figs/w7_shiftshare.R`, AKM inference, Autor-Dorn-
  Hanson 2019, then Derenoncourt 2022 AER Great Migration in four frames: the three-frame pattern
  plus a 'Shock' frame (predictors by item + LASSO; equations frame dropped 2026-10-07; results table rows all / Black men (T6) / white men (T7)); numbers from the Aug 2021 draft posted by WCEG Feb 2022, Tables 3, 4, 9; not checked against the AER print) -> Practice. New frames unstarred; the user decides stars. Lab 7 = Card proximity IV.
  2026-10-07 (user): shift-share 'Proof by Simulation' and AKM 'Inference' frames DROPPED, Card (2001)
  row dropped from the keeps/throws-away table; ADH 2013 row = decade change in US imports per worker
  (no extra note, user found it confusing); SHEN (2026) DROPPED 2026-10-07 (frames, ref, lab mention); 'Two Routes' table replaced by three frames after keeps/throws-away: Two Routes (goal+plan, ADH s and g), Route 1 shares (furniture-town hypothetical, ADH Table 2 pre-trend 0.43/-0.13 vs -0.75), Route 2 shocks (industry pre-trend, consumer electronics, 'factors specific to China'); ADH 2019 frame = change in import PENETRATION (1991 absorption).
  figs/w7_shiftshare.R and shiftshare_sim.pdf now unused (kept).
  2026-10-07: new frame 'When Is AR Useful?' after the AR Card/weak-draw figure (hopeless / weak / Card
  lab numbers / beta=0 test); 'Wald interval' renamed 'usual 95% CI' in slides and lab.
  2026-10-07 (user): OVERIDENTIFICATION AND LIML DROPPED from slides and lab (no J/Sargan frame,
  no LIML/Fuller frames; weak-IV section is AR only; Wu-Hausman and Sargan not taught). Lab 7 uses
  age (not experience) as the exogenous control throughout (one endogenous regressor); 4.3 shows
  feols for several endogenous regressors (evaluated: second stage + all three first stages; text says order does not matter, every regressor on all instruments); 5.3 = ivDiag::AR_test (AR F 4.14 =
  reduced-form t^2, set [0.004, 0.256]); section 6 uses the lecture DGP (corr 0.9) and shows Wald
  coverage among SIGNIFICANT draws (0.94/0.84/0.69), no funnel figure (user cut it); Wu-Hausman hidden in all printed output (diagnostics = FALSE, fitstat = ~ ivwald); compliers by hand kappa
  plus ivdesc (unconditional, balance test rejects). ivreg/ivDiag/ivdesc installed 2026-10-07.
- Week 9 (RD, 104 pp; 'Why Not a Global Polynomial: the Huai River' after Other Checks (690S figure = Ebenstein et al. 2017 Fig 3, local linear, -3.1 [1.3,4.9]; Chen et al. 2013 cubic 5.5 [0.8,10.2]; web-verified via PMC); Heaping slide corrected: heaps at 1,500 g and 1,503 g are on the UNTREATED side (IZA DP 5106 checked); Close Elections Under Scrutiny DROPPED; 'What Sorting Does' rewritten as the incumbent-machine story (intro, then manip_density.pdf, then three intuition bullets; no simulation numbers); Flexible Covariate Adjustment, RD-vs-experiment power (3/4, 2/pi) frames and the 'Covariates, Power, and What to Report' list DROPPED; 'Covariates, Power, and What to Report' kept as ONE frame (covariates, effective n, power expanded with rdpower/rdsampsi intuition; discrete-X bullet dropped) ; power bullet and the 'Power: Would We Detect the Effect?' frame DROPPED by user 2026-10-20 (w9_estimation.R section 6 still prints rdpower numbers); covariates table AND balance table both at h_CER (bwselect cerrd), bias-corrected estimates, robust p (w9_senate.R 8b and 9a): no covs 8.19, lag vote jump 3.68 p .20, all four 7.45; balance lag vote 3.72 p .18; 'Why tau Moves Only If Z Jumps' DROPPED; Senate covariates table = tau_Z (p) and tau-tilde one covariate at a time (note: fixed h in rdrobust forces b = h); covariates rewritten plainly: 'Adding Covariates' = spec with Z'gamma, pre-determined (turning-21: sex, race, schooling), tau barely moves / se shrinks, big move = Z jumps, no DxZ, no post-treatment; 'Why tau Moves Only If Z Jumps' = one-Z identity tau~ = tau_Y - gamma tau_Z without c(V) notation; Repair 3 (honest) frames DROPPED by user, honest row in the repairs table carries an adv star; '92% not 95%' adv frame DROPPED by user (picture slide carries it); inference reorganized: 'Three Repairs, Side by Side' table after the 92% frame (none / 1 undersmooth h=10 [4.38,11.59] / 2 RBC [4.09,10.92] + at h_CER [4.07,11.29] / 3 honest [3.83,12.23]); later frames titled 'Repair 2: ...', 'Repair 3: ...'; sensitivity table says 'robust bias-corrected'; RBC s.e. shown via b = h: local quadratic, se = sqrt(9s+^2/m+ + 9s-^2/m-) = 2.21 (edge factor 9 vs 4; rdrobust 2.06; default b 1.74), weighted-sum version REJECTED by user as too complex; 'bias-corrected with the old s.e.' row and bullets REMOVED (user: confusing); 2026-10-19 Bol et al. 2018 (3 frames) MOVED to Local Randomization after 'Choosing the Window'; Carpenter-Dobkin 2009 drinking-age example added after Winners vs Losers (2 frames, Fig 3 cropped to figs/carpenter_dobkin_fig3.png; Tables 2,4,5 verified from the AEJ PDF); new 'Inference: the Standard Error' frame before the How Sure picture (se = sqrt(4s+^2/m+ + 4s-^2/m-) = 1.47 from figs/w9_estimation.R section 5, rdrobust 1.46, usual CI [4.56, 10.27]); h_CER named + cited on the sensitivity slide; 2026-10-19 user approved blue stars on: What Is Left?, Shape: Two Ways, Bias and Variance Definitions, Bias of the Line Step 1 picture + Step 4, (How Sure picture and RBC Idea stars REMOVED by user; RBC Formulas made adv); h_CER defined on 'Repair 2 on the Senate' (row 7.68, 1.84, [4.07, 11.29]; b = h identity rows dropped), not on the simulation slide, Senate in Five Lines; the four estimation 'Steps' RENAMED by choice (Shape / Weights / Bandwidth / Inference: ...) so 'Step N' is used only inside derivations; roadmap table has a 'what is at stake' bias/variance column; bias of the line now 4 frames: the picture (curve, fitted line, gap at c; no new notation, user dislikes extra symbols like Int[g_i]), Taylor = 'we cannot fit a line to an unknown curve, but can to a parabola', Step 3 plugs a+bu+qu^2 into the OLS slope/intercept formulas (b cancels, -q/6; user rejected 'line fitted to qu^2' pieces), both sides; 2026-10-19 user DROPPED "Identification, Step by Step" and both Lee "Why Continuity Should Hold" frames; 2026-10-19 user: estimation made intuitive for first-years; then every advanced derivation
  slowed into 2-3 frames: means-vs-line algebra + Senate ('Every Fit Is a Weighted Average' frame and the high-degree-polynomial bullet and 'Global Polynomials Weight Distant Races' DROPPED by user),
  bias via parabola (u^2 line, -1/6), variance in 3 frames (Step 1 mean+slope variances + Cov = 0 with a one-bullet intuition: a high error raises Ybar, raises the slope right of ubar, lowers it left, these cancel; user found the algebra too hard; Step 2 one side; Step 3 both sides), all conditional on bold X = (X_1..X_m), homoskedastic (mean and slope variances + Cov = 0 derived; one side 4 sigma^2/m; both sides V/(nh)), MSE derivative, bias = se/2 and 92% coverage, RBC formulas,
  honest idea + Senate arithmetic, covariate identity derived, 3/4 and 2/pi derived; also a Bias-Variance definitions frame): Sharp RD opens with the Senate
  example + winners-vs-losers-in-shrinking-windows table, then identification, then 'Identification Is Done. What Is
  Left?' (four steps: shape, weights/kernel, bandwidth, inference); Estimation = Step 1 means vs lines (figs/w9_estimation.R:
  senate_means_lines.pdf, senate_three_h.pdf) -> one regression -> degree 0/1/2 -> Step 2 kernel ->
  Step 3 three bandwidths -> bias/variance/MSE derivations (intuition lead lines) -> sensitivity -> Step 4 picture ->
  RBC idea + formulas -> simulation -> honest -> 'Senate in Five Lines' recipe. Earlier build (2026-10-08): sharp RD identification (Lee 2008 argument) -> estimation
  on the Senate data (`figs/w9_senate.R`, `figs/w9_sims.R`: bias/variance/h* derivations, RBC, CER,
  honest CIs, Gelman-Imbens weights) -> covariates and power -> validity -> local randomization ->
  fuzzy (AR at the cutoff) -> Dell & Querubin 2018 Vietnam bombing (8 frames, NBER WP numbers) ->
  extensions -> sociology: Bol et al. 2018 PNAS, Legewie 2016 AJS. Lab 9 (Senate, 16 pp) extended 2026-10-20 for PS3: conventional-vs-robust rows, h_CER (cerrd), p = 1 vs 2, covariate balance and covs at cerrd (numbers match w9_senate.R 8b/9a), fvextra header added.
- Week 10 (Panel + DiD, 95 pp, rebuilt 2026-10-08; user frames kept verbatim): panel FE on
  `wooldridge::wagepan` (`figs/w10_panel.R`: marriage premium pooled .108/RE .064/FE .047/FD .038,
  Mundlak, FE variance weights, Imai-Kim DAG, FEIS, impact function; Budig & England 2001; Ludwig &
  Bruderl 2018 FE 8.3% -> FEIS 0.5%) -> 2x2 (Card-Krueger 2.76; Roth-Sant'Anna scale; OR/IPW/DR;
  covariates in TWFE; placebo laws `figs/w10_placebo.R` iid .477 vs cluster .049) -> event studies
  (Roth 2022, base periods, plotting, Kleven et al. 2019 child penalty) -> staggered on
  `bacondecomp::divorce` (`figs/w10_divorce.R`: TWFE -3.05; Bacon table; 267 negative weights;
  CS/SA/ETWFE-never -10.56, imputation = ETWFE-notyet -4.85; HonestDiD breakdown < .05) ->
  beyond (dose, on/off, DDD, distributions, design-based) -> sociology (Harknett et al. 2021;
  Choper et al. 2026 ASR; Kalil et al. 2023) -> Chiu et al. 2026 lessons. Lab 10 not yet aligned.
- Week 11 (SC, 50 pp, rebuilt 2026-10-08; user frames kept): Prop 99 rebuilt from tidysynth::smoking
  (`figs/w11_prop99.R`: weights Utah .342/Nevada .238/Montana .209/Colorado .149/CT .062, gap -18.8,
  placebo rank 1/39, in-time placebo -3.4, leave-one-out, min-norm regression weights 17 negative,
  DiD/SC/SDiD -27.3/-19.6/-15.6, ASCM -16.0, conformal p .097); factor-model bias + simulation
  (`figs/w11_sims.R`); Basque two specs and gsynth EDR turnout (`figs/w11_more.R`: TWFE .78 vs IFE r=2
  4.90); German reunification (ADH 2015); penalized/augmented/SDiD/MC/IFE/staggered/recent;
  Bartos & Kubrin 2018 Prop 47. synthdid + augsynth are GitHub-only. Lab 11 not yet aligned.
- Week 12 (ML, 69 pp, rebuilt 2026-10-08; user asked to merge 690S Weeks 2+3: trees AND neural nets
  and beyond): bias-variance proof, optimism 2p sigma^2/n, CV (Bates et al.), ridge SVD/Hoerl-Kennard,
  soft thresholding, trees/pruning, RF variance, boosting = gradient descent, stacking, classification;
  GSS benchmark (`figs/w12_gss.R`: test R2 boosting .381, RF .373, lasso .344, OLS .338, NN .342;
  n=300: expanded OLS collapses); lasso keeps SEI, prestige 1/100; neural nets (690S content:
  X1X2 example, backprop, SGD, dropout); embeddings/CNN/attention/PPI; Fragile Families, Kleinberg
  bail, Obermeyer, fairness impossibility. Subtitle changed; folder name unchanged. Lab 12 not aligned.
- Week 13 (causal ML, 53 pp, rebuilt 2026-10-08; user frames kept): simulation `figs/w13_sims.R`
  (design A = CCDDHNR Fig. 1: naive bias/sd -3.2, DML coverage .93; design B very nonlinear: DML
  also biased, l-hat RMSE 1.02 -> "orthogonality makes errors multiply, not small"); orthogonality
  derived (Gateaux), product-of-errors expansion, single vs double selection (BCH abortion-crime),
  DML variance, AIPW score, PLIV, folds/clusters; 401(k) via hdm::pension (`figs/w13_401k.R`: raw
  19,559, OLS 5,896, single 8,099, double 8,956, DML PLR lasso/RF/boost 9,609/8,965/9,907, IRM
  9,319/8,022/8,232; causal forest ATE 7,964, calibration diff .03 p=.47, income GATES 3,946 ->
  19,326, RATE n.s.); Long-story-short sensitivity; meta-learners, causal forest weights, BLP/GATES,
  RATE, policy learning, conformal ITE; Athey-Wager 2019 (SIMULATED NSLM data, said on slide),
  Brand et al. 2021 (2019 WP numbers). Lab 13 not aligned.
- `Math Review/math_review.tex`: 24-page standalone review (calculus, Taylor, linear algebra,
  probability, asymptotics, integrals, penalized optimization), linked from README.
- Problem Set 2 (`Homework/homework2/homework2.qmd`, assigned Thu Sep 24, due Thu Oct 8; 9 pp,
  reworked 2026-09-24): four parts ("Part N:" headings, each on a new page; question titles on their own line, (a)(b)(c) as lettered lists) with an overview table on page 1; NO points anywhere (user:
  "not needed for us"). Part 1 ML in practice: ONE real glm output
  (trace + summary + logLik) in a fancyvrb frame, a note defining deviance and Fisher scoring at
  slide level, two items (likelihood curve; read the lines: deviance = -2 loglik, AIC, LR, why the
  trace stops). The separation example (Output B) was cut 2026-09-24: "fine for general research". Part 2 Killewald 2016 ASR, written for average students: paper
  reference + Duke Libraries DOI, "The question / The data / Reading Table 3 / Reading Figure 1 /
  Two formulas" walkthrough, Table 3 excerpt typed as a `[H]` table (with couples/divorces from
  Table 2 and the cross-cohort star on housework); Figure 1 is NOT reproduced as an image; its twelve bars are
  transcribed in a `[H]` table right after "Reading Figure 1" (user 2026-09-24; item 1(c) "copy the bars" cut). Seven items that build the reading from
  scratch (user 2026-09-24: "logical and slow ... for an entry level student"): the data
  (couple-years, per-year rates); the model written as an equation with the
  printed numbers; odds defined and computed; odds ratio DERIVED by subtracting two couples'
  log-odds and exponentiating; OR -> probabilities from 3.3% and from an imaginary 30%; z, stars,
  results sentence, cross-cohort difference with sqrt(se1^2+se2^2); units (0-1 housework). Part 3 "own logit reported like a paper" CUT 2026-09-24 (user); Parts 3-4 now:
  counts (Poisson vs vcovHC SE, then dispersion test; hurdle), groups (clustered SE -> RI only; the FE / three-slopes / three-questions item was cut 2026-09-24). Answer key
  `homework2_answers.qmd` is gitignored, fully executed, mirrors the numbering.
- Remaining to build: Weeks 11-14 slides+labs, Problem Sets 3-5, midterm review, final packet.
  Source decks for later weeks: `../SOCIOL690S` (see napkin Build Plan).

## Gotchas
- Proportional odds is tested against the cumulative logit with cut-specific slopes
  (generalized ordered logit), NOT against the multinomial (different family, not nested).
  Tools: `brant::brant(polr_fit)` (Wald) and `VGAM::vglm(..., cumulative(parallel = FALSE ~ x))`
  (LR, one covariate at a time; freeing all slopes gives non-monotone probabilities and NaN).
  `brant` was installed to the user library on 2026-09-16. The lab's proportional-odds
  check was dropped 2026-09-16 (user not confident presenting it); the slide frame keeps it.
- Quarto renders can fail on this machine for two reasons unrelated to the lab: the sandbox
  blocks the launcher's `sysctl` call, and `~/Library/TinyTeX` has tcolorbox 6.9 on a 2025-06
  kernel (fix: `tlmgr update --self --all`). Workarounds are in the napkin.
- Dropbox sometimes drops `slides.pdf` mid-session; just recompile. Delete any
  "conflicted copy" build artifacts (.aux/.out/figure pdfs); they are junk.
- `glmer(..., nAGQ = 10)` in lab 4 takes about a minute.
- `gss_earnings.rds` has a column `wt` (survey weight): never write `lm(..., weights = wt)` with a
  local vector called `wt`; model.frame takes the data column first. Use another name.
- `degree` in the GSS extract is an ordered factor: use `factor(as.character(...))` before
  `lm()` with it as a dummy set, or `contr.poly` errors appear.
