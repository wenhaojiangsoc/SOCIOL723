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
- No 'Step N:' in frame titles (user 2026-10-27).
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
- Week 9 (RD, 104 pp; 'Why Not a Global Polynomial: the Huai River' after Other Checks (690S figure = Ebenstein et al. 2017 Fig 3, local linear, -3.1 [1.3,4.9]; Chen et al. 2013 cubic 5.5 [0.8,10.2]; web-verified via PMC); Heaping slide corrected: heaps at 1,500 g and 1,503 g are on the UNTREATED side (IZA DP 5106 checked); Close Elections Under Scrutiny DROPPED; 'What Sorting Does' rewritten as the incumbent-machine story (intro, then manip_density.pdf, then three intuition bullets; no simulation numbers); Flexible Covariate Adjustment, RD-vs-experiment power (3/4, 2/pi) frames and the 'Covariates, Power, and What to Report' list DROPPED; 'Covariates, Power, and What to Report' kept as ONE frame (covariates, effective n, power expanded with rdpower/rdsampsi intuition; discrete-X bullet dropped) ; power bullet and the 'Power: Would We Detect the Effect?' frame DROPPED by user 2026-10-20 (w9_estimation.R section 6 still prints rdpower numbers); covariates table AND balance table both at h_CER (bwselect cerrd), CONVENTIONAL estimates (coef[1]) with robust p (w9_senate.R 8b and 9a; switched from bias-corrected 2026-10-25, user): no covs 8.13, lag vote jump 3.47 p .20, all four 7.44; balance lag vote 3.49 p .18. Rule for slides and labs: report rdrobust's conventional estimate with the robust interval/p (what summary() prints); the robust CI is centred at the bias-corrected estimate; CER is a separate bandwidth choice; 'Why tau Moves Only If Z Jumps' DROPPED; Senate covariates table = tau_Z (p) and tau-tilde one covariate at a time (note: fixed h in rdrobust forces b = h); covariates rewritten plainly: 'Adding Covariates' = spec with Z'gamma, pre-determined (turning-21: sex, race, schooling), tau barely moves / se shrinks, big move = Z jumps, no DxZ, no post-treatment; 'Why tau Moves Only If Z Jumps' = one-Z identity tau~ = tau_Y - gamma tau_Z without c(V) notation; Repair 3 (honest) frames DROPPED by user, honest row in the repairs table carries an adv star; '92% not 95%' adv frame DROPPED by user (picture slide carries it); inference reorganized: 'Three Repairs, Side by Side' table after the 92% frame (none / 1 undersmooth h=10 [4.38,11.59] / 2 RBC [4.09,10.92] + at h_CER [4.07,11.29] / 3 honest [3.83,12.23]); later frames titled 'Repair 2: ...', 'Repair 3: ...'; sensitivity table says 'robust bias-corrected'; RBC s.e. shown via b = h: local quadratic, se = sqrt(9s+^2/m+ + 9s-^2/m-) = 2.21 (edge factor 9 vs 4; rdrobust 2.06; default b 1.74), weighted-sum version REJECTED by user as too complex; 'bias-corrected with the old s.e.' row and bullets REMOVED (user: confusing); 2026-10-19 Bol et al. 2018 (3 frames) MOVED to Local Randomization after 'Choosing the Window'; Carpenter-Dobkin 2009 drinking-age example added after Winners vs Losers (2 frames, Fig 3 cropped to figs/carpenter_dobkin_fig3.png; Tables 2,4,5 verified from the AEJ PDF); new 'Inference: the Standard Error' frame before the How Sure picture (se = sqrt(4s+^2/m+ + 4s-^2/m-) = 1.47 from figs/w9_estimation.R section 5, rdrobust 1.46, usual CI [4.56, 10.27]); h_CER named + cited on the sensitivity slide; 2026-10-19 user approved blue stars on: What Is Left?, Shape: Two Ways, Bias and Variance Definitions, Bias of the Line Step 1 picture + Step 4, (How Sure picture and RBC Idea stars REMOVED by user; RBC Formulas made adv); h_CER defined on 'Repair 2 on the Senate' (row 7.68, 1.84, [4.07, 11.29]; b = h identity rows dropped), not on the simulation slide, Senate in Five Lines; the four estimation 'Steps' RENAMED by choice (Shape / Weights / Bandwidth / Inference: ...) so 'Step N' is used only inside derivations; roadmap table has a 'what is at stake' bias/variance column; bias of the line now 4 frames: the picture (curve, fitted line, gap at c; no new notation, user dislikes extra symbols like Int[g_i]), Taylor = 'we cannot fit a line to an unknown curve, but can to a parabola', Step 3 plugs a+bu+qu^2 into the OLS slope/intercept formulas (b cancels, -q/6; user rejected 'line fitted to qu^2' pieces), both sides; 2026-10-19 user DROPPED "Identification, Step by Step" and both Lee "Why Continuity Should Hold" frames; 2026-10-19 user: estimation made intuitive for first-years; then every advanced derivation
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
  extensions -> sociology: Bol et al. 2018 PNAS, Legewie 2016 AJS. Lab 9 (Senate, 16 pp) extended 2026-10-20 for PS3: conventional-vs-robust rows, h_CER (cerrd), p = 1 vs 2, covariate balance and covs at cerrd (numbers match w9_senate.R 8b/9a), fvextra header added. 2026-10-25: fuzzy LATE written out in 3 frames (types / numerator / denominator, Week 7 style); 'Estimation, Route 1: Two Sharp RDs and Their Ratio' (added 2026-10-25, user; same h/kernel/polynomial, SPP .269/.619 = .435) then 'Estimation, Route 2: Local 2SLS' (ivreg on SPP .435, figs/w9_fuzzy.R section 3) = Stage 1 D-hat + Stage 2 (slope terms X-c and Z(X-c) in both stages; the old D(X-c)-endogenous version was not the ratio); 'Fuzzy RD: Choosing the Bandwidth' frame and both Lab 9 simulation bullets DROPPED by user 2026-10-25; Local 2SLS ends with one bullet: rdrobust picks one MSE-optimal h for the ratio, same h in both stages; 'Weak First Stage: Anderson--Rubin at the Cutoff' frame DROPPED 2026-10-25 (user), weak-first-stage bullet now says use the Week 7 first-stage F; Feir et al. 2016 and Noack-Rothe 2024 refs removed; 2026-10-25 (user): two picture frames after 'When the Rule Is Not Followed': 'The Jump in D' / 'The Jump in Y' on Ser Pilo Paga (figs/w9_fuzzy.R section 3 -> spp_jump_D.pdf, spp_jump_Y.pdf; h 9.04, jumps .619/.269, ratio .435 = rdrobust); 2026-10-25 (user): 'Geography as a Running Variable: Three Specifications' DROPPED (with refs Dell 2010, CTY 2025, Papay et al. 2011, Reardon-Robinson 2012) and the 'Specification 1 of the previous slide' bullet on 'the Specification'. Lab 9 adds a real fuzzy RD: Ser Pilo Paga (Londono-Velez et al. 2020) from the CIT 2024 Extensions replication file, saved as Data/spp_colombia.csv (in the CSV T = 1 iff X1 >= 0, sign opposite to the book's text); first stage .625, RF .269, fuzzy .435 [robust .366,.524], one-sided non-compliance. 2026-10-25: lab aligned with slides: Step 3 prints first-stage F = 1288 (rdrobust tau_T/se_T squared); Step 4 shows Route 1 (same-h reduced form .269 / first stage .619 = .435) next to 2SLS; exercise 6 points to the 'Reading and Reporting an RD' checklist (7 items, not 'six checks'). 2026-10-25 (user): Dell-Querubin boundary now 5 frames (setup+map / Geography as a Running Variable: assumption, estimand, checks, compound treatment / Three Specifications: lat-lon polynomial + segment FE (Dell 2010 cubic, DQ local linear 25 km), distance to line, point-by-point rd2d; two scores / Specification / Balance (Table 9) / Boundary Results (Table 10)) + 'What the Boundary Cannot Separate'; NBER WP 22395 numbers. Several Cutoffs, Away From the Cutoff, Boundaries and Two Scores frames DROPPED (pooling-weights point moved to 'The Score Nobody Saw'). Extensions (RD in Time moved FIRST 2026-10-25, then kink, then repeated cutoffs) = Regression Kink (3 frames: Austrian UI picture + hypothetical 0.18, estimator kappa_Y/kappa_B (chain-rule derivation DROPPED 2026-10-25, user explains in lecture), CLPW 2015 elasticity 1.4-1.9) -> RD in Time (setup with formula + conceptual sharp-RD-in-calendar-time figure figs/w9_rdtime.R -> rdtime_concept.pdf, hypothetical; turning-21 table DROPPED 2026-10-25, user: turning 21 IS RD in time once realigned to own birthday, the difference is one common calendar date; Legewie 2 frames MOVED here, 'Applications in Sociology' section removed; 'What to Watch' frame DROPPED 2026-10-25 by user) -> Repeated Cutoffs (Problem / Two Effects / Peeling the ITT with hypothetical 10,12,11 / Assumptions and Results: CFR 2010 $1.50 per $1, Hsu-Shen 2024, Baron 2022) -> FEMA design exercise DROPPED 2026-10-25 by user (3 frames + Garrett-Sobel 2003, Howell-Elliott 2019 refs).
- Week 10 (Panel + DiD, 110 pp, rebuilt 2026-10-08; user frames kept verbatim): opens with 'A Third Source of Variation'
  (2026-10-26, user: INTUITION ONLY, NO FORMULAS; one 'core idea' block: stable traits removed by the before-after
  change = FE, common change measured by untreated units = DiD; one-assumption line; NO 'Two Kinds' frame, NO Plan frames anywhere: user never talks about them)
  -> PANEL SECTION REBUILT SLOW 2026-10-25 (user: start with alpha_i only, no time; FD before FE; examples inside the
  theory; `figs/w10_panel_slow.R` -> pan_intercepts.pdf, pan_demeaned.pdf, pan_time.pdf): The Example (data first) ->
  Step 1 pooled .220 -> Step 2 FD 1980-87 no intercept .445 -> Step 3 man intercepts (four men; pooled .56 vs within .13;
  all men .243) -> Error Components (FE.1-3, alpha only, marriage-data column) -> Step 4 demeaning (.2427 both ways; 235
  stayers add nothing) -> Step 5 time intuition (wages 1.39->1.87, married .19->.61) -> Step 5 control group (FD with
  intercept: growth .488/.476/.200, lambda .451, beta .051 = DiD) -> Step 6 two-way FE + double demeaning (.0606) ->
  strict exogeneity + lead test -> Four Ways -> What FE Averages -> What FE Costs -> FEIS -> impact function -> Budig &
  England, Ludwig & Bruderl x2 -> lagged DV block (FE or LDV?, Nickell, Anderson-Hsiao, Arellano-Bond, dynamic wage eq).
  DROPPED 2026-10-25 (user): Imai-Kim frame, First Differences (old), FE or FD?, Cluster by Unit (and Arellano 1987 ref),
  Panel FE Goal and Plan. RE-in-panel and Mundlak-panel frames MOVED to the FE vs RE section ('On the Marriage Data: ...') ->
  short section 'Fixed or Random Intercepts, Without Time' (2026-10-25, user: replaced the 21-frame HSB schools section
  with a simulation; key point = the only difference is whether alpha_j may covary with X; `figs/w10_fe_sim.R`:
  fere_worlds.pdf + fere_sims.pdf, 100 groups of 4, theta .65; world A pooled/RE/FE .995/.999/1.000 sd .126/.080/.088;
  world B 1.790/1.348/1.003; Hausman rejects .056/1.00, Mundlak clustered .061/1.00; How to Choose frame kept).
  The old HSB section (figs/w10_fe.R, fe_*.pdf, re_theta.pdf) is now UNUSED but kept; restorable from commit ba1e741. Panel section
  rebuilt Wooldridge-style 2026-10-25 (`figs/w10_panel_std.R`: FE.1-3, double demeaning, strict exogeneity + lead test,
  FE vs FD + FD-residual AR -0.40, clustering, RE theta .64, Nickell sim, Anderson-Hsiao, Arellano-Bond + AR(2); DiD untouched) on
  `wooldridge::wagepan` (`figs/w10_panel.R`: marriage premium pooled .108/RE .064/FE .047/FD .038,
  Mundlak, FE variance weights, Imai-Kim DAG, FEIS, impact function; Budig & England 2001; Ludwig &
  Bruderl 2018 FE 8.3% -> FEIS 0.5%) -> 2x2 (Card-Krueger 2.76 + ck_parallel.pdf from figs/w10_ck_parallel.R (2026-10-28: DiD frames carry worked examples: CK numbers in regression form, parallel-trends picture, double difference split in two with CK bias example, hypothetical two-store-type covariate example, base-period table gamma .1; allowframebreaks removed from DiD frames, they were spilling); Roth-Sant'Anna scale; OR/IPW/DR;
  covariates in TWFE; placebo laws `figs/w10_placebo.R` iid .477 vs cluster .049) -> event studies
  (Roth 2022, base periods, plotting, Kleven et al. 2019 child penalty) -> staggered on
  `bacondecomp::divorce` (`figs/w10_divorce.R`: TWFE -3.05; Bacon table; 267 negative weights;
  CS/SA/ETWFE-never -10.56, imputation = ETWFE-notyet -4.85; HonestDiD breakdown < .05) ->
  beyond (dose, on/off, DDD, distributions, design-based) -> sociology (Harknett et al. 2021;
  Choper et al. 2026 ASR; Kalil et al. 2023) -> Chiu et al. 2026 lessons. Lab 10 aligned 2026-10-28 (see below).
  2026-10-27 Codex review applied (see napkin): AB lag-3 row now reports the AR(3) test (p .97), event-study date = G_i. 'Step N:' prefixes DROPPED from all Week 10 titles
  (user 2026-10-27: steps are confusing); pooled-OLS u_it = stable traits + what changes over time; alpha_i = combined effect of everything stable.
  2026-10-28 PANEL SECTION REBUILT AROUND TWO IDEAS (user accepted my outline): Idea 1 = compare each man with himself
  (removes alpha_i), Idea 2 = compare with men who did not change (removes lambda_t). ACT 1 (2x2, 528 men, 1980/87, blue
  stars): Example -> Pooled .358 -> Compare Each Man With Himself (Idea 1) -> Control Group .012 (Idea 2). ACT 2 (545 men,
  1980-87): 'All Men, All Years: The Model' (alpha_i who he is, lambda_t what year it is) -> 'Idea 1 Only' (pan_paths_man.pdf,
  .243) -> 'Idea 2 Only' (pan_paths_year.pdf, .150 (.028), two black lines) -> 'Both Ideas' (pan_twoway_paths.pdf, .061) ->
  'How Software Does It' (toy 2 men x 3 years: Y 1.5/1.8/2.4, 1.2/1.5/1.8; man 1 marries 1982; beta .3) -> 'Two-Way FE Can Be
  Demeaned: the Algebra' (adv) -> 'Scoreboard' (all six estimates). ACT 3: Budig & England spec (TWFE, p. 213 'fixed for
  years and persons'; no age: person + year fix it) + Results. ACT 4: 'What Two-Way FE Assumes' (FE.1-3 table) -> 'Strict
  Exogeneity, in Words' (feedback; something else changing) -> algebra (adv) -> lead test -> Ludwig & Bruderl -> Random Trends
  -> Impact Function -> L&B over time -> What FE Averages -> 'TWFE: Strengths and Weaknesses'. DROPPED: 2x2 picture, Why One Year
  Is Dropped, Four Ways, What FE Costs (folded into strengths/weaknesses), old 2x2 dummy tables. figs/w10_panel_slow.R cleaned
  (a splice had duplicated sections 3-5).
  2026-10-28 (user: 'expand equations slowly'): theta split (derivation line by line + 'What theta Is' with n = 1/4/20 ->
  .40/.65/.84); Mundlak add-and-subtract step; What FE Averages split (one man's b_i example 1.75 - 1.58 = .17, then the
  weights with the 0/1-variance step); Random Trends = 3-step recipe + 'What Changes'; DiD covariates: 'Estimator 1,
  Regression' and 'Estimator 2, Weighting' worked on the two-store toy (both 2.0); 'Reading the Event-Study Equation' (man
  married 1984, k table); Kleven 'Reading the Child Penalty' (hypothetical 40,000/200,000 = .20); Forbidden Comparison split
  Algebra / Numbers; CS split 'Build Clean Blocks' (toy ATT(4,4) = 1) / 'Then Aggregate'; 'Imputation on the Toy Example'
  (1.75; TWFE 1.10, 0.50 without never-treated). Toy + theta numbers: figs/w10_toy_staggered.R. Deck 122 pp. Still spilling
  (pre-existing, adv): GB Weights Explicitly, Where the Negative Weights Come From, Negative Weights: Consequences, Honest
  Inference About Pre-Trends.
  2026-10-28 round 3 (user): Model slide has no bold, ends 'people call it TWFE'; 'Man Fixed Effects: Each Man His Own Line'
  uses pan_man_lines.pdf (MARRIED ON THE X AXIS, same four men as the calendar figures; pooled .66, within .31; all men
  .243); 'Year Fixed Effects: One Common Year Path' (pan_paths_year.pdf with the +0.41 common-growth arrow); 'Two-Way FE: Own
  Level, Common Year Path'; algebra (adv) BEFORE the toy 'How Software Does It: Dummies, or Means' (right table = each man's
  MEAN, not demeaned; bullet: TWFE compares each man with himself over time); Scoreboard DROPPED. Strict exogeneity: 'in
  Words' corrected (a raise before marriage lifts the UNMARRIED baseline -> FE understates); algebra now two adv frames
  (estimator's error step by step; T = 2 = FD with same-year vs across-year products, raise example). FE weights now
  three frames (Step 1 N_i = S_i b_i; Step 2 S_i = Tp(1-p) with T=8, m=2 check; Step 3 two men 2.0/0.875 -> .19 vs .25,
  numbers in figs/w10_toy_staggered.R). Deck 124 pp.
  2026-10-28 round 4: a two-way (double-dot, rows-indexed-by-r) strict-exogeneity version was tried and REVERTED
  2026-10-29 by the user: the two adv frames are man effects only (tilde, no lambda_t), 'Two Years' = FD without
  intercept. 2026-10-29 (user): FE-averages block (one man's b_i + Weights Steps 1-3) MOVED right after the toy 'How
  Software Does It'; 'Strict Exogeneity, Step by Step: Two Years' and 'The Impact Function' DROPPED (Ludwig-Bruderl 2021
  ref removed); NEW 'When Men on Steeper Paths Marry' before Ludwig & Bruderl (figs/w10_slopes.R -> pan_slopes.pdf:
  hypothetical, true effect 0, steep men marry, TWFE .216, FEIS 0). Slide 8 bullet: the year FE makes the stayers the
  control group. NEW 'Why Put Last Year's Outcome in a Fixed-Effects Model?' opens the lagged-outcome block
  (marriage follows wages; examples: health and employment, parental help and test scores, policing and crime).
  2026-10-29 (user): MUNDLAK DROPPED from slides and lab (FE vs RE uses Hausman only: 'Testing Fixed
  Against Random: Hausman' explains numerator/denominator, sim 5.6%/100%, marriage chi2 37.0 + efficiency caveat;
  'On the Marriage Data: Mundlak' frame, hybrid-model bullet, Mundlak 1978 and Allison 2009 refs removed; How to
  Choose table rewritten). FEIS slide equation now includes lambda_t. Deck 122 pp; lecture stops at 'Kleven:
  Results' (p. 71).
  Lab 10 (2026-10-29, user): feols(..., cluster = ~nr) is the default; felm() (lfe: y ~ x | FE | IV | cluster) shown to
  match (clustered se .0215 vs .0215, small-sample correction differs in the 4th decimal); stargazer tables built from
  felm/plm objects (stargazer cannot read feols) for the four-model table and pooled/RE/FE; decomposition and
  event-study sections DROPPED from the lab; 3.3 = 'How to cluster by man'. 20 pp.
  2026-10-29: 'Inference: Placebo Laws on Real Data' DROPPED (Conley-Taber, MacKinnon-Webb refs removed; figs/w10_placebo.R
  unused). NEW 'Why Control for Covariates in a DiD?' (X matters only if it predicts the trend; examples minimum wage, job
  training, marriage) and 'Covariates in TWFE: What Works' (figs/w10_did_cov.R: TWFE 3.20, + type dummy 3.20 absorbed,
  + type x period 2.00, regression 2.00; heterogeneous A+4/B+1: ATT 1.60, type x period TWFE 2.50 = equal variance
  weights). Estimator 1 = g-computation on Delta Y. Deck 123 pp; lecture stops at 'Kleven: Results' (p. 72).
  Pushed 4e98cc5 (slides, lab, scripts, used figures).
  Lab 10 Part 3 (2026-10-30, user): DiD covariate adjustments on unilateral divorce, by hand + DRDID. 2x2 = 11 states
  adopting 1969-71 vs 11 not treated by 1976 (adopt 1977+ or never); pre 1964-68, post 1972-76; X = South (census region,
  DC = South) and 1964 persincome. Unadjusted = TWFE = TWFE + south dummy -0.32; regression = IPW = DR (region) 0.947;
  region + income OR 1.01 / IPW 0.38 / DR 0.52 (se 2.5); TWFE + south x period 1.16 (variance weights) vs ATT weights
  0.947. Design chosen by principle (most not-yet-treated controls), not by result; other windows give -6.7 to +5.8.
  Staggered part is now Part 4. Lab 26 pp.
  2026-10-31 (user: slower, examples first): DiD covariates now = Why Control -> worked example -> 'The Usual Fix: Add X
  to the Regression' (naive; X fixed per store is absorbed: types 3.20 -> 3.20, size -0.04 -> -0.04) -> 'What Else Goes
  Wrong' (Caetano-Callaway list) -> G-Computation Step by Step (store rows) / in a Picture (figs/w10_gcomp.R ->
  gcomp_picture.pdf: continuous size, plain -0.04, g-comp 2.01, shaded extrapolation) / in Code -> Weighting -> In General
  -> 'Back to TWFE: Interact X With Period'. Event studies: equation -> reading -> Kleven x3 -> 'Questions to Ask of Any
  Event Study' -> spec details, pre-trend power, base periods, drawing. Staggered: forbidden-comparison figure REDRAWN in R
  (figs/w10_forbidden.R -> forbidden.pdf; the tikz version hid the late jump); toy decomposition (2.0/1.0/1.5/-0.5, weights
  .3/.3/.2/.2 = 1.10) and toy negative weights (.3/.3/-.1/.5) from figs/w10_toy_staggered.R via bacondecomp::bacon();
  GB weights, negative weights, consequences split (no allowframebreaks left except References). Beyond: Doses x2
  (hypothetical .5/1.4, .9 = .6 causal + .3 selection), on/off history table, DDD 3 - 1 = 2, Distributions and Uncertainty
  split; Honest Inference with round-number M-bar. Deck 130 pp.
  2026-11-01 (user): order is now DID (2x2 + covariates) -> NEW section 'Many Periods: Two-Way Fixed Effects' (common
  adoption date; 'From Two Periods to Many', 'What One Number Hides': toy growing/fading 2.0/2.0, window to t5 1.5, pre-trend
  no effect 1.2; figs/w10_twfe_many.R) -> NEW section 'Event Studies' (opens with 'Event Studies', then 'The Event Study on the
  Toy Example' twfe_many_es.pdf: more transparent, not more causal; then reading, Kleven, critiques) -> Staggered. Deck 137 pp.
  2026-11-01 later (user): DiD COVARIATE BLOCK DROPPED from slides (too abstract; restorable from scratchpad copy / git; refs
  Abadie 2005, Caetano-Callaway, HIT 1997, Sant'Anna-Zhao removed; gcomp_picture.pdf unused; Lab 10 Part 3 still has it).
  Many Periods now adds 'TWFE Gets Run on Any Treatment' (four conditions for the per-unit reading) + Doses x3 (new 'Doses:
  What TWFE Reports': 0.7 = .5(.5) + .5(.9), figs/w10_twfe_many.R) + On/Off, all MOVED from Beyond (now 'Beyond the Basic
  Design': DDD, distributions, uncertainty). Deck 126 pp.
  2026-11-01 (user): Running Example = text intro (policy, question, data, adoption-cohort table 9/14/20/3/5), divorce_raw
  figure dropped from it. Forbidden comparison = Toy Panel table -> 'Late vs. Early, t = 2-4' picture (forbidden.pdf redrawn:
  before t=2,3 / after t=4 windows, early's before-mean 1.5) -> 'the Numbers' table (1 - 1.5 = -0.5; the old t=3-vs-4 'in
  Numbers' frame giving 0 was INCONSISTENT and dropped) -> Algebra (+ toy line). User rule: whenever a 2x2 is named, state
  its before and after periods; decomposition table now has a 'before -> after' column. Deck 127 pp.
  Then (user): forbidden-comparison picture/numbers/algebra use ONLY t = 2 -> t = 4 (late 0->1, early 1->3, 2x2 = 1 - 2 = -1);
  decomposition table keeps GB's window means (-0.5) with a footnote saying why it differs from -1.
  'Forbidden Comparison, in Algebra' DROPPED (user: too complex). GB Decomposition frame now links to the panel FE weights
  (T p(1-p) per man <-> size x Dbar(1-Dbar) per 2x2 pair). Deck 126 pp.
  2026-11-01 STAGGERED REORDERED (user): section renamed 'Staggered Adoption'. Order: Setting -> NEW 'The Marriage Data Are a
  Staggered Design' (162 never / 73 always / 239 staggered 51,33,40,27,30,27,31 / 71 switch off) -> Running Example -> forbidden
  x3 -> GB decomposition, toy, weights explicit, what they imply -> divorce decomposition -> NEW 'Decomposition on the Marriage
  Data' (474 absorbing men, TWFE .065: never .48/.041, earlier-vs-later .14/.043, later-vs-always .21/.133, later-vs-earlier
  .17/.068; figs/w10_marriage_timing.R) -> negative weights (where, toy, intuition, divorce, consequences, diagnostics) ->
  Modern Estimators overview -> CS x2, imputation x2, ETWFE, stacked -> six estimators, event studies side by side ->
  Honest Inference + Sensitivity -> Choices (summary). Deck 128 pp.
  2026-11-01: 'The Event Studies, Side by Side' = figs/w10_divorce_es.R -> divorce_es5.pdf (ggplot, 5 panels: TWFE, Sun-Abraham,
  CS never, imputation, ETWFE never; SA = CS = ETWFE: avg k 0-10 -6.53, k15 -14.85; TWFE -3.09; imputation -0.97) + NEW
  [fragile] 'How to Run Them in R'. didimputation + etwfe installed to user lib; DIDmultiplegtDYN 2.4.0 needs polars <2
  (polars 2.0 dev breaks it; removed), so no dCDH panel.
  Later 2026-11-01 (user): CS split into 'Cohorts and Blocks' (figs/w10_cs_blocks.R -> cs_blocks.pdf) + 'Every Block on the
  Toy' (1,2,3,1 -> 1.75) + 'Then Aggregate'; NEW 'Covariates Inside Each Block' (hypothetical South block: unadjusted 2.0,
  OR = IPW = 1) and 'Sun-Abraham: the Same Blocks, as One Regression' (SA vs CS table). 'How to Run Them in R' DROPPED.
  BEYOND SECTION DROPPED (DDD, distributions, uncertainty; refs Olden-Moen, Ortiz-Villavicencio-Sant'Anna, Rambachan-Roth
  2026 removed). Deck 128 pp.
  2026-11-01 REORDER (user): Many Periods section DROPPED ('From Two Periods to Many', 'What One Number Hides' cut: multi-period
  TWFE is already in the panel section). Order now DID -> Event Studies (D binary 0/1, said on the slide; toy ES figure kept)
  -> Staggered -> NEW section 'TWFE Beyond a One-Time 0/1 Treatment' (TWFE Gets Run on Any Treatment w/ sociology examples:
  number of children, hours, neighborhood poverty; married/union on-off; four conditions) -> Doses x3 -> NEW 'Doses With
  Staggered Timing: What to Do Instead' (CGS 2025 ATT(d|g,t), contdid; dCDH 2026) -> On/Off -> Sociology -> Practice. 127 pp.
  2026-11-02 REORDER (user): DID -> Staggered (static TWFE = 'the 2x2 with many periods'; forbidden comparison, GB,
  negative weights) -> Event Studies (D binary; static beta vs one tau_k per period; 'Each tau_k Is a 2x2 Against
  k = -1' by plug-in with G = 0; toy ES; reading; Kleven x3; spec details; NEW 'The Event Study Has the Same Problem':
  toy with late effect 3, TWFE ES -0.75/-0.25/2.00/2.75/3.25 vs truth 0/0/2/2/3, figs/w10_toy_staggered.R) -> NEW
  section 'Clean Comparisons: the Modern Estimators' (CS ... ES side by side, Choices) -> Beyond 0/1. DROPPED: Questions
  to Ask, Pre-Trend Power, Base Periods x2, Drawing, Honest Inference, Sensitivity (refs Roth 2022/2026, Freyaldenhoven
  x2, Rambachan-Roth, BDM removed). Deck 122 pp.
  Then (user): 'Modern' section DISSOLVED. Staggered ends with the single-number fix ('The Fix: Clean 2x2s, Then One
  Average' table, CS x4, imputation x2, ETWFE, stacked, Six Estimators); Event Studies ends with Same Problem ->
  Sun-Abraham -> Event Studies Side by Side -> Choices. Deck 121 pp.
  [MOVED, see below] 'Why Not Just Run an Event Study?' after 'CS: Every Block on the Toy' (p. 79): TWFE ES exact on the toy (effects
  depend only on k), wrong when late's effect is 3 (-0.75/-0.25/2.00/2.75/3.25); figs/w10_toy_staggered.R. Deck 122 pp.
  Then (user: p. 79 used the event study before it was introduced): slide 79 DROPPED; 'Same Problem' REPLACED by two
  event-study frames: 'A Staggered Event Study, in 2x2s' (plug Y = a + l + tau_{t-G}; early vs never 1->2 = tau_0, late vs
  never 3->4 = tau_0, late vs early 3->4 = tau_0 - (tau_2 - tau_1) = 0 on the toy: forbidden 2x2 still in the regression,
  harmless only if cohorts share the path) and 'When Cohorts Differ, the 2x2s Disagree' (late effect 3: 1, 3, 2; TWFE ES
  -0.75/-0.25/2.00/2.75/3.25 vs truth and clean blocks 0/0/2/2/3). Deck 122 pp.
  2026-11-03 (user): 'The Marriage Data Are MOSTLY a Staggered Design' (p. 58): 71 men leave marriage = on/off, not
  staggered; 474 of 545 absorbing; new bullet points to the Beyond 0/1 section.
  2026-11-03 (user): 'Negative Weights on the Divorce Data' splits the 267 cells: always treated 207 / -0.36 (net
  weight 0: controls only), 1969-70 60 / -0.02 (figs/w10_divorce.R section 3). STACKED DiD DROPPED (frame, table row,
  Cengiz et al. 2019 and Wing et al. 2024 refs); sociology frames still say the papers used stacking. Deck 120 pp.
  2026-11-03 (user): EXTENDED TWFE (Wooldridge 2025) DROPPED from slides and lab (frame, ref, etwfe panel; SA = ETWFE-never
  in the event study). Event-study figure now figs/divorce_es4.pdf (4 panels; divorce_es5.pdf unused). Staggered
  scoreboard 'The Estimators on One Dataset' has no SA row (SA is introduced in Event Studies; its frame says overall
  -10.56 = CS never); Fix table SA row = 'CS's blocks as one regression (Event Studies)'. Deck 119 pp.
  2026-11-03 (user): SOCIOLOGY (Harknett x3, two recent studies) and PRACTICE (Chiu et al. 49 studies, DiD checklist)
  sections DROPPED; refs Harknett, Choper, Kalil, Chiu, Baker removed. Kleven 'non-parents' bullet dropped; spec-details
  never-treated bullet cut to one line. t = 0 column added to 'Each tau_k Is a 2x2'; toy ES slide links to Ludwig-Bruderl.
  Deck 110 pp, ends Beyond 0/1 -> Looking Ahead.
  2026-11-03 (user): ES 2x2 frames rewritten on the ORIGINAL toy: 'The Event Study on the Staggered Toy' (forbidden
  2x2 late vs early 3->4 = 0: static reads beta = 0 -> 1.10; ES reads tau_0 - (tau_2 - tau_1) -> tau_0 = 1, exact 1,2,3)
  and 'What the Event Study Still Assumes' (one path for all cohorts; break it with late = 3 -> -0.75/-0.25/2/2.75/3.25).
  2026-11-03 (user): Side by Side figure back to figs/divorce_es5.pdf = TWFE, SA, CS never, CS NOT YET (avg k0-10 -3.10,
  k15 -14.82), imputation (divorce_es4.pdf unused). Slide 94 bullet: why leads move (alpha_late 0.75).
  2026-11-03 (user): Side by Side = one row of 5 panels, no legend. Doses: ATT(a|d) = E[Y(a)-Y(0)|D=d] defined (a = d
  observed only); 'What TWFE Reports' = three 2x2s before->after ($1 vs $0 0.5, $2 vs $1 0.9, $2 vs $0 1.4 over $2 = 0.7).
  2026-11-03 (user): ES forbidden comparison clarified: 'Staggered Toy' says the forbidden 2x2 is still in the regression but
  corrected, harmless if every cohort shares one path; 'Still Assumes' says the late = 3 distortion comes from cohort
  differences alone, plus a three-cohort contamination line (all 1 at k = 0, later paths differ: tau0-hat 1.05, leads
  -0.02/-0.21, clean 1/0/0; figs/w10_toy_staggered.R end). Lab text matched (no new chunk). Then (user): framed as 'an implicit forbidden comparison'; assumption =
  each cohort's effect at k is the same (may grow with k, not differ by start date).
  2026-11-03: Lab 10 Part 4 aligned (tau_0 in the 2x2 check; ES on original toy exact + forbidden 3->4 = 0; toy2 with
  late alpha 0.75; five divorce ES incl. CS not yet). 15 pp. Pushed 62068a2 (slides, lab, scripts, used figs).
  LAB 10 REWRITTEN 2026-11-02 (user: FE/RE quick; staggered + modern + dynamic ES matter most): Part 1 2x2 four routes
  (.0125); Part 2 four feols models via etable (felm/stargazer/lfe DROPPED), RE + Hausman in one chunk (37.0); Part 3
  staggered one number: toy (forbidden -1, TWFE 1.10, bacon, CS blocks 1.75), divorce TWFE -3.05 + bacon by type,
  scoreboard (CS never/notyet -10.56/-8.69, SA -10.56, imputation -4.85, ETWFE -10.56), CS xformla ~south reg=ipw=dr -3.68;
  Part 4 ES: toy tau_k as 2x2, ES fails on toy2, five divorce ES (avg k0-10 -6.53 x3, TWFE -3.09, imp -0.97) + facet fig.
  Old DiD-covariate (DRDID) part, theta, lead test, three routes, base_stagg/mpdta parts DROPPED. 5 exercises.
  Dropbox gotcha: figure files can turn 'dataless' (online-only) and pdflatex hangs or fails reading them; check
  `ls -lO figs | grep dataless` and `cat` the files to materialize. LAB 10 REBUILT for slides 1-73 on wooldridge::wagepan (installed to user lib 2026-10-28):
  2x2 four routes (cell means, interaction, TWFE, FD = .0125), pooled/man/year/TWFE, three routes to TWFE, iid vs
  clustered se, FE decomposition man by man (one-way .2427 vs plain .2547; two-way .0606 vs .0656), lead test, event
  study (i(k)), RE via plm + theta by hand .643, pooled/RE/FE .108/.064/.047, Hausman 37.0, Mundlak (F 22.7) +
  justification paragraph; old staggered sections kept as Part 3 (after Kleven). 23 pp.
- Week 11 (SC, 38 pp, reworked 2026-11-04 with the user; all frames SCALAR). Order: naive donor average -> two donors,
  one weight -> many donors (outcome-only weights, -19.5) -> constraints -> predictors + v (slide 13: X_jk = one number
  per state over a researcher-chosen window; outer step uses EVERY pre-year 1970-88) -> 'Two Steps on Two Donors' with two
  COVARIATES (price 1980-88, beer 1984-88; w = Utah's weight; v_price .768, w .574; figs/w11_scalar.R 2b) -> Synthetic
  California (ADH predictors: Utah .342/Nevada .238/Montana .209/Colorado .149/CT .062, gap -18.8, RMSPE 1.78) -> searching
  for v -> What Have We Assumed -> 'SC Is a 2x2 With a Built Control' + 'The Two 2x2s on Prop 99' (figs/w11_2x2.R ->
  prop99_2x2.pdf: donor avg gap -14.4/-41.7 = -27.3 = TWFE; synthetic 0.2/-18.8 = -19.1; period means, not single years)
  -> 'When Parallel Trends Fails After Weighting' (6 intuitive failures, incl. spillover direction and Prop 99 = tax +
  program) -> 'What to Check, What to Do' (donor pool first, pre-fit plot + RMSPE, in-time 1981, LOO, anticipation at the
  vote) -> 'Inference: the Placebo Test' (rank p = 1/39 IS reported as the permutation p-value; state the floor) ->
  placebos figure -> 'A Standard Error From the Placebos' (synthdid placebo SE: SC -19.6, SE 11.1, CI [-41.4, 2.1]) ->
  in-time + LOO with ADH predictors (in-time fit 1970-80, covariates 1972-80 + sales 1970/75/80: -2.9, RMSPE .89; LOO
  -17.6 to -20.6; figs/w11_prop99.R 3b, identical to the lab) -> German reunification -> Prop 47 (Bartos & Kubrin x2) ->
  Extensions: penalized SC (scalar sums, argmin; toy X1=5, A4 B6 C0 D10, penalty 1 vs 25; pensynth) / augmented SC (T =
  one post year; ridge of Y_jT on the donor's own 19 pre-period outcomes, pooled donors, no FE; effect = SC gap - gap the
  pasts predict: -19.5 - (-3.6) = -16.0) / SDiD x2 (why: free constant c, match trend not level; then weights + 2x2:
  gaps -25.4/-41.0 = -15.6) -> Choosing -> Looking Ahead.
  DROPPED 2026-11-04 (user): factor-model frames (fixed gap, one factor, several traits, simulation), Four Tries of v,
  Relation to DiD, Diagnostics and Failure Modes, Spillovers frame (merged into slide 20), Basque, Reporting, Bartos-Kubrin
  Reading It Critically, interactive FE (gsynth), Many Treated Units; refs Abadie-Gardeazabal, Xu, BFR 2022, Doudchenko-
  Imbens, Ferman-Pinto, Kubrin-Bartos 2020 removed. figs/w11_sims.R and figs/w11_more.R are now unused (kept).
  Lab 11 (16 pp, aligned 2026-11-05): fvextra header + width 76, ## notes, optimizer settings as in w11_prop99.R; 'two
  kinds of years' callout; 2x2 section DROPPED (user; only exercise 4 asks for it); section 5 Inference adds a placebo CI
  (sd of the 38 placebo average gaps: -18.8, se 13.8, [-46.0, 8.3]; 14 well-fitting placebos: se 8.3, [-35.1, -2.6]);
  in-time and LOO as on slide 25; section 7 Synthetic DiD via synthdid (set.seed(723) before vcov(method = "placebo")
  reproduces slide 24: SC 11.1, SDiD 8.9; weights; synthdid_plot); section 6 penalized SC via
  pensynth (CRAN; toy .5/.5; lambda 0 = classic SC; cv_pensynth picks ~1e-4; silence solver with
  clarabel::clarabel_control(verbose = FALSE)); exercises: predictor sensitivity, windows, spillovers via Nevada,
  single-year 2x2s, random treated unit. synthdid + augsynth are GitHub-only (reinstalled to the system R lib 2026-11-04).
- Week 12 (ML, 120 pp as of 2026-11-08; slide 3 "A Different Goal" (prediction vs inference) DROPPED 2026-11-08 by user; REBUILT 2026-11-05 in the 690S voice, user: "use my 690S idea and expression", "slower, with
  examples", "use my LASSO/Ridge illustrations"): Bias-Variance = 690S W3 (CEF target, Z-a lemma, decomposition) + toy
  9/10/14 vs rigid 11 (MSE 5.67 vs 1) + new-outcome sigma^2 frame; Regularization = 690S W2 order (high-dim sqrt(p/n),
  three reasons I/II/III, setup, approximate sparsity fig3_1, lasso def with loadings, benefit vs cost, FOC with PARTIAL
  residual + zero-or-not (adv), numbers 30/15/-30 -> .10/0/-.10, three penalties, fig3_2, caveats x2, GSS selection,
  post-lasso + fig3_3 + GSS (female -.110 -> -.173), rates, lasso path, sparse/dense + fig3_4, ridge (marginal-cost table),
  matrix form, SVD + Hoerl-Kennard (kept), elastic net, lava, horse-race table from LAB 12 (not the 690S numbers), CV in
  words/formal/GSS fold table (.865/.579/.618), tuning-lambda tradeoff, two rules, Bates, plug-in x2 (363.8 -> 297.6, 14
  terms)); Trees = 690S W3 (step function, leaf = average derived, step1/2 figs, wage tree, SSE(j,s), six-person split,
  GSS root split SEI 41.6, depth table, fitML curve figure, pruning, bagging (-0.162 -> .379), RF (m = sqrt p), decorrelation
  (.40/.35), boosting idea/algorithm/by hand (SSR 1.573 -> .069)/steps, GD (adv)) then benchmark, stacking, binary; Neural
  nets = 690S W3 frames (single layer, picture, two steps, activations, X1X2 x3 with 2,3 -> 6 check, parameters, MNIST,
  two-layer, GD picture, GD by hand Y=1,2,6, gradient, backprop, backprop by hand, SGD x2, weight decay, dropout, early
  stopping fitml_nn.pdf, tuning) + GSS network. Numbers: figs/w12_690s.R, figs/w12_fitml.R, figs/w12_gss.R. Stars kept on
  carried-over frames; 'Choosing lambda, and a Warning' merged into 'Lasso Caveats' (star moved there). Lab 12 REBUILT:
  690S regularization.qmd (three worlds, lava) + GSS (zero-or-not vs glmnet, CV by hand, plug-in by hand + rlasso,
  post-lasso, instability, leak) + fitML trees/RF/gbm + GSS trees, bagging vs RF, boosting by hand + by rpart loop +
  GD/backprop by hand + nnet (keras3 code shown eval: false). 22 pp.
  2026-11-06 (user: drop what is advanced or not useful in practice; figures in the deck's style): DROPPED Ridge SVD,
  Hoerl-Kennard, Lava (frame + table row + lab code), What CV Estimates (1-se rule + optimism bullet moved into 'CV: Two
  Rules'), Plug-in sigma-iteration frame (folded into one plug-in frame), Boosting Is GD, Backprop by Hand, Why Stochastic
  Gradients, Attention/Transformers, Split Conformal, Double Descent; refs removed. 690S fig3_1-4 REDRAWN in base R
  (figs/w12_sparsity.R: sparse_beta, lasso_sim, postlasso_sim, dense_beta; same DGP/seed as the lab: lasso keeps 7 = 2 big
  + 5 noise, post-lasso inflates the noise ones), gradient picture redrawn (figs/w12_gradient.R, hypothetical), NN and
  dropout diagrams now TikZ (nnin/nnhid/nnout/nnoff styles in the preamble), fitml figures navy truth/firebrick fit.
  Old fig3_*.jpg, gradient.pdf, nn_*.pdf, dropout*.pdf unused (kept). Deck 118 pp.
  2026-11-07 (user): MSE = Week 9 RD derivation (two frames: 'Bias and Variance: Definitions' + 'Decomposing the MSE',
  add-and-subtract; the Z-a lemma frame dropped). ALL MATRIX NOTATION REMOVED: sums sum_j X_ij b_j everywhere; optimism
  derived for the sample mean (2 sigma^2/n), general 2p sigma^2/n stated; 'Ridge, One Coefficient at a Time' (lasso's
  S_j, D_j; b = OLS * D/(D+lambda); D 100, lambda 25: .30 -> .24, .15 -> .12 vs lasso .10/0; figs/w12_690s.R B).
  LITERATURE NEXT TO EACH METHOD (all web-verified from the papers): Blumenstock et al. 2015 Science (elastic net,
  856 respondents, thousands of phone metrics, CV r .68, district r .92) after Elastic Net; Berk et al. 2009 JRSS-A
  (random forests, 30,000 training, 322 = 1.1%, logit flags 2, forest 137/322 caught, 1,901 flagged, 10:1 cost) after
  decorrelation; Kleinberg et al. 2018 (GBDT) moved after boosting; Fragile Families moved after the GSS benchmark;
  Gebru et al. 2017 (CNN 2,657 car classes, ridge/logit to ACS, income r .82, sedans 88%/pickups 82%) after the GSS
  network. Social Science section renamed 'Prediction and Fairness' (Obermeyer + fairness). Deck 121 pp.
  2026-11-08 (user): 'Beyond: Text and Images' replaced by a 22-frame section 'Representations: Text and Images'
  from the 690S Week 14 / SICSS NYUSH deck, in scalar notation with worked numbers (PCA on GSS status variables,
  PCA = linear autoencoder, word vectors by hand, negative sampling = logit, attention by hand, BERT, probing vs
  fine-tuning, text as a control); figs/w12_repr.R. Deck 142 pp. Lab 12 not aligned to it.
  2026-11-09 (user): Today/Plan frames dropped; NEW opening section 'Why Machine Learning?' (three jobs -> CEF ->
  best predictor -> GSS cell means vs OLS -> cells run out -> k-NN bias/variance; figs/w12_intro.R); BV adds the
  standard tradeoff curve; regularization drops CML-specific pieces (Theorem 1.2.1, rates, loadings, T(W)) and adds
  the lasso/ridge constraint picture; NN figures redrawn without overlaps (nnlink style) plus RNN, ELMo, attention
  figures. Deck 148 pp.
  2026-11-09 (user): Trees adds variable importance (3 frames after decorrelation; figs/w12_importance.R ->
  rf_importance.pdf, permutation vs impurity, correlated-credit table). Deck 151 pp.
  2026-11-09 (user): Bias-Variance ends with 'Training, Validation, Test' (4 frames: test set, third piece,
  K-fold picture, choosing k by CV; figs/w12_split.R -> cv_knn.pdf); 'Cross-Validation for lambda' points back. Deck 155 pp.
  2026-11-09 (user): slide 9 'Three Kinds of ML'; PCA derived in 690S NYUSH notation (eigen-equation via Lagrangian,
  2-variable solution, rebuild error = dropped eigenvalues; w12_repr.R H); 690S-style encoder-decoder figures;
  confusion matrix / precision / recall / F1 frames (Berk + figs/w12_classify.R). Deck 163 pp.
  2026-11-09 RESTRUCTURE (user): NO CEF framing anywhere in Week 12 (just 'predict Y'); opening = three reasons
  (poverty/Blumenstock, text+images, causal ingredients -> DML) -> kinds -> line misses -> flexible OLS backfires;
  section 'Overfitting and Honest Evaluation' (k-NN, train/test, CV, then Bias and Variance as the explanation, 'So
  Accept a Little Bias'); regularization slimmed (no FOC/subgradient, no formal CV, no plug-in frame; one-coefficient
  lasso/ridge with sum X^2 = 1). Deck 149 pp.
  2026-11-09 (user): GSS predictors EXCLUDE SEI and prestige (occupation constructs leak earnings); hours kept;
  23 main effects / 243 expanded; all GSS numbers in slides + Lab 12 rerun. PCA section uses 690S matrix notation
  (exception to the scalar rule). Slide 5 = 'Linear Regression Is Already Machine Learning'.
  2026-11-09 REORDER (user): overfitting -> lasso -> ridge/elastic net -> choosing lambda (train/val/test, K-fold, GSS CV)
  -> bias-variance as the explanation of the CV U-curve (lasso simulation figs/w12_lasso_bv.R) -> after tuning. NO k-NN
  anywhere (one dial: lambda). Slide 4 kinds table one line per row + GSS 3-panel figure (figs/w12_kinds.R). Deck 148 pp.
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
- Problem Set 4 (`Homework/homework4/`, built 2026-11-06; assigned Fri Nov 6, due Fri Nov 20; Weeks 10-11): Part 1 DiD =
  Mark & Wu 2022 PNAS read + run on `Data/sexed_county.csv` (CDC model-based teen birth rates + their public funding file,
  `Data/build_sexed_extract.R`); Part 2 SC = Abadie-Gardeazabal 2003 read + run on `Synth::basque` (tidysynth, synthdid).
  Key `homework4_answers.qmd` (gitignored). PS5 = Weeks 12-13, assigned Fri Nov 20, due Fri Dec 4 (day after the final; user's choice). Numbers in the napkin.
- Remaining to build: Weeks 13-14 labs, Problem Set 5, midterm review, final packet.
  Source decks for later weeks: `../SOCIOL690S` (see napkin Build Plan).

## Gotchas
- FEMA RD paper (Week 9 figs/fema): raw + derived DATA live in `figs/fema/data/` (gitignored), scripts in `figs/fema/pipeline/code/`
  (data/code is a symlink to it), rebuild with `pipeline/download_raw.sh` then `pipeline/rebuild.sh`. The session scratchpad is wiped
  on restart (2026-10-26): never leave data only there.
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
