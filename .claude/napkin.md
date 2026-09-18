# Napkin

Course repo: SOCIOL 723 "Social Statistics II: Estimation, Causal Inference, and Machine
Learning" (Duke Sociology, Fall 2026). Second course in the grad stats sequence; follows
*Social Statistics I* (Correll et al. model-comparison approach + Vaisey's DAMCA).

## Corrections
| Date | Source | What Went Wrong | What To Do Instead |
|------|--------|----------------|-------------------|
| 2026-09-15 | user | Pager frame derived the intercept FROM the 34% (log(.34/.66) = -0.66) | Go the model's direction: start from beta-hat, exponentiate, then the probability |
| 2026-09-15 | user | Multinomial log-lik written as sum log p_{i,y_i}; Y_i invisible | Write sum_i sum_j 1[Y_i=j] log p_ij and show the J=2 reduction |
| 2026-09-15 | user | Interaction frames answered Ai-Norton but not the reader's questions | Lead with the two questions: include the product? do the two models agree on probabilities? Answer each in one bullet |
| 2026-09-15 | self | lab4's VGAM `cumulative(parallel = FALSE)` LR test printed `LR NaN` in the committed PDF too (nonparallel fit has no usable logLik) | Dropped VGAM. Proportional odds now checked as on the slide: four binary logits at each cut + LR/AIC/BIC ordered vs `multinom` (LR 27.3 on 12 df; BIC keeps ordered) |
| 2026-09-15 | self | `options(scipen = 999)` in lab setup makes tiny p-values print as 40-digit decimals | Round LR, print `p < 0.001` as a logical, or `round(p, 3)` |
| 2026-09-15 | self | Ajrouch et al. 2016 ZINB was described with hurdle language (whether / how much among those who do) | Describe ZI as structural-zero logit + count that can be zero; say the authors read it as a hurdle and that in practice the two are blurry |

## User Preferences
- Never add `Co-Authored-By` to commits.

## Build Plan / Source Material Per Week
- **Weeks 1-2 (OLS, inference)**: built from scratch; loosely follows `../SOCIOL690S` Week 1.
- **Weeks 3-4 (MLE theory + GLM applications)**: base on
  `/Users/wenhao/Library/CloudStorage/Dropbox/archive/Course Materials/POL-2251 Quantitative Methods III/`
  -> `Week 2 MLE.pdf`, `Week 3 MLE.pdf`, `Week 4 MLE.pdf` (+ matching `Week N Lab.pdf`).
  User's ask: expand for *first-time* MLE learners, but keep the emphasis on **derivation**,
  with clean/elegant beamer design.
- **Weeks 5-14**: 690S is the entry point — expand and mimic those decks
  (W5 DAGs<-690S W5; W6 matching/PS/weighting<-690S W6; W7 IV<-690S W7; W9 RDD<-690S W10;
  W10 DiD<-690S W11; W12 ML basics<-690S W2+W3; W13 causal ML<-690S W4+W12/13).

## Domain Notes
- Structure: Tuesday = lecture (beamer slides), Thursday = hands-on R lab. 14 weeks.
- **FONT DECISION (2026-08-10, user)**: do NOT use the 690S `newpxtext`/`newpxmath` stack.
  Use Latin Modern (the LaTeX/Overleaf default) and **serif, not sans**:
  ```
  \usepackage[T1]{fontenc}\usepackage{lmodern}
  \usefonttheme{serif}\renewcommand{\familydefault}{\rmdefault}
  ```
  (First attempt used `\usefonttheme[onlymath]{serif}` -> sans body text; user corrected it.)
- `mathtools.sty` is NOT installed in this TeX tree and `tlmgr install` needs sudo.
  Stick to `amsmath,amsfonts,amssymb,bm`.
- No poppler (`pdftoppm`/`pdfinfo`) on PATH. To eyeball a slide:
  `gs -dNOPAUSE -dBATCH -sDEVICE=png16m -r90 -dFirstPage=N -dLastPage=N -sOutputFile=out.png slides.pdf`
- Lab data: **real GSS data**. `Data/build_gss_extract.R` pulls the GSS cumulative file via
  the `gssr` package (installed locally) and writes `Data/gss_earnings.rds`, which is what
  students actually get. `gssr` is NOT on CRAN, so never make a lab depend on it.
- Sibling repo one level up: `../SOCIOL690S` (Fall 2025, Causal ML). Its Weeks 1-2 are the
  closest analog to 723's Weeks 1-2 (CEF, OLS matrix form, FWL, asymptotics, Neyman
  orthogonality). Reuse style and some content, but 723 is broader/more standard and less
  ML-focused early on.
- 690S slide conventions to match:
  - `\documentclass[aspectratio=1610,12pt,xcolor=dvipsnames]{beamer}`,
    `\usetheme{default}` + `\useoutertheme{miniframes}`, navy `main`/`nagivation` rgb 0,0,0.5.
  - Fonts: `newpxtext` + `newpxmath` + `dsfont` (Palatino-like). Same stack in syllabus.
  - Duke logo at `Misc/duke_logo.png` (relative `../Misc/duke_logo.png` from slide dir).
  - `\newcommand{\indep}{\perp\!\!\!\, \perp}`, `\DeclareMathOperator*{\argmin}{arg\,min}`.
  - `\citep` redefined to render small + gray; `natbib` + `asr` bib style.
- Labs/handouts are `.qmd` (Quarto) -> PDF, `pdf-engine: pdflatex`, 12pt, 1.1in margins,
  same newpx font stack, custom `\maketitle` block. See `../SOCIOL690S/Homework/homework1/`.
- Quarto binary is NOT on PATH: `/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto`.
  **Sandbox gotcha (2026-09-15)**: the launcher calls `/usr/sbin/sysctl`, which the sandbox
  blocks ("quarto script failed: unrecognized architecture"). Fix: copy the launcher to
  `$TMPDIR`, sed `FULLARCH="Apple"` and hard-code `SCRIPT_PATH` to the real bin dir, run that.
- On the wj93 machine `pscl`, `AER`, `VGAM`, `brglm2` are NOT in the system R library. Install
  into `$TMPDIR` with `allowed_domains` cloud.r-project.org and render with `R_LIBS=$TMPDIR`.
  The user should `install.packages(c("pscl","AER","VGAM","brglm2"))` to render locally.
- **TeX tree mismatch on wj93 (2026-09-15)**: `~/Library/TinyTeX` has tcolorbox 6.9.0 (Nov 2025)
  on a LaTeX kernel 2025-06-01; tcolorbox calls `\NewStructureName` etc. and every Quarto
  callout fails ("Undefined control sequence ... \NewStructureName"). Slides are unaffected.
  Sandbox cannot write TinyTeX; workaround: tcolorbox 6.6.0 from
  texlive.info/tlnet-archive/2025/06/15/tlnet/archive/tcolorbox.tar.xz extracted under
  `$TMPDIR/tex`, render with `TEXINPUTS="$TMPDIR/tex//:"`. Real fix for the user:
  `tlmgr update --self --all` (kernel + tcolorbox together). Math Review uses tcolorbox too.
- LaTeX toolchain: TinyTeX / TeX Live 2025 at `/usr/local/bin/pdflatex`.
- Syllabus lives in `syllabus/` (`main.tex` + `schedule.tex`).

## Patterns That Work
- **Problem sets (user, 2026-09-18)**: "intuition and how to interpret results, not nitty gritty
  of likelihood solving"; cover the most important things. PS2 mirrors PS1's YAML/format
  (Homework/homeworkN/homeworkN.qmd, eval:false code stubs, points per section, sentences
  demanded). Render with the patched quarto + TEXINPUTS; no R packages needed since nothing
  executes.
- 2026-09-18 (user): PS2 Part 1 was "still a bit hard": replaced the Wald-vs-LR boundary
  question with plug-in items (dbinom curve, two curves shifted to zero, SE + 95% interval
  from sqrt(p(1-p)/n), fix the "most probable value" sentence). Rule for this course's
  problem sets: every likelihood item is a picture or a plug-in, never a test derivation.
- 2026-09-18 (user, second revision): "focus on application in actual research". PS2 now
  opens with READING PUBLISHED RESULTS (Pager 2003 Table B1 numbers -0.99/-1.25/-0.29 and
  34/17/14/5; Killewald 2016 3.3% vs 2.5%; find-your-own ASR/AJS table) and asks for a
  paper-style results paragraph; by-hand Pearson phi, the Mundlak check, and likelihood
  curves were cut. Dates: assigned Fri Sep 18, due Fri Oct 2 (14 days); README and the
  Week 4 Looking Ahead block updated. Paywalled PDFs (Pager, Killewald, PMC author
  manuscripts) cannot be fetched from the sandbox; Europe PMC fullTextXML works only for OA
  papers (Tomaskovic-Devey 2020 = PMC7196797).
- 2026-09-18 (user): Pager item dropped from PS2; Part 1 is now Killewald 2016 only, and
  students fetch the article themselves (table of post-1975 logit coefficients + predicted
  probability figure; 3.3% vs 2.5% verified via the OOW "Work in Progress" post and the ASA
  press release, >6,300 couples). NEVER use "risk ratio" or "incidence-rate ratio" in course
  materials: the user flagged them as untaught. The taught scales are log-odds, odds
  (ratio), probability (difference); for counts, "exp(beta) multiplies the expected count".
- **Week 5 rebuilt 2026-09-17 (user: "too SOCIOL690S-alike", "drop do unless standard",
  "fix figures")**: body rewritten, preamble untouched (16:9, 11pt, its own headline).
  CausalML-textbook leftovers removed: 401(k) example -> college and earnings (D, Y, X family
  background, A test score, U unmeasured -> A and Y, M occupation); SWIG frame dropped;
  Hollywood T/B/C collider algebra -> T test, A athletics, C admitted (matches lab5);
  "First Law" frame merged into back-door; Wooldridge quote dropped. do-notation KEPT (user
  said keep if standard) alongside Y(d): intervention-graph frame (Samii lecture 2), back-door
  adjustment in do form with Y(d) ⊥ D | Z beside it, front-door formula in do form.
  Added from Samii (cyrussamii.com/?page_id=4190, lectures 1-2, verified by download): Manski
  identification frame, ATE decomposition with "selection on the effect" term, VanderWeele &
  Robinson 2014 on race effects. Added Blau & Duncan 1967 path diagram frame (V, X, U, W, Y).
  Front door respecified for APC after Winship & Harding 2008 SMR 36(3): 362-401 (web-verified):
  cohort effect through mechanisms M; leg 2 has A, P, M and no C; leg 1 needs an exclusion.
  ALL 13 "(cont.)" spill pages removed (every frame fits one page); 26 DAGs redrawn with one
  \tikzset style (dag/unobs/cond/lab/arr/uarr/bias; 9mm nodes, >=2.4cm spacing,
  scale+transform shape when a figure must shrink). Deck 67 -> 54 pages. Old body backed up in
  the session scratchpad only; git has it at commit 51df853.
- 2026-09-17 (user): the 690S good-and-bad-controls DAGs were brought back into Week 5's
  Controls section as two three-panel frames redrawn in the shared style: "Causes of the
  Outcome Only, or of the Treatment Only" (Z->Y precision; Z->D variance; Z->D with U = bias
  amplification, Z is an instrument) and "Mediators, Their Descendants, and Their Causes"
  (control M bad; control Z<-M bad; control Z->M neutral). The earlier two-panel "Predictors
  of Treatment" frame in the DAG section was removed to avoid duplication. Deck = 55 pages.
  Source: ../SOCIOL690S/Week 5 Causal Inference through DAG/slides.tex lines 818-975.
- Week 5 DAG style rule: use the `dag` tikz style; shrink with
  `[dag,scale=0.8,every node/.append style={transform shape}]`, never by reducing node size.
- lm() aliasing gotcha: with a group-constant regressor and group dummies, lm() drops the
  LAST collinear column. Put the dummies first (`y ~ School + x + Sector`) so the group-level
  variable shows NA; otherwise it reports a meaningless coefficient relative to a dropped dummy.
- Proportional odds is tested against the cumulative logit with cut-specific slopes
  (generalized ordered logit), NOT against the multinomial (different family, not nested).
  Tools: `brant::brant(polr_fit)` (Wald) and `VGAM::vglm(..., cumulative(parallel = FALSE ~ x))`
  (LR, one covariate at a time; freeing all slopes gives non-monotone probabilities and NaN).
  `brant` was installed to the user library on 2026-09-16.
- Lab 4 robust-SE section (2026-09-16): four vcov choices (classical/HC0/HC1/~psu) for
  coefficients, AMEs and avg_predictions, plus a by-hand delta-method check that reproduces
  marginaleffects' clustered AME SE. GSS psu clusters are tiny (839, median 4) so clustering
  barely moves anything here; say so rather than pretend otherwise.
- marginaleffects 0.32 gotcha: `datagrid()` default and `newdata = "mean"` use the MODE for
  binary and integer-valued variables (female 0, black 0, wordsum 6), not the mean. For the
  slide's MEM (true means, 0.092 for wordsum) build the grid explicitly with mean(...). Lab 4 fixed.
- marginaleffects gotcha (2026-09-16): `plot_predictions(m, by = "x")` with no newdata averages
  fitted values AMONG units that have each observed x (subgroup means), NOT the counterfactual
  observed-value curve. For the latter use `newdata = datagrid(x = ..., grid_type =
  "counterfactual")` or `avg_predictions(m, variables = list(x = ...))`. Lab 4 Figure 1 fixed.
- Week 3 BIC frames (2026-09-14): causal caveat added (selection criteria do not choose
  controls) and a frame on LR growing with n (South dummy at n=500/1000/3509) plus the
  Grusky & Hauser 1984 / Raftery 1986 ASR / Hout 1988 AJS origin story. Week 3 = 72 pages.
- **Event history REPLACED by multilevel models (2026-09-14, user)** in Week 4 slides, lab4,
  syllabus (schedule.tex + main.tex), README, and root Syllabus.pdf (copied from
  syllabus/main.pdf after pdflatex). Lab uses nlme::MathAchieve/MathAchSchool (HSB, 7,185
  students / 160 schools) with lme4; numbers on slides: ICC 0.18 (tau2 8.55, sigma2 39.15),
  shrinkage schools 8367 (n=14) and 2305 (n=67), SES 2.95/2.38/2.19 pooled/RE/FE, Catholic
  2.10 (0.34), glmer top-quartile tau2 0.34. Literature: Sampson, Raudenbush & Earls 1997.
- User wants slides SUCCINCT: no chatty asides ("that is not a bug", "say the word",
  "nobody thinks here"), no forward/backward narration sentences. A concision pass was done
  2026-09-14; keep new frames terse.
- Week 4 interpretation arc as of 2026-09-14 evening: Three Scales -> Predicted probabilities
  -> Pager read slowly (2 frames) -> AME person-by-person table -> AME/MEM readings ->
  Uncertainty -> Probit -> Recommendation -> Interactions (concept) -> Interactions real output
  (pareduc x female; product 0.021 n.s.; AME gap 0.006). Separation frame DROPPED (Zorn ref
  and one protocol line remain). Multinomial now uses region4 (nominal); ordered keeps degree
  with a "cutpoints are gates" frame. User is weighing multilevel in place of event history.
- 2026-09-15 lab4 rebuilt to follow the slides: main logit is `ba ~ pareduc + wordsum + female
  + black` (no exper, matches slide table); order predicted probabilities -> AMEs -> odds ratios
  as footnote -> robust/cluster -> probit AMEs agree; interaction `pareduc * female` with the
  no-product vs with-product AME columns; NEW multinomial on region4; ordered logit with the
  "gates" computation by hand; counts: Pearson phi by hand, three repairs, hurdle, AIC/BIC
  table by hand (Poisson 10778/10815, NB 10775/10818, hurdle 10479/10553: AIC and BIC DISAGREE
  on NB vs Poisson, both pick hurdle). Exercises 5-6 now hurdle-vs-ZINB by AIC/BIC and an LR/BIC
  block test in the multinomial.
- **2026-09-16 event history RESTORED (user: "I wanted to talk more on Thursday")**: section
  re-inserted after Multilevel, before Practice (8 frames: durations/censoring, S and h,
  likelihood, Kaplan-Meier, Cox, discrete time, Killewald 2016 ASR literature frame [PSID
  person-years; post-1975 marriages: husband not full-time 2.5% -> 3.3%], R + what to watch).
  Lab: simulated union-formation duration (set.seed(4), n=3000, true log HR -0.5, Cox gives
  -0.497) + person-period logit; exercise 8 "censoring harder". README, syllabus main.tex
  (Week 4 Lab sentence + Allison 2014 in further reading), Syllabus.pdf updated. Deck = 71 pp.
- 2026-09-16 (user): "(Thursday's Session)" tags removed from slide/lab section titles; the
  user finds them unnecessary. Do not add day-of-week tags to frame titles.
- 2026-09-16 (user): multilevel motivation order he wants: (1) we care about GROUP
  characteristics too (Catholic school; Coleman, Hoffer & Kilgore 1982 on HSB); (2) the
  intuitive move is one OLS with individual + group variables; (3) what is wrong on clustered
  data: Catholic 1.94, OLS SE 0.15 vs clustered 0.32 (school evidence counted many times);
  (4) the BONUS is the between-school variance (18%, Coleman 1966). "Why not clustered SE"
  frame then follows. Lead with the intuitive model and what breaks, not with the variance.
- 2026-09-16 (user): the "Why Not Just OLS With Clustered SE?" frame is now TWO numbered points,
  no table (user: "the table is too complicated"): (1) no number for between-school variance
  (18%, Coleman 1966, Sampson 1997) = the bonus, merged here; (2) coefficient stays at the
  pooled 2.95 (vs 2.38 modelled, 2.19 within). Define u_j in passing before using "random
  intercept"; the user flagged the undefined term.
- 2026-09-16 (user, third pass on that frame): "too wordy, hard for a regular person". Final
  form: OLS has one error -> split it into u_j (school) + e_ij (student), plain-language
  definition of u_j ("how far school j sits above or below the average school"), then three
  bullets: clustered OLS never separates it; FE estimates every u_j (160 dummies, absorbs
  Catholic); this week estimates their variance (18%). No "shared part of the error" jargon;
  answer "how is this different from FE" on the same frame.
- 2026-09-16 (user): u_j must be LINKED to Week 2's error components model (eps_ig = nu_g +
  eta_ig, "The Error Components Model" frame, Week 2 slides ~line 830): slide 47 now opens
  with that split and says u_j is nu_g with school as cluster; ICC frame says "Week 2's rho_e,
  now estimated rather than only corrected for". Always tie new notation to where it appeared.
- 2026-09-16 (user: "write more slowly about the likelihood for the random intercept"): two
  new frames after "The Random-Intercept Model": "The Likelihood, Step by Step" (core: goal,
  plan, three-line align* with reasons: f(y_j|u_j) product, L_j = integral over u, sum of
  logs; step 2 "not a choice", censoring is the same move; where each parameter lives) and
  "The Integral Has a Closed Form Here" (adv: 2-student school bivariate normal with V_j,
  off-diagonal tau^2 = Week 2's shared shock, general V_j = sigma^2 I + tau^2 11', MVN
  loglik, Newton; glmer nAGQ grid). Deck = 73 pages.
- 2026-09-16 (user): the two likelihood frames ("Step by Step", "Closed Form") were DROPPED
  the same day as "too complex"; a one-line Estimation note is back on the model frame. Lesson:
  for this user, a derivation the user did not ask for by name gets cut; ask before adding
  >1 derivation frame to an applied section.
- 2026-09-16 (user: "no x, the null model, is weird"): ICC frame now says WHY no predictors
  (raw split of variation before anything explains it), why sigma^2 is within-school
  (e_ij is distance from own school's mean; anything classmates share is in u_j), and what
  tau^2/sigma^2 mean with controls (3.62 / 37.0, between fell 58%, within 5%).
- 2026-09-16 (user): "two classmates correlate at 0.09" phrasing of the ICC confused him
  ("9% is between schools, they are not classmates!"): state the ICC only as a variance share
  (9% between, 91% within). Shrinkage frame + lab section DROPPED. "Fixed or Random?" replaced
  by "When You Need a Random Intercept": (1) the split is the finding: Tomaskovic-Devey et al.
  2020 PNAS (14 countries, 25 years, 2bn job-years in 50m workplace-years; between-workplace
  share rose in 12 of 14, fell in none, fastest in private sector; web-verified 2026-09-16);
  (2) group-level variable; (3) within effect with u_j correlated with x -> FE or Mundlak.
  "In the Literature: Multilevel" (Sampson 1997) frame and its reference DROPPED. Deck = 70 pp.
- **2026-09-16 (user): EVENT HISTORY DROPPED AGAIN** ("too much"), same day it was restored.
  Removed from slides (section + Allison 2014, Cox 1972, Killewald 2016 refs), lab (section,
  exercise 8, library(survival), goal 6), syllabus main.tex, README. Restorable from commit
  9ee5254. Do not re-add unless asked by name.
- 2026-09-16 (user): "Two Assumptions About u_j, and Why Estimation Needs Them" frame added
  after the model frame: (1) u_j ~ N(0, tau^2) = the distribution to average over (mean 0 a
  normalization, normal gives closed form, one parameter); (2) E[u_j | X] = 0 = same
  distribution for high- and low-SES schools, else beta_1 credited with u_j (omitted-variable
  logic); FE avoids by estimating u_j. Check on HSB 2.38 vs 2.19.
- 2026-09-16 (user: "plot the predicted probability in the RI logit, too abstract"): new saved
  script figs/w4_multilevel.R -> figs/glmer_pred.pdf: Pr(top quartile) vs SES for public
  schools at u_j = -tau, 0, +tau (blue) and the average over schools (orange, numeric
  integration). Numbers at SES=0: 0.12 / 0.19 / 0.29, average 0.20; tau = 0.58.
- 2026-09-16 (user): Practice section reduced to "Looking Ahead" (placed right after the RI
  logit frame, no section header) + References. Assessing Fit and A Practical Protocol frames
  DROPPED. Deck = 58 pages, lab = 23.
- 2026-09-16 (user: "group-level variable: why not just OLS + clustered SE?"): he is right;
  the frame now says so. "When You Need a Random Intercept" = (1) the between/within split is
  the finding (Tomaskovic-Devey 2020); (2) a group-level variable with FEW groups (clustered
  SEs unreliable below a few dozen clusters, Cameron & Miller 2015; HSB 160 schools: 1.94
  (0.32) vs 2.10 (0.34) either works); "Otherwise: many groups, no interest in the variance,
  clustered OLS is enough." Mundlak scenario dropped from slides (refs removed); the lab keeps
  its short "Is u_j uncorrelated with SES?" subsection. Never sell RI on a claim clustered OLS
  can also deliver.
- **2026-09-16 (user, decisive): NO null-model / "with no predictors" anywhere in the multilevel
  section.** All "8.55 -> 3.62" comparisons deleted from slides AND lab. Everything (ICC 0.09,
  shrinkage, LR 332) is computed from the full model math ~ SES + Catholic + (1|School).
  Shrinkage = u_hat_j = w_j * (school's average residual from the fixed part): school 8367
  n=14 resid -7.23 w=0.58 u_hat -4.18; school 2305 n=67 resid -1.19 w=0.87 u_hat -1.03.
- 2026-09-16 (user): "coefficients are within-school" (glmer frame + lab) was WRONG/misleading;
  RI uses both within and between variation. Reworded: "log-odds change holding the school's
  u_j fixed; population-average probability averages over u_j".
- 2026-09-16 (user: "how come SES is uncorrelated with u_j?"): answered on "Fixed or Random?"
  and in a new lab subsection: the assumption is FALSE on HSB (cor(u_hat, school-mean SES) =
  0.51; slope 2.38 vs within 2.19); fix = Mundlak within-between (add school-mean SES): SES
  2.19 (0.11) = FE exactly, meanSES 3.14 (0.38) contextual effect, Catholic 1.23 (0.30),
  tau^2 2.31. References Mundlak 1978, Bell & Jones 2015 added. Lab 4 = 27 pp.
- 2026-09-16 (user: "why start from no predictors? report all values and interpret"): the ICC
  frame is now "Reading the Fit, and the ICC": full-model table (b0 11.72, SES 2.38 (0.11),
  Catholic 2.10 (0.34), tau^2 3.62, sigma^2 37.0; ICC 0.09; LR vs OLS 332), interpretation
  bullet per row, and the no-predictor split (8.55/39.15, ICC 0.18) as a comparison at the end.
  Shrinkage frame flagged as the no-predictor model (anchor = grand mean 12.64). Lesson: the
  user wants the FULL fitted model reported and read first; special cases come after.
- 2026-09-16 (user: "don't you have Catholic in your equation?"): the Random-Intercept Model
  frame now carries the SAME equation as slides 46-47 (math = b0 + b1 SES + b2 Catholic + u_j
  + e_ij), five parameters, lmer(math ~ ses + catholic + (1|school)); the likelihood frames
  use X_ij'beta so they cover both covariates. Keep one running equation across a section.
- User's mental model to reinforce: each school has its own intercept b0 + u_j; FE estimates
  160 constants, RI assumes 160 normal draws and estimates tau^2; individual u_j come back
  afterwards as shrinkage predictions (ranef), not as likelihood parameters.
- 2026-09-15 multilevel motivation (user asked "why not OLS with clustered SE"): new frame "Why
  Not OLS With Clustered Standard Errors?" after "Observations Come in Groups" with a
  question-by-question table (SE right both ways; ICC, shrinkage, SES 2.95 vs 2.38, random
  slopes only in the model) + Sampson 1997 as "the variance is the finding". Deck = 61 pages.
- 2026-09-15 Week 4 pass (user): "Three Readings of beta_record" frame dropped; \core removed from
  the two overdispersion-repair frames (understanding only, no star); exposure bullet gets \adv;
  "Lesson:" line, "What no count model fixes" bullet, and the DiPrete 2011 paragraph+reference all
  dropped. Deck is 60 pages. Pre-existing overfulls at slides.log lines ~306 (hbox 3pt) and ~784
  (vbox 1.7pt) are tolerated.
- Week 4 "In the Literature" frames (2026-09-14): every citation was web-verified this session
  (Pager 2003 AJS incl. Table B1 coefficients; Killewald 2016 ASR 2.5%/3.3%; Kuo & Raley 2016
  Demography; Zajacova et al. 2017 SSM; Olzak 2021 ASR 11%/7%; DiPrete et al. 2011 AJS; Ajrouch
  et al. 2016 J Gerontology B; Reyes et al. 2018 JHSB HRs). No hurdle/ZI example exists in
  ASR/AJS that could be verified; the slide says so. Firebaugh & Schroeder 2009 was NOT used
  because its model could not be verified.
- **2026-09-14 restructure (user)**: score test dropped from Week 4 entirely; LR-block +
  AIC/BIC + "BIC in practice" frames now END Week 3 (after In Practice). Week 4 order:
  Where Week 3 left us -> X inside theta -> logit two ways -> S-curve & why not OLS on
  log-odds -> fitted GSS logit -> Newton by hand (ba ~ wordsum) -> score = normal eqs ->
  three scales -> predicted probabilities FIRST (ASR-style figure) -> AMEs -> uncertainty
  -> probit as another S-curve (no latent variable) + recommendation -> interactions ->
  separation -> categories: multinomial setup/derivation/GSS, then ordered (cumulative
  logit, no latent variable) -> counts. User dislikes latent-variable motivations.
- `Math Review/math_review.tex` (2026-09-11): standalone article, ~24 pp, tcolorbox rules /
  takeaways / "where this appears" boxes, titlesec + fancyhdr. Sections: functions/logs;
  derivatives; Taylor; linear algebra (incl. projection/FWL); probability (E, Var, Cov, IV
  as a Cov ratio); conditional expectation (LIE, ATE and IPW proofs, Bayes, bias-variance);
  asymptotics (LLN, CLT, Slutsky); integrals (survival/hazard, kernels); constrained and
  penalized optimization (ridge closed form, lasso corner, Lagrange, Neyman orthogonality,
  influence functions). Six exercises with answers.
- **Week 4 rebuilt (2026-09-10)** around real GSS examples; all numbers/figures come from
  `Week 4 .../figs/w4_examples.R` (score-test figure on childs, logit-vs-LPM on wordsum,
  count-distribution figure). Order: bridge from Week 3 (score test, LR block, AIC/BIC
  disagreement on region) -> X inside theta (link = inverse of the range-fixing function)
  -> binary -> ordered/nominal -> counts (overdispersion, hurdle) -> event history -> practice.
- **Score/LM test dropped from Week 3 (2026-09-10, user)**: survives only as the third
  ruler drawn in the "Three Rulers" picture with a "Week 4" pointer. Week 4 must introduce
  the score test from scratch (definition, LM = S(theta0)^2 / I(theta0), overdispersion).
- **AIC/BIC dropped from Week 3 (2026-09-10, user)**: slides, lab, README all cleaned.
  Week 4 slides (Assessing Fit, negative binomial) and lab4 still USE AIC/BIC without
  defining them; the dangling "last week" pointer was removed but nothing was added.
- Lab code style the user wants: every chunk carries `##` explanatory notes (what the
  line does and why), not bare code.
- Week 3 Testing figures are regenerated by `Week 3 .../figs/make_testing_figs.R`
  (Poisson three-ruler scatter, exact Bernoulli rejection rates, parabola-drop checks).
  The older figs (sim_z_*, loglik_n20, parabola_*) came from an unsaved script; keep them.
- Testing-section order the user settled on (2026-09-10): goal+plan -> null = smaller
  model -> z ruler + large-n sim -> square to chi2 -> STRAIGHT to the uncomfortable case
  (Sim 2 small n + the curve-vs-parabola picture) -> three rulers picture -> formulas ->
  "on a parabola all three are one number" -> score all three on Sim 2 -> regression:
  count events not rows (figs/sim_reg_rare.R) -> which test software ran -> restricted vs
  unrestricted + when only Wald is possible -> what to report. He cut: the large-n parabola
  derivation, the three-rulers-at-large-n scatter, Wald reparametrization, LR = R^2-change,
  the closing summary, AND the References frame. figs rulers_large/parabola_n/parabola_reg
  are now unused but kept.

## Patterns That Don't Work
- (accumulate)

## Build Log (2026-08-10)
- **Done**: Weeks 1-4 complete (slides.tex+pdf, labN.qmd+pdf), Problem Set 1, README,
  `Data/build_gss_extract.R` + `gss_earnings.rds` (n = 3,509).
- Slide conventions now locked in (reuse the Week 1 preamble verbatim for new weeks):
  - `\adv` = star marking "for understanding, not examined"; scalar-first ordering.
  - `\section[Short]{Long}` — the miniframes bar only fits ~7 short names.
  - `\setbeamertemplate{mini frames}{}` kills the per-frame dot clutter.
  - Global `\AtBeginDocument` display-skip tightening + custom frametitle template are
    what keep frames from overflowing; without them ~20 frames overflow per deck.
  - `[allowframebreaks]` on any long References frame.
- **BCa bootstrap gotcha**: `boot.ci(type="bca")` fails with "estimated adjustment 'a'
  is NA" — the default `empinf` regression estimate returns NA. Fix:
  `boot.ci(bo, type="bca", L = empinf(bo, type="jack"))`. Also make the statistic
  function return an `unname()`d scalar.
- `logistf` is NOT installed; use `brglm2` (`glm(..., method = "brglmFit")`) for Firth.
- Quarto renders take 2-10 min when a lab has simulations — run them backgrounded.
- **Weeks 5-7, 9-10 done.** Week 5 (Potential Outcomes + DAGs). DAGs drawn in raw TikZ (`every node/.style=
  {circle,draw=dukeblue,...}`), not ggdag — keeps them crisp and editable in the deck.
  `dagitty` + `ggdag` ARE installed and used in lab5.
- Remaining to build: **Weeks 11, 12, 13, 14** (slides + labs), Problem Sets 2-5,
  midterm review, final-project replication packet. Source mapping is in the Build Plan
  section above.
- Lab datasets in use: GSS extract (W1-6), `ivreg::SchoolingReturns` = Card proximity IV
  (W7), `rdrobust::rdrobust_RDsenate` (W9), `fixest::base_stagg` + `did::mpdta` (W10).
- Package notes: `WeightIt` will not install here (do IPW by hand — better pedagogy
  anyway); `did`, `bacondecomp`, `sensemakr`, `estimatr` installed on 2026-08-10.
- `base_stagg` gotchas: never-treated coded `year_treated == 10000`; it ships a `treated`
  column that collides with `bacondecomp::bacon()` internals — drop/rename it first.
- `did::mpdta`'s `treat` is a time-INVARIANT cohort flag; build
  `D = as.integer(first.treat > 0 & year >= first.treat)` for TWFE or feols errors on
  collinearity.
- `rddensity` results: use `dt$test$t_jk` / `$p_jk` (the asymptotic versions are NA).
- TikZ curves that leave the axes cause 150pt overfull vboxes — wrap plots in
  `\begin{scope}\clip (0,0) rectangle (w,h); ... \end{scope}`.
- When a frame still overflows after trimming, `\begin{frame}[shrink=8]{...}` is the
  reliable fix. Check WHICH frame: the log line number is the `\end{frame}`, so scan
  backwards for the `\begin{frame}`.
- Reusable recipe for a new week: copy the Week 3 preamble verbatim (everything before
  `\begin{document}`), change only the `%%` header comment and `\subtitle`.

## Build Log (2026-08-10, session 2)
- **All 14 weeks now built**: slides.tex+pdf for every week; labs for 1-13
  (Week 14 is a workshop, no lab). Problem Set 1 done; PS 2-5 still to write.
- **User correction #1 (important)**: my first pass at Weeks 5-11 was too thin
  relative to the 690S decks. Expanded W5 28p->55p, W6 29p->53p, W7 25p->37p,
  W9 16p->21p, W10 21p->30p, W11 16p->20p by adding the DERIVATIONS, not prose.
- **User correction #2**: do NOT copy 690S wholesale. The triangular SEM /
  gasoline-demand model is econometrics-specific, not standard Stats II ---
  compressed from 11 frames to 3 (history + generic DAG + "same variable, two
  roles"). Rule: cover the STANDARD content in depth; marginalize the specialist
  material (e.g. DML-for-IV is a preview line, not a section).
- Derivations that were missing and are now in: regression-anatomy
  delta_R = E[sigma_D^2(X) delta(X)]/E[sigma_D^2(X)] (W6); matching-anatomy via
  Bayes (W6); AIPW Gateaux-derivative orthogonality proof (W6); collider algebra
  E[T|B,C]=(C-B)/2 (W5); birth-weight CEF (1-kappa/2)S (W5); front-door
  do-calculus + APC application (W5); LATE derivation by type (W7); IV matrix
  form + IV sandwich (W7); AR statistic + test inversion (W7); local-linear RD
  objective + why MSE-optimal h breaks the CI (W9); within-transformation and
  Goodman-Bacon/negative-weight formulas (W10); SC optimization + factor model
  (W11).
- `/tmp/fixover.py <dir>` auto-detects overfull frames and adds `[shrink=10]`.
  Reusable: it maps the log's line number back to the enclosing \begin{frame}.
- Lab datasets added: `tidysynth::smoking` (W11), GSS + synthetic (W12),
  `DoubleML::fetch_401k` + simulation (W13).
- **DML sim gotcha**: a random-forest DGP made naive/orthogonal/DML all biased
  the SAME way (forest underfits m, so residuals keep confounding). Switched to
  the canonical sparse-lasso DGP where naive bias is 0.11 and orthogonal is 0.02.
  Also `ranger(x=matrix)` needs column names or it errors "No covariates found."

## Codex Review (2026-08-10, session 3) — findings ACCEPTED and fixed
Reviews saved at `.claude/codex_slides.md` and `.claude/codex_labs.md`.

**Real statistical errors I had shipped (verified numerically before fixing):**
- **FWL**: I claimed `Cov(Ytilde, D)/Var(Dtilde)` was WRONG. It is CORRECT --
  Ytilde ⟂ Dhat, so Cov(Ytilde,D) = Cov(Ytilde,Dtilde). Verified: all three
  numerators give 0.1405320. The real asymmetry is the **denominator**: it must be
  Var(Dtilde). `lm(y_tilde ~ educ)` gives 0.1293 (attenuated). Was wrong in W1
  slides, lab1, AND homework1 Q2.2 — all three fixed.
- **Collider algebra (W5)**: "average ½(C−b) over C given B=b to get −b/2" is
  wrong, because E[C|B=b] = b, so that average is 0. Verified by simulation.
  Correct move: read the partial coefficient −½ straight off E[T|B,C]=½C−½B.
  (This error was inherited from the 690S deck.)
- **tF (W7)**: Lee et al. (2022) Table 3 gives adjusted CRITICAL VALUES, not SE
  multipliers. At F=10 the crit is 3.43 (not a 4.35 multiplier); implied widening
  is 3.43/1.96 ≈ 1.75. Fixed in slides and lab.
- **Lab 7 IV**: the three "equivalent" implementations were three DIFFERENT models
  (m_iv made experience endogenous too). Added a single-endogenous-regressor spec
  `m_iv1`; coefficients now genuinely match to 6 dp, SEs differ as claimed.
- **W6 SIPW**: in the Hájek ratio the stabilizing constant CANCELS, so
  "stabilization reduces variance" was wrong. The gain is HT → Hájek normalization.
- **W4**: deviance is 2(ℓ_sat − ℓ_fit), not −2ℓ; ordered vs multinomial logit are
  NOT nested so the deviance-difference test was invalid (now a VGAM
  parallel=TRUE vs FALSE LR test).
- **W10 lab**: two-way demeaning added the grand mean twice.
- Others: A5 needed conditioning on X; E[ê²]=σ²(1−h) only under homoskedasticity;
  ridge closed form penalized the intercept; RD bias² curve was plotted as h²;
  Moulton slide had both "constant within clusters" and ρ_x.

**Scope (user's main complaint) — causal-ML footprint now confined to Wk13:**
- Cut the entire Neyman-orthogonality / Gateaux-derivative / cross-fitting
  subsection from Week 6 (4 frames → 1 plain-language "why the cancellation
  matters" frame). W6 53p → 48p.
- Deleted the recurring DML/cross-fitting forward-references from Weeks 1, 7, 9,
  10, 11 and demoted the FWL→AIPW→DML diagram in Week 14.
- Grep check to keep it that way:
  `for f in Week*/slides.tex; do grep -ci "neyman\|gateaux\|cross-fitting\|DML" $f; done`
  should be 0 everywhere except Wk13 (~15) and Wk14 (~6).
- **The line that resolves the tension** with "add at least as much as 690S":
  keep the STATISTICS derivations (regression anatomy, propensity theorem, double
  robustness, LATE, IV matrix form) — those are MHE-level Stats II. Cut the
  CAUSAL-ML machinery (orthogonality, Gateaux, cross-fitting, DML previews).

## Publishing (2026-08-10)
- Course repo: **https://github.com/wenhaojiangsoc/SOCIOL723** (public, matches the CML
  pattern). Local repo is `Teaching/SOCIOL723`, remote `origin` over **HTTPS**.
- **SSH to github is proxied through 127.0.0.1:10810 and intermittently drops** --
  `ssh -T git@github.com` succeeded but `git push` over SSH failed with
  "Connection closed by 127.0.0.1 port 10810". **Use HTTPS remotes**; the
  osxkeychain helper holds a token with `repo` scope, so pushes and even repo
  creation via the API work without `gh` (which is NOT installed).
  Retrieve it with: `printf 'protocol=https\nhost=github.com\n\n' | git credential fill`
- The website repo `wenhaojiangsoc.github.io` remote was temporarily switched to
  HTTPS to push, then **restored to SSH** (its original state).
- `_pages/teaching.md` lists courses newest-first: SOC723, then SOC690S, then SOC590.
  Each entry is 3 lines: course link \ Syllabus \ Course materials.
  Syllabus for 723/690S is linked from inside the GitHub repo (`Syllabus.pdf` at root).

## Overleaf (2026-08-10)
- Overleaf syncs via **Dropbox**: each subfolder of
  `~/Library/CloudStorage/Dropbox/Apps/Overleaf/` is one Overleaf project.
  Created `Apps/Overleaf/SOCIOL723/` alongside the existing `CML/`.
- Layout mirrors CML: per-topic folders each with a standalone `slides.tex`,
  `Misc/duke_logo.png` at the project root, `syllabus/{main,schedule}.tex`.
  I zero-padded the week numbers (`Week 01 ...`) so Overleaf's file tree sorts in
  teaching order; CML uses bare topic names and therefore sorts alphabetically.
- **CRITICAL PATH GOTCHA**: Overleaf resolves `\includegraphics` and `\input`
  relative to the **project root**, not to the .tex file's own directory. So:
    - GitHub/local copy uses `../Misc/duke_logo.png`  (compiles in its own dir)
    - Overleaf copy must use `Misc/duke_logo.png`     (compiles from root)
  CML has exactly this same split between its two copies. **The two copies of
  slides.tex are therefore NOT byte-identical** -- if you sync one to the other,
  rewrite that one path or the logo silently disappears.
  Verified by compiling all 13 decks with `-output-directory` from the project
  root and rendering the title page.
- Only `.tex` + figures go to Overleaf. The `.qmd` labs, `.rds` data and R scripts
  stay in the GitHub repo -- Overleaf cannot run them.

## Typography and layout conventions (2026-08-12)

User corrections, all applied across every deck:

- **No wide hats on operators.** `\widehat{\Var}` / `\widehat\Cov` were rejected
  as visually too wide. The convention now is:
    - `\Var_n(X)`, `\Cov_n(X,Y)`  -- sample moments computed from data
      (matches the existing `\En = \mathbb{E}_n` sample-expectation convention)
    - `\hat{V}(\hat\beta)`        -- an *estimated sampling variance* of an estimator
    - `\SE` is now `\mathrm{se}`  (Wooldridge convention: se() is already an estimate)
  These are two genuinely different objects; do not collapse them back into one.
- **No em dashes (`---`) anywhere in prose.** Converted ~425 of them. Rules used:
  paired dashes -> commas (or parentheses if the enclosed clause has a comma);
  single dash before a conjunction -> comma; before an independent clause ->
  semicolon. Watch for comma splices when the dash was at a line break: the
  script cannot see the next line, so it defaults to a comma. Four had to be
  hand-fixed to semicolons.
- **No per-slide font shrinking.** `[shrink=N]` was explicitly rejected: font size
  must be identical on every slide. All 83 `shrink=` options were removed. The
  replacement is `\begin{frame}[allowframebreaks,t]{...}`, which spills onto a new
  slide at the SAME font size. The `t` matters: without it the continuation slide
  is vertically centred and looks half-empty. Continuation marker is set to
  `(cont.)` via `\setbeamertemplate{frametitle continuation}`.
  Frames with `\pause` or `columns` cannot auto-break and must be split by hand.
- **Header**: miniframes is gone. It reserved a bar per slide-dot and, with
  `\setbeamertemplate{mini frames}{}`, left a *thick empty* band. Restoring the
  dots is not an option either: one dot per slide, and Week 1's "Scalar OLS"
  section alone has 28 slides, so they wrap to 4 rows. User chose a **thin
  one-line header** built from `\insertsectionnavigationhorizontal`, with section
  names clickable and the current one highlighted via
  `section in head/foot shaded`.
- **`\argmin` / `\argmax` put their subscript UNDERNEATH even inline.**
  `\DeclareMathOperator*` only does this in display math, so the macros are now
  `\newcommand{\argmin}{\argminop\limits}`.
- **Sans-serif variant: REJECTED and removed (2026-08-12).** A sans version of
  Week 1 (`slides_sans.tex`, `\sfdefault` + the `sansmath` package for sans math)
  was built on request as an experiment, then deleted from the repo and from
  Overleaf. The course is **serif everywhere**; do not reintroduce a sans variant.
  Recoverable from commit 50bdd54 if ever needed.
  (`sansmath` is not in TinyTeX by default: `tlmgr install sansmath`.)

## Checking layout overflow
`Overfull \vbox` in the pdflatex log = text spilling below the frame (this is
what the user sees as "words outside the frame"). Map the reported line number
back to the enclosing `\begin{frame}` to find the culprit. Anything under ~1pt is
a hairline and invisible. Render pages to inspect with:
`gs -sDEVICE=png16m -r120 -dFirstPage=N -dLastPage=N -sOutputFile=out.png slides.pdf`
(ImageMagick's `magick` is installed but its ghostscript delegate is broken:
"Unknown device: png16malpha". `pdftotext`/`pdfinfo` are NOT installed.)

## Two-star convention (2026-08-22, user request)
- Every deck now has TWO stars: `\core` (blue, RGB 0,83,155 = Duke Royal Blue)
  = essential/examinable, `\adv` (orange accent) = for understanding, not examined.
  Macro + `corestar` color defined in every preamble right after `\adv`.
- Each deck's Today/TOC frame carries a one-line legend; Week 1's "How to Read
  These Slides" and Week 2's "Reminder" frame explain both; Week 14's exam frames
  reference \core as the study spine (replacing a false claim that every deck had
  a checkpoint frame - only Week 1 does).
- Star titles are appended INSIDE the title braces: `{Title\core}`. A star can
  wrap a long frametitle to 2 lines and overflow the frame (hit once in Week 5,
  fixed by shortening the title). Check `Overfull \vbox` after adding stars.
- User (mid-session): students have NO linear algebra and NO prior exposure to
  standard errors. Added to Week 1: two "from zero" matrix primer frames before
  "Stacking the Data". Added to Week 2: "The Question, in Plain Words" (what an
  SE is) + "Two Theorems We Lean On All Semester" (LLN/CLT gently) before Route 1.
- Em-dash conversion artifacts fixed: bare-comma table cells in W2 Scoreboard,
  W4 GLM family table, W5 taxonomy table. Also `\widehat\Corr` -> `\Corr_n` (W1).
- Dedup pass: W10 deleted "What Goes Wrong, Precisely" (dup of Negative Weights),
  trimmed repeated Nickell/FWL bullets; W11 trimmed factor-model dup in
  "Relation to DID". W13 fixed stale claim that Gateaux derivation was in Week 6.
- Overleaf copies re-synced (logo path rewritten ../Misc -> Misc) for all 13 decks.
- GOTCHA: `git stash` in this repo stashes PDFs too; recompiling then `stash pop`
  conflicts on slides.pdf. Fix: `git checkout -- <pdf>` then pop.

## Session 2 additions (2026-08-22, same day)
- Week 1 REORDERED for the teaching split (user: Tue = all scalar, Thu = linear
  algebra): section order is now Orientation -> Scalar OLS -> CEF/BLP ->
  Properties -> Adjustment || Matrix Form -> Partialling Out. The two matrix
  frames formerly inside Properties (matrix unbiasedness, Gauss-Markov proof)
  moved to the end of the Matrix Form section; Properties frames de-matrixed
  (A1/A3/A5 restated scalar, sigma^2 matrix bullet now a forward pointer,
  G-M stated in words before the Reading frame). New "Where Tuesday Leaves Us"
  frame closes the Tuesday half; Today frame notes the Tue/Thu split.
- Week 10 EXPANDED on user request (DiD + negative weights "too simplified"):
  new frames (a) "What the Double Difference Identifies, Exactly" (Delta1-Delta0
  = ATT + differential trend derivation), (b) "The Forbidden Comparison, in
  Algebra" (2x2 = ATT_late - growth in ATT_early, with a numeric table),
  (c) "The Goodman-Bacon Weights, Explicitly" (s_kU ~ n_k n_U Dbar_k(1-Dbar_k)
  etc. - the n_a n_b x window-treatment-variance forms, verified against the
  paper's Theorem 1), (d) "Where the Negative Weights Come From" (FWL/double-
  demeaned D: w_it ~ Dtilde_it, negative iff Dbar_i + Dbar_t > 1 + Dbar).
  "Negative Weights" frame rewritten as consequences/diagnostics
  (TwoWayFEWeights). CS frame now displays the ATT(g,t) identification equation.
- **(cont.) BUG FIX, all decks**: the frametitle-continuation template fired on
  EVERY page of an allowframebreaks frame, including the first. Fixed with
  `\ifnum\insertcontinuationcount>1 ... \fi`. Weeks 2-3 never had the template
  at all (default roman numeral) - added the guarded version there too.

## Syllabus compile paths (2026-08-24)
- `syllabus/main.tex` now uses `\InputIfFileExists{syllabus/schedule}{}{\input{schedule}}`
  so it compiles from the repo root (official `Syllabus.pdf`, and Overleaf) AND from
  inside `syllabus/` (IDE builds -> `syllabus/main.pdf`, gitignored). Overleaf copy
  patched identically. `.gitignore` gained `*.fls`, `*.fdb_latexmk`, `syllabus/main.pdf`.
- Official artifact remains repo-root `Syllabus.pdf`, built with
  `pdflatex -jobname=Syllabus syllabus/main.tex` from the root.

## Problem set reschedule (2026-08-24, user request)
- PS3 due moved off midterm week: now due Mon Oct 19 (W9). PS4: assigned W10
  (Oct 27), due Mon Nov 9 (W12). PS5: assigned W12 (Nov 10), due Mon Nov 23
  (W14 row). Coverage labels unchanged (PS3: W5-6, PS4: W7+9, PS5: W10-11);
  the shift also fixed PS5 being assigned before its W11 material was taught.
- Touched: syllabus/schedule.tex + PS blocks in decks W6 (date), W9 (now a
  reminder frame block), W10 (now announces PS4), W12 (new PS5 block).
- Syllabus final-exam paragraph updated for the two-star system (blue = examined
  core, orange = not examined); was written for the old single-star convention.

## Examinability policy + W1 review fixes (2026-08-24)
- POLICY (user confirmed): blue star = where exam questions CONCENTRATE (not an
  exclusive whitelist); unmarked frames = fair game as supporting material at a
  lighter level; orange = never examined. Stated on W1 p5. W2 reminder frame,
  W14 exam frames, and the syllabus paragraph still use the older looser wording
  (offered to align; user has not asked yet).
- W1 "What You Already Know: Model Comparison" now glosses PRE / PA / PC / F
  (Judd-McClelland notation from Stats I) - user themselves didn't recognize it,
  so students won't either.
- Fixed three small (5-8pt) overfull frames introduced by the reorder session
  (Classical Assumptions, Reading Gauss-Markov, Multiplying and Inverting -
  latter's 2-line title was the culprit, shortened). NOTE: check overfulls with
  threshold ([0-9]+\.) not ([0-9]{2,}) - the 5-9pt ones are visible.

## W1 bivariate-model status made explicit (2026-08-24, user request)
- "The Bivariate Linear Model" now says we POSIT a working model, with a
  footnote flagging that the "on average" readings carry an assumption.
- New \core frame follows: "What Does That Equation Actually Claim?" -
  E[eps|X]=0 <=> E[Y|X] linear, population status of beta, and the honest
  admission that it is false for earnings~schooling, forward-linking to the
  CEF/BLP section. Matches the deck's existing "Estimating What, Exactly?" arc.
- Matrix-form framing softened on the model-comparison and Tuesday-close frames
  (user: "we don't use matrix notation in most cases"): matrix form is for
  general understanding / reading modern papers, not "the language the course
  speaks".

## W1 bivariate presentation, final form (2026-08-24, user simplification)
- User rejected the two-frame treatment (posited model + separate assumption
  frame) as "too much at this stage". Final form: ONE bivariate-model frame
  stating Y = b0 + b1 X + eps WITH E[eps|X]=0 inline, so E[Y|X=x] is the line
  and the intercept/slope readings are justified on the spot. The separate
  "One Assumption" frame is DELETED.
- The uncorrelated/mean-independent/independent ladder frame moved to AFTER
  "What the Normal Equations Say" (user: better logical delivery there).
- Lesson: user prefers assumptions stated inline and simply at first exposure;
  meta-commentary frames about model status get cut. Keep the philosophy in
  the CEF/BLP section where the deck already handles it.

## W1 FWL expansion (2026-08-24, user request)
- Scalar FWL now: recipe frame -> "Why the Recipe Works, Slowly: Raw Y First"
  (substitute long regression into sum Xtilde1*Y, each zero labeled) ->
  "Residualizing Y Too Changes Nothing" (equivalence + the denominator trap,
  moved here from Why This Matters) -> "FWL in Practice: the Added-Variable
  Plot" (two-column: Arbatli/Ashraf/Galor/Klemp 2020 Econometrica figure,
  copied from 690S W1 as Misc/FWL_example.png; also in Overleaf Misc/).
- "Why This Matters" trimmed to its 3 non-duplicated bullets.
- Overleaf sync sed generalized to 's|\.\./Misc/|Misc/|g' (figure + logo).
- Arbatli et al. 2020 added to W1 references.

## Framing rule: matrix form = general understanding ONLY (2026-08-25, user)
- Course-wide preference, stated twice now: never claim the course "builds on"
  or "speaks" matrix/linear algebra. Matrix form is for general understanding
  and for reading modern papers; everything USED and examined stays scalar.
- Final wording pattern: "we keep working in scalars, but recognizing this
  form is what lets you read modern papers." W1's "Adding a Second Regressor"
  and "Why Bother" softened accordingly; other decks scanned clean.
- Also: user reverted my chi-square/t glosses on the finite-sample frame
  ("explained too much. Original is fine") - keep unpacking in lecture-prep
  chat, not on slides, unless asked. But DO define acronyms at first use
  (BLUE now spelled out; G-M assumptions named on the reading frame).

## Week 1 split + Palatino title (2026-08-25, user request)
- Week 1 now TWO decks: slides_tuesday.tex (Orientation..Adjustment + refs,
  74pp) and slides_thursday.tex (own title/Today + Matrix Form + Partialling
  Out + Where This Leaves Us + refs, 28pp). Combined slides.tex/pdf DELETED
  (locally and on Overleaf); README links updated to the two PDFs. Rationale:
  user edits Thursday without shifting pages under students' Tuesday notes.
- Title pages now URW Palladio (Palatino, \fontfamily{ppl}) with larger sizes
  (title \LARGE bold, subtitle \Large) via \setbeamerfont family=; body stays
  Latin Modern. T1/ppl bx->b substitution in log is normal. Palatino available:
  toolchain is FULL TeX Live 2026 now (not TinyTeX; old napkin notes about
  missing packages are stale). Other weeks still have the old smaller title.
- README star note updated to the two-star system.

## Title style, final (2026-08-25): Palatino REVERTED same day. Titles are
Latin Modern like the body, but keep size/weight: title \LARGE\bfseries,
subtitle \Large, author/date \large, via \setbeamerfont (no family= key).
Course is serif Latin Modern everywhere, full stop.

## Tuesday deck: Properties moved BEFORE CEF (2026-08-25, user-approved swap)
- Order now: Orientation -> Scalar OLS -> Properties -> CEF/BLP -> Adjustment.
  Rationale: keep the working-model arc contiguous, and let the circularity
  rug-pull land AFTER unbiasedness is actually proven ("what delivered
  E[bhat]=beta" is now past tense and true).
- Rewired transitions: checkpoint leads into Properties; "Before the List"
  motivation no longer references the CEF section; the finite-sample frame
  closes Properties with the launch of the CEF question; circularity block
  rewritten; Where Tuesday Leaves Us summary reordered.
- GOTCHA: the user edits slides_tuesday.tex directly between sessions (found
  two hand-edited passages that broke exact-match replaces). Re-grep exact
  text before scripted replacements; never assume my last version is current.

## Thursday deck starter-friendly pass (2026-08-25, user's 6 points)
- "Least Squares in Matrix Form" split into 3 slow frames: (1) expansion with
  the two transpose rules ((AB)'=B'A') and the 1x1-scalar-equals-its-transpose
  argument for merging b'X'y with y'Xb; (2) matrix derivatives from zero
  (stacked partials + scalar shadows d(ab)/db=a, d(ab^2)/db=2ab); (3) SOC
  frame defining positive semidefinite (succeq = matrix >=, v'X'Xv=||Xv||^2).
- Rank defined in words on the estimator frame ("number of genuinely distinct
  columns"); primer teaser reworded ("explained shortly", not frame-counted).
- New "Where Projection Comes From: a Change of Picture" frame before the
  formal projection frame: axes=observations flip, Xb sweeps a plane, closest
  point = perpendicular, X'e=0 = normal equations, "normal"=perpendicular.

## 2026-08-25 later: SVD/LSA coda, lab time, final options, event history, exam TBD
- W1 Thursday gained a 2-frame \adv coda after the geometry: SVD as Eckart-Young
  "same projection logic aimed at the data" + LSA/Geometry-of-Culture
  (Deerwester 1990, Kozlowski-Taddy-Evans 2019 added to refs). References
  frames now [allowframebreaks,t] in both W1 decks.
- Lab time is now Thu 9:05-11:35AM (lecture unchanged Tue 10:05-12:35).
- Final assessment (35%) now TWO options: A exam-only 35%; B exam 25% + project
  memo 10% (6-10pp, own data public-or-not, one course method, memo+replication
  code due Fri Dec 11 11:59PM). Updated: syllabus Expectations block, Week 14
  Format-and-Rules frame, syllabus W14 line.
- Week 4 Thursday now includes a half-session intro to event-history (survival)
  analysis (censoring, hazards, Kaplan-Meier, Cox); schedule row renamed;
  Allison 2014 added to W4 further reading. MINI-DECK STILL TO BUILD when
  Week 4 review comes up.
- Final exam date/time now TBD everywhere (class poll; Registrar language
  removed): syllabus paragraph, schedule row, Week 14 logistics + Today.

## Thursday geometry, FINAL shape (2026-08-26, after several user iterations)
- Arc: argmin-as-distance bridge -> mixtures/subspace (no metaphors,
  'observations' not 'people') -> 3-D picture (n=3, X=[1 x]) -> perpendicularity
  derivation of beta-hat (X'(y-Xb)=0 -> normal equations, no calculus; P/M only
  a naming footnote) -> matrix unbiasedness/G-M -> FWL.
- CUT after experimentation: n=2 demo frame, P/M properties frame, leverage
  frame (leverage now defined inline in W2 HC0-HC3 frame), sums-of-squares
  frame, SOC frame (footnote), and the SVD/LSA coda (user approved it, then
  found it disconnected once P was demoted; frames + Deerwester/Kozlowski refs
  removed). Lesson: dessert frames only survive if the machinery they lean on
  stays on screen.
- W2's HC0-HC3 frame has ZERO vertical slack; any addition must be paid for.

## Lab 1 scalar rewrite + title rollout (2026-08-26)
- lab1.qmd is now "OLS by Hand, in Scalar Form": sums-only bivariate OLS
  (matches lecture numbers), weighted-unit-slopes check, residual mechanics,
  scalar s2/SE, THREE simulations (linear DGP recovery; quadratic CEF ->
  cor(x,resid)=0 but curved residual plot = the ladder demo; heteroskedastic
  DGP -> slope still centered), then lm() multiple regression, R2/PRE, FWL,
  OVB (kept). ALL matrix content removed (matrix algebra, P/M, leverage,
  qr/crossprod exercises). Exercises 1,2,5,6 rewritten scalar.
- Deck/syllabus lab promises updated (Thursday Why Bother + reading block;
  syllabus W1 lab line; "reviewed in first two labs" -> "introduced from zero
  in the first two weeks").
- Tuesday title format (size*={16}{19} bold, \Large subtitle, no inline
  \large) rolled to ALL 12 other decks; all compile clean.
- User made direct edits to slides_thursday.tex (trimmed FWL notes) -
  preserved and committed as-is.

## 2026-08-27: W3 slow OLS-is-MLE; W4 event history merged into main deck
- W3: single OLS=MLE teaser frame expanded to 3 scalar frames (one observation's
  bell / whole sample product+log / read-off vs Week 1's S(b)). Part 3 is core.
- W4: event history is a full SECTION in slides.tex (user chose merge over a
  separate deck), placed after More Outcomes, before Practice: censoring,
  survival/hazard, censoring likelihood, Kaplan-Meier, Cox (partial likelihood
  marked adv), discrete-time-as-logit (the person-period trick, ties to the
  week's logit), R + pitfalls frame. Allison 2014 + Cox 1972 added to refs.
  Lab 4 does NOT yet have an event-history component - flag when W4 lab review
  happens.
- W2 sandwich is scalar-first as of 9b0eb93 (matrix frames moved after White).

## Lab 4 event history + R environment note (2026-08-28)
- Lab 4 gained "# Event history (Thursday's session)": simulated union-
  formation durations (true log HR -0.5, censor at 12y), naive-fixes table,
  KM by group, coxph recovering truth, survSplit person-period logit
  (-0.523 ~ Cox -0.497), one new exercise (censor at 6y). survival pkg added
  to the library block.
- ENVIRONMENT: R upgraded to 4.5.2 since the Aug-10 builds; personal library
  did NOT migrate. Had to reinstall AER, brglm2, pROC, VGAM, pscl (CRAN
  binaries, quick). OTHER LABS (2,3,5-13) have not been re-rendered under
  R 4.5.2 - expect missing packages on next render; check installed.packages
  first. Notably still missing per old notes: logistf, WeightIt (by design).

## Codex review of Week 2 (2026-08-29), findings triaged and applied
- Full review saved at .claude/codex_week2_review.md. Note: codex model flag
  gpt-5.3-codex NOT supported on this ChatGPT account - omit -m, use default.
- ACCEPTED (~18): reminder frame's organizing claim fixed (HC = variances,
  CR = + covariances); SD-vs-SE wording; pto/dto glossed; k glossed;
  Route-2 Xbar->E[X] footnote; NEW core frame "The Vocabulary, Pinned Down"
  (H0, t, p-value, CI reading - framed as Stats I words given foundations);
  CI critical value t_{.975,n-k} vs 1.96; Fieller for near-zero ratios;
  multiple-testing caveats; A5-two-parts transition; White frame: FWL-residual
  generalization d_i -> dtilde_i + Stata=HC1 fix; matrix tail deleted from
  one-residual frame (redundant after the moved matrix frames); bivariate
  leverage h=1/n+d^2/sum d^2 replaces matrix def; WLS + error-components in
  scalar; coarsest-clustering rule replaced with design-justified language;
  few-clusters cutoffs labeled heuristics; BDM softened; bootstrap theta/B
  defs + Yhat notation + regularity wording; Freedman rewritten as three-way
  (nonlinear CEF vs exogeneity vs estimand - BLP-consistent with Week 1);
  scoreboard cells qualified; protocol step 2 reworded.
- REJECTED (boundary errors in my prompt to codex): "Where We Left Off"
  variance formula (Week 1 DID derive the general form), s^2/En/F-from-
  Stats-I as unknown (students had Correll et al.), full two-frame testing
  bridge (one frame suffices given Stats I).

## Week 2 instructor pass (2026-08-30 to 09-01), pushed as 9579d56
- Wenhao read the deck line by line asking conceptual questions; each confusion
  became a slide fix. Final architecture of the sampling-distribution section:
  plain-words -> sampling dist -> two theorems -> "as n grows" overview
  (goal + 4-step plan) -> Step 1 LLN denominator -> Step 2 mean via LIE
  written out -> Step 3 variance term-by-term n=3 (derives sqrt-n) ->
  Step 4 CLT + divide (braced fraction) -> meat collapse under A4+A5.
- DROPPED whole frames at his request: Route 1 (exact normal), joint/Wald/
  delta/multiple testing, WLS, "Repeated Samples/Which Parts Got Redrawn",
  conditional-collapse twin. Rule: when a distinction confuses, he prefers
  deletion over hedging. Route naming purged entirely.
- NOTATION rules he insisted on: no shorthand variables (Z_i banned - write
  (X_i-mu_X)eps_i out); mu_X swap made ONCE, flagged harmless; denominator
  keeps Xbar (sample variance -> population variance); no "freeze" language
  (convergence, not fixing); Omega not Omega-star; derivation frames get \adv.
- Robust-SE section now fully unconditional: V = meat/bread^2, White = sample
  analogue, one-residual objection (noise averages, shared bias survives ->
  leverage deflation frame + sigma^2(1-h_ii) derivation \adv), HC table.
  Matrix bridge: dictionary frame (X_i=(1,X_i)') + sandwich multiplied out
  as literal 2x2s until slope entry = scalar V.
- A4 SAGA (important for teaching Qs): codex adjudication saved at
  .claude/codex_a4_check.md. Key truths: conditional Var formula needs no A4
  but computes only E[Var|X]; total Var adds Var(E[.|X]) which A4 kills;
  the meat (mean square) contains that second piece - why White survives
  A4-lite failure (BLP) and classical doesn't; orthogonality suffices for
  slope centering; E[Xeps]=0 untestable (residuals orthogonal by construction).
  None of this on slides (he chose simplicity: A4 just assumed) - back-pocket.
- Pedagogy review 2 saved at .claude/codex_week2_review2.md (9 findings, all
  applied; note the review predates the Route-1 drop).

## Week 2 second pass + labs/HW alignment (2026-09-01/02)
- Wenhao's directive: "learn practice, not math tricks and IQ test." Deck
  changes since push 9579d56: dropped Route 1, Moulton, Few-Clusters,
  bootstrap Variants/BCa/bootstrap-t, When-Bootstrap-Fails, Other Dependence,
  Regrouping-by-Cluster, one-residual objection frame; route naming purged.
  Added: HC deflation frame + sigma^2(1-h) derivation (\adv), matrix
  dictionary + sandwich multiplied out to 2x2 (\adv), clustering rebuilt
  (words-intuition frame -> error components w/ eps in equation + Cov derived
  + cross-cluster independence + rho_e; unconditional breaks frame; Fix =
  sample analogue of per-cluster mean squares; Why Summing First Works),
  which-level examples frame (state/county both ways; nested lotteries GSS;
  "strata are exhaustive; clusters are sampled"), Bootstrapping Clustered
  Data frame (with-replacement mechanics), CI frame (histogram shape, B
  footnote), outlier-exposure bullet on The Idea. References pruned to 7.
- KEY NOTATION DECISIONS: no Z_i (write (X_i-mu_X)eps_i out); Omega not
  Omega*; no "freeze" (settles); B-vs-n distinction on CI frame.
- BACK-POCKET (not on slides, for student Qs): product rule (cross terms
  need error co-movement x regressor co-movement; Moulton rho_x*rho_e);
  total-variance decomposition & A4 kills Var(E[.|X]); BLP escape (robust SE
  right width wrong centre; classical wrong width too via m(X)^2);
  E[Xeps]=0 untestable (residuals orthogonal by construction); coarser
  clustering = generous assumption, noisy-not-larger; stratified vs cluster
  sampling litmus ("would it come out different on redraw"); weights fix
  representation, clusters fix dependence (CPS/ACS: household min, state if
  state-level X); wild cluster bootstrap (dropped from deck).
- vcovCL facts: default = HC1 + cadjust = G/(G-1)*(n-1)/(n-k) * base (CR1,
  Stata default); scalar Sg formula matches type="HC0",cadjust=FALSE exactly.
- Lab 2 renamed "Simulation and Robust Inference": calculus/joint tests/
  few-clusters/clubSandwich/BCa dropped; stargazer added twice (3.3 HC types
  digits=4; 4.2 classical/HC3/clusterPSU); CR1-by-hand with scaling; "Which
  level for GSS: psu, full stop." Answers sheets NEW: lab1_answers.qmd,
  lab2_answers.qmd (self-contained, executed).
- PS1: no proofs; SEs section = plot/by-hand HC0/coverage/by-hand CR
  (cadjust=FALSE)/five-way+referee merged; OVB+bonus+Moulton dropped; 3pp.
- Quarto render REFLOWS qmd source (visual editor: merges $$, rewraps lists)
  — python str.replace anchors go stale after every render; re-grep first.
- gs txtwrite chokes partway on lab2.pdf (stops ~p3); use png16m or Rscript
  to verify outputs instead.

## Week 3 pass (MLE theory) — NOT pushed until Wenhao says done
- Arc: motivation section (What LS Bought/Cost; Where the Line Runs Out
  fig-left/text-right; Question Underneath; Two Stances; Where Going) ->
  marbles (one draw -> one sequence -> all sequences, binom{10}{3}=120) ->
  WE1 binomial count (Bernoulli sequence vs count = constant factor;
  sufficient stat) -> Units-or-Events frame BEFORE Poisson -> WE2 Poisson as
  binomial limit (write Pr(Y=y|n,p) first, then arrests/time-slicing;
  n->inf, p->0, np=E[Y]=theta; "neither n nor p survives"; three stem
  panels theta=1,3,8) -> log-link preview -> WE3 normal linear model (two
  assumptions: linear CEF + normal errors; no mu version) -> Cashing the
  Hook (l = const - S/(2 sigma^2) => MLE=OLS digit for digit) -> What the
  Hook Does Not Say (OLS never needed normality; assumptions define the
  likelihood, not the answer).
- Stars set by Wenhao: motivation frames, marbles, WE1-3, Score, deriving
  frames, Curvature/Fisher, LR/Wald, AIC/BIC frames = \core; information
  equality proof = \adv.
- Newton-Raphson picture (after "When There Is No Closed Form"): TWO panels,
  x=0.55cm,y=0.47cm; example is the POISSON log-lik l=5 ln(theta)-theta
  (y=5, closed form ignored), guesses 1.5 -> 2.55 -> 3.80 -> (4.71) -> 5;
  left = l with colored dots (accent, !65, !35); right = l''=-5/theta^2 as
  ONE curve below the axis (yshift=1.55cm), same dots, theta-hat marked. No
  parabolas anywhere (Wenhao: "put the single second derivative line at the
  right"). Earlier symmetric quartic was dropped because its l'' peaked at
  theta-hat by coincidence and Wenhao read that as a rule.
- Standing 1.86pt overfull at Fisher Information frame accepted.
- Chat-only Q&A (not on slides): MLE=OLS regardless of truth because the
  Gaussian likelihood's argmax is the SSR argmin algebraically; if CEF is
  nonlinear/errors non-normal both estimate the same BLP, only the
  likelihood interpretation is wrong; normality is the price of admission
  to write a likelihood at all.
- Information equality proof split into FOUR \adv frames (pp. 38-41): Goal
  and Plan (tool = int f = 1 for all theta; 4-step plan) / Steps 1-2
  (E[S]=0 line by line; E[(f''/f)]=0) / Step 3 (quotient rule with u,v
  named) / Step 4 (expectations; n-obs additivity; misspecification ->
  sandwich). Chat-only: Var(S)=I vs Var(theta-hat)=1/I reconciled via
  S ~ curvature x displacement; angle-vs-slope illusion for "where the curve
  bends"; 5 log theta - theta is n=1, y=5 (not 5 log theta - 5 theta).
- Info section notation: one-observation score is lowercase s(y;theta) on
  the proof frames; S = sum_i s(y_i;theta) is the full score (Score frame,
  Why Care, Fisher). Why Care frame added before Goal (Var(theta-hat) ~
  Var(S)/(-E l'')^2 via LLN-freeze + Var(cS)=c^2Var(S); "≈" annotated as
  first-order, exact in sqrt-n limit). True value written theta_0 on Why
  Care/Fisher; testing section reuses theta_0 for the null (left as is).
- Four Properties: title unstarred; Consistency & Normality \core, Efficiency
  & Invariance \adv. "Regularity Conditions, and When They Fail" frame DROPPED
  (Wenhao). Deck = 60 pages, zero overfull.
- Testing section: "Why Three Tests\core" lead-in added before the Three
  Ways figure (Wald = summary(glm) z; LR = anova on glm / deviance; score =
  overdispersion/BP; agree via information equality; Wald breaks near
  separation; AIC/BIC for non-nested). Not-Unbiased frame cut to sigma^2
  example + general claim (no promise in finite n; consistency is the
  guarantee) + trade box. Sandwich-for-MLE practice rule stays chat-only
  unless asked (cluster always; robust for counts; diagnostic for logit).
- Week 3 late pass: Question Underneath rewritten in plain voice (no bold
  labels), star legend moved onto it; "Where We Are Going" DROPPED.
- Week 4 pass (reflecting Week 3): Today legend matches ("Stars: ..."); GLM
  frame opens from Week 3 picture + preview (mu_i = last week's theta_i);
  Family frame notes Gaussian GLM = LS hook; IRLS = Newton picture with
  expected curvature; LPM opens from the picture; Logit Likelihood opens from
  WE1 (one theta -> one p_i); Logit Score now \core, closes with curvature
  inverse = variance; Poisson frame opens with units/events rule + preview;
  Overdispersion: robust SE = last week's sandwich, "report as a matter of
  course"; dispersion test = third distance; Separation: Wald z meaningless,
  LR fine; Protocol item 6 = cluster always / robust for counts / logit gap is
  a warning; Assessing Fit points to AIC/BIC for non-nested; delta-method
  "(Week 2)" reference removed (Week 2 dropped it), bootstrap gets "(Week 2)".
  PS2 due Mon Sep 28 matches schedule.tex. Week 4 NOT pushed either.
- Week 3 wordiness pass (derivations untouched): trimmed LS-Bought block,
  Two Stances, Prob-vs-Likelihood block, marbles footnote (dup of WE1
  sequence/count), Preview bullet 3, Hook-Does-Not-Say (bullet 3 dropped),
  Score frame bullet 3 (E[S]=0 now "derived in Section 4"), Normal-derivation
  trailing warning (dup of Not-Unbiased), No-Closed-Form block, optim
  Hessian bullet, Variance-of-MLE footnote+bullet (dups of Fisher/Curvature),
  Poisson-info closing, Four Properties item 3, Why Three Tests = ONE example
  (zero successes; separation as a parenthetical; scale point lives on Wald
  frame only), LR bullets merged, AIC/BIC practice bullets, closing frame.
- Week 4 wordiness pass: GLM bullets, latent closing, logit footnote, Three Scales, Multinomial, Ordered, Excess Zeros block, EH-in-R watch-for, Assessing Fit, Looking Ahead trimmed.
- Added 'Worked Example 1: Reading the Binomial- Wenhao's slide-by-slide pass (old numbering): Preview frame -> \core;
  Hook bullet 2 no longer says "keep OLS with robust SE"; Score frame bullet 3
  states the DIRECTION (theta_0 fixed, data vary = sampling view; estimation
  is the reverse); Newton frame now has a step table (theta/S/-l''/step:
  1.50/2.33/2.22/1.05, 2.55/.96/.77/1.25, 3.80/.32/.35/.91, 4.71/.06/.23/.27)
  and says step is a RATIO (learning rate largest near peak but slope -> 0
  faster); Practical Matters defines the Hessian; Curvature frame: "more
  negative, more information"; Fisher defined at theta_0, expected vs
  observed explained (same number for Bernoulli/Poisson at ybar); Variance
  frame says "one over minus the second derivative"; "Sandwich Returns" frame
  DROPPED (+ White 1982 ref), sandwich mentions removed from Why Care/Step 4;
  Week 4 overdispersion now cites "Week 2's sandwich". Deck 60 pages.
- PUSHED 5de90ca (Week 3 + Week 4 slides, README roadmaps). Left out: Wenhao's own lab2.qmd edit (stargazer type=html, out=table.html; Quarto reflow) and untracked table.html — looks like a scratch experiment; his call.
- Testing section rebuilt (order): "Testing: One Question, Three Rulers\core"
  (null hypothesis = smaller model; beta_j=0 is the model without X_j;
  three rulers horizontal/vertical/slope) -> "Three Ways to Measure the Same
  Distance\core" (figure with LR label left of the drop arrow, Wald ruler
  BELOW the axis, score tangent; three one-line bullets with formulas) ->
  "When the Intuitive Ruler Fails\core" (Wald zero-successes; parabola
  identity LR=W=LM=I d^2; software mapping) -> LR (restriction = smaller
  model; "significant" = "adding X_j helps") -> Wald -> Score -> AIC/BIC
  (needs no null; nested case: dBIC = LR - q log n). Deck 61 pages. NOT
  pushed since 5de90ca.
- Figure frame bullets = 'all three are a squared z-score' (Wald standardizes gap, score standardizes slope via info equality, LR: 2 restores z^2 scale from drop = 1/2 I d^2). Fails frame: Wald extrapolates a parabola; normal linear model = exact parabola (t and F agree); Poisson 5 log theta - theta example theta_0=1 vs 5: W=3.2 vs LR=8.1; zero-successes extreme case.
- Testing section re-split into 10 uncrowded frames: One Question / Three Rulers (figure only) / Three Rulers, One Formula (squared z-scores) / When the Ruler at the Peak Misleads (parabola + Poisson table) / Extreme Case + software block / LR / Wald / Score / Model Comparison / AIC-BIC practice. Deck 63 pages. Unpushed.
- Testing section now OPENS with 'Start From the Standard Error: the z-Test' (Wald first, bridging from SE = 1/sqrt(observed info)); separate Wald frame removed; One Question frame follows. 63 pages. Unpushed.
- Added 'Why chi^2 Keeps Appearing- Lab 3 revised to match the deck: goals; Poisson SE in observed-info terms;
  normal section = "Least squares falls out of the normal likelihood"
  (optim over b0,b1,log sigma for lnearn~educ vs lm; sigma_mle vs s; Hessian
  SE = lm SE * sqrt((n-2)/n); surface over (b0,b1)); tests section reordered
  Wald z first (wordsum: z^2, LR, score ~ equal), then q=2 block, then
  AIC/BIC (drop black: LR=1.09 n.s., dBIC=-7.07 = LR - log n; Raftery
  "strong" for the smaller model). Exercises: Bernoulli info / curvature width
  / Newton table / sigma^2 bias n=20 vs 200 / zero-successes Wald vs LR /
  probit from scratch (uniform-MLE + delta-method + sandwich exercises
  dropped). NEW lab3_answers.qmd (5 pp) + README link. Lab 3 = 13 pp.
- Three explainer frames added (- Testing section now opens with a SIMULATION ARC (all \core): The Obvious
  Plan (theta-hat/SE vs t_{n-1}? three checks) -> Simulation 1 (Poisson
  theta0=2, n=200: z rejects 5.1%, t199 same; figs/sim_z_large.pdf) -> Why
  Week 2 Had t (OLS n=12: 1.96 -> 7.8%, t10 -> 5.0%; MLE SE from curvature,
  nothing exact) -> Simulation 2 (Bernoulli theta0=.1, n=20 exact: 12.2%
  undefined, min z=-1.03, rejects 0.2%; figs/sim_z_small.pdf) -> Sim 2 cont.
  table (n=20/50/100/500: z undefined .122/.005/0/0; z rejects .002/.116/
  .068/.057; LR .133/.058/.044/.052) -> What the Simulations Say (plan).
  "Why z and chi2, Not t and F" frame folded (F line added to chi2 frame);
  "Extreme Case" frame -> "Which Test Your Software Ran" (rule: when Wald
  and LR disagree believe LR). figs/ copied to Overleaf. Deck 71 pages.
- Independent pedagogy review (fresh-context agent; Codex blocked by spend
  cap) saved at .claude/review_week3_pedagogy.md. APPLIED: hook named on
  Question Underneath (+ legend removed, Today has it); Preview moved to end
  of Likelihood section; WE1 retitled "Bernoulli Draws"; "Fix the data,
  compare the panels"; Poisson limit two-line \adv footnote + theta recycled
  note; WE3 "one assumption from Week 1 and one new, sigma^2 fixed"; Hook
  caveat reconciled with t-frame; Score frame defines theta_0 (user had
  already cut the 'centre the bell' line); Newton superscripts [t] not (t);
  Newton bullet 2 no longer claims 'largest learning rate near peak', SE
  misstatements fixed (Newton, Practical); Fisher paragraph simplified with
  info-equality bullet; VARIANCE FRAME MOVED right after Fisher with the
  one-step Newton 'why'; Why Care trimmed to ratio-collapse; Goal-and-Plan
  regularity clause; 'Why Consistency and Normality Hold' RETIRED (3-line
  footnote on Four Properties); 'regular'/'continuous' dropped; Not-Unbiased
  consistency wording; Testing: invariance frame moved after Misleads as
  "Wald's Second Weakness"; chi2 duplicates cut (Simulations Say item 2, LR
  footnote); Sim 2 cont. n=50 flip bullet; Score-test frame RETIRED (which-to-
  use folded into Which Test); LR frame 'usually'; LR=R^2 frame rewritten
  with sigma^2 fixed (exact, then s^2 -> qF; n log ratio in footnote);
  Model Comparison example fixed (no income vs log income), duplicate
  same-data bullet cut. NOT applied: score-tangent Newton picture (Wenhao
  chose the l'' panel), chi2 frame stays before the rulers (he asked for it
  ahead), Two Stances kept. Deck 69 pages.
- Applied Wenhao's round: t-frame rewritten (t exact under normal errors; MLE has no exact df; Poisson n=10 table z 5.3/t9 4.5/LR 4.4); Sim 2 figure redrawn from 20,000 draws (no normal overlay); 'Three Rulers, One Formula' split into 'the Pieces' (gap/curvature/slope/drop; parabola identities) + 'Each Ruler Is a Squared z' (no d); NEW 'Why Large n Makes a Parabola' (figs/parabola_n.pdf; drops at 2 SE: 2.18/2.40/3.35; n^(-1/2) argument). 71 pages. PENDING (Wenhao wants to review a plan first): Poisson likelihood solved by Newton by hand; theta_i = exp(b0+b1 X) with b1 by hand; small-to-large-n parabola simulation for that case.
- Added by-hand frames (all - 'beta1 by Hand' split into 'the Function' (all four terms written with X,y numbers: l = 34 b1 - 1 - e^b1 - e^2b1 - e^3b1; S = 34 - e^b1 - 2e^2b1 - 3e^3b1; S(log 2)=0 shown) and 'the Iterations' (table + first row spelled out + SE = 1/sqrt(90) = .105). 76 pages.
- 'Regressors Inside: Two Parameters' DROPPED (Wenhao); one sentence on the Iterations frame points to glm for the intercept case.
- Testing arc: t detour DROPPED ("Why Week 2 Had t" frame gone; Sim 1 no
  t curve; Obvious Plan uses 1.96 with a one-line footnote). NEW "Simulation
  2: Why z Breaks Here" (figs/loglik_n20.pdf: s=2 log-lik vs parabola; wall
  at theta=0 = 1.5 SE below; drop at +1.96 SE 1.16 vs parabola 1.92).
  Wenhao's standing rule (saved to memory show-proof-for-claims): every
  non-obvious claim gets a proof/simulation on the slide. Added one-line
  proofs: Poisson mean=var from np, np(1-p); binomial mean/var; Fisher
  additivity; dBIC = LR - q log n algebra. Deck 75 pages. Unpushed.
- Standard-axis figures: loglik_n20 now theta on x, l(theta)=2log th+18 log(1-th) on y, parabola p(th) = -6.50 - 111(th-.1)^2, Wald set (-.03,.23) vs LR set (.02,.28); parabola_n and parabola_reg are 2x2 panels in raw theta/beta1. 'Why z Breaks' split: step-by-step definitions frame + curve-vs-parabola frame. 76 pages.

- 2026-09-16 lab 4: dropped the Mundlak within-between subsection (u_hat, meanSES, m_wb) at the user's request; glmer chunk now ends with `avg_predictions(g1, variables = list(SES = c(-1, 0, 1)))` (observed-value, own-school u_j kept by default, re.form = NA sets u_j = 0; 0.137/0.232/0.365) instead of printing tau2. marginaleffects warns that SEs cover fixed effects only; chunk has `warning: false`.
- 2026-09-16 lab 4 (final pass): dropped 'Checking proportional odds' (Brant + VGAM partial PO; restorable from git, `library(brant)` removed); dropped the AIC/BIC callout at the end of 'Comparing the count models'; RI-logit subsection now shows avg_predictions at SES -2..2 by 0.5 and reproduces the slide figure (u_j = +/-tau, 0, population average; tau = 0.58) in ggplot.
- 2026-09-16 lab 4: new subsection 'Reading the lmer formula' (model equation, (1 | School) = random intercept, (1 + SES | School) / (0 + SES | School) / two grouping levels in an eval=false chunk) before 'Predictors at both levels'; ## note on REML = FALSE vs the REML default (ML vs REML differ in the third decimal on HSB).
