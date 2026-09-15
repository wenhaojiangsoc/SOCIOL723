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
  `figs/sim_reg_rare.R`, `Week 4 .../figs/w4_examples.R`. Rerun those, do not retype.

## Conventions (locked in)
- Fonts: Latin Modern, serif body (`\usefonttheme{serif}`), NOT newpx. Reuse Week 1's preamble.
- `\core` blue star = examinable; `\adv` orange star = for understanding only. The user decides stars.
- `mathtools` is not installed; use amsmath/amsfonts/amssymb/bm only.
- Column vectors, prime for transpose, `X_i'\beta` for the linear predictor.
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

## State of the course (as of 2026-09-15)
- Weeks 1-2: done. Week 3 (MLE theory): testing section rebuilt; score test dropped from the
  course except as the third line of the rulers picture; LR-for-a-block + AIC/BIC + BIC-in-
  practice + "large n rejects everything / where BIC came from" close the deck (72 pp).
- Week 4 (MLE applications, 60 pp): X inside theta -> logit (two ways, S-curve, fitted GSS
  logit, Newton by hand, score = normal equations) -> interpretation (three scales, predicted
  probabilities first, Pager 2003 read slowly, AME person by person, AME/MEM, uncertainty,
  probit, recommendation, interactions with real output) -> several categories (multinomial on
  four-region residence, derivation, computation, ordered logit on degree, cutpoints as gates,
  proportional odds) -> counts (Poisson, overdispersion: three beliefs + where the numbers come
  from, excess zeros, hurdle) -> MULTILEVEL (replaced event history 2026-09-14; HSB data via
  nlme, lme4) -> practice. "In the Literature" frames: Pager 2003, Kuo & Raley 2016, Breen &
  Jonsson 2000, Zajacova et al. 2017, Olzak 2021, Ajrouch et al. 2016,
  Sampson et al. 1997; all web-verified.
- `Math Review/math_review.tex`: 24-page standalone review (calculus, Taylor, linear algebra,
  probability, asymptotics, integrals, penalized optimization), linked from README.
- Remaining to build: Weeks 11-14 slides+labs, Problem Sets 2-5, midterm review, final packet.
  Source decks for later weeks: `../SOCIOL690S` (see napkin Build Plan).

## Gotchas
- Dropbox sometimes drops `slides.pdf` mid-session; just recompile. Delete any
  "conflicted copy" build artifacts (.aux/.out/figure pdfs); they are junk.
- `glmer(..., nAGQ = 10)` in lab 4 takes about a minute.
- `degree` in the GSS extract is an ordered factor: use `factor(as.character(...))` before
  `lm()` with it as a dummy set, or `contr.poly` errors appear.
