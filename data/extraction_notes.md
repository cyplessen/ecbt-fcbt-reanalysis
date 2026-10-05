# Extraction notes: reconciliation of the 14 primary studies

Prepared 2026-09-10 from the screenshots in `screenshots/` (the author's 2020 captures of the primary-study tables). Every value in `extraction_reconciled.csv` was read from a screenshot unless marked as derived or as taken from another source. The author's 2020 extraction files are no longer available; the "2020 Rmd" column below is filled only where the author's 2020 notes record those values. Where the screenshot does not show a needed quantity, this is stated rather than filled from memory.

Conventions: e = eCBT arm, c = face-to-face arm. All comparisons are of the raw values (means, SDs, n); the sign convention of the effect sizes is set in the analysis scripts.

## 1. Summary of what changed relative to the corrigendum Table 1

| Study | Corrigendum Table 1 | Screenshot | Consequence |
|---|---|---|---|
| Wright 2005 | post 10.7 (8.1) / 9.7 (8.5), pre eCBT 31.1 | the paper reports **change** scores only; 10.7 (8.1) / 9.7 (8.5) are the BDI week-4 completer changes; endpoint ITT change is 17.5 (10.8) / 14.7 (8.0); pre eCBT is 31.4 | corrigendum replaced a correct ITT change (17.5) with an implied 20.4 |
| Kay-Lambkin 2009 | f2f post 16.65 (10.63) | f2f 3-month value is 13.04 (10.51); 16.65 (10.63) is the **computer arm at 6 months** | column slip in the corrigendum; the study flips direction |
| Poppelaars 2016 | pre 62.61 (11.97) / 63.35 (10.39) = **T1**; post = T9 | pre-intervention is **T0**: 69.33 (8.37) / 66.94 (7.09); post T9 is correct | pre wrong in the corrigendum; post wrong in the 2020 Rmd (used T12) |
| Luxton 2016 | pre 26.65 (11.80) / 29.69 (11.74); post eCBT 13.63 (12.47); n 62/59 | ITT panel: pre 27.60 (10.45) / 29.71 (11.33); post 13.82 (12.02) / 11.74 (12.08), observed n 45/42 | corrigendum values not in the ITT panel (probably mixed with the per-protocol panel, which is not in the screenshot) |
| Glueckauf 2012 | n 7/7 | n 6/5 | small; also the Fig 3 eCBT change 9.4 should be 8.0 |
| Mohr 2012 | SDs from 95% CI with ITT n; pre SD 4.60 | same method gives 4.55 (N = 325) and 7.55 / 8.41 at post | agrees to rounding; n choice documented below |
| Others | match | match | Choi's SEs convert to the corrigendum SDs via SE x sqrt(n) |

## 2. The four disputed trials

### Wright 2005 (Am J Psychiatry; computer-assisted CT vs standard CT vs wait list; BDI and HAM-D)

**Resolved 2026-09-10 against the paper's full Table 1** (baseline; change by week 4, completers; change by week 8, completers; change by endpoint, ITT). Screenshots: `wright pre.png`, `wright_post.png`, plus two further captures of the same table being added to `screenshots/`. Wright reports **no post-treatment means or SDs**, only baseline values and change scores; positive change = benefit (paper's footnote). n = 15 per arm (df 2,42).

| BDI | Screenshot / paper Table 1 | Corrigendum Table 1 | 2020 Rmd |
|---|---|---|---|
| baseline eCBT / f2f | 31.4 (8.2) / 24.4 (6.8) | 31.1 (8.2) / 24.4 (6.8) | 31.4 (8.2) / 24.4 (6.8) |
| change, endpoint ITT | 17.5 (10.8) / 14.7 (8.0) | implied 20.4 / 14.7 | 17.5 (10.8) / 14.7 (8.0) |
| change, week 8 completers | 19.0 (10.1) / 15.2 (7.1) | | |
| change, week 4 completers | 10.7 (8.1) / 9.7 (8.5) | **used as post-treatment means** | |
| post-treatment | not reported | 10.7 (8.1) / 9.7 (8.5) | derived pre - change |

| HAM-D | Screenshot / paper Table 1 |
|---|---|
| baseline eCBT / f2f | 16.6 (3.2) / 17.1 (5.4) |
| change, endpoint ITT | 7.5 (6.3) / 8.3 (6.6) |
| change, week 8 completers | 8.2 (6.4) / 8.4 (6.3) |
| change, week 4 completers | 5.3 (4.8) / 4.7 (5.8) |

What happened in the corrigendum: its "corrected" post-treatment values 10.7 (8.1) and 9.7 (8.5) are Wright's **BDI change-by-week-4 completer scores** read as post-treatment means. The original 2020 Fig 3 value of 17.5 was already the correct ITT change; the corrigendum replaced a correct number with a wrong one, giving an implied eCBT change of 20.4 (31.1 - 10.7) against the paper's 17.5. Its face-to-face change of 14.7 is right only by coincidence (24.4 - 9.7 = 14.7). The corrigendum also has baseline 31.1 where the paper says 31.4.

Extraction rule adopted (and encoded in `R/00_extraction_from_screenshots.R`):
- Change-score metric: use the reported change means and change SDs directly. No imputation of r is needed for Wright; it is the only study in the set with reported change SDs.
- Post-test metric: derive post = baseline - change; the post SD must be borrowed from the change distribution. Flagged in the dataset (`post_sd_imputed = TRUE`).
- Default time point: endpoint ITT. Week 8 completers is a multiverse branch (`data/wright_2005_change_by_timepoint.csv`). Week 4 is recorded only to document the corrigendum's error.
- In the multiverse, Wright's post-test handling is a three-level factor: exclude Wright from the post-test arm; derived post with borrowed SD; change-score SMD substituted for Wright only.

Why Wright is the worked example for metric choice. Baseline imbalance on the BDI is about 0.9 SD (eCBT arm more depressed; Wright's own ANOVA p = 0.001), with 15 per arm. Positive favours face-to-face: BDI change g = -0.29 [-1.01, 0.43]; BDI derived post g = +0.43 [-0.29, 1.15]; HAM-D change g = +0.12; HAM-D derived post g = +0.05. The sign flips with the metric on the BDI, and the two instruments disagree. Regression to the mean under this imbalance is why post-test and change-score analyses disagree study by study across the set (section 6 below).

Effect on the pooled post-test estimate (REML, positive favours face-to-face, 14 studies):

| Dataset | g [95% CI] | HKSJ CI | tau^2 | I^2 (Q) |
|---|---|---|---|---|
| Table 1 as published | 0.11 [-0.17, 0.38] | [-0.25, 0.47] | 0.19 | 68% |
| Table 1, Wright corrected only | 0.13 [-0.15, 0.41] | [-0.24, 0.50] | 0.21 | 68% |
| Reconciled extraction, all corrections | 0.16 [-0.12, 0.45] | [-0.21, 0.53] | 0.21 | 69% |
| Reconciled, without Sethi 2010 and 2013 | 0.04 [-0.08, 0.16] | [-0.10, 0.19] | 0.00 | 14% |

Wright alone moves the pooled estimate by 0.02 (its g goes from 0.12 to 0.43); Kay-Lambkin's column slip is the other material change (0.04 to 0.35). Luxton and Glueckauf move by 0.02 or less; Poppelaars' post (T9) is unchanged, only its pre moves. The reconciled dataset leans slightly more towards face-to-face than the published Table 1, and the conclusion is unchanged: the CI includes zero in every version, and the lean disappears without the two MoodGYM trials.

### Kay-Lambkin 2009 (Addiction; SHADE; computer vs therapist vs brief intervention; BDI-II)

Screenshot: `kay-lambkin.png`, Table 1 "for participants who completed all assessments", depression n = 67 across all three arms.

| Quantity | Screenshot | Corrigendum Table 1 | 2020 Rmd |
|---|---|---|---|
| pre eCBT (CM) | 28.57 (9.89) | 28.57 (9.89) | 28.57 (9.89) |
| pre f2f (T) | 34.91 (9.70) | 34.91 (9.70) | 34.91 (9.70) |
| post eCBT, 3 months | 17.09 (12.14) | 17.09 (12.14) | 17.09 (12.14) |
| post f2f, 3 months | **13.04 (10.51)** | 16.65 (10.63) | 13.04 (10.51) |
| CM 6 months | 16.65 (10.63) | (used as f2f post) | |
| n | not in screenshot; table n = 67 is the three-arm completer total | 32 / 35 | 32 / 35 |
| time point / sample | 3 months = end of the 10-session treatment; completers of all assessments | | |

Recommendation: f2f post = 13.04 (10.51). The corrigendum value is the computer arm's 6-month entry, one row down and one column right. On n: 32 + 35 = 67 equals the table's three-arm completer total, so 32 / 35 cannot be completer n for two arms; they are most likely randomised n. The means are completer means. This mismatch is not resolvable from the screenshot; per-arm completer n would need the paper's CONSORT figure. Flag in the report, keep 32 / 35 for now.

### Poppelaars 2016 (Behav Res Ther; SPARX vs OVK vs both vs control; RADS-2)

Screenshot: `poppelaars.png`, Table 2, T0 to T12. Design (from the abstract): assessed before the intervention, weekly during the 8 sessions, immediately after, and at 3, 6 and 12 months. So T0 = pre, T1 to T8 = weekly, T9 = immediately post, T10 to T12 = 3, 6, 12 months.

| Quantity | Screenshot | Corrigendum Table 1 | 2020 Rmd |
|---|---|---|---|
| pre eCBT (SPARX) | **T0 69.33 (8.37)** | T1 62.61 (11.97) | 69.33 (8.37) |
| pre f2f (OVK) | **T0 66.94 (7.09)** | T1 63.35 (10.39) | 66.94 (7.09) |
| post eCBT | **T9 57.88 (12.57)** | T9 57.88 (12.57) | T12 57.08 (14.21) |
| post f2f | **T9 59.33 (13.27)** | T9 59.33 (13.27) | T12 62.44 (12.77) |
| 12-month FU | T12 57.08 (14.21) / 62.44 (12.77) | 57.08 (14.2) / 62.44 (12.77) | (used as post) |
| n | not in screenshot; randomised SPARX 51, OVK 50 (abstract) | 51 / 50 | |

Recommendation: pre = T0, post = T9. Each earlier source got one of the two right. The large T0 to T1 drop in all arms (about 4 to 7 points) means the choice of pre matters for change scores. Whether Table 2 descriptives are observed or model-imputed is not visible in the screenshot. Note: subclinical sample, prevention trial, SPARX is a self-help game and OVK a school-based group programme; eligibility for "eCBT vs face-to-face CBT for depressive disorders" is debatable and could be an inclusion factor.

### Luxton 2016 (J Consult Clin Psychol; home-based videoconference vs in-person behavioural activation; BDI-II)

Screenshot: `luxton_es.png`, Table 3, intent-to-treat panel only. The per-protocol panel is not in the screenshot.

| Quantity | Screenshot (ITT panel) | Corrigendum Table 1 | 2020 Rmd |
|---|---|---|---|
| pre eCBT | 27.60 (10.45), n 62 | 26.65 (11.80), n 62 | 27.6 (10.45), n 62 |
| pre f2f | 29.71 (11.33), n 59 | 29.69 (11.74), n 59 | 29.71 (11.3), n 59 |
| post eCBT | 13.82 (12.02), n 45 | 13.63 (12.47), n 62 | 13.82 (12.02), n 45 |
| post f2f | 11.74 (12.08), n 42 | 11.74 (12.08), n 59 | 11.74 (12.08), n 42 |
| adjusted difference at post | b = 4.23, 90% CI [0.66, 7.81]; standardised B = 0.36 [0.06, 0.66] | | |
| pre-post ICC (BDI) | 0.60 | | |

Recommendation: screenshot values (identical to the 2020 Rmd). The corrigendum's eCBT values and its 29.69 (11.74) do not appear in the ITT panel and are probably from the per-protocol panel or a mix of both; unverifiable here. The table is labelled ITT but the n falls from 62 / 59 to 45 / 42 at post, so these are observed cases within the ITT frame. For the post-test SMD use n 45 / 42; for change scores the n question is open. Two useful extras: the multilevel model gives an adjusted post difference (positive = in-home higher, favours face-to-face) for the adjusted arm, and the reported ICC of 0.60 for the BDI is an empirical anchor for the pre-post correlation used in the SMC arm. Eligibility note: the treatment is behavioural activation (BATD), not CBT in the narrow sense.

## 3. Second instruments (for the instrument arm of the multiverse)

All values read from the screenshots; none of these appear in the corrigendum. The author's 2020 extraction contained them but is no longer available for comparison.

| Study | Instrument | Rater | pre e | pre c | post e | post c | n at post | Notes |
|---|---|---|---|---|---|---|---|---|
| Andersson 2013 | MADRS-S (primary) | self | 23.6 (4.8) | 24.1 (5.0) | 13.6 (9.8) | 17.1 (8.0) | 65 total, split not shown | observed cases |
| Andersson 2013 | BDI | self | 24.0 (7.0) | 25.3 (6.6) | 13.6 (10.1) | 17.9 (8.8) | as above | |
| Andersson 2013 | HRSD | clinician | 15.4 (3.8) | 16.4 (5.0) | 6.3 (5.0) | 8.1 (4.9) | 29 / 30 | footnote gives the split |
| Himelhoch 2013 | HAM-D (primary) | clinician | 22.6 (5.2) | 24.6 (5.3) | 16.3 (8.6) | 15.9 (7.2) | 16 / 18 | |
| Himelhoch 2013 | QIDS-SR | self | 16.3 (4.1) | 17.0 (3.5) | 10.8 (5.5) | 9.2 (3.7) | 16 / 18 | |
| Kalapatapu 2014 | HAM-D (primary) | clinician | 22.2 (4.7) | 21.8 (3.8) | 12.8 (9.2) | 11.8 (7.2) | 50 / 53 (footnotes not visible) | |
| Kalapatapu 2014 | PHQ-9 | self | 16.8 (5.1) | 16.0 (5.1) | 6.9 (7.2) | 5.9 (5.4) | as above | |
| Mohr 2012 | Ham-D (primary) | clinician | 22.83 [22.34, 23.33] | same (constrained) | 13.58 [12.42, 14.74] | 12.51 [11.22, 13.81] | observed 152 / 141; ITT 163 / 162 | model-based means; adjusted diff 1.07 [-0.63, 2.76] |
| Mohr 2012 | PHQ-9 | self | 16.76 [16.24, 17.29] | same | 6.65 [5.72, 7.58] | 6.74 [5.74, 7.73] | observed 150 / 136 | adjusted diff -0.09 [-1.35, 1.17] |
| Wright 2005 | BDI (primary) | self | 31.4 (8.2) | 24.4 (6.8) | change 17.5 (10.8) | change 14.7 (8.0) | 15 / 15 ITT | no post SD |
| Wright 2005 | HAM-D | clinician | 16.6 (3.2) | 17.1 (5.4) | change 7.5 (6.3) | change 8.3 (6.6) | 15 / 15 ITT (one missing) | no post SD |

Mohr SDs: derived as sqrt(n) x (upper - lower) / 3.92. The baseline estimate is a single constrained value for both arms, so its SD is derived with N = 325 (4.55 for Ham-D, 4.83 for PHQ-9); post SDs with the ITT n of each arm (Ham-D 7.56 / 8.41; PHQ-9 6.06 / 6.46). Using the observed n instead (152 / 141) gives smaller SDs (7.30 / 7.85 for Ham-D). The means are multiply-imputed ITT estimates, so the ITT n is the consistent choice; the corrigendum made the same choice, the 2020 Rmd used observed n. Either is defensible; I recommend ITT n and listing the alternative as a multiverse option only if it is cheap.

Andersson HRSD n at post (29 / 30) differs from the other instruments (65 in total); per-arm split for MADRS-S and BDI at post is not in the screenshot. The 2020 extraction's n of 32 / 33 sums to 65 and is plausible but unverified.

## 4. Other observations relevant to the multiverse

- **Kalapatapu 2014 is a secondary analysis of the Mohr 2012 trial** (the problematic-alcohol subgroup of the same telephone vs face-to-face CBT trial, same arms, week-18 endpoint). The two are not independent; the 14-study set double counts about 100 participants. The multiverse needs an inclusion option that drops Kalapatapu, or a model that nests it under Mohr.
- **Delivery is heterogeneous**: telephone (Mohr, Kalapatapu, Himelhoch, Glueckauf), videoconference (Luxton, Nelson, Choi), clinic-based computer with therapist (Wright, Kay-Lambkin), internet self-help with minimal guidance (Sethi 2010, Sethi 2013, Andersson, Wagner), computer game (Poppelaars). The two MoodGYM trials that drive all residual heterogeneity are the two unguided self-help programmes with under 25 per arm.
- **Intervention content**: Choi is problem-solving therapy, Luxton is behavioural activation, Poppelaars is a school prevention programme in a subclinical sample. These three plus the MoodGYM pair are natural inclusion factors.
- **Model-based versus observed means**: Mohr, Choi (and possibly Poppelaars) report model-predicted means; the rest report observed means, mostly completers. The "sample" column in the CSV records this per row.
- **Pre-post correlation**: Luxton reports ICC = 0.60 for the BDI; this is the only empirical value in the set and supports the r grid centred on 0.5 to 0.7.

## 5. What the screenshots do not contain

- Luxton 2016 per-protocol panel of Table 3.
- Per-arm n for Kay-Lambkin, Andersson (except HRSD), Sethi 2010, Sethi 2013, Wagner, Poppelaars, Choi; these were carried over from the corrigendum and the 2020 Rmd, which agree with each other on all of them except Andersson.
- Instrument names for Sethi 2010, Sethi 2013, Glueckauf, Nelson (the depression scale is not labelled in the visible part of the table).
- Kay-Lambkin per-arm completer n.

## 6. The original -1.73 reproduces exactly

`luo.png` is the original 2020 Fig 3 with all 14 input rows. Feeding them to metafor with RevMan's SMD variance, 1/n1 + 1/n2 + g^2 / (2 (N - 3.94)), gives -1.73 [-2.72, -0.74], tau^2 3.32, Q 548.35 (published Q 548.36). The corrigendum inputs give -0.92 [-1.76, -0.09] with the same formula. metafor's default large-sample variance (2N in the denominator) gives -1.75 and -0.93, which is why Session 1 was off by 0.02. Script 01 now reports both. Sign here is Luo's (negative = less change in eCBT).

## 7. `luo_yves_2020_extraction.csv`

Reconstruction of the author's unpublished 2020 primary-instrument extraction table from the author's working notes. The 2020 source files are no longer available, so this is a reconstruction: studies marked "matches Table 1" copy the corrigendum values; the disputed studies carry the values from the author's 2020 notes. Mohr post SDs were recomputed from the 95% CIs with the completer n the Rmd used (141 / 152). Wright's post SD in the Rmd is unknown and left blank. This table is a record only; the multiverse data-source factor is corrigendum Table 1 vs the reconciled extraction. Replace this file with the real tribbles once the Rmd is available.

## 8. Cross-check against the three PDFs (2026-09-10, `pdf/`)

Read: Luo et al. 2020 (16 pp.), the addendum (2 pp.) and the corrigendum (2 pp.). Findings, in order of consequence for our analyses and the report.

1. **Our data files match the PDFs.** `luo_corrigendum_table1.csv` agrees with the corrigendum's Table 1 in all 14 x 10 cells. `luo_fig3_original_2020.csv` and `luo_fig3_corrigendum.csv` agree with the rendered forest plots (original p. 7; corrigendum p. 1), including totals 563 / 573 (N = 1136 as the text states), tau^2 3.32 / 2.34, Chi^2 548.36 / 420.21, I^2 98% / 97%.
2. **The addendum's figure is the original figure relabelled.** Same 14 rows, same numbers, same -1.73 [-2.72, -0.74]; only the axis text changed to "Less mean change / More mean change" and the caption now says "the pre-score minus the post-score". The `addendum_2020` block in `04_per_study_comparison.csv` therefore correctly copies the original inputs.
3. **"SD" = SE of change holds in 27 of 28 arms** (within 0.1), not in every arm. The exception is Sethi 2013's face-to-face arm: 1.3 entered, 1.63 computed. The report wording was corrected from "in every trial" to "27 of 28 study arms". The Glueckauf eCBT mean (9.4 entered, 8.0 = 12.67 - 4.67 from Table 1) is the one mean that does not follow from Table 1.
4. **The corrigendum's Fig. 3 disagrees with its own Table 1 MD/SE columns in four places.** Two are material: Glueckauf eCBT mean 9.4 vs 8.0 (= 12.67 - 4.67); Sethi 2013 f2f "SD" 1.3 vs 1.63. Two are small: Mohr f2f mean 10.35 vs 10.32 (Table 1 and the original figure both have 10.32; difference 0.03, below the 0.06 screening threshold but visible in the rendered figure); Wright eCBT "SD" 2.9 vs 2.98 (rounding). The published -0.92 reproduces only with the Fig. 3 values, not with Table 1's own MD/SE columns.
5. **The corrigendum's footnote shows the original had Wright right and the correction made it wrong.** "mean post treat intervention should be 10.7 instead of 13.9": 13.9 = 31.4 - 17.5 is the post-test mean derived from the ITT change score; 10.7 is the week-4 completer change score. The original's Wright "SD"s (2.3 / 1.9) reproduce from the SE-as-SD formula with the pre-correction post SD of 3.1329 in both arms (2.27 / 1.93); where 3.1329 came from is not stated.
6. **Choi: the corrigendum describes an SE/SD mix-up only, but the means also changed** (eCBT 10.6 -> 9.86; f2f 14.1 -> 13.67) without explanation. The original's Choi "SD"s (0.2 / 0.2) reproduce from the SE-as-SD formula applied to Choi's SEs (0.18 / 0.16). Mohr's f2f mean also changed (10.32 -> 10.35) without mention.
7. **Methods text contradicts the corrigendum.** Section 2.5.5 of the original: "All pool standard mean deviations were calculated using baseline scores." The corrigendum's formula uses pre and post SEMs. Blog material; not in the report.
8. **The outlier sensitivity analysis was in the original**, not new in the addendum: Section 2.5.8 names Choi 2014, Appendix I Fig. 8 shows it, and the addendum quotes its result (-0.79 [-1.60, 0.02]). Script 01 and section 6 above were reworded.
9. **The addendum's post-intervention analysis (0.21 [-0.05, 0.46]) does not reproduce** from any input set we can construct: corrected Table 1 gives 0.09 [-0.13, 0.32] (DL, RevMan variance); the pre-correction values from the corrigendum's footnotes give 0.14 [-0.13, 0.40], or 0.19 [-0.09, 0.46] without Choi. The Reproducibility paragraph of the report now lists these.
10. **The corrigendum's own Fig. 3 settles the sign question.** Its axis reads "eCBT (less mean change) / fCBT (more mean change)" with a boxed note "Less mean change = less reduction in depression with eCBT favoring fCBT". So the corrigendum's -0.92 favours face-to-face by the authors' own labelling, which is where our figures place it; the sentence "did not change the results, direction, or the conclusions" refers to the addendum's reading, while the abstract of the version of record still says eCBT was more effective.
11. Original Table 2 (GRADE) restates the estimate as "SMD 1.73 SD lower (2.72 lower to 0.74 lower)" for 563 / 573 participants; consistent with the figure.
