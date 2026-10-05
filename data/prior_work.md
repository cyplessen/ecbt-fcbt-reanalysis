# Prior work: Kriston, Liebherz and Koehnen (2022)

Kriston L, Liebherz S, Koehnen M. Concerns about 'A comparison of electronically-delivered and face to face cognitive behavioural therapies in depressive disorders: A systematic review and meta-analysis'. eClinicalMedicine 2022;54:101763. doi 10.1016/j.eclinm.2022.101763. PMID 36471730, PMC9719072. Correspondence, two pages, with a supplementary workbook (four sheets: S1 reproduced extraction, S2 reproduced estimates, S3 bias, S4 conventional analysis). Both are in `pdf/`; S1 and S4 are transcribed verbatim to `data/kriston_supplement_s1_extraction.csv` and `data/kriston_supplement_s4_analysis.csv` (script 07). Fig 1 inputs as printed are in `data/kriston_fig1_as_printed.csv`.

Sign convention: their Fig 1 pools eCBT minus face-to-face, so **positive favours eCBT**. Every number in this file is quoted in their sign unless marked "our axis" (positive favours face-to-face).

## 1. What they did

1. Two reviewers independently re-extracted sample size, pre mean, pre SD, post mean and post SD per arm for all 14 studies (140 cells), reached consensus, and compared the consensus with the corrigendum's Table 1 (S1: per-cell percent deviation, source table and page in the primary report).
2. They reproduced the authors' per-study estimates from the corrigendum's own inputs and identified the calculation: the mean change divided by the standard error of the mean change instead of a standard deviation (S2; letter: "instead of using standard deviations, they used standard errors in the denominator of the SMD formula").
3. They derived the resulting bias analytically: the authors' "SMD" overestimates the conventional SMD by a factor that grows with sample size, about ten for trials with 200 participants (S3).
4. They reran the meta-analysis with their re-extracted data and a conventional SMD, "in every other aspect the same meta-analytic approach as applied by the authors" (S4, Fig 1).

## 2. Their effect size and estimator, recovered from the S4 cell formulas

Per arm: change = pre mean minus post mean; SD of change = sqrt(SD_pre^2 + SD_post^2 - 2 r SD_pre SD_post) with **r = 0.3** for every study. Between arms: pooled SD sqrt(((n1 - 1) s1^2 + (n2 - 1) s2^2) / (N - 2)); g = (change_eCBT - change_f2f) / pooled SD times (1 - 3 / (4N - 9)); SE = sqrt(N / (n1 n2) + g^2 / (2 (N - 3.94))). These are RevMan's formulas (small-sample correction J = 1 - 3/(4N - 9), variance with N - 3.94). Pooling: inverse-variance random effects, DerSimonian-Laird (RevMan's "IV, Random").

Sample sizes: analysed (completer) n where the primary report gives it (Luxton 45/42, Mohr 152/141, Choi 49/54, Wagner 25/28, Wright 15/15, Himelhoch 14/17, Kay-Lambkin 23/23), randomised n otherwise. Total 508 eCBT / 506 face-to-face.

## 3. What they report

- Fig 1: SMD **-0.20 [-0.44, 0.05]**, tau^2 0.14, Chi^2 41.88 (df 13, p < 0.0001), I^2 69%, Z 1.54 (p 0.12). Their reading: "no statistically significant superiority of one treatment over the other, which, however, should not be interpreted as evidence of equivalence".
- Per-study SMDs from Andersson 0.34 [-0.15, 0.83] to Sethi 2010 -1.48 [-2.53, -0.44].
- Data extraction: "notable disagreement regarding around ten percent of the data points".
- On interpretation: the authors "offer several, methodologically distinct and partly conflicting, interpretations of their original findings" (more effective than; at least as effective as; as effective as; and, in the addendum, face-to-face superior) and, even after the direction reversed, "the overall conclusions are essentially the same".

## 4. Reproduction (script 07, `output/tables/07_kriston_reproduction.csv`)

From the Fig 1 inputs as printed, with RevMan's J and variance and DerSimonian-Laird: **-0.1951 [-0.4431, 0.0529]**, tau^2 0.1363, Q 41.88, I^2 69%, z -1.54. Every printed quantity reproduces at the printed precision; all 14 per-study SMDs match at two decimals. From the S4 per-study g and SE at full precision the result is identical. Their supplement's per-study g and SE recompute from their inputs with their formulas to machine precision (max deviation 2e-16).

With metafor's exact J and large-sample variance: -0.197 [-0.447, 0.052]. REML: -0.200 [-0.481, 0.082]; REML with Hartung-Knapp: -0.200 [-0.533, 0.134]. So the estimate is stable to the estimator; the interval widens by 35% with HKSJ but the conclusion is unchanged.

On our axis their result is +0.20 [-0.05, 0.44]. Their exact specification (their data, change-score SMD at r = 0.3, DL, all 14 studies) is a member of the multiverse (`data_source = kriston`, `es_metric = chg_smd_r0.3`) and returns 0.197 [-0.052, 0.447] there (metafor variance).

## 5. Where their extraction agrees and disagrees (`data/source_comparison.csv`)

Against the corrigendum's Table 1, over 28 trial x arm rows: sample size agrees in 12 (they use completer n), pre mean and SD in 21, post mean and SD in 20. Against our reconciled extraction: pre agrees in 26, post in 22, n in 14 (we use randomised n, as the corrigendum does).

Disagreements with the corrigendum that they caught, and we cite rather than claim:

- **Kay-Lambkin 2009** face-to-face post 13.04 (10.51), not 16.65 (10.63): the corrigendum's value is the computer arm's 6-month figure. Their S1 records a 28% deviation.
- **Poppelaars 2016** pre = T0 (69.33 (8.37) / 66.94 (7.09)), not the corrigendum's T1 values.
- **Glueckauf 2012** n = 6 / 5, not 7 / 7.
- **Andersson 2013** n = 32 / 33, not 33 / 36.
- **Luxton 2016** ITT panel values (27.60 (10.45) / 29.71 (11.33) pre; 13.82 (12.02) / 11.74 (12.08) post) rather than the corrigendum's, which are not in the ITT panel.
- **Wright 2005**: they list post means 13.9 / 9.7 (baseline minus endpoint ITT change) with post SDs 9.91 / 6.72, back-solved so that the change SD at r = 0.3 equals the reported 10.8 / 8.0. Their reviewer 2 initially recorded 20.7 (4.56) / 14.7 (7.53) with n 14 / 14; the means are baseline minus the corrigendum's "post" values (see the note below), the SDs have no visible source; resolved at consensus.
- **Corrected Fig 3 vs Table 1 transfer errors**: their S2 sheet reproduces the authors' estimates from Table 1 and therefore encounters the Glueckauf (8.0 vs 9.4) and Sethi 2013 (1.63 vs 1.3) discrepancies; they did not comment on them in the letter.

Where they differ from us:

- **Sample size**: they use completer n throughout where reported; we use randomised n for the main analysis (with the analysed n recorded), matching the corrigendum. For Choi and Mohr this also changes the SDs, which are back-calculated from standard errors or confidence intervals and so scale with n.
- **Wright 2005**: they treat the post SD as an unknown to be back-solved from the change SD under r = 0.3; we borrow the change SD directly and flag it as imputed, and we run Wright's change scores as a separate metric branch. Neither approach yields a reported post SD, because Wright reports none. They did **not** identify that the corrigendum's post values 10.7 (8.1) / 9.7 (8.5) are the week-4 completer change scores (their S1 simply records a 23% deviation for the eCBT post mean). That mechanism is ours to report.
- **Instruments**: they extract one instrument per study; we also carry second instruments (Andersson, Himelhoch, Kalapatapu, Mohr, Wright) for the multilevel and RVE branches.
- **Pre-post correlation**: fixed at 0.3; we use 0.3 (their value) and 0.6 (the pre-post ICC Luxton 2016 reports for the BDI) for the change-score SMD, and 0.3 to 0.9 for the standardized mean change, where r enters only the variance.
- **Mohr 2012**: their pre means 22.9 / 22.8 vs 22.83 / 22.83 (rounding in the JAMA table); immaterial.

Note on the Wright reviewer-2 values: 20.7 = 31.4 - 10.7 and 14.7 = 24.4 - 9.7, i.e. reviewer 2 derived post means by subtracting the corrigendum's "post" values, treating them as changes; the consensus dropped this. This is indirect evidence that they recognised the corrigendum's Wright numbers as change scores, but neither the letter nor the supplement says so.

## 6. What they did not do, and this reanalysis adds

- No sensitivity to the pre-post correlation, estimator, instrument, inclusion set or Wright handling: one specification. The multiverse here has 2388.
- No identification of the Wright week-4 mechanism, or that the corrigendum's "correction" of Wright replaced a correct derived post mean (13.9) with a wrong one (10.7).
- No comment on the corrigendum's Fig 3 disagreeing with its own Table 1 (Glueckauf, Sethi 2013, Mohr, Wright cells), nor on the unexplained change of Choi's means between the original and corrected figures.
- No link to GRADE: the original's sole downgrade for symptom severity is "very serious" inconsistency, justified by I^2 of 98%. Kriston et al. report I^2 69% but do not draw the consequence that the heterogeneity, and with it the downgrade, was manufactured by the units error. On post-test SMDs from the corrigendum's own table I^2 is 68% (script 06); across the multiverse it is lower still for many specifications.
- No reproducibility verdict on the four documents as such.

## 7. Authors' reply

None found. Two web searches (September 2026) and the journal's linked-article lists for the letter, the original, the addendum and the corrigendum return no reply by Luo, Sanger, Samaan or Thabane. eClinicalMedicine's linked articles for the letter point only to the corrigendum (10.1016/j.eclinm.2021.101182) and the original (10.1016/j.eclinm.2020.100442). No correction notice on the original refers to the letter. Google Scholar (screenshot `pdf/Luo citations.png`, September 2026) shows the original cited about 30 to 46 times per year from 2021 through 2026, with no visible change after the letter appeared in December 2022.

## 8. Bibliographic entry

Added to `report/references.bib` as `kriston2022concerns` with the DOI.
