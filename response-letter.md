# Response to Reviewers — Elements-HSS3-2024-0018.R1

**Manuscript:** "Policy Learning and Information Processing Dynamics"
**Status:** Draft skeleton — placeholders marked `[TODO]` need to be completed before submission.

---

## Before you submit (not part of the letter itself)

- **The revision deadline (09-Jun-2026) has already passed** (today is 2026-07-28). Contact the Editorial Office to arrange a new date before doing anything else — the decision letter explicitly says they may otherwise treat this as a new submission.
- Once the outstanding items below are resolved, delete this section and the Revision Roadmap at the bottom before sending the letter — editors only want the point-by-point response.

---

## Cover note (draft)

Dear Dr. Ramesh, Dr. Araral, and Reviewers,

Thank you for the opportunity to revise "Policy Learning and Information Processing Dynamics." We appreciate the reviewers' continued engagement with the manuscript and have used this round to make substantial changes rather than incremental ones, including [TODO: one-sentence summary once all fixes below are complete — e.g., "restructuring the book so the literature-synthesis chapter now follows and explicitly builds on the model and framework rather than preceding them, simplifying language throughout, adding an organizing figure, and expanding our engagement with policy innovation and public-opinion information-processing literatures."]

Below we respond to each comment in turn.

---

## Response to the Associate Editor (Eduardo Araral)

**Comment 1 — Clarity of writing.** *"The writing would benefit from being clearer, simpler, and more engaging... phrases in the abstract such as 'heralded as a versatile... often treated as an ontology of the policy process'... are not easily interpretable."*

**Response:** We have rewritten the abstract in plain language, removing terms like "ontology of the policy process" and "micro-foundational fineness" and stating directly what the Element does and finds. [DONE — abstract.md / index.qmd]

We have also completed a full prose-simplification pass across Chapters 1 through 6, removing dense or circular phrasing (e.g., "an ontology of the policy process" -- the exact phrase flagged in the abstract -- has been removed everywhere else it recurred, including Chapter 6) while deliberately preserving the book's technical vocabulary: Bayesian terminology, the formal notation, and "policy-embedded learning" and "micro-foundational," which are terms of art central to the argument rather than avoidable jargon. [DONE — chap1.qmd through chap6.qmd]

**Comment 2 — Purpose and contribution; an organizing figure.** *"A figure showing how the manuscript is organized—how chapters connect and how the theoretical arguments develop—would help guide readers."*

**Response:** We have added a figure to Chapter 1 (@fig-roadmap) showing how the Element is organized, grouped into its two parts (Part I: Microfoundations, covering Chapters 2–3; Part II: Learning in Context, covering Chapters 4–5), with labeled arrows indicating how the argument develops from chapter to chapter: the introduction builds a model, the model is applied and then scaled up to the broader policymaking process, and the Element then connects back to the literature before concluding. [DONE — chap1.qmd]

**Comment 3 — Tables.** *"Table 4.1 includes starred values without explanation... Descriptive statistics and correlation matrices are also missing... Table 5.1 is similarly unclear, and the meaning of 'X' should be explicitly defined."*

**Response:** We have added significance notes to the tables in the empirical chapter (now Chapter 3, "Table 4.1" in the reviewer's numbering). [DONE — tbl-dims, tbl-results]

Standard errors were already reported in parentheses below each coefficient in the OLS table by default; we have added an explicit note to the table stating this so it is unambiguous. We have also added two new tables reporting descriptive statistics (Table A.1) and pairwise correlations among the core continuous variables (Table A.2) for the analytic sample. These are reported in a new appendix to Chapter 3 rather than in the body of the Results section, with a footnote at the point in the text where they are discussed directing readers to the appendix. [DONE — chap3.qmd, Appendix, Table A.1, Table A.2]

The table the reviewer refers to as "Table 5.1" (in the chapter that was Chapter 5 at the time of review, now Chapter 4) has since been replaced entirely by a figure (@fig-paths) illustrating the pathways from policy learning to policy change. This addresses the reviewer's concern directly: the ambiguous "X" notation no longer exists, having been replaced by an explicit visual pathway diagram. [DONE — chap4.qmd, `fig-paths`]

**Comment 4 — Response rates.** *"The authors' claim that response rates are 'not a useful metric'... is problematic... If response rate information is unavailable, this should be acknowledged as a limitation."*

**Response:** We have rewritten the relevant footnote (Chapter 3) to state plainly that the absence of response-rate data is a limitation of the study. The revised footnote draws on the total survey error framework [@grovesTotalSurveyError2010] to note that response rate alone is an incomplete indicator of non-response bias, and on the AAPOR task-force report on online panels [@bakerResearchSynthesisAAPOR2010] to support quota-based demographic matching as a recognized, if partial, method for improving representativeness — while explicitly acknowledging that it does not rule out bias on unobserved or attitudinal dimensions. [DONE — chap3.qmd, `[^chap4-2]`]

**Comment 5 — Policy innovation.** *"The manuscript would benefit from a more substantive engagement with relevant literature, including Goyal et al. (2025), Kemmerling (2023), Huang & Wiebrecht (2021), and Xun Wu and M. Ramesh (2014)."*

**Response:** We have substantially expanded our discussion of policy innovation in Chapter 6's future-research section, engaging all four sources directly: Goyal et al. to frame policy innovation as adoption/diffusion/mainstreaming; Kemmerling to connect policy innovation explicitly to policy learning via South–North diffusion; Huang and Wiebrecht to connect multi-level government roles in innovation adoption to our policy-embedded framework's institutional structure; and Wu and Ramesh to connect policy-mix innovation to instrumental/secondary-belief learning in our model. [DONE — chap6.qmd]

---

## Response to Reviewer 1

Reviewer 1 raised largely the same concerns as the Associate Editor. Rather than repeat each response, we address them in the same order below and note where our response differs.

**Comment 1 — Same as AE Comment 1 (clarity, abstract example).** [See response above; same status.]

**Comment 2 — Same as AE Comment 2 (contribution unclear, organizing figure).** As detailed above, Chapter 1 now includes a figure (@fig-roadmap) showing how the chapters connect and how the argument develops. [DONE]

**Comment 3 — Tables (same examples: Table 4.1 stars, missing descriptives/correlations, Table 5.1 "X").** Significance notes, the standard-error clarification, the new descriptive-statistics/correlation tables, and the replacement of "Table 5.1" with @fig-paths are all done, as detailed above. [DONE]

**Comment 4 — Response rates**, with the additional citation to Wooldridge (2010) on nonresponse bias in econometrics.

**Response:** As detailed above, the footnote now cites Groves and Lyberg's total survey error framework and the AAPOR report on online panels directly, and states plainly that the absence of response-rate data is a limitation. [DONE — chap3.qmd]

**Comment 5 — Policy innovation**, naming the same four sources and noting they "do not appear to be integrated into the manuscript" despite being suggested in the previous round.

**Response:** As detailed above, all four sources are now integrated in Chapter 6. We regret that this was not completed in the prior round and appreciate the reviewer's patience in re-raising it. [DONE]

---

## Response to Reviewer 2

**Comment 1 — "Conflation of two different research agendas"; the book "reads... like two insufficiently developed research projects that have been awkwardly combined."**

**Response:** We have restructured the Element to address this directly. The chapter that surveys the broader policy learning literature (Chapter 5) previously preceded the model and framework and read as a free-standing literature review; it has been moved to follow Chapters 2 through 4 and substantially rewritten so that it no longer summarizes the field in isolation, but explicitly shows what our model and framework contribute to each part of it — see our responses to Comments 2 and 3 below for specifics. Chapter 6 has also been revised to remove duplicated and inconsistent material left over from the earlier structure. [DONE — chap5.qmd, chap6.qmd]

[TODO — this restructuring addresses the organizational half of this comment. The reviewer's underlying concern about theoretical depth (see Comment 4 below) is not yet resolved and will need to be addressed for this comment to be fully answered.]

**Comment 2 — The micro/meso/macro distinction is "underdeveloped... not discussed sufficiently... not illustrated with examples."**

**Response:** Chapter 5 now includes a dedicated discussion connecting each level explicitly to material already in the Element: the model in Chapter 2 as the micro-level mechanism, the coalition/subsystem discussion in Chapter 4 as the meso-level extension, and the institutional-structure discussion in Chapter 4 (decision costs, veto players) as the macro-level extension, with the feedback loop in `@fig-framework` offered as the specific aggregation mechanism. We have also added a worked example directly addressing the reviewer's request: the Germany vignette from Chapter 4 is now cross-referenced in this discussion, tracing a single sequence from Chancellor Merkel's individual belief change (micro), through the governing coalition's preference and behavior change under electoral pressure (meso), to Germany's 2022 phase-out legislation (macro) — a traceable sequence rather than an assumed correspondence between levels. [DONE — chap5.qmd]

**Comment 3 — The overview of types and modes of learning is incomplete and "the concepts are not explained at all and not used in the rest of the book."**

**Response:** Chapter 5 now includes explicit discussion connecting each descriptor family to the model and framework: outcomes (the model's four belief-level outcomes plus the Chapter 4 pathway), mechanisms (inferential and contingent learning as the same underlying mechanism under different signal-strength conditions), modes (subsystem type in Chapter 4 predicting learning mode), types (the issue-dimension mechanism in Chapter 2 mapped onto the loop-learning tradition), and forms (the Fukushima country vignettes in Chapter 4 as an illustration of cross-jurisdictional information processing). These concepts are therefore no longer introduced once and abandoned. [DONE — chap5.qmd]

**Comment 4 — The model relies on the Advocacy Coalition Framework, "which is concerned with something quite different" from public opinion; the manuscript should "engage seriously with the research on information processing in politics and extend it to voters," pointing toward punctuated-equilibrium-adjacent work on information processing.**

**Response:** We have substantially rewritten the introduction to Chapter 3 to address this directly, in three parts. First, rather than relying solely on the claim that public opinion approximates elite belief structure, we now ground our Bayesian updating model independently in the mass political behavior literature on citizen learning (Gerber and Green 1998; Bullock 2009) and directly engage the information-processing-in-politics tradition the reviewer named (Zaller 1990, 1992; Lodge 1986; Kuklinski et al. 2000). Second, we substantially strengthen the elite/mass belief-structure argument, anchoring it in Converse's (1964) foundational finding that belief constraint is concentrated among the more attentive segment of the public, and now reporting the specific empirical findings behind this claim rather than citing them in passing: Herron and Jenkins-Smith (2002) compared elite and mass belief systems using the same core/policy-core/secondary hierarchy this Element relies on, on the closely related domain of nuclear security, and found the same direction and statistical pattern of relationships across elite and mass samples, though elite belief systems were more tightly organized (explaining more variance); Jenkins-Smith, Mitchell, and Herron (2004) show this same hierarchical structure holds for mass publics in both the US and Great Britain despite the two countries differing substantially in their average beliefs; and Tumlison and Song (2019) find the same values-to-trust-to-risk-perception pathway operates for both policy elites and the general public, with trust playing a larger mediating role for elites. This gives the elite/mass similarity claim empirical specificity, including an honest account of where elites and the public differ in degree, rather than a general assertion of similarity. It also justifies and explains our restriction to an advanced-degree subsample, rather than treating that restriction as unexplained, and we add Kuklinski et al.'s (1982) study of citizen knowledge specifically on nuclear energy as a directly on-topic precedent. Third, we directly address the skeptical view that citizens do not update beliefs in a considered, Bayesian fashion (Achen and Bartels 2017), noting that this critique targets low-attentiveness citizens specifically excluded by our sampling strategy, and add a second, independent line of support from the low-information-rationality literature (Lupia and McCubbins 1998) showing that highly salient focusing events like Fukushima can be processed meaningfully even without specialized expertise. [DONE — chap3.qmd, introduction]

**Comment 5 — Overall assessment: the manuscript "needs to strengthen its coherence and fit with the series," and the reviewer is "left with the impression that the authors have not taken the review process seriously."**

**Response:** [TODO — this closing comment should be answered directly and candidly once the above items are resolved, rather than left implicit. Consider a short paragraph acknowledging the reviewer's frustration, summarizing concretely what changed in this round (the restructuring, the theoretical connections added in Chapter 5, the new engagement with policy innovation and, once complete, mass-public information processing), and inviting the reviewer to assess whether the Element now reads as a single coherent contribution rather than two combined projects.]

---

## Revision Roadmap (internal — delete before sending)

| # | Item | Status | Where |
|---|------|--------|-------|
| 1 | Abstract jargon removed | Done | `abstract.md`, `index.qmd` |
| 2 | Policy innovation literature integrated | Done | `chap6.qmd` |
| 3 | Chapter 5 repositioned and reworked (contribution-mapping) | Done | `chap5.qmd` |
| 4 | Chapter 6 numbering/duplication cleanup | Done | `chap6.qmd` |
| 5 | Table significance notes (Chapter 3) | Done | `chap3.qmd` |
| 6 | Manuscript-wide prose simplification (beyond abstract) | Done | `chap1.qmd`–`chap6.qmd` |
| 7 | Organizing figure (chapter roadmap) | Done | `chap1.qmd`, `@fig-roadmap` |
| 8 | Standard errors/t-values in OLS table | Done (already present; note added for clarity) | `chap3.qmd`, `tbl-results` |
| 9 | Descriptive statistics / correlation table | Done — moved to Appendix (Table A.1, A.2) | `chap3.qmd`, Appendix |
| 10 | Locate/clarify "Table 5.1 with unexplained X" | Done — replaced by `fig-paths` | `chap4.qmd` |
| 11 | Response-rate footnote rewrite + fix empty citation | Done | `chap3.qmd`, `[^chap4-2]` |
| 12 | Micro/meso/macro worked example (Germany vignette cross-reference) | Done | `chap5.qmd` |
| 13 | Mass-public information-processing literature engagement | Done | `chap3.qmd`, introduction |
| 14 | Contact Editorial Office re: passed deadline | Outstanding — logistics, not content | N/A |
