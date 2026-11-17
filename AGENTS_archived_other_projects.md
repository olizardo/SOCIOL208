# Archived Project Specs (extracted from SOCIOL208 AGENTS.md 2026-11-17)

These are specs for three projects that do NOT live in c:/projects. Move each to its own repo's AGENTS.md.

---

## Project Architecture: Classifying and Predicting Degree Trajectories in Longitudinal Ego Networks

### 1. Overview & Collaborators
- **Project Title:** Classifying and Predicting Degree Trajectories in Longitudinal Ego Networks
- **Authors:** Omar Lizardo and David Hachen
- **Overleaf Project URL:** https://www.overleaf.com/project/6a9db1ea255f30d10b63a391
- **Overleaf Git Endpoint:** `https://git.overleaf.com/6a9db1ea255f30d10b63a391`
- **Google Doc URL:** https://docs.google.com/document/d/147PaI7iC0LPB12CI_sT6XY_gCg76JKFkPP18maTc9TQ
- **Google Doc ID:** `147PaI7iC0LPB12CI_sT6XY_gCg76JKFkPP18maTc9TQ`
- **Data Source:** NetHealth Study (University of Notre Dame, 8 waves over 4 years, Fall 2015–Spring 2019: https://sites.nd.edu/nethealth/).
- **Analytic Samples:**
  - *Benchmark Waves 1–6 Sample:* $N = 450$ participants with $\ge 3$ network surveys in Waves 1–6, complete baseline survey, and passive phone compliance.
  - *Expanded Waves 1–8 Sample:* $N = 457$ participants tracked through senior-year graduation.

### 2. Theoretical Grounding & Empirical Design
The project investigates how personal network size (degree) changes dynamically across the complete collegiate life course, whether individuals follow distinct, predictable degree pathways, and how baseline psychological traits and sociodemographic background predict these trajectories:
1. **Unified Eight-Wave Panel Architecture ($N = 457$)**:
   - Spans eight full semesters from freshman matriculation in August 2015 to senior graduation in May 2019 ($N = 457$ analytical cohort, $N = 432$ complete-case modeling sample).
   - Positions Chandler & Hachen (Sunbelt 2018) as the foundational preliminary study, while unifying the entire empirical investigation around principled count mixtures across all four collegiate years.
2. **The Collegiate Network Landscape & Dunbar Cognitive Layer Decomposition**:
   - Grounded in Robin Dunbar's cognitive layering perspective \citep{dunbar1992neocortex, dunbar1998social, dunbar2018anatomy} alongside \citet{marsden1984measuring}, avoids single-dimension frequency contamination (spatial co-presence) and closeness ceiling compression by defining three concentric tiers:
     * **Tier 1 (Support Clique)**: Alters evaluated as ``Especially Close'' AND maintained through Daily contact (~4-5 alters).
     * **Tier 2 (Sympathy Shell)**: Active close ties maintained through Weekly contact (Especially Close weekly or Merely Close daily, ~4-5 alters).
     * **Tier 3 (Peripheral Perimeter)**: All other nominated alters in the ego network (casual, dormant, or monthly contacts).
   - **Key Substantive Discovery**: Total network size contracts steadily from 14.19 to 10.80 alters (-23.9%), but the intimate Support Clique (4.55 $\rightarrow$ 3.43 alters, ~32-36% of network) and Sympathy Shell (3.76 $\rightarrow$ 3.72 alters) remain remarkably stable, keeping the cumulative core at 7.15 to 8.31 alters (66%--72% of personal networks). Aggregate degree decay is driven almost entirely by the winnowing of Tier 3 peripheral contacts (falling from 5.88 to 3.38 alters, a -42.5% collapse).
3. **Trivariate Multi-Trajectory Latent Class Growth Analysis (Poisson Mixtures)**:
   - Replaces univariate counts with formal **Trivariate Multi-Trajectory Poisson finite mixture models** in `flexmix` \citep{grun2008flexmix}, simultaneously estimating the co-evolution of Support Clique, Sympathy Shell, and Periphery.
   - Robustness checks demonstrate that while formal BIC declines through higher-order solutions, $K = 3$ captures the primary structural elbow ($\Delta\text{BIC} = -4,502$ for $K = 2$, $-1,112$ for $K = 3$), cleanly distinguishing three developmental archetypes:
     * **Periphery Conservers** ($n = 129$, 28.2\%): Maintain moderate Support Cliques ($\approx 2.6\text{--}3.6$ alters) and Sympathy Shells ($\approx 4.6\text{--}5.3$ alters), while uniquely sustaining an expansive, invariant Periphery ($\approx 7.0\text{--}8.0$ alters).
     * **Peripheral Winnowers** ($n = 205$, 44.9\%): The modal pathway; preserves an intimate Support Clique ($\approx 3.0$ alters) and Sympathy Shell ($\approx 1.9$ alters) while their peripheral perimeter collapses by 83% (from 4.38 to 0.76 alters).
     * **Clique Conservers** ($n = 123$, 26.9\%): Enter college with and sustain an exceptionally large, dense Support Clique ($\approx 5.0\text{--}6.5$ alters) and Sympathy Shell ($\approx 5.2\text{--}6.4$ alters) alongside a pruned periphery ($\approx 2.7$ alters).
4. **Predicting Trajectory Group Membership (Endogenous Concomitant Models)**:
   - Incorporates baseline covariates directly into `flexmix` as endogenous concomitant variables via `FLXPmultinom(~ ...)` on complete cases ($N = 432$ egos, 2,451 observations):
     * Full Multivariable Model is highly significant vs. base ($\text{LRT } \chi^2 = 65.70, df = 30, p < 0.001$).
     * Race/Ethnicity is the sole significant demographic block ($\chi^2 = 22.13, df = 8, p = 0.005$, lowering AIC to 26,996.0).
     * Extraversion alone is highly significant ($\chi^2 = 18.61, df = 2, p < 0.001$, lowering AIC to 26,987.6).
     * Generalized Trust alone is highly significant ($\chi^2 = 15.19, df = 2, p < 0.001$).
     * Family SES ($\chi^2 = 6.59, df = 4, p = 0.159$), Gender ($\chi^2 = 1.00, df = 2, p = 0.605$), and Religious Affiliation on this Catholic campus ($\chi^2 = 3.11, df = 4, p = 0.539$) are completely null.
5. **Continuous Growth Modeling as Supplementary Robustness Check (Appendix)**:
   - Multilevel Poisson mixed-effects models (`glmer` in `lme4`) with random ego intercepts across all eight waves confirm that student networks contract by ~5.5% per wave ($\text{IRR} = 0.945, p < 0.001$), trust expands network volume ($\text{IRR} = 1.071, p = 0.006$), and extraverts experience significantly steeper winnowing over time ($\text{Time} \times \text{Extraversion IRR} = 0.994, p = 0.032$).

### 3. Directory Structure & Asset Taxonomy
```
project/
├── AGENTS.md                                # Project-specific guidelines and asset inventory
├── manuscript.tex                           # Canonical master LaTeX manuscript (bivariate multi-trajectory)
├── references.bib                           # Standalone BibTeX bibliography with software citations
├── manuscript.pdf                           # Compiled publication PDF (Overleaf rendered)
├── draft_manuscript.md                      # Active local markdown mirror
├── README.md                                # Full technical and empirical documentation
├── data/
│   ├── raw/                                 # Symlinked raw survey microdata (.gitignored)
│   └── processed/                           # Cleaned analytical datasets (.gitignored)
├── Plots/                                   # Publication-grade PNG figures (6.5 in wide, 300 DPI)
├── cache/                                   # Pre-compiled APA markdown tables
└── Scripts/                                 # Turnkey modular execution pipeline
    ├── 01_prepare_trajectory_data.R         # Ingestion, covariate cleaning, and 8-wave cohort creation
    ├── 02_fit_bivariate_trajectory_models.R # Bivariate LCGA, decomposition, Figs 1-3, Tabs 1-3
    ├── 04_appendix_multilevel_growth.R      # Appendix 8-wave Multilevel Poisson GLMM, Fig A1, Tab A1
    ├── sync_manuscript.py                   # In-place OpenXML table & figure injector
    └── sync_manuscript.R                    # Master Drive sync driver (Rscript Scripts/sync_manuscript.R)
```

### 4. Tables and Figures Inventory in Live Document & LaTeX (Strict Sequential Order)
- **Table 1**: Longitudinal Means and Standard Errors of Decomposed Relational Layers Across Eight Collegiate Waves ($N = 457$) (`cache/table1_bivariate_trajectory_means.md`, LaTeX `\label{tab:decomp}`)
- **Figure 1**: Decomposing Ego Network Evolution Across Eight Collegiate Waves: Total Degree, Strong Ties, and Weak Ties (`Plots/fig1_compound_strong_weak_trajectories.png`, LaTeX `\label{fig:decomp}`)
- **Table 2**: Bivariate Latent Class Growth Analysis (LCGA) Model Fit Statistics Across Candidate Poisson Mixture Models on Eight-Wave Panel ($K = 1 \dots 5$) (`cache/table2_bivariate_model_selection.md`, LaTeX `\label{tab:biv_lcga}`)
- **Figure 2**: Bivariate Multi-Trajectory Latent Class Growth Analysis Profiles Across Eight Collegiate Waves ($K = 3$) (`Plots/fig2_bivariate_lcga_trajectories.png`, LaTeX `\label{fig:biv_lcga}`)
- **Table 3**: Model Fit Comparison of Endogenous Concomitant Bivariate Mixture Specifications ($K = 3, N = 432$) (`cache/table3_bivariate_concomitant_model_comparison.md`, LaTeX `\label{tab:concomitant_comparison}`)
- **Figure 3**: Model-Implied Marginal Predicted Class Probabilities from Bivariate LCGA across Generalized Trust, Extraversion, and Race/Ethnicity (`Plots/fig3_bivariate_marginal_effects.png`, LaTeX `\label{fig:biv_marginal_effects}`)
- **Table A1**: Fixed Effects Estimates from Multilevel Poisson Growth Curve GLMM with Random Ego Intercepts Across Eight Waves (`cache/tableA1_multilevel_glmm_estimates.md`, LaTeX `\label{tab:glmm_appendix}`)
- **Figure A1**: Predicted Ego Degree Growth Trajectories by Personality Profiles Across Eight Waves from Multilevel Poisson GLMM (`Plots/figA1_multilevel_predicted_trajectories.png`, LaTeX `\label{fig:glmm_predicted}`)

### 5. Strict Protocol for Autonomous Execution & Synchronization
1. **The Live Google Doc Is King (ZERO OVERWRITING)**: Human authors edit and style text online. Never overwrite the live document by uploading a fresh Pandoc-compiled `.docx` file.
2. **Download First**: Always download `draft_live.docx` via `googledrive::drive_download(as_id(doc_id), path = "draft_live.docx", overwrite = TRUE)`.
3. **In-Place OpenXML Injection Only**: All asset updates must be applied directly to the downloaded `draft_live.docx` XML tree using DOM sibling search / relationship mapping (`Scripts/sync_manuscript.py`), updating only `<w:tbl>` and `<w:drawing>` containers.
4. **Intermediate File Cleanup**: Always purge scratch files (`draft_*.docx`, `draft_*.txt`, `*.tmp`, `replacements.json`) via `on.exit()` in R or `try...finally` in Python.
5. **Style Compliance**:
   - Double typographic quotes (“...”) for named classes.
   - Never use "democratic" for egalitarian / widely distributed / pervasive.
   - Use "men" and "women" (never "male" / "female" as nouns).
   - Use "Gender Identity" (or "Gender") rather than "Sex".
   - Avoid generic "robust" (reserve strictly for formal statistical terms like "cluster-robust standard errors").
   - Every visual figure must be accompanied by its dedicated analytical discussion paragraph.

### 6. LaTeX Manuscript Architecture & Mathematical Notation Standards
To ensure rigorous typesetting of mathematical formulations and empirical statistics, the primary manuscript is maintained in LaTeX (`manuscript.tex`). 

**CRITICAL INSTRUCTION -- Zero Local LaTeX Compilation (Never run pdflatex/xelatex):**
- **Do not render or compile the LaTeX manuscript locally.** The author renders, compiles, previews, and debugs the PDF directly online within Overleaf.
- Agents must NEVER spend time or tokens executing `pdflatex`, `latex`, `bibtex`, or local compilation commands. Focus exclusively on high-precision editing of the source text files (`manuscript.tex`, `references.bib`, R/Python scripts, and markdown cache files) and synchronizing changes directly to the git remotes (`origin` and `overleaf`).

1. **Compilation Protocol**:
   - Compiles online in Overleaf with standard pdfLaTeX + BibTeX.
   - Zero syntax errors, proper escaping of special characters, and valid cross-references.
2. **Required Packages & Layout**:
   - Document Class: `\documentclass[12pt]{article}` with `\usepackage[margin=1in]{geometry}` and `\onehalfspacing`.
   - Fonts & Microtypography: `\usepackage[T1]{fontenc}`, `\usepackage{lmodern}`, `\usepackage{microtype}`.
   - Mathematics: `\usepackage{amsmath,amssymb,amsfonts}`.
   - Tables: `\usepackage{booktabs}`, `\usepackage{tabularx}`, `\usepackage{pdflscape}`. Tables use `\small` sizing and calibrated `\tabcolsep` (e.g., 3.5pt--4.5pt) to prevent margin overflow.
   - Figures: `\usepackage{graphicx}`, `\usepackage{caption}`. Figures are imported from `Plots/` at native aspect ratios (`width=\linewidth` or `width=0.85\linewidth`).
   - Citations & Bibliography: `\usepackage[authoryear,round]{natbib}` with `\bibliographystyle{apalike}` linking to `references.bib` (40 complete entries with verified DOIs, including Mario Small's 2017 *Someone to Talk To*).
   - Hyperlinks: `\usepackage{hyperref}` with `\hypersetup{colorlinks=true, linkcolor=blue!80!black, citecolor=blue!80!black, urlcolor=blue!80!black}`. Section headings containing math mode must use `\texorpdfstring{$...$}{...}` to prevent PDF bookmark warnings.
3. **Rigorous Mathematical Notation Conventions**:
   - **Degree Variable & Trajectory Vectors:** Always index degree as $D_{it}$ for ego $i$ at wave $t$, with discrete bounds $D_{it} \in \{0, 1, \dots, 25\}$. Ego trajectory vectors are denoted $\mathbf{y}_i = (D_{i1}, D_{i2}, \dots, D_{iT})$. Wave-to-wave differences are $\Delta_t = D_{i,t+1} - D_{it}$.
   - **Trivariate Dunbar Poisson LCGA Formulation:** Mixture classes are denoted with uppercase $K$ ($K = 1 \dots 5$, optimal $K = 3$):
     \begin{equation}
     \log(\mathbb{E}[\mathbf{Y}_{it} \mid C_i = k]) = \begin{pmatrix} \beta_{0k}^{(C)} + \beta_{1k}^{(C)} \text{Time}_{it} + \beta_{2k}^{(C)} \text{Time}_{it}^2 \\ \beta_{0k}^{(S)} + \beta_{1k}^{(S)} \text{Time}_{it} + \beta_{2k}^{(S)} \text{Time}_{it}^2 \\ \beta_{0k}^{(P)} + \beta_{1k}^{(P)} \text{Time}_{it} + \beta_{2k}^{(P)} \text{Time}_{it}^2 \end{pmatrix}
     \end{equation}
     tracking Tier 1 Support Clique ($C$), Tier 2 Sympathy Shell ($S$), and Tier 3 Peripheral Perimeter ($P$).
   - **Multilevel Poisson GLMM Equation (Appendix):**
     \begin{equation}
     \log(\mathbb{E}[D_{it}]) = (\beta_0 + u_{0i}) + \beta_1 \text{Time}_{it} + \mathbf{X}_i \boldsymbol{\beta} + (\text{Time}_{it} \times \mathbf{Z}_i) \boldsymbol{\gamma}
     \end{equation}
     where $u_{0i} \sim \mathcal{N}(0, \sigma_u^2)$, $\text{Time}_{it} \in \{0, 1, \dots, 7\}$ denotes elapsed semester centered at Wave 1 baseline, $\mathbf{X}_i$ is the vector of baseline sociodemographics and personality main effects, $\mathbf{Z}_i$ represents focal personality traits (Extraversion and Neuroticism), and $\boldsymbol{\gamma}$ estimates differential winnowing slopes. Effect sizes are expressed as Incidence Rate Ratios ($\text{IRR} = \exp(\beta)$).
   - **Statistical Diagnostics & Fit Metrics:** Likelihood ratio tests are denoted $\chi^2$; information criteria are $\text{AIC}$, $\text{BIC}$, and $\Delta\text{BIC}$; standardized traits are $z$-scores ($\mu = 0, \sigma = 1$). Sample sizes are denoted with capital $N$ for overall samples ($N = 457$ analytical cohort, $N = 432$ complete-case models) and lowercase $n$ for subsample counts ($n = 205$, 44.9\%). All $p$-values are formatted as $p < 0.05$, $p < 0.001$, or $p = 0.006$.
4. **Recent Content & Structural Revisions (September 2026)**:
   - **Author Line:** Updated to Omar Lizardo and David Hachen.
   - **Trivariate Dunbar Cognitive Layer Architecture:** Grounded in Robin Dunbar's cognitive layering perspective \citep{dunbar1992neocortex, dunbar1998social, dunbar2018anatomy} and Mario Small's (2017) *Someone to Talk To* \citep{small2017someone}:
     1. *Collegiate Landscape & Dunbar Layer Decomposition:* Table 1 and Figure 1 establish the overall baseline trajectory across college, demonstrating that aggregate contraction (-24%) is confined to peripheral ties (-42.5%) while the Support Clique (4.55 $\rightarrow$ 3.43 alters, ~32-36% of personal network) and Sympathy Shell (3.76 $\rightarrow$ 3.72 alters) remain invariant, keeping the cumulative core at 66%--72% of total personal network volume.
     2. *Trivariate LCGA Mixture Modeling:* Table 2 and Figure 2 identify the three developmental trajectory archetypes across all 8 waves:
        - **Periphery Conservers** ($n = 129, 28.2\%$): Maintain moderate Support Cliques ($\approx 2.6\text{--}3.6$) and Sympathy Shells ($\approx 4.6\text{--}5.3$), but uniquely sustain an expansive, durable Periphery ($\approx 7.0\text{--}8.0$ alters).
        - **Peripheral Winnowers** ($n = 205, 44.9\%$): The modal pathway; preserves an intimate Support Clique ($\approx 3.0$ alters) and Sympathy Shell ($\approx 1.9$ alters) while their peripheral perimeter collapses by 83% (from 4.38 alters to 0.76 alters).
        - **Clique Conservers** ($n = 123, 26.9\%$): Enter college with and sustain an exceptionally dense core of intimate confidants ($\approx 5.0\text{--}6.5$ alters) and active companions ($\approx 5.0\text{--}6.5$ alters).
     3. *Predictive Model Fit Hierarchy (Table 3):* Model fit comparison evaluating 11 endogenous concomitant specifications ($K = 3, N = 432$), showing that Generalized Trust ($\chi^2 = 15.19, p < 0.001$) and Extraversion ($\chi^2 = 18.61, p < 0.001$) drive trajectory sorting, Race/Ethnicity is the sole significant demographic block ($\chi^2 = 22.13, p = 0.005$), while Family SES ($\chi^2 = 6.59, p = 0.159$), Gender Identity ($\chi^2 = 1.00, p = 0.605$), and Religious Affiliation ($\chi^2 = 3.11, p = 0.539$) are completely null.
     4. *Marginal Predicted Probabilities (Figure 3):* Displays model-implied class probabilities across Generalized Trust (quadrupling Clique Conservers), Extraversion (shifting introverts into Periphery Conserving and extraverts into Peripheral Winnowing), and Race/Ethnicity (showing minority sorting into Peripheral Winnowing).
     5. *Continuous GLMM Growth Modeling (Appendix):* Table A1 and Figure A1 provide continuous mixed-effects robustness checks confirming that extraverts winnow significantly faster over time ($\text{IRR} = 0.994, p = 0.032$).
   - **Tripartite Discussion Architecture (CUA Beauty Survey Standard):** Restructured the Discussion into three rigorous, publication-grade subsections: `\subsection{Summary of Key Results}`, `\subsection{Limitations and Suggestions for Future Work}` (addressing nomination bounding, multivariate mixture extensions $M \ge 3$, causal inference, feedback loops, granularity, and scope conditions in full paragraphs), and `\subsection{Implications: The Adaptive Architecture of Personal Communities}`.
   - **Direct Overleaf Git Remote Integration:** Fully synchronized with Overleaf project `https://git.overleaf.com/6a9db1ea255f30d10b63a391` via non-interactive token authentication (`~/.netrc`), maintaining identical parity across Overleaf and GitHub (`origin`).


---

## Project Architecture: Social Signatures in the NetHealth Study

### 1. Overview & Document Identification
- **Project Title:** Examining the Relational Content and Personal Determinants of Social Signatures Across Multiple Time Scales (Working Subtitle: *The Persistent Architecture of Relational Investment: Social Signatures in the NetHealth Study*)
- **Authors:** Omar Lizardo (Department of Sociology, University of California, Los Angeles) and David Hachen (Department of Sociology, University of Notre Dame)
- **Primary Publication Manuscript:** `manuscript.tex` (Synchronized directly with Overleaf and GitHub)
- **Bibliography:** `references.bib` (BibTeX database integrated via `natbib`)
- **Overleaf Project URL:** https://www.overleaf.com/project/6aa09a088d46d1a81ec5e22c
- **Overleaf Git Endpoint:** `https://git.overleaf.com/6aa09a088d46d1a81ec5e22c`
- **GitHub Repository:** https://github.com/olizardo/social-signatures-nethealth.git
- **Collaborative Google Doc URL:** https://docs.google.com/document/d/1CAVM2L25colX4ok5fdewBGo1-wWk-NzhX9esQ2nBU_o
- **Collaborative Google Doc ID:** `1CAVM2L25colX4ok5fdewBGo1-wWk-NzhX9esQ2nBU_o`
- **Data Sources:** NetHealth Study (University of Notre Dame, continuous smartphone sensing and longitudinal surveys, 2015–2017: https://sites.nd.edu/nethealth/).
- **Primary Analytic Sample:** $N = 491$ egos, 498,237 outgoing iOS voice calls spanning June 30, 2015 through July 2, 2017, linked to 8 sociocentric network survey waves ($N = 35,913$ alter nominations), complete alter-alter structural edge lists ($174,748$ ties), and longitudinal psychometric batteries ($N = 722$ egos).

### 2. Theoretical Lineage & Empirical Grounding (Literature Review Synthesis)
The project synthesizes and advances a decade of multidisciplinary research building on the social signatures framework coined by \citet{saramaeki2014persistence}:
1. **Foundational Origins & Cognitive Limits**:
   - Grounded in Dunbar's social brain hypothesis \citep{dunbar1992neocortex,dunbar1998social,dunbar2018anatomy}, neocortical volume limits, and time as a finite zero-sum budget \citep{miritello2013time,roberts2009exploring,sutcliffe2012relationships}.
   - \citet{saramaeki2014persistence}: Formalized social signatures as normalized interaction allocation vectors ($p_i(r)$), quantified dissimilarity via zero-padded Jensen-Shannon Divergence ($\JSD$), established intra-individual persistence over time ($d_{\mathrm{self}} < d_{\mathrm{ref}}$), and hypothesized the "slot-filling" mechanism under high alter turnover ($1 - J \approx 0.60$--$0.80$).
2. **Multichannel & Multimodal Frontiers**:
   - \citet{heydari2018multichannel}: Demonstrated persistence within voice call and SMS channels across half a million egos, identifying cross-channel resemblance despite minimal overlap in specific alters.
   - \citet{li2018social}: Evaluated online collaborative interaction dynamics in Facebook Wall posts and Wikipedia Talk pages, introducing "structural signatures" based on triadic embeddedness (shared neighbors) and showing that structural embeddedness mirrors behavioral communication profiles.
   - \citet{li2022evidence}: Analyzed the Copenhagen Networks Study ($N > 700$ students) across phone calls, text messages, and Bluetooth physical proximity; confirmed signature persistence across all modalities while demonstrating that mediated channels exhibit higher structural stability than face-to-face proximity.
   - \citet{liu2024uncovering}: Analyzed dual-channel mobile records, revealing a U-shaped trade-off between calling and texting as contact portfolios expand, alongside sharp communication vitality ceilings (10 contacts for calls, 100 for texts, 20 for hybrid).
3. **Theoretical Models of Resource Allocation & Universal Scaling**:
   - \citet{tamarit2022beyond}: Formulated a maximum entropy continuous resource allocation model reconciling discrete Dunbar circles ($5, 15, 50, 150$) with continuous signatures, identifying a universal scaling parameter $\eta \approx 6$ governing relational maintenance costs across calls, face-to-face, and Facebook.
   - \citet{iniguez2023universal}: Analyzed millions of individuals across calls, SMS, Twitter, and Wikipedia to reveal universal tie strength distributions driven by a microscopic competition between cumulative advantage (preferential tie reinforcement $\propto w^\beta$) and random choice (exploratory communication), parameterized by the alter-preferentiality exponent $\beta$.
   - \citet{adel2026preferentiality}: Extended this framework to offline wearable sensors and colocation data, demonstrating that passive colocation is a noisy proxy, whereas genuine face-to-face interaction follows universal scaling between tie preferentiality and bounded social bandwidth.
4. **Methodological Refinements (Disentangling Degree & Tie Strength)**:
   - \citet{heydari2024disentangling} & \citet{heydari2024dynamic}: Identified that standard $\JSD$ distance conflates network size (degree $k$) with internal allocation shape. Using degree-invariant metrics (the Gini coefficient distance $\Delta G$ and normalized alter-preferentiality distance $\Delta \beta$), they proved that egos maintain authentic, persistent tie strength heterogeneity independent of degree.
5. **Longitudinal Boundaries & Biometric Uniqueness**:
   - \citet{koltsova2021social}: Tracked social networking site messaging (VKontakte) over 18 months, showing that while short-term stability holds, signatures undergo statistically significant structural drift across longer developmental horizons.
   - \citet{jia2025multidimensional}: Demonstrated that the idiosyncratic shape and stability of multidimensional social signatures allow user re-identification and de-anonymization (up to 87\% accuracy) in low-sensitivity interaction data, establishing that communication profiles function as behavioral biometric fingerprints.

### 3. Five Open Frontiers Addressed and Resolved by NetHealth
1. **The "Content Black Box" & Functional Support Grounding**:
   - *The Gap*: Prior work relied exclusively on telecommunications metadata devoid of relational content, leaving open whether rank order reflects mere call frequency or qualitative functional specialization.
   - *NetHealth Resolution*: Links $N = 13,174$ call-ranked dyads to longitudinal sociocentric surveys recording multidimensional social support. Rank 1 is a specialized kinship-dominated support hub (69.7\% family, 85.4\% emotional support, 87.6\% advice, 64.1\% financial aid), transitioning to sympathy groups (Ranks 2--3: 62.2\% kin, 77.1\% emotional support), peer friendship dominance (Ranks 4--5: 56.9\% friends; Ranks $>20$: 87.8\% friends), while companionship remains sustained across all tiers ($>69\%$).
2. **Direct Empirical Verification of the Slot-Filling Mechanism**:
   - *The Gap*: Previous studies hypothesized slot-filling under alter turnover, but could not test whether stability survives complete replacement of the primary alter versus alter retention.
   - *NetHealth Resolution*: Across eight semesters with 80.1\% average alter turnover, we separate transitions where the top alter (Rank 1) is retained from those where Rank 1 is completely replaced. Even when Rank 1 is replaced, self-divergence increases only marginally (from 0.043 to 0.078), providing direct empirical proof of the slot-filling mechanism.
3. **Psychological Foundations of Signature Curvature**:
   - *The Gap*: Theoretical models hypothesize that internal cognitive traits drive alter-preferentiality, but commercial CDRs possess zero psychometrics.
   - *NetHealth Resolution*: Econometric regression of objective power-law decay exponents ($\alpha_i$) on longitudinal Big Five personality traits, CES-D depression, and UCLA loneliness scores shows that **Neuroticism strongly predicts steeper, hyper-concentrated signatures ($\beta = 0.098, t = 5.08, p = 6.96 \times 10^{-7}$)**, whereas Extraversion displays no statistically significant effect ($t = -1.01, p = 0.314$).
4. **Multi-Timescale Hierarchy and Parameter Burn-In Convergence**:
   - *The Gap*: Prior work examined arbitrary, isolated observation windows (1 month, 6 months, 9 months) without establishing how long an ego must be observed for parameter estimates to stabilize.
   - *NetHealth Resolution*: Evaluates signatures across eight temporal window schemes (from 1-week discrete windows to full academic years) and tracks cumulative parameter stability from 2 to 24 months, showing that estimated decay exponents stabilize asymptotically within 6 to 8 months.
5. **Sociocentric Structural Embedding and Multilevel Panel Dynamics**:
   - *The Gap*: Egocentric communication has traditionally been modeled in isolation from the alters' own structural ties.
   - *NetHealth Resolution*: Embeds communication logs within the complete sociocentric graph ($174,748$ alter-alter structural ties), estimating multilevel linear mixed-effects models that show alter turnover ($\beta = 0.351, p < 10^{-15}$) and volume shifts ($\beta = 0.0014, p < 10^{-10}$) drive temporal stability, while local network clustering does not alter stability net of turnover.

### 4. Canonical NetHealth Study Citations (Representing All Primary PIs)
Any manuscript revision, grant submission, or documentation referencing the NetHealth Study must cite the foundational project publications representing the primary PIs (David Hachen, Omar Lizardo, Christian Poellabauer, Aaron Striegel, Tijana Milenković, and Nitesh Chawla):
- **Liu, S., Hachen, D., Lizardo, O., Poellabauer, C., Striegel, A., & Milenković, T. (2018)**. Network analysis of the NetHealth data: Exploring co-evolution of individuals' social network positions and physical activities. *Applied Network Science*, 3(1), 45. https://doi.org/10.1007/s41109-018-0103-2
- **Purta, R., Mattingly, S., Song, L., Lizardo, O., Hachen, D., Poellabauer, C., & Striegel, A. (2016)**. Experiences measuring sleep and physical activity patterns across a large college cohort with Fitbits. *Proceedings of the 2016 ACM International Symposium on Wearable Computers (ISWC '16)*, 28–35. https://doi.org/10.1145/2971763.2971767
- **Faust, L., Purta, R., Hachen, D., Striegel, A., Poellabauer, C., Lizardo, O., & Chawla, N. V. (2017)**. Exploring compliance: Observations from a large scale Fitbit study. *Proceedings of the 2nd International Workshop on Social Sensing (SocialSens '17)*, 55–60. https://doi.org/10.1145/3055601.3055608
- **Liu, S., Vahedian, F., Hachen, D., Lizardo, O., Poellabauer, C., Striegel, A., & Milenković, T. (2021)**. Heterogeneous network approach to predict individuals' mental health. *ACM Transactions on Knowledge Discovery from Data (TKDD)*, 15(2), 1–26. https://doi.org/10.1145/3429446

### 5. Tables and Figures Inventory in Publication Manuscript
- **Table 1**: NetHealth Cohort Summary Across Temporal Window Definitions (`cache/table1_cohort_summary.md`)
- **Figure 1**: Empirical Social Signatures across Temporal Window Resolutions (Ranks 1–15; 2-row bottom legend layout preventing clipping) (`Plots/fig01_mean_signatures_by_window.png`)
- **Table 2**: Statistical Tests of Social Signature Persistence Across Window Schemes (`cache/table2_stability_tests.md`)
- **Figure 2**: Persistence of Social Signatures: Self vs. Reference Divergence across Temporal Resolutions (`Plots/fig02_self_vs_ref_divergence.png`)
- **Table 3**: Parametric Model Evaluation Across Window Resolutions (Power-Law vs. Exponential) (`cache/table3_parametric_models.md`)
- **Figure 3**: Power-Law vs Exponential Model Fits and Exponent Distribution (`Plots/fig03_power_law_vs_exponential.png`)
- **Figure 4**: Parameter Burn-In Convergence Over 24 Months (`Plots/fig04_parameter_burnin.png`)
- **Table 4**: Relational Composition and Support Functions Across Signature Rank Tiers ($N = 13,174$ Dyads) (`cache/table4_support_tiers.md`)
- **Figure 5**: Functional Support Dimensions Across Signature Rank Tiers (High-contrast accessible color palette with zero light yellow; multi-row bottom legend) (`Plots/fig05_rank_by_support_tiers.png`)
- **Figure 6**: The 'Slot-Filling' Dynamic: Alter Turnover vs. Signature Divergence ($r = 0.456, p < 10^{-15}$) (`Plots/fig06_turnover_vs_stability.png`)
- **Table 5**: Multilevel Linear Mixed-Effects Models Predicting Signature Self-Divergence ($\JSD$) (`cache/table5_multilevel_models.md`)
- **Table 6**: Personality Determinants of Social Signature Power-Law Alpha ($\alpha_i$) (`cache/table6_personality_models.md`)
- **Figure 7**: Neuroticism as a Driver of Egocentric Relational Concentration (Two-panel visualization centering on the statistically significant Neuroticism effect; Panel A: scatterplot and regression fit of $\alpha_i$ on Neuroticism; Panel B: mean signatures across ranks 1--10 stratified by Neuroticism tertiles with a multi-row legend, showing high-Neuroticism individuals allocate 31.7% to Rank 1 vs. 25.3% for low-Neuroticism peers; Extraversion removed) (`Plots/fig07_personality_signature_effects.png`)

### 6. Directory Structure & Asset Taxonomy
```
social-signatures-nethealth/
├── manuscript.tex                         # Primary LaTeX manuscript (synchronized with Overleaf)
├── references.bib                         # BibTeX reference library (36 verified entries)
├── manuscript.pdf                         # Compiled 23-page publication PDF
├── draft_manuscript.md                    # Local Markdown mirror synchronized with Google Drive
├── run_all.R                              # Master analytical reproduction pipeline
├── data/
│   ├── raw/                               # Call logs, calendars, and survey microdata (.gitignored raw files)
│   └── processed/                         # Harmonized analytical datasets
├── Plots/                                 # Publication-grade PNG figures (6.5 in wide, 300 DPI)
├── cache/                                 # Pre-compiled APA markdown tables
├── templates/                             # Document reference styles and fontTable XML
└── Scripts/                               # Turnkey modular execution pipeline
    ├── 01_prepare_call_windows.R          # Temporal window binning & cohort filtering
    ├── 02_compute_signatures_and_jsd.R    # Social signature computation, fast JSD, Wilcoxon tests
    ├── 03_fit_parametric_models.R         # Power-law vs exponential models & burn-in analysis
    ├── 04_egonet_topology_and_turnover.R  # Egonet metrics & alter turnover dynamics
    ├── 05_survey_linkage_psychometrics.R  # Support dimensions & Big Five psychometrics
    ├── 06_expansion_statistical_models.R  # Dyadic support tiers & multilevel LMM regressions
    ├── 07_generate_figures_and_tables.R   # Publication-quality figure and table rendering
    ├── generate_md_tables.R               # Pre-compiles APA markdown tables into cache/
    ├── sync_manuscript.py                 # OpenXML table & figure DOM injector
    ├── format_manuscript.py               # Typography, margins, indents, and style normalizer
    └── sync_manuscript.R                  # Master Google Drive synchronization driver
```

### 7. Core Operational Lessons & Style Standards for Social Signatures
1. **Zero Local LaTeX Compilation When Connected to Overleaf (CRITICAL)**:
   - Never run `pdflatex`, `xelatex`, `bibtex`, or `latexmk` locally when connected to Overleaf. Overleaf compiles and previews PDFs online. Focus exclusively on editing source files (`manuscript.tex`, `references.bib`, `Plots/`) and pushing commits directly (`git push overleaf main`).
2. **Abstract Word Count Strict Enforcement**:
   - Always verify that the abstract is strictly $\le 250$ words (currently 229 words).
3. **Discussion Section CUA Tripartite Standard (MANDATORY)**:
   - Structure the concluding Discussion into three distinct subsections:
     1. `Summary of Key Results` (Bird's-Eye Synthesis, Systematic Walking of Findings, Concluding Transition).
     2. `Limitations and Future Work` (Full academic prose covering 5 analytical dimensions: operationalization/measurement, threats to causal inference, directionality/selection, temporal granularity/attrition, institutional scope conditions).
     3. `Implications: Cognitive Templates and Capital` (Theoretical stakes, resolving zero-sum paradoxes, broader sociological synthesis).
4. **Complete Elimination of Bullet Points and Numbered Lists**:
   - Compose the entire manuscript in full, flowing academic prose paragraphs. Zero `\begin{enumerate}` or `\begin{itemize}` blocks in the body text.
5. **Italics for In-Text Emphasis**:
   - Use italics (`\textit{...}` or `\emph{...}`) instead of boldface for emphasizing terms, concepts, and key statistical statements in running text. Reserve boldface strictly for section titles and table column headers.
6. **Avoidance of "Architecture" When Specific Terms Apply**:
   - Reserve the term "architecture" for overarching metaphorical titles; in running text, use precise terms: "binning schemes", "temporal definitions", "qualitative organization", "relational geometry", and "communication networks".

### 5. Exhaustive Replication Audit Against Social Signatures.pptx
Every slide, figure, and empirical study from Matthew Chandler's 26-slide presentation (May 7, 2019) has been comprehensively replicated and extended:

| Slide(s) in PPTX | Topic / Analysis in Slide Deck | Implementation in This Project | Location in Paper & Pipeline |
|:---|:---|:---|:---|
| **Slides 2 & 12** | Social signature formulation & generalized n-ary JSD (Lin 1991) | Matrix-accelerated zero-padded pairwise JSD and multi-window generalized entropy | `scripts/02_compute_signatures_and_jsd.R`, Section 2.3 |
| **Slide 6 & 13** | 7 Window binning definitions (Academic Years, Semesters, Quarters, Months, 3-Wk Moving, 2-Wk Discrete, 1-Wk Discrete) | Complete calendar and weekly binning algorithms with >=2 alters & >10 calls criteria | `scripts/01_prepare_call_windows.R`, Table 1 |
| **Slide 7** | Alter variation & Jaccard turnover by year (n = 411) | Year-over-year alter degree correlations and Jaccard turnover distributions | `scripts/04_egonet_topology_and_turnover.R`, Section 4.2 |
| **Slide 8** | Scatterplot matrix of alter counts across 8 quarters (n = 229) | Quarter-by-quarter alter degree correlation matrix | `scripts/01_...` & `scripts/04_...` |
| **Slides 9–11** | Sample individual social signatures across Semesters, Quarters, and 3-Week Moving | Individual ego curves and aggregate mean signature profiles | `scripts/02_...`, Figure 1 |
| **Slide 14** | Pairwise Mean Self-Divergence vs. Combined Generalized JSD across narrow windows (n = 70) | Comparative evaluation of pairwise consecutive JSD vs. multi-window generalized JSD | `scripts/02_...`, Section 2.3 |
| **Slide 15** | Reference divergence formulation (all-pairs vs. by-ego averaged) | Both reference divergence calculations implemented across all window resolutions | `scripts/02_...`, Table 2 |
| **Slide 16 & 17** | Self vs. Reference divergence and turnover scatterplots; correlation distributions | Dyadic Jaccard turnover vs. JSD regressions and stability distributions | `scripts/04_...`, Figure 6 |
| **Slide 18** | Self vs. Reference divergence distributions (No Limit vs. Top 20) with Wilcoxon / Mann-Whitney tests | Formal non-parametric location tests confirming persistence (p < 10^-12) | `scripts/02_...`, Table 2, Figure 2 |
| **Slide 19** | Log-log social signatures by semester showing linear power-law decay | Individual and aggregate log-log decay curves | `scripts/03_fit_parametric_models.R`, Figure 3 |
| **Slide 20** | Power-law models f(x) = beta * x^-alpha (p < 0.00001 for all models) | NLS and log-linear estimation across all ego-windows (97.7% significant at p < 0.0001) | `scripts/03_...`, Table 3 |
| **Slide 21** | Distributions of power-law intercept beta and slope -alpha | Parameter distribution density modeling across timescales | `scripts/03_...`, Figure 3 |
| **Slide 22** | Individual differences in parameters by year (Delta beta, Delta alpha) | Year-over-year individual parameter stability tracking | `scripts/03_...`, Section 3.3 |
| **Slide 23** | Longitudinal parameter trajectories across Semesters, Quarters, Months | Longitudinal panel modeling of ego-level decay exponents | `scripts/03_...` & `scripts/06_...` |
| **Slide 24** | Power-law vs. Exponential models (f(x) = beta * exp(-alpha * x)) | Direct goodness-of-fit comparison (AIC, BIC, R^2; power-law preferred in 97.0%) | `scripts/03_...`, Table 3, Figure 3 |
| **Slide 25** | Parameter burn-in: How long before an ego's model is stable? | Cumulative 2-to-24 month parameter convergence tracking (stabilizes at 6-8 months) | `scripts/03_...`, Figure 4 |
| **Slide 26** | Covariates to tease out (degree, clustering, turnover, activity, modularity) | Multilevel mixed-effects models & support tier linkages | `scripts/05_...` & `scripts/06_...`, Tables 4–6, Figures 5–7 |

### 6. Technical Lessons & Best Practices
5. **LaTeX Migration & Mathematical Consistency Standards (Math-Heavy Papers)**:
   - **Primary Typesetting in LaTeX**: For papers characterized by intensive mathematical formulations (information-theoretic metrics, power-law vs. exponential decay equations, Shannon entropy, dyadic turnover formulas, and multi-level mixed-effects models), LaTeX (`manuscript.tex` + `references.bib`) serves as the canonical authoring and submission medium, bypassing word-processor equation limitations.
   - **Consistent Notation Hierarchy**: Maintain uniform mathematical symbols across all prose, equations, tables, and figure captions:
     - Ego $i$, window $w$, alter rank $r$, alter identity $j$, time $t$.
     - Interaction weights: $w_{i,j,w}$, total volume $W_{i,w} = \sum_{j=1}^{k_{i,w}} w_{i,j,w}$.
     - Social signature proportion: $p_{i,w}(r) = w_{i,(r),w} / W_{i,w}$ where $\sum_{r=1}^{k_{i,w}} p_{i,w}(r) = 1$.
     - Entropy and divergence: $H(P) = -\sum p_r \log_2(p_r)$, $\JSD(P \parallel Q) = H(M) - 0.5[H(P) + H(Q)]$.
     - Divergence metrics: self-divergence $\dself(i)$, reference divergence $\dref(i)$.
     - Turnover: $\Turnover_{i,w} = 1 - |A_{i,w} \cap A_{i,w+1}| / |A_{i,w} \cup A_{i,w+1}|$.
     - Panel models: $\JSD_{it} = \beta_0 + \beta_1 \Turnover_{it} + \beta_2 \Delta \mathrm{Activity}_{it} + \mathbf{X}_{it}\boldsymbol{\gamma} + u_i + \epsilon_{it}$.
   - **Table Design**: Use `booktabs` (`\toprule`, `\midrule`, `\bottomrule`) with `\resizebox{\textwidth}{!}{...}` on wide multi-column tables to ensure seamless margin compliance.
   - **Zero-Warning Compilation Pipeline**: Always compile via `pdflatex -interaction=nonstopmode manuscript.tex && bibtex manuscript && pdflatex -interaction=nonstopmode manuscript.tex && pdflatex -interaction=nonstopmode manuscript.tex`.


1. **Translating Math to Document Text (Bypassing Cambria Math & oMath Issues)**:
   - When authors request standard manuscript typography (e.g., Alegreya Sans 11pt) across the entire paper, never use LaTeX math delimiters in markdown, as Pandoc converts them into Word `<m:oMath>` blocks which default to Cambria Math and fail Google Docs import conversion.
   - Write equations in clean Unicode text with semantic italics and superscripts/subscripts.
   - `format_manuscript.py` must run `convert_omath_to_runs()` to convert any lingering `<m:oMath>` into standard `<w:r>` text runs styled in the document font (`Alegreya Sans 11pt`).
2. **Preventing Generic Word Style Resets in Google Docs**:
   - Pandoc compiles markdown paragraphs with styles `BodyText` and `FirstParagraph`. If these style IDs are missing from `styles.xml`, Google Docs falls back to its generic default styles (Calibri 11pt, 1.15 line spacing, 0 indent).
   - In `format_manuscript.py`, always strip `BodyText` and `FirstParagraph` from paragraphs so they default to `Normal`, ensure `docDefaults` has `rFonts w:ascii="Alegreya Sans"` without theme overrides (`asciiTheme`), and inject proven `styles.xml` and `fontTable.xml` templates.
3. **High-Performance JSD Vectorization in R**:
   - Calculating pairwise JSD across hundreds of egos and windows iteratively hangs in R.
   - Precompute Shannon entropy H(P) once per signature vector. Pairwise JSD then only requires calculating H(M) for the midpoint M = 0.5(P + Q).
   - Pad all signatures in a window into a matrix and compute midpoint entropy vectorized across columns, achieving a 400x speedup.
4. **Fast GZ Ingestion without Extra Packages**:
   - To bypass `fread()`'s prompt for `R.utils` when reading `.csv.gz`, pass the decompressed stream command: `fread(cmd = sprintf("zcat %s", gz_file), select = ...)`.


## Project Architecture: Hysteresis in Recent Sociological Research (Current Sociology)

### 1. Overview & Collaborators
- **Manuscript Title:** *Hysteresis in Recent Sociological Research*
- **Authors:** Aya Konishi and Omar Lizardo (University of California, Los Angeles)
- **Target Journal:** *Current Sociology* (Major Revision / R1, Manuscript ID: `CS-24-0216`)
- **Editors:** Qiushi Feng, Daniel P.S. Goh, Kelvin E.Y. Low, and Joonmo Son
- **Target Word Limit:** 6,000 – 8,000 words (*strictly inclusive of title, abstract, body text, notes, references, and Table 1*). Current length: **~6,080 words**.

### 2. Google Drive Document Identification & Live Synchronization
The active working draft is maintained as a set of synchronized Google Docs:
- **Clean Main Manuscript:**
  - Google Doc ID: `1CWOTzN7RiS9AFNc8TRh_CUALPc_Y0BWoBttXOGzYw4Q`
  - URL: `https://docs.google.com/document/d/1CWOTzN7RiS9AFNc8TRh_CUALPc_Y0BWoBttXOGzYw4Q/edit`
  - Local Path: `CSR-R1/CSR-main-R1.docx`
- **Marked Manuscript (Blue Text Additions):**
  - Google Doc ID: `1ht0RqS1IdUEkWklpsjjCeEmMG9XGgR5YmKPeereNerE`
  - URL: `https://docs.google.com/document/d/1ht0RqS1IdUEkWklpsjjCeEmMG9XGgR5YmKPeereNerE/edit`
  - Local Path: `CSR-R1/CSR-main-R1-marked.docx`
- **Response to Reviewers Memo:**
  - Google Doc ID: `1vUc0HMUu-a7EYkgWIOVfZKWdMmy4XopKtI6XBZ_kYAg`
  - URL: `https://docs.google.com/document/d/1vUc0HMUu-a7EYkgWIOVfZKWdMmy4XopKtI6XBZ_kYAg/edit`
  - Local Path: `CSR-R1/RESPONSE-TO-REVIEWERS.docx` and `CSR-R1/RESPONSE-TO-REVIEWERS.md`
- **Literature Table / Sources:**
  - Google Doc ID: `1XUi0f5JJPsAYKqB6gb1tJFw0eXFMz9ItTkTkQCLda9o`
  - Local Path: `CSR-R1/Sources Table.docx` (integrated as Table 1 at the end of the manuscript)

### 3. Turnkey Synchronization Commands
Non-interactive authentication is handled seamlessly via `googledrive::drive_auth(email = "omarlizardo@gmail.com", cache = "~/.cache/gargle")`:
```bash
# Manuscript synchronization (CSR-main-R1.docx)
Rscript Scripts/sync_manuscript.R --pull   # Pull live edits from Google Drive (creates safety backup)
Rscript Scripts/sync_manuscript.R --push   # Push local docx back up to Google Drive
Rscript Scripts/sync_manuscript.R --status # Check modification timestamps

# Response to Reviewers memo synchronization
Rscript Scripts/sync_response_memo.R --pull   # Pull live edits from Google Drive
Rscript Scripts/sync_response_memo.R --push   # Push local docx back up to Google Drive
Rscript Scripts/sync_response_memo.R --status # Check modification timestamps
```

### 4. Theoretical & Structural Architecture of the Revision
1. **Bourdieusian Triad Grounding (Section 2):**
   - *Habitus:* Temporal lag, Don Quixote effect, *habitus clivé*.
   - *Field:* Doxa disruption, rules of the game, manufactured hysteresis by state actors.
   - *Capital:* Asymmetrical vulnerability, asset conversion ruptures.
   - *Dynamics:* Psychic suffering, defensive persistence, creative adaptation, agentic dissidence.
2. **Empirical Literature Corpus (Section 3.1 & Table 1):**
   - 34 empirical studies (2001–2025) systematically reviewed and categorized.
   - Epistemic reflexivity: Author (XXXXa) utilized as a pragmatic bibliometric seed, supplemented by backward/forward snowballing and *Web of Science* / *Sociological Abstracts* cross-checks. Bounded strictly to the Bourdieusian tradition.
3. **Four Comparative Thematic Arenas (Sections 3.2–3.5):**
   - *Educational Fields:* Transnational identity improvisation vs. rural-to-urban bifurcated adjustment / compartmentalized fit; institutional corporatization and academic labor audit precarity.
   - *Labor Market & Life-Course:* Credential devaluation and defensive symbolic capital; chronological career doxa, retirement transitions (*ikigai* vs. American dream), *habitus clivé*, parenthood food practices.
   - *Institutional, Bureaucratic, & Movement Fields:* Professional autonomy under managed care siege; state bureaucracy and clientelist prevention of hysteresis; hysteresis as an affective spark for political and ecological mobilization.
   - *Geographical Space:* Embodied vigilant vs. aspirational street life; material consumer cultural bridges; dominated cosmopolitanism.
4. **Critical Appraisal & Methodological Tensions (Section 4):**
   - *Conceptual Stretching:* Rigorous boundary between *objective field ruptures* (macro-historical transformations where rules dissolve around stationary agents) and *biographical trajectory shifts* (individual mobility into pre-existing stable fields).
   - *Methodological Interview Paradox:* Interrogating why a concept defined as a pre-reflexive practical consciousness lag is studied 90%+ via retrospective qualitative interviews; calling for in situ ethnography, digital trace tracking, and cognitive experiments.
5. **Standalone Conclusion (Section 5):**
   - 3-paragraph synthesis of empirical maturation, critical theoretical lessons, and the imperative to expand research into the Global South.
6. **Strict Anonymization Standards:**
   - In-text citations censored as `Author (XXXXa)`, `(Author, XXXXb)`, `(Author et al., XXXXc)`.
   - References censored as `Author. (XXXXa).`, `Author. (XXXXb).`, `Author et al. (XXXXc).`
   - Identifying details restricted strictly to the standalone `CSR-title-page.docx`.
