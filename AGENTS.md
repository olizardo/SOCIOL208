# AGENTS.md — SOCIOL 208B Course & Automation Architecture

This guide documents the architecture, course sequence, lesson plan, credentials, and automation workflows for **SOCIOL 208B: Social Network Methods** on **UCLA Bruin Learn (Instructure Canvas LMS)**.

---

## 1. Course Context & Metadata

* **Institution:** University of California, Los Angeles (UCLA)
* **Department:** Sociology (SOCIOL)
* **Course:** `SOCIOL 208B - Social Network Methods` (Graduate Reading Seminar)
* **Term:** Fall 2026
* **Lecture Section:** Lec 1
* **Class ID:** `747049100`
* **Canvas Course ID:** `239524`
* **LMS Platform:** Bruin Learn (`https://bruinlearn.ucla.edu/courses/239524`)
* **Instructor:** Prof. Omar Lizardo (`olizardo@soc.ucla.edu`)
* **Meeting Schedule:** Mondays, 3:00 p.m. – 5:50 p.m.
* **Location:** Public Affairs Building Room 2325
* **Units:** 4.0
* **Course Readings Repository:** [Bruin Learn Course Files](https://bruinlearn.ucla.edu/courses/239524/files)

---

## 2. Authentication & Credentials

Canvas REST API credentials are loaded non-interactively from `~/.Renviron` or system environment variables:

```bash
# In ~/.Renviron
CANVAS_URL="https://bruinlearn.ucla.edu"
CANVAS_API_TOKEN="<token>"
```

* **Header Requirements:**
  ```python
  headers = {
      "Authorization": f"Bearer {token}",
      "Content-Type": "application/json",
      "Accept": "application/json"
  }
  ```
* **Security Rule:** Never hardcode, log, or commit the API token to terminal outputs or version control.
* **🚨 Mandatory Automatic Live Deployment:** Whenever the instructor/user requests additions, removals, updates, or edits to course content, readings, syllabus, or schedule, these changes **must be deployed automatically and immediately** to the live UCLA Bruin Learn (Canvas LMS) course site by executing `python3 deploy_all_to_canvas.py`, in addition to updating local repository files (`.qmd`, `.html`, `.json`). Do not wait for a separate prompt or confirmation—all content change requests implicitly carry permission and mandate for immediate deployment.

---

## 3. Course Requirements & Pedagogical Workflow

### A. Weekly Analytic Memos (50% of Grade)
* **Frequency:** Due every week starting in **Week 1** through **Week 10** (10 memos total).
* **Deadline:** **Sundays at 5:00 p.m. PT** (the day before class meeting) via Bruin Learn Assignments.
* **Length:** 500 to 1,000 words.
* **Reading Requirement:** Each memo must select **at least four readings total** from that week's syllabus, ensuring you include **at least one reading from Part 1 and at least one reading from Part 2**, putting them in substantive conversation with one another.

### B. Seminar Discussion Leaders (Starting in Week 2)
* **Role:** Students take turns serving as co-discussion leaders starting in Week 2.
* **Question Requirement:** Discussion leaders collaborate to formulate and submit **at least two discussion questions on each assigned reading** for their scheduled week.
* **Deadline:** Posted to the **Discussion Leaders Summary and Questions** forum on Bruin Learn by **Sunday at 5:00 p.m. PT** prior to class.
* **Sign-Up:** Students submit their preferences during Week 1 via the **Discussion Leader Week Preference** survey (Quiz ID: `1131287`).
* **Finalized Two-Person Schedule (Weeks 2–10):** All 7 enrolled students responded to the preference survey; the schedule below was built to respect every student's stated availability while spreading the 18 total slots as evenly as possible (4 students lead 3 weeks, 3 students lead 2 weeks). Posted live to the [Discussion Leaders Summary and Questions forum](https://bruinlearn.ucla.edu/courses/239524/discussion_topics/1466035) and to the front page (`front-page.html`) and each weekly overview page (`WEEKS_DATA` `leaders` field in `deploy_all_to_canvas.py`) on 2026-09-28.

  | Week | Leader 1 | Leader 2 |
  |---|---|---|
  | Week 2 | George Garcia | Anoushka Patel |
  | Week 3 | Alvin Chu | Noor Amanullah |
  | Week 4 | Anoushka Patel | Sarah Stigers |
  | Week 5 | Alvin Chu | Sarah Stigers |
  | Week 6 | Travis Ashby | Noor Amanullah |
  | Week 7 | Alvin Chu | Anoushka Patel |
  | Week 8 | Travis Ashby | Alvin Chu |
  | Week 9 | Noor Amanullah | Travis Ashby |
  | Week 10 | Travis Ashby | Sarah Stigers |

  Mayra Varillas Cilia withdrew/was excluded from the discussion-leader rotation (2026-09-28). George Garcia subsequently dropped the class (2026-10-05); his remaining slots (Weeks 5 and 10) were redistributed and the Weeks 3–10 rotation rebalanced per instructor instructions. Final rotation (Weeks 3–10, 16 slots among 5 students): Alvin Chu (Weeks 3, 5, 7, 8 — 4 weeks), Travis Ashby (Weeks 6, 8, 9, 10 — 4 weeks), Noor Amanullah (Weeks 3, 6, 9 — 3 weeks), Sarah Stigers (Weeks 4, 5, 10 — 3 weeks), Anoushka Patel (Weeks 4, 7 — 2 weeks). Week 2 (George Garcia & Anoushka Patel) was already held and is kept for the record.
  * **To update this schedule going forward:** edit the table above, edit the `'leaders'` key in each week's dict in `WEEKS_DATA` within `deploy_all_to_canvas.py`, edit the matching "🗳️ Discussion Leaders" `<div>` inside each week's card in `front-page.html`, then run `python3 deploy_all_to_canvas.py` to push both to Canvas. Note: since 2026-10-12 the leader names in each `front-page.html` week card are wrapped in a hyperlink to the Discussion Leaders forum (`<a href="/courses/239524/discussion_topics/1466035" style="color: #7c3aed; text-decoration: underline;">...</a>`) — when editing names, keep the anchor wrapper intact.
  * **Front-page forum links (deployed to Canvas 2026-10-12):** the Quick Access navigation bar now carries a dedicated "🗳️ Discussion Leaders Board" card linking directly to the Discussion Leaders Summary and Questions forum (`/discussion_topics/1466035`); the old Week 1 preference-survey card was relabeled "🗳️ Leader Sign-Up Survey / Week 1 preference survey"; and all nine weekly "🗳️ Discussion Leaders: [names]" lines inside the module cards (Weeks 2–10) are hyperlinks to the same forum, so students land on the posted questions from any week card. Canvas normalizes these relative `/courses/...` URLs to absolute ones and appends `data-api-endpoint`/`data-api-returntype` attributes on deploy — expected behavior, not duplication.
  * **Leader info now appears on ALL major pages (synced 2026-10-05):** the weekly overview pages (Weeks 2–10 "Discussion Leaders This Week" line, generated from `WEEKS_DATA`), the front-page module cards, and the dedicated Discussion Leader Week Assignments page were already in sync. The **Reading Schedule page** (`bruinlearn_course_materials/pages/sociol-208b-reading-schedule-fall-2026.html`) now carries a per-week "🗳️ Leaders:" chip under each memo-due box (Weeks 2–10) plus an alert-box link to the leader-assignments page, and the **Syllabus page** (`bruinlearn_course_materials/pages/sociol-208b-syllabus-fall-2026.html`) carries a "🗳️ Finalized Discussion Leader Rotation" table inside the Seminar Discussion Leaders card. When changing the rotation, update the chips and the syllabus table too.
  * **⚠️ `deploy_all_to_canvas.py` does NOT push the native syllabus** (`/assignments/syllabus` via `PUT /courses/:id` with `syllabus_body`) — it only deploys the Syllabus *page*. The native syllabus must be updated with a separate one-off API PUT; on 2026-10-05 it was fully synced to the Syllabus *page* (its body was overwritten with the live page body fetched via `GET /pages/sociol-208b-syllabus-fall-2026`, which at that point already carried the rotation table). The two bodies are now byte-identical. Whenever the syllabus page is edited, re-run this one-off sync so the native syllabus stays in parity — the previous minor prose drift (e.g., "two or more readings" vs. "four or more"; "Google Drive" vs. "Bruin Learn Course Files") was eliminated by this overwrite.

### C. Final Course Paper (50% of Grade)
* **Options:**
  1. Draft of an empirical research paper (3,500–9,000 words)
  2. Conceptual / theoretical paper (5,000–10,000 words)
  3. Extended critical literature review (10–15 pages)
  4. Research grant / dissertation proposal (10 pages)
  5. Data analysis exercise / research note (2,500–5,000 words)
* **Milestone:** **Final Project Extended Abstract** (1 page, single-spaced) due **Friday of Week 6 (Nov. 6, 2026) at 11:59 p.m. PT**.
* **Submission Box:** Final paper submitted at the end of the quarter via Bruin Learn.

---

## 4. 10-Week Course Sequence & Lesson Plan

*Weekly reading lists below are synced to the live Bruin Learn site (source of truth: `WEEKS_DATA` in `deploy_all_to_canvas.py`) as of 2026-10-05.*

### Module 1: Foundations & The Relational Turn (Weeks 1–2)

#### Week 1: Networks As Theory and Perspective (Mon., Sep. 28)
* **Part 1: Structural Foundations & Network Theory**
  * Wellman, B. (1988). Structural Analysis: From Method and Metaphor To Theory and Substance. In B. Wellman & S. D. Berkowitz (Eds.), *Social Structures: A Network Approach* (pp. 19–61). Cambridge University Press.
  * Martin, J. L. (2009). *Social Structures*. Princeton University Press (selections).
  * Borgatti, S. P., & Halgin, D. S. (2011). On Network Theory. *Organization Science*, 22(5), 1168–1181.
  * Rivera, M. T., Soderstrom, S. B., & Uzzi, B. (2010). Dynamics of Dyads in Social Networks: Assortative, Relational, and Proximity Mechanisms. *Annual Review of Sociology*, 36(1), 91–115.
* **Part 2: Relational Sociology, Culture, and Agency**
  * Erikson, E. (2013). Formalist and Relationalist Theory In Social Network Analysis. *Sociological Theory*, 31(3), 219–242.
  * Mische, A. (2011). Relational Sociology, Culture, and Agency. In J. Scott & P. J. Carrington (Eds.), *The Sage Handbook of Social Network Analysis* (pp. 80–97). Sage.
  * Fuhse, J. (2020). Theories of Social Networks. In R. Light & J. Moody (Eds.), *The Oxford Handbook of Social Networks* (pp. 34–49). Oxford University Press.
* **Deliverable:** Analytic Memo 1 (due Sun., Sep. 27 at 5:00 pm PT).

#### Week 2: Brokerage and Intermediation (Mon., Oct. 5)
* **Part 1: Weak Ties, Brokerage Roles, and Mediation**
  * Granovetter, M. S. (1973). The Strength of Weak Ties. *American Journal of Sociology*, 78(3), 1360–1380.
  * *(Optional)* Rejection letter from *American Sociological Review* of the first (1969) version of the paper (`https://scatter.wordpress.com/wp-content/uploads/2014/10/granovetter-rejection.pdf`).
  * *(Optional)* Granovetter, M. S. (1969). Alienation Reconsidered: The Strength of Weak Ties. Reprinted in *Connections* 5(2): 4–16 (`https://assets.noviams.com/novi-file-uploads/insna/Connections_Archive/1982_Volume_5__Issue_2-b2c89e8d.pdf`).
  * Kim, M., & Fernandez, R. M. (2023). What Makes Weak Ties Strong? *Annual Review of Sociology*, 49(1), 177–193.
  * Gould, R. V., & Fernandez, R. M. (1989). Structures of Mediation: A Formal Approach To Brokerage In Transaction Networks. *Sociological Methodology*, 89–126.
  * Obstfeld, D., Borgatti, S. P., & Davis, J. (2014). Brokerage As a Process: Decoupling Third Party Action From Social Network Structure. *Research In The Sociology of Organizations*, 40, 135–159.
  * Stovel, K., & Shaw, L. (2012). Brokerage. *Annual Review of Sociology*, 38, 139–158.
* **Part 2: Structural Holes, Embeddedness, and Information Bandwidth**
  * Burt, R. S. (2004). Structural Holes and Good Ideas. *American Journal of Sociology*, 110(2), 349–399.
  * Goldberg, A., Srivastava, S. B., Manian, V. G., Monroe, W., & Potts, C. (2016). Fitting In Or Standing Out? The Tradeoffs of Structural and Cultural Embeddedness. *American Sociological Review*, 81(6), 1190–1222.
  * Aral, S., & Van Alstyne, M. (2011). The diversity-bandwidth trade-off. *American Journal of Sociology*, 117(1), 90–171.
* **Deliverable:** Analytic Memo 2 & Discussion Leader Questions (due Sun., Oct. 4 at 5:00 pm PT).

---

### Module 2: Structural Positions & Substantive Arenas (Weeks 3–5)

#### Week 3: Networks and the Economy and Organizations (Mon., Oct. 12)
* **Part 1: Embeddedness, Price Setting, and Market Dynamics**
  * Uzzi, B. (1996). The Sources and Consequences of Embeddedness for the Economic Performance of Organizations: The Network Effect. *American Sociological Review*, 61(4), 674–698.
  * Fernandez-Mateo, I. (2007). Who Pays the Price of Brokerage? Transferring Constraint through Price Setting in the Staffing Sector. *American Sociological Review*, 72(2), 291–317.
  * Duxbury, S. W., & Haynie, D. L. (2021). Shining a Light on the Shadows: Endogenous Trade Structure and the Growth of an Online Illegal Market. *American Journal of Sociology*, 127(3), 787–827.
* **Part 2: Market Status, Interfirm Networks, and Hybrid Economic Forms**
  * Podolny, J. M. (2001). Networks as the Pipes and Prisms of the Market. *American Journal of Sociology*, 107(1), 33–60.
  * Uzzi, B. (1997). Social Structure and Competition in Interfirm Networks: The Paradox of Embeddedness. *Administrative Science Quarterly*, 42(1), 35–67.
  * Mani, D., & Moody, J. (2014). Moving beyond Stylized Economic Network Models: The Hybrid World of the Indian Firm Ownership Network. *American Journal of Sociology*, 119(6), 1629–1669.
* **Deliverable:** Analytic Memo 3 & Discussion Leader Questions (due Sun., Oct. 11 at 5:00 pm PT).

#### Week 4: Networks In Science (Mon., Oct. 19)
* **Part 1: Specialization, Novelty, and Idea Diffusion**
  * Smith, D., Kennard, N., Du, T., & McFarland, D. (2025). How Values and Uncertainty Shape Scientific Advance in Peer Review. *American Sociological Review*, 90(5), 879–915.
  * Heiberger, R. H., Munoz-Najar Galvez, S., & McFarland, D. A. (2021). Facets of specialization and its relation to career success: An analysis of US sociology, 1980 to 2015. *American Sociological Review*, 86(6), 1164–1192.
  * Leahey, E., Lee, J., & Funk, R. J. (2023). What types of novelty are most disruptive?. *American Sociological Review*, 88(3), 562–597.
* **Part 2: Innovation Strategies, Scientific Consensus, and Curation**
  * Foster, J. G., Rzhetsky, A., & Evans, J. A. (2015). Tradition and Innovation In Scientists’ Research Strategies. *American Sociological Review*, 80(5), 875–908.
  * McMahan, P., & McFarland, D. A. (2021). Creative Destruction: The Structural Consequences of Scientific Curation. *American Sociological Review*, 86(2), 341–376.
  * Shwed, U., & Bearman, P. S. (2010). The Temporal Structure of Scientific Consensus Formation. *American Sociological Review*, 75(6), 817–840.
* **Deliverable:** Analytic Memo 4 & Discussion Leader Questions (due Sun., Oct. 18 at 5:00 pm PT).

#### Week 5: Collaboration, Creativity, and Field Dynamics (Mon., Oct. 26)
* **Part 1: Structural Folds & Small Worlds of Creativity**
  * Vedres, B., & Stark, D. (2010). Structural Folds: Generative Disruption In Overlapping Groups. *American Journal of Sociology*, 115(4), 1150–1190.
  * Uzzi, B., & Spiro, J. (2005). Collaboration and Creativity: The Small World Problem. *American Journal of Sociology*, 111, 447–504.
  * De Vaan, M., Stark, D., & Vedres, B. (2015). Game Changer: The Topology of Creativity. *American Journal of Sociology*, 120(4), 1144–1194.
  * Lutter, M. (2015). Do Women Suffer from Network Closure? The Moderating Effect of Social Capital on Gender Inequality in a Project-Based Labor Market, 1929 to 2010. *American Sociological Review*, 80(2), 329–358.
* **Part 2: Interorganizational Fields & Team Spillovers**
  * Powell, W. W., White, D. R., Koput, K. W., & Owen-Smith, J. (2005). Network Dynamics and Field Evolution: The Growth of Interorganizational Collaboration In The Life Sciences. *American Journal of Sociology*, 110(4), 1132–1205.
  * Rossman, G., Esparza, N., & Bonacich, P. (2010). I’d Like To Thank The Academy, Team Spillovers, and Network Centrality. *American Sociological Review*, 75(1), 31–51.
* **Deliverable:** Analytic Memo 5 & Discussion Leader Questions (due Sun., Oct. 25 at 5:00 pm PT).

---

### Module 3: Meaning, Culture & Historical Dynamics (Weeks 6–8)

#### Week 6: Networks and Culture and Culture in Networks (Mon., Nov. 2)
* **Part 1: Dualities of Culture & Structure and Tie Formation**
  * Lewis, K., & Kaufman, J. (2018). The Conversion of Cultural Tastes Into Social Network Ties. *American Journal of Sociology*, 123(6), 1684–1742.
  * Fuhse, J. A., & Gondal, N. (2024). Networks from culture: Mechanisms of tie-formation follow institutionalized rules in social fields. *Social Networks*, 77, 43–54.
  * Boutyline, A., & Vaisey, S. (2017). Belief Network Analysis: A Relational Approach to Understanding the Structure of Attitudes. *American Journal of Sociology*, 122(5), 1371–1447.
  * *(Removed 2026-10-05)* Goldberg, A. (2011). Mapping Shared Understandings Using Relational Class Analysis: The Case of the Cultural Omnivore Reexamined. *American Journal of Sociology*, 116(5), 1397–1436. — deleted from Week 6 Part 1 across all pages and deployed live.
* **Part 2: Meaning Structures, Publics, and Cultural Categories**
  * Fuhse, J. A. (2009). The Meaning Structure of Social Networks. *Sociological Theory*, 27(1), 51–73.
  * Ikegami, E. (2000). A Sociological Theory of Publics: Identity and Culture As Emergent Properties In Networks. *Social Research*, 989–1029.
  * Askin, N., & Mauskapf, M. (2017). What Makes Popular Culture Popular? Product Features and Optimal Distinctiveness in Music. *American Sociological Review*, 82(5), 910–944.
  * Leung, M. D. (2014). Dilettante or Renaissance Man? How the Order of Job Experiences Shapes Evaluator Perceptions of Categorical Spanners. *American Journal of Sociology*, 119(5), 1362–1403.
* **Deliverables:**
  * Analytic Memo 6 (due Sun., Nov. 1 at 5:00 pm PT).
  * 📌 **Final Project Extended Abstract** (due Fri., Nov. 6 at 11:59 pm PT).

#### Week 7: Diffusion In Networks (Mon., Nov. 9)
* **Part 1: Complex Contagion, Clustering, and Spatial Diffusion**
  * Centola, D., & Macy, M. (2007). Complex Contagions and the Weakness of Long Ties. *American Journal of Sociology*, 113(3), 702–734.
  * Centola, D. (2015). The social origins of networks and diffusion. *American Journal of Sociology*, 120(5), 1295–1338.
  * Cheng, M., Smith, D. S., Ren, X., Cao, H., Smith, S., & McFarland, D. A. (2023). How new ideas diffuse in science. *American Sociological Review*, 88(3), 522–561.
  * Keuchenius, A., Törnberg, P., & Uitermark, J. (2021). Adoption and Adaptation: A Computational Case Study of the Spread of Granovetter's Weak Ties Hypothesis. *Social Networks*, 66, 10–25.
* **Part 2: Associative Diffusion & Public Conversation**
  * DellaPosta, D., Shi, Y., & Macy, M. (2015). Why Do Liberals Drink Lattes? *American Journal of Sociology*, 120(5), 1473–1511.
  * Bail, C. A., Brown, T. W., & Mann, M. (2017). Channeling hearts and minds: Advocacy organizations, cognitive-emotional currents, and public conversation. *American Sociological Review*, 82(6), 1188–1213.
* **Deliverable:** Analytic Memo 7 & Discussion Leader Questions (due Sun., Nov. 8 at 5:00 pm PT).

#### Week 8: Networks In History (Mon., Nov. 16)
* **Part 1: Robust Action, Mobilization, and Historical Networks**
  * Padgett, J. F., & Ansell, C. K. (1993). Robust Action and the Rise of the Medici, 1400–1434. *American Journal of Sociology*, 98(6), 1259–1319.
  * Gould, R. V. (1991). Multiple Networks and Mobilization In The Paris Commune, 1871. *American Sociological Review*, 56(6), 716–729.
  * Becker, S. O., Hsiao, Y., Pfaff, S., & Rubin, J. (2020). Multiplex Network Ties and the Spatial Diffusion of Radical Innovations: Martin Luther's Leadership In The Early Reformation. *American Sociological Review*, 85(5), 857–894.

* **Part 2: Empires, Trade, and Long-Distance Networks**
  * Barkey, K., & Van Rossem, R. (1997). Networks of Contention: Villages and Regional Structure in the Seventeenth-Century Ottoman Empire. *American Journal of Sociology*, 102(5), 1345–1382.
  * Erikson, E., & Bearman, P. (2006). Malfeasance and the Foundations For Global Trade: The Structure of English Trade In The East Indies, 1601–1833. *American Journal of Sociology*, 112(1), 195–230.
  * Stamatov, P. (2010). Activist Religion, Empire, and the Emergence of Modern Long-Distance Advocacy Networks. *American Sociological Review*, 75(4), 607–628.
* **Deliverable:** Analytic Memo 8 & Discussion Leader Questions (due Sun., Nov. 15 at 5:00 pm PT).

---

### Module 4: Stratification, Micro-Macro & Final Synthesis (Weeks 9–10)

#### Week 9: Networks and Inequality (Mon., Nov. 23)
* **Part 1: Homophily, Elite Kinship, and Intergroup Network Inequalities**
  * DiMaggio, P., & Garip, F. (2011). How network externalities can exacerbate intergroup inequality. *American Journal of Sociology*, 116(6), 1887–1933.
  * Leszczensky, L., & Pink, S. (2019). What drives ethnic homophily? A relational approach on how ethnic identification moderates preferences for same-ethnic friends. *American Sociological Review*, 84(3), 394–419.
  * O’Brien, S. (2026). Kinship Interlocks: How the Intimate Exchange of Wealth, Status, and Power Generates Upper-Class Persistence. *American Sociological Review*, 91(2), 191–226.
* **Part 2: Academic Status Hierarchies, Network Segregation, and Migration**
  * Gondal, N. (2018). Duality of departmental specializations and PhD exchange: A Weberian analysis of status in interaction using multilevel exponential random graph models (mERGM). *Social Networks*, 55, 202–212.
  * Zhao, L. (2025). Uneven Mixing, Network Segregation, and Immigrant Integration. *American Sociological Review*, 90(3), 521–559.
  * Leal, D. F. (2021). Network inequalities and international migration in the Americas. *American Journal of Sociology*, 126(5), 1067–1126.
* **Deliverable:** Analytic Memo 9 & Discussion Leader Questions (due Sun., Nov. 22 at 5:00 pm PT).

#### Week 10: Networks and The Micro/Macro Link (Mon., Nov. 30)
* **Part 1: Network Ecology & Romantic/Sexual Networks**
  * McFarland, D. A., Moody, J., Diehl, D., Smith, J. A., & Thomas, R. J. (2014). Network Ecology and Adolescent Social Structure. *American Sociological Review*, 79(6), 1088–1121.
  * Bearman, P. S., Moody, J., & Stovel, K. (2004). Chains of Affection: The Structure of Adolescent Romantic and Sexual Networks. *American Journal of Sociology*, 110(1), 44–91.
* **Part 2: Spatial Mobility, Segregation, and Labor Markets**
  * Levy, B. L., Phillips, N. E., & Sampson, R. J. (2020). Triple Disadvantage: Neighborhood Networks of Everyday Urban Mobility and Violence In US Cities. *American Sociological Review*, 85(6), 925–956.
  * Papachristos, A. V. (2009). Murder By Structure: Dominance Relations and the Social Structure of Gang Homicide. *American Journal of Sociology*, 115(1), 74–128.
  * Huang, P., & Butts, C. T. (2023). Rooted America: Immobility and Segregation of the Intercounty Migration Network. *American Sociological Review*, 88(6), 1031–1065.
  * Cheng, S., & Park, B. (2020). Flows and Boundaries: A Network Approach to Studying Occupational Mobility in the Labor Market. *American Journal of Sociology*, 126(3), 577–631.
* **Deliverable:** Analytic Memo 10 & Discussion Leader Questions (due Sun., Nov. 29 at 5:00 pm PT).
* **Final Requirement:** Final Course Paper submitted via Bruin Learn Submission Box.

---

## 5. Live Bruin Learn Pages & Endpoints

| Resource | URL Slug / Path | Canvas ID |
|:---|:---|:---|
| **Front Page** | `front-page` | — |
| **Syllabus (Page)** | `sociol-208b-syllabus-fall-2026` | — |
| **Syllabus (Native)** | `/assignments/syllabus` | — |
| **Reading Schedule** | `sociol-208b-reading-schedule-fall-2026` | — |
| **Extended Reading List** | `extended-reading-list-208B` (https://olizardo.github.io/SOCIOL208/extended-reading-list-208B.html) | — |
| **Week 1 Guide** | `week-1-networks-as-theory-and-perspective-readings-and-overview` | — |
| **Week 2 Guide** | `week-2-networks-and-the-economy-and-organizations-readings-and-overview` | — |
| **Week 3 Guide** | `week-3-brokerage-and-intermediation-readings-and-overview` | — |
| **Week 4 Guide** | `week-4-networks-in-science-readings-and-overview` | — |
| **Week 5 Guide** | `week-5-collaboration-creativity-and-field-dynamics-readings-and-overview` | — |
| **Week 6 Guide** | `week-6-networks-and-culture-and-culture-in-networks-readings-and-overview` | — |
| **Week 7 Guide** | `week-7-diffusion-in-networks-readings-and-overview` | — |
| **Week 8 Guide** | `week-8-networks-in-history-readings-and-overview` | — |
| **Week 9 Guide** | `week-9-networks-and-inequality-readings-and-overview` | — |
| **Week 10 Guide** | `week-10-networks-and-the-micro-slash-macro-link-readings-and-overview` | — |
| **Discussion Leader Survey** | `/quizzes/1131287` | `1131287` |
| **Discussion Questions Forum** | `/discussion_topics/1466035` | `1466035` |
| **General Q&A Board** | `/discussion_topics/1466036` | `1466036` |

---

## 6. Site Architecture, Layouts, & UI Component Guidelines

Future AI agents must preserve the following page structures and interactive UI patterns across all updates:

### A. Front Page Architecture (`front-page.html` / `sociol-208b-social-network-methods`)
1. **Hero Banner:** Dark gradient (`#0f172a` → `#1e3a8a` → `#2563eb`) with course code badge, title, subtitle, meeting schedule badge, location badge, and instructor badge.
2. **Quick Access Navigation Grid:** Responsive 5-card grid linking to Syllabus, Reading Schedule, Canvas Modules, Course Files (non-DOI PDFs), and Discussion Leader Preferences Survey.
3. **Core Seminar Modules Grid (2x2 Layout):**
   * 4 Module cards (Foundations, Structural Positions, Meaning & Culture, Stratification & Micro/Macro).
   * **Collapsible Reading Disclosure Widgets (`<details>` / `<summary>`):** Every single week in the module cards MUST feature an HTML disclosure widget (`<details><summary>View Assigned Readings (X items)</summary></details>`). Clicking the triangle opens a shaded list of Part 1 and Part 2 readings with direct `[DOI ↗]` links or `[Course Files ↗]` links. **Never remove these disclosure widgets.**
4. **Modular Resources & Research Hubs:** Structured cards for Reference Works (Methods, Handbooks, Overviews, Theory), Textbooks & Online Courses, Data Repositories, Organizations, Specialist Journals, and Curated Resources.

### B. Extended Reading List (`extended-reading-list-208B.qmd`, added 2026-10-12)
* **Purpose:** A growing, non-required thematic bibliography for memos, discussion leadership, and final papers. Not a Canvas page — deployed only to the public lecture-notes website (https://olizardo.github.io/SOCIOL208/extended-reading-list-208B.html) via the standard `_freeze`/publish workflow.
* **Structure:** Organized thematically (not by week) into ~20 sections that mirror the sections of the main reading schedule; each section is seeded with the assigned readings and accumulates supplementary entries over the quarter. Entries use the format `Author (Year). Title. *Journal*, vol(issue), pages. [link](https://doi.org/...)` — DOIs link directly; no-DOI items link to Bruin Learn Course Files.
* **Update workflow:** Edit `extended-reading-list-208B.qmd` at the repo root, render it (`quarto render extended-reading-list-208B.qmd` — it has no code chunks, so it produces no `_freeze` artifact; committing the `.qmd` alone is sufficient, as the CI render builds the body from source for pure-markdown pages), commit, and push to `origin main` (~2 min deploy).
* **Entry history:** Created 2026-10-12 (commit `ab6f188`); supplemented with four verified readings (`f5e0afb`) and Fernandez & Weinberg 1997 (`f4ad7d9`) from the other machine; McFarland & Wolff (2022), "Writing into relationships," *Social Networks* 71, 96–114 (DOI 10.1016/j.socnet.2022.06.006) added to "Dualities of Culture & Structure and Tie Formation" on 2026-11-06 (commit `c8244b9`).
* **Multi-machine note:** This repo is edited from multiple machines; before editing, `git pull --rebase --autostash origin main` and verify the file exists locally (a missing file usually just means a stale local checkout, not a deleted file).

### C. Reading Schedule Page (`sociol-208b-reading-schedule-fall-2026`)
* Live schedule alert box.
* Links to internal Canvas Course Files for non-DOI PDFs (`https://bruinlearn.ucla.edu/courses/239524/files`).
* 10-week breakdown with Part 1 and Part 2 headers, exact DOIs for published articles (`[DOI ↗]`), and links to course files for books/chapters (`[Course Files ↗]`).

### D. Weekly Overview Pages (`week-[1-10]-readings-and-overview`)
* **Week Banner:** Blue gradient header card with week number, title, date, and room.
* **Quick Action Card:** Submission box link, due date, reading requirements (pick 1 from Part 1 and 1 from Part 2), Discussion Leaders board link.
* **Part 1 & Part 2 Cards:** Styled table-cards with reading list and links (`[DOI ↗]` or `[Course Files ↗]`).
* **Footer Navigation:** Links back to Modules and Full Schedule.

### E. Automated Deployment Scripts
* **`deploy_all_to_canvas.py`**: Reads `WEEKS_DATA`, updates local HTML templates (`front-page.html`, `schedule-208B.html`, `bruinlearn_course_materials/`), and deploys all pages directly to Canvas LMS via REST API.
* **`deploy_weekly_readings_to_modules.py`**: Pushes weekly pages into Canvas Modules and publishes them.

**🚨 Protocol for Updating Readings:** 
Readings are not currently centralized in a single database or generated automatically. To update a reading, you must edit it in **three** separate places to ensure consistency across the course:
1. In `deploy_all_to_canvas.py` (update the `WEEKS_DATA` array and `doi_map`, which dynamically generates the weekly module overview pages).
2. In `front-page.html` (update the raw HTML for the front page).
3. In `schedule-208B.html` and/or `bruinlearn_course_materials/pages/sociol-208b-reading-schedule-fall-2026.html` (update the raw HTML for the reading schedule page).

*After editing all required files, run `python3 deploy_all_to_canvas.py` to push changes to the live Canvas course.*

## 7. SOCIOL 208A Course & Automation Architecture

SOCIOL 208A is the companion methods seminar to 208B, sharing this same repository (`SOCIOL208`) and the same public lecture-notes website (**https://olizardo.github.io/SOCIOL208/**, built by `_quarto.yml` from the `basic-*.qmd`, `cent-*.qmd`, `prest-*.qmd`, `sim-*.qmd`, `comm-*.qmd`, `tm-*.qmd`, and `stat-*.qmd` files at the repo root). 208A's Bruin Learn site was rebuilt (Sep 2026) to mirror 208B's visual architecture (hero banner, quick-access bar, thematic module cards with collapsible reading disclosures) while linking out to the lecture notes rather than duplicating their content in Canvas.

### A. Course Metadata
* **Canvas Course ID:** `232537` (`26S-SOCIOL-208A-LEC-1`, Spring 2026 term — term/dates are stale from a past offering; the site content was restructured independent of specific calendar dates, which remain **TBD** until the next offering is scheduled).
* **Meeting Pattern (per `syllabus-208A.qmd`):** Wednesdays, 9:00–11:50 am, Haines A37.
* **Instructor:** Prof. Omar Lizardo.
* **Local source files:** `syllabus-208A.qmd` (course policies/grading: 25% Attendance & Discussion, 25% Weekly Data Analysis Exercises, 50% Final Data Analysis Paper — the live Canvas syllabus previously drifted to a mismatched 10%/50%/40% split from a manual hand-edit and was corrected back to 25/25/50 to match this file) and `schedule-208A.qmd` (the canonical 10-week reading list — mirrors the live Canvas reading-schedule page).
* **Local front-page mirror:** `front-page-208A.html` (regenerated by `deploy_208A_to_canvas.py` — see Section D below), which splices in the hand-maintained `front-page-208A-resources-partial.html` (Lecture Slides, Reference Works, Textbooks, Data Repositories, Organizations, Journals, Resources cards) rather than regenerating those from scratch each run.
* **Dates:** fully cleared. All 12 assignments (10 exercises + Extended Abstract + Final Paper) have `due_at`/`unlock_at`/`lock_at` set to `null`, and Canvas's native auto-generated Syllabus "Course Summary" widget is disabled (`syllabus_course_summary: false` in course settings) because it can only display real due dates, not custom week labels. In its place, both `syllabus-208A.qmd` and the live syllabus body carry a hand-authored "Course Summary" table listing deliverables by Week 1–10 plus "End of Quarter," with a note that calendar dates are TBD pending the next term's scheduling.

### B. Finalized 10-Week Structure (No Travel/No-Class Weeks)
Originally 8 substantive weeks + 2 "No Class (Traveling)" placeholders; restructured into a full 10-week sequence by splitting Community Detection and Statistical Network Models into foundations/advanced pairs:

| Week | Topic | Exercise (Assignment ID) | Primary Lecture-Note Files |
|---|---|---|---|
| 1 | Basic SNA Concepts | Exercise I (`1956037`) | `basic.qmd`, `basic-dir.qmd`, `data-loading.qmd`, `data-network-lists.qmd`, `data-subgraph-select.qmd` |
| 2 | Centrality | Exercise II (`1956039`) | `cent-deg.qmd`, `cent-clos.qmd`, `cent-bet.qmd`, `cent-cube.qmd`, `cent-random.qmd`, `cent-edge.qmd`, `cent-tm.qmd` |
| 3 | Ego Networks | Exercise III (`1956044`) | `basic-ego.qmd` |
| 4 | Status and Prestige | Exercise IV (`1956040`) | `prest-eigen.qmd`, `prest-pagerank.qmd`, `prest-hits.qmd`, `prest-ml.qmd`, `prest-tm.qmd`, `prest-tm-gen.qmd` |
| 5 | Similarity, Roles, and Structural Equivalence | Exercise V (`1956041`) | `sim-structequiv.qmd`, `sim-loc.qmd`, `sim-tm.qmd`, `sim-glob.qmd`, `sim-gen.qmd`, `sim-blondel.qmd` |
| 6 | Community Detection — Foundations | Exercise VI (`1956042`) | `comm-mod.qmd`, `comm-edge.qmd`, `comm-spectral.qmd` |
| 7 | **Advanced Community Detection Methods** | Exercise VII (`2044561`, new) | `comm-overlap-clique.qmd`, `comm-overlap-edge.qmd`, `comm-overlap-fitness.qmd`, `comm-ca.qmd`, `comm-tm-spectral.qmd` |
| 8 | Analyzing Two-Mode Networks | Exercise VIII (`1956043`, renamed from a duplicate "Exercise VI") | `tm-basic.qmd`, `tm-duality.qmd`, `tm-weighted-proj.qmd`, `tm-ca.qmd`, `tm-reflections.qmd`, `tm-fitness.qmd` |
| 9 | Statistical Network Models I — Random Graphs, QAP, and Permutation Methods | Exercise IX (`2044562`, new) | `stat-swap.qmd`, `stat-qap.qmd`, `stat-tm.qmd`, `stat-tm-bb.qmd` |
| 10 | **Statistical Network Models II — ERGMs** | Exercise X (`2044563`, new) | `stat-ergm1.qmd`, `stat-ergm2.qmd` |

Plus `Extended Abstract of Final Project` (`1956045`) and `Final Data Analysis Paper` (`1956046`). All Data Analysis Exercise assignments live in Canvas assignment group `625852`, positioned 1–10 in week order.

### C. Live Bruin Learn Pages & Modules
* **Front Page:** slug `sociol-208a-social-network-methods` (title "SOCIOL 208A: Social Network Methods"; `front_page: true`). Contains hero banner, quick-access bar (Syllabus, Reading Schedule, Canvas Modules, Lecture Notes ↗), four 2×2 thematic module cards (Foundations [Wks 1–2]; Structural Positions & Similarity [Wks 3–5]; Community Detection [Wks 6–7]; Two-Mode & Statistical Network Models [Wks 8–10]) each with per-week `<details>` reading disclosures plus a visually distinct indigo "🎓 Lecture Notes" chip row, and preserved/restyled legacy resource cards (Lecture Slides, Reference Works & Further Reading, SNA Textbooks & Online Courses, Network Data Repositories, Organizations, Network Journals, Resources).
* **Reading Schedule Page:** slug `sociol-208a-reading-schedule` (regenerated from `schedule-208A.qmd`).
* **Canvas Modules:** 10 published modules (one per week; `deploy_208A_to_canvas.py` deletes and recreates all 10 on every run, so module IDs are **not stable across deploys** — always look them up live via `GET /courses/232537/modules` rather than hardcoding IDs), each bundling exactly: (1) a `Page` item linking to the reading-schedule page ("Week N Overview & Reading Schedule"), (2) an `Assignment` item for that week's Data Analysis Exercise, (3) one `ExternalUrl` item per lecture-note file for that week (new tab). All items published.
* **Lecture-note integration convention:** links out only (no iframe embedding, no copying lecture content into Canvas) — every week's reading disclosure and Module both link directly to `https://olizardo.github.io/SOCIOL208/<file>.html`.

### D. Automation Scripts & Update Protocol
208A now has a single consolidated deployment script, **`deploy_208A_to_canvas.py`**, mirroring 208B's `deploy_all_to_canvas.py`. Running `python3 deploy_208A_to_canvas.py` (using the same `~/.Renviron`-sourced `CANVAS_URL`/`CANVAS_API_TOKEN` credential pattern as 208B):
1. Parses `schedule-208A.qmd` (the single source of truth) into structured week/section/reading data.
2. Regenerates `front-page-208A.html` — the 4 thematic module cards are built from the parsed schedule data, while the legacy resource-hub cards (Lecture Slides, Reference Works, Textbooks, Data Repositories, Organizations, Journals, Resources) are spliced in unchanged from `front-page-208A-resources-partial.html` (edit that file directly to update those sections; it is not regenerated from `schedule-208A.qmd`).
3. Regenerates the flat reading-schedule HTML from the same source file.
4. Pushes both pages to Canvas, **looking up each page's current slug dynamically** (via `front_page: true` for the front page, and a `reading-schedule` substring/title match for the schedule page) rather than hardcoding slugs, and passes `body` only (never `title`) on updates so Canvas does not regenerate the slug again on re-runs.
5. Deletes and recreates all 10 Canvas Modules with their Page/Assignment/ExternalUrl items, publishing everything.

Data Analysis Exercise assignment **descriptions** are intentionally *not* touched by this script (they contain hand-written, exercise-specific instructions) — edit those directly via the Canvas UI or a targeted one-off script, same convention as 208B's `deploy_all_to_canvas.py` never touching front-page prose outside the templated blocks.

**To update readings or lecture-note links going forward:** edit `schedule-208A.qmd` first, then run `python3 deploy_208A_to_canvas.py` — it handles the front page, reading-schedule page, and all 10 Modules in one pass. Only the resource-hub cards (`front-page-208A-resources-partial.html`) and assignment descriptions require separate manual edits.

* **Known Canvas quirk hit repeatedly on this course:** changing a Page's `title` via the API regenerates its URL `slug` (e.g. `reading-schedule` → `sociol-208a-reading-schedule`; `front-page` → `sociol-208a-social-network-methods`), even though the `front_page: true` flag itself is preserved. `deploy_208A_to_canvas.py` works around this by looking up slugs dynamically and omitting `title` from update payloads; any *new* one-off script touching these pages should do the same rather than hardcoding slugs.

## Function-Explainer Style (lecture-note .qmd files)

Conventions for the "It works like this:" walkthroughs that follow every hand-written function in the lecture notes (established in `comm-mod.qmd`; apply everywhere):

- **Format:** after the code chunk, a lead-in sentence ("It works like this:"), then one `- Line N ...` bullet per logical block, in code order.
- **Line numbering:** line 1 is the function header (`foo <- function(...) {`); every physical line counts, including comments and closing braces. **Re-verify all numbers after ANY edit to the function** — reformatting (e.g. collapsing a two-line header to one line) shifts everything, and the explainer has drifted and needed fixing repeatedly. Before finishing, read the chunk and the bullets side by side.
- **Bullet headers describe tasks, not mechanics.** Prefer short task labels ("aggregate and decide", "keep the books", "set things up") over mechanical "Lines X-Y do Z" phrasing.
- **Bullets explain the why, comments carry the what.** Code comments are short reasons/labels (`# each node moves to where its community sits`); the prose bullet explains the rationale, the math, and any non-obvious index-space trick (e.g. community labels doubling as node positions in the next coarsened graph; name-indexing a matrix by `dimnames` instead of renumbering).
- **No colons in running prose; no sentence-opening pronouns or demonstratives without referents; "show" not "demonstrate", "use" not "utilize"** (see the authorial-voice rules in user memory — they apply to lecture-note prose too).
- **Demos follow a fixed rhythm:** "Here is a function that does exactly that:" → code chunk → explainer bullets → "Let's see it in action ...:" → demo chunk → interpretation paragraph that references the *actual printed values* (seeds, Q scores, community counts). If the code or seed changes, the interpretation paragraph must be re-verified against the new output, not just re-worded.
- **Inline references stay live:** prose that interpolates results with `r var` must point at a variable the rendered chunks actually create; prose that hardcodes numbers must match chunk output exactly.
- **Figures comparing partitions** use one shared subfigure group (`#| fig-subcap`, `#| layout-ncol: 2`, `echo: false`, common `layout_with_kk` layout computed inside the chunk) so all networks render on identical coordinates; each panel gets a full-sentence caption ("Law Firm Friendship Network by The Best K-Community Partition Using the X Approach."). Prose cites panels as `@fig-label-N`.
- **Removed comparisons stay removed** (e.g. do not reintroduce an igraph `cluster_louvain` benchmark); deletions in these notes are deliberate pedagogical choices, not oversights.
- **Commented-out material is dead:** `knitr` ignores HTML comments but still parses ` ```{r} ` fences inside them, which breaks the render — comment out code fences by stripping the braces (` ```r `) or delete the section outright rather than leaving commented blocks behind.

## Lecture-Note Content Notes (comm-mod & bibliography)

- **The Louvain material now lives on its own page, `comm-louvain-book.qmd` (2026-11-18), titled "The Louvain Approach to Community Detection".** It was moved out of `comm-mod.qmd` on 2026-11-18, which now ends with the greedy section plus a one-paragraph link out to the new page; `comm-mod.qmd`'s `fig-mod` group kept only its three non-Louvain panels. The page's first half is the four-part sequence originally built in `comm-mod` (commits `0742186`–`2d982ff`), the second half the pseudocode version:
  - **Step 1 (`louvain.step1(x, seed = 123)`):** transparent, deliberately inefficient local node-moving phase — scores every candidate move by literally recomputing `modularity()` before/after (no B-matrix gain shortcuts); follows the same loop structure as Algorithm 8.7 of Fouss, Saerens & Shimbo (2016) but scores moves the from-scratch way (seeded random node order, adjacent clusters only via `Ni`/`commNi`/`adjclust`, strictly positive gains with `best.mod`/`t`, full-pass termination). Walked through in the notes' standard "- Line N ..." style; keep the explainer in sync with the code after every edit (it has drifted and been fixed three times already). **Seed must be passed through to step-1 calls from wrappers** — the function re-seeds internally, so a wrapper-level `set.seed` alone is silently overridden.
  - **Step 2 (`louvain.step2(x, comm)`):** aggregation/coarsening — double loop over the adjacency matrix accumulates community-level weights into `C` (returned as graph `G`), rows/columns named with `u <- unique(comm)` and cells indexed *by name* (`as.character`) instead of `match()`-renumbering; diagonal halved after the loop (the double sum counts each internal edge twice; igraph counts a self-loop once in modularity). Section includes a modularity-invariance check: the coarsened graph read as singletons reproduces the original partition's modularity exactly. (A 2026-11-18 rewrite that folded the halving into an `i == j` branch broke invariance — the diagonal halving after the double loop is load-bearing.)
  - **Full run (`louvain.full(x, seed)`):** alternates step 2 then step 1 per level, tracks a per-level `history` data frame (node count + Q), and keeps a `slot` vector mapping each original node to its current stand-in node on the current graph; when the search continues, `slot <- match(lv1[slot], unique(lv1))` re-addresses every node to its community's position on the coarsened graph (labels ≠ positions — labels have gaps), and the final partition is read off with `lv1[slot]`. Stops when `Q.new <= Q` (invariance makes "no moves" detectable as exact Q equality, so no tolerance needed). Returned Q is computed on the original graph via the stashed `x0`. Demo: seed 123 runs two levels (68 → 7 nodes, Q = 0.3256 → 0.3336); the old seed-208 "lucky single-level" example was removed on 2026-11-18.
  - **Multi-restart:** a visible inline tournament chunk (no separate `louvain.multi` function; the old commented-out section was deleted) runs `louvain.full` over seeds 1:10 with `bestQ <- -0.5` init and keeps the winner. Law-firm winner: Q = 0.3724, four communities (sizes 22/18/5/23). The winner is now panel 1 of the page's own `fig-louvain` two-panel figure (panel 2 is the deterministic pseudocode partition), which is placed at the end of the page because it needs both versions' results to exist.
  - **Do not reintroduce an igraph `cluster_louvain` comparison:** step 1 alone on the law-firm network (seed 208: Q = 0.3707, five communities) *beats* a full `cluster_louvain` run (Q = 0.3656, three communities); the comparison was deliberately removed. Both are stochastic local searches (cluster_louvain is itself seed-sensitive); modularity maximization is NP-hard [@brandes07].
  - **The pseudocode half of `comm-louvain-book.qmd` (added 2026-11-18, commit `c7bb650`; merged with the notes' version on 2026-11-18)** translates Fouss, Saerens & Shimbo (2016), *Algorithms and Models for Network Data and Link Analysis*, Section 8.7 (Algorithms 8.7/8.8) into R, on the same law-firm `ug` as the notes' version so the numbers compare directly. Notation follows `comm-mod` (modularity matrix $\mathbf{B}$ with cells $b_{ij}$, adjacency $\mathbf{A}$, membership matrix $\mathbf{U}$, volume $\sum_i\sum_j a_{ij}$, gain $\Delta Q$; the two display equations carry `{#eq-louvain-gain}` and `{#eq-louvain-coarsen}` and are referenced as `@eq-...`). Cite it with page locators, never as "the textbook": Algorithm 8.7 = p. 379, Algorithm 8.8 and `A' = U^T A U` = p. 380, Eq. 8.60 and the randomized-order remark = p. 381, the two-step description and the modularity-invariance footnote = pp. 377–378. Book-faithful: modularity-matrix column gains (Eq. 8.60, verified exact vs brute force), membership matrix `U` with delete-zero-columns/recompute-labels bookkeeping, **fixed 1..n visitation order (fully deterministic, no seed)**. Two teachable findings baked into the page: (1) literal `A' = U^T A U` double-counts self-loop weight under igraph's conventions — coarsened modularity inflates (0.3448 → 0.4865) and the phantom "improvement" corrupts the stopping rule (literal run climbs 0.3448 → 0.4878 → 0.4899 but final true Q = 0.3687, *worse* than the halved version's 0.3712); the `halve.diag` switch repairs it. (2) They are silent on un-coarsening; the page's `louvain.book.full` adds the `slot <- lab[slot]` bookkeeping. Book deterministic run on `ug`: Q = 0.3712, sizes 27/3/20/18 (vs seed-123 randomized 0.3336 and tournament best 0.3724 with sizes 22/18/5/23 — different optima, near-tied Q). Cite the book as `fouss_etal16` (already in `networks.bib`). The PDF source lives in the gitignored `algorithms/finding_dense_regions.pdf`.
  - **Writing conventions fixed on 2026-11-18 (commits `c2a4ab4`, `9b266bd`, `aa02195`, `90475bb`):** notation follows `comm-mod` (modularity matrix $\mathbf{B}$ with cells $b_{ij}$, adjacency $\mathbf{A}$, membership matrix $\mathbf{U}$, volume $\sum_i\sum_j a_{ij}$, gain $\Delta Q$); the two display equations carry `{#eq-louvain-gain}` and `{#eq-louvain-coarsen}` and are referenced as `@eq-...`; every reference to the source is a page-located citation or an algorithm/equation number, never "the book", "the textbook", or bare "the pseudocode"; section headings are "Step 1, the Modularity-Matrix Way", "The Coarsening Step in One Line", "Assembling the Two Algorithms", "What the Algorithms Buy and What It Costs"; cross-page links to `comm-mod.qmd` use anchor fragments (see the website cross-reference rule below).
- **Bibliography key trap:** `blondel_etal04` in `networks.bib` is Blondel, Gajardo et al. (2004), a graph-*similarity* paper — **not** the Louvain method paper. Cite Louvain as `blondel_etal08` (Blondel, Guillaume, Lambiotte & Lefebvre 2008, JSTAT, added 2026-11-18).
- **`algorithms/` (lecture-notes PDFs incl. `finding_dense_regions.pdf`) is gitignored on purpose**, along with `algorithms.zip`; don't commit them or un-ignore.

## GitHub Pages Lecture-Notes Deployment (SOCIOL208 Website)

The public lecture-notes site (**https://olizardo.github.io/SOCIOL208/**) is deployed by `.github/workflows/publish.yml` (Quarto publish to `gh-pages`, triggered on every push to `main`). Critically, `_quarto.yml` sets **`freeze: true`**, so the CI build does **not** execute R chunks — it reuses the rendered results stored in the `_freeze/<page>/execute-results/` artifacts.

**🚨 Rule: Always commit `_freeze/` together with `.qmd` edits.**
When editing any lecture-note `.qmd` file at the repo root, the deployed site body is built from the committed `_freeze` artifact, not from the `.qmd` source. Committing only the `.qmd` produces a silently broken deploy: the page body reverts to the last-rendered (stale) version while the "View Source" modal shows the new source — the two visibly diverge.

**Correct workflow for any `.qmd` change:**
1. Edit the `.qmd` file.
2. Render locally so the freeze artifact updates: `quarto render <file>.qmd` (or the full site).
3. Commit **both** the `.qmd` and the updated `_freeze/<page>/` directory together (e.g., `git add <file>.qmd _freeze/<page> && git commit`).
4. Push to `origin main`; the workflow deploys automatically (~2 minutes). Verify the deployed page's `last-modified` header if in doubt.

**🚨 Rule: cross-page references are links, not `@`-refs (website project).**
`_quarto.yml` declares `type: website`, and Quarto resolves `@fig-`, `@eq-`, `@tbl-`, and `@sec-` only within a single page. Across pages the `@...` text leaks through literally, and `@name` for anything that is not a bibliography key throws `Citeproc: citation name not found` (and `@sec-name` throws `Unable to resolve crossref`). Link to another note with a markdown link plus the destination's label or heading slug, e.g. `[the modularity matrix](comm-mod.qmd#the-modularity-matrix)`, `[the gain equation](comm-mod.qmd#eq-mod-eig)`, `[the greedy section](comm-mod.qmd#an-agglomerative-approach-based-on-the-modularity)`. Write the noun yourself, since a link does not render as "Figure 3". Numbered cross-file references would require converting the project from `website` to `book`.

**Other notes:**
- **`schedule-208B.html` was removed from the repo on 2026-11-18** (commit `f394067` on `origin/main`); the file is no longer tracked and the old Quarto-render-clobber workaround is moot. `deploy_all_to_canvas.py` still regenerates its own copy of the schedule template when needed.
- Local rendering requires the `renv` library to be restored (`renv::restore()`); knitr/rmarkdown must be installed or `quarto render` fails with "The knitr package is not available."
- `ggbump` is archived on CRAN (archived 2025-12-04); its `renv.lock` entry must keep `"Source": "GitHub"` (`davidsjoberg/ggbump`) so restore can resolve it. Do not run a bare `renv::snapshot()` while the library is partially populated — implicit snapshot will shrink the lockfile to the current (broken) library state. Restore the lockfile from git and fix records surgically if that happens.
- Never push directly to `main` from the assistant environment (blocked by safety rule); ask the user to push, or use a branch + PR.

## Code Block Width, Scrolling, and Output Alignment (styles.css / custom.scss)

Code-block presentation lives entirely in `styles.css` (loaded via `format: html: css`), which applies to both the light and the dark theme and comes after the Bootstrap theme CSS in the cascade. `custom.scss` (light) and `custom-dark.scss` (dark) carry theming only — **do not duplicate the code-block rules there.** The duplication had already drifted: `custom-dark.scss` still had a 600px `max-height` until the copies were deleted on 2026-11-18. `styles.css` has CRLF line endings; a plain Python text-mode read/write silently converts the whole file to LF and produces a 28-line diff for a one-line change. Edit it in binary (`open(p,'rb')`, append `\r\n`) or restore from git and patch bytes.

Current rules (set 2026-11-18):

```css
pre.sourceCode { max-height: 900px; overflow-y: auto; }   /* ≈37 code lines at the 23.9px line height */
main .cell { grid-column: page-start / page-end; display: grid; grid-template-columns: subgrid; }
main .cell > * { grid-column: 1 / -1; }
main .cell > .cell-output-stdout, main .cell > .cell-output-stderr {
  grid-column: body-content-start / body-content-end;
}
```

- The `max-height` cutoff was raised from 600px (≈25 lines) to 900px, so no chunk on `comm-louvain-book.qmd` (longest: 34 lines) scrolls.
- The `main .cell` rules widen **every** code cell on **every** page to the page column, so per-chunk `#| column: page` options in the `.qmd` files are now redundant (harmless; a chunk-level layout class still wins on specificity where present).
- Because `.cell` is made a subgrid of the page grid, printed output (`cell-output-stdout`/`-stderr`) can be placed on the body column and lines up exactly with the prose, while source code, figures, and `quarto-layout-panel` cells keep the wide column. Quarto itself has no source-only column option — `#| column:` widens the whole cell, output included — so this exemption can only be done in CSS.
- The rule uses grid line names, not a pixel cap, so alignment tracks the body column at any window width. An earlier `max-width: 946px` version capped the width but left the output hugging the cell's left edge, which looked pushed-left; the grid placement is the correct fix.

Measured widths at a 1500px viewport with a docked sidebar (body column = 946px):

| Setting | Cell spans |
|---|---|
| no column option | 254..1200 (946px) |
| `column: page` | 27..1427 (1400px) |
| `column: page-inset` | 127..1377 (1250px) |
| `column: screen-inset` | 27..1458 (1431px) |
| `column: screen` | 0..1485 (1485px) |

`screen-inset` adds only ~31px over `page` because both start on the same left grid line with a docked sidebar, so `page` is the sensible default; `column: screen` is the only real step up. A fresh build of a page also needs the shared stylesheet refreshed in the local preview (`cp styles.css _site/styles.css`) before measuring; CSS-only changes need no render and leave `_freeze` alone.

**Verifying layout without the Viewer:** `chrome-headless-shell --dump-dom` a copy of the built page with a small injected `<script>` that reports `getBoundingClientRect()` for the elements of interest. Fragment-hash screenshots of a scrolled page come out blank, and mid-page screenshots of wide cells are hard to judge by eye, so measure instead.

**Warning — `comm-louvain-book.qmd` is not render-reproducible:** its timing comparison (§ "What the Algorithms Buy and What It Costs") bakes wall-clock seconds into the frozen output and into interpolated prose ("0.779 seconds … 4.561 … roughly 5.9 times longer"). Every re-render rewrites those numbers (a later run gave 0.802 / 4.844 / "6 times longer") and dirties `_freeze`. When a render was only done to inspect layout, restore the artifact with `git checkout HEAD -- _freeze/comm-louvain-book/execute-results/html.json` rather than committing the new timings.
