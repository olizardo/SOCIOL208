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

### Module 1: Foundations & The Relational Turn (Weeks 1–2)

#### Week 1: Networks As Theory and Perspective (Mon., Sep. 28)
* **Part 1: Structural Foundations & Network Theory**
  * Wellman, B. (1988). Structural Analysis: From Method and Metaphor To Theory and Substance. In B. Wellman & S. D. Berkowitz (Eds.), *Social Structures: A Network Approach* (pp. 19–61). Cambridge University Press.
  * Martin, J. L. (2009). *Social Structures*. Princeton University Press (selections).
  * Borgatti, S. P., & Halgin, D. S. (2011). On Network Theory. *Organization Science*, 22(5), 1168–1181.
  * Rivera, M. T., Soderstrom, S. B., & Uzzi, B. (2010). Dynamics of Dyads in Social Networks: Assortative, Relational, and Proximity Mechanisms. *Annual Review of Sociology*, 36(1), 91–115.
* **Part 2: Relational Sociology, Culture, and Agency**
  * Erikson, E. (2013). Formalist and Relationalist Theory In Social Network Analysis. *Sociological Theory*, 31(3), 219–242.
  * Erices-Ocampo, P., Lubbers, M. J., & adams, j. (2025). Toward a Unified Conceptualization of Social Capital. *Annual Review of Sociology*, 51.
  * Fuhse, J. (2020). Theories of Social Networks. In R. Light & J. Moody (Eds.), *The Oxford Handbook of Social Networks* (pp. 34–49). Oxford University Press.
* **Deliverable:** Analytic Memo 1 (due Sun., Sep. 27 at 5:00 pm PT).

#### Week 2: Brokerage and Intermediation (Mon., Oct. 5)
* **Part 1: Weak Ties, Brokerage Roles, and Mediation**
  * Granovetter, M. S. (1973). The Strength of Weak Ties. *American Journal of Sociology*, 78(3), 1360–1380.
  * *(Optional)* Rejection letter from *American Sociological Review* of the first (1969) version of the paper (`https://scatter.wordpress.com/wp-content/uploads/2014/10/granovetter-rejection.pdf`).
  * *(Optional)* Granovetter, M. S. (1969). Alienation Reconsidered: The Strength of Weak Ties. Reprinted in *Connections* 5(2): 4–16 (`https://assets.noviams.com/novi-file-uploads/insna/Connections_Archive/1982_Volume_5__Issue_2-b2c89e8d.pdf`).
  * Kim, M., & Fernandez, R. M. (2023). What Makes Weak Ties Strong? *Annual Review of Sociology*, 49(1), 177–193.
  * Obstfeld, D., Borgatti, S. P., & Davis, J. (2014). Brokerage As a Process: Decoupling Third Party Action From Social Network Structure. *Research In The Sociology of Organizations*, 40, 135–159.
  * Stovel, K., & Shaw, L. (2012). Brokerage. *Annual Review of Sociology*, 38, 139–158.
* **Part 2: Structural Holes, Embeddedness, and Information Bandwidth**
  * Burt, R. S. (2004). Structural Holes and Good Ideas. *American Journal of Sociology*, 110(2), 349–399.
  * Goldberg, A., Srivastava, S. B., Manian, V. G., Monroe, W., & Potts, C. (2016). Fitting In Or Standing Out? The Tradeoffs of Structural and Cultural Embeddedness. *American Sociological Review*, 81(6), 1190–1222.
  * Aral, S., & Van Alstyne, M. (2011). The diversity-bandwidth trade-off. *American Journal of Sociology*, 117(1), 90–171.
* **Deliverable:** Analytic Memo 2 & Discussion Leader Questions (due Sun., Oct. 4 at 5:00 pm PT).

---

### Module 2: Structural Positions & Substantive Arenas (Weeks 3–5)

#### Week 3: Networks and Markets (Mon., Oct. 12)
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
  * Lizardo, O. (2023). Culture and Networks. Pp. 188–201 in *The SAGE Handbook of Social Network Analysis*. SAGE Publications Ltd.
  * Lewis, K., & Kaufman, J. (2018). The Conversion of Cultural Tastes Into Social Network Ties. *American Journal of Sociology*, 123(6), 1684–1742.
  * Fuhse, J. A., & Gondal, N. (2024). Networks from culture: Mechanisms of tie-formation follow institutionalized rules in social fields. *Social Networks*, 77, 43–54.
* **Part 2: Meaning Structures, Relational Sociology, and Publics**
  * Fuhse, J. A. (2009). The Meaning Structure of Social Networks. *Sociological Theory*, 27(1), 51–73.
  * Mützel, S., & Breiger, R. (2021). Duality Beyond Persons and Groups. In *The Oxford Handbook of Social Networks*. Oxford University Press.
  * Fuhse, J., & Mische, A. (2024). Relational Sociology: Networks, Culture and Interaction. In J. McLevey, J. Scott, & P. J. Carrington (Eds.), *The SAGE Handbook of Social Network Analysis* (2nd ed., pp. 55–71). SAGE.
* **Deliverables:**
  * Analytic Memo 6 (due Sun., Nov. 1 at 5:00 pm PT).
  * 📌 **Final Project Extended Abstract** (due Fri., Nov. 6 at 11:59 pm PT).

#### Week 7: Diffusion In Networks (Mon., Nov. 9)
* **Part 1: Complex Contagion, Clustering, and Spatial Diffusion**
  * Centola, D., & Macy, M. (2007). Complex Contagions and the Weakness of Long Ties. *American Journal of Sociology*, 113(3), 702–734.
  * DellaPosta, D., Shi, Y., & Macy, M. (2015). Why Do Liberals Drink Lattes? *American Journal of Sociology*, 120(5), 1473–1511.
  * Centola, D. (2015). The social origins of networks and diffusion. *American Journal of Sociology*, 120(5), 1295–1338.
  * Cheng, M., Smith, D. S., Ren, X., Cao, H., Smith, S., & McFarland, D. A. (2023). How new ideas diffuse in science. *American Sociological Review*, 88(3), 522–561.
* **Part 2: Associative Diffusion & Cultural Currents**
  * Goldberg, A., & Stein, S. K. (2018). Beyond Social Contagion: Associative Diffusion and The Emergence of Cultural Variation. *American Sociological Review*, 83(5), 897–932.
  * Bail, C. A., Brown, T. W., & Mann, M. (2017). Channeling hearts and minds: Advocacy organizations, cognitive-emotional currents, and public conversation. *American Sociological Review*, 82(6), 1188–1213.
* **Deliverable:** Analytic Memo 7 & Discussion Leader Questions (due Sun., Nov. 8 at 5:00 pm PT).

#### Week 8: Networks In History (Mon., Nov. 16)
* **Part 1: Robust Action, Mobilization, and Historical Networks**
  * Padgett, J. F., & Ansell, C. K. (1993). Robust Action and the Rise of the Medici, 1400–1434. *American Journal of Sociology*, 98(6), 1259–1319.
  * Gould, R. V. (1991). Multiple Networks and Mobilization In The Paris Commune, 1871. *American Sociological Review*, 56(6), 716–729.
  * Hillmann, H. (2008). Mediation in Multiple Networks: Elite Mobilization Before the English Civil War. *American Sociological Review*, 73(3), 426–454.
  * Becker, S. O., Hsiao, Y., Pfaff, S., & Rubin, J. (2020). Multiplex Network Ties and the Spatial Diffusion of Radical Innovations: Martin Luther's Leadership In The Early Reformation. *American Sociological Review*, 85(5), 857–894.
  * Erikson, E., & Feltham, E. (2021). Historical Network Research. In *The Oxford Handbook of Social Networks*. Oxford University Press.
* **Part 2: Global Trade, Malfeasance, and Historical Sequences**
  * Erikson, E., & Bearman, P. (2006). Malfeasance and the Foundations For Global Trade: The Structure of English Trade In The East Indies, 1601–1833. *American Journal of Sociology*, 112(1), 195–230.
  * Bearman, P., Faris, R., & Moody, J. (1999). Blocking The Future: New Solutions For Old Problems In Historical Social Science. *Social Science History*, 23(4), 501–533.
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

### B. Reading Schedule Page (`sociol-208b-reading-schedule-fall-2026`)
* Live schedule alert box.
* Links to internal Canvas Course Files for non-DOI PDFs (`https://bruinlearn.ucla.edu/courses/239524/files`).
* 10-week breakdown with Part 1 and Part 2 headers, exact DOIs for published articles (`[DOI ↗]`), and links to course files for books/chapters (`[Course Files ↗]`).

### C. Weekly Overview Pages (`week-[1-10]-readings-and-overview`)
* **Week Banner:** Blue gradient header card with week number, title, date, and room.
* **Quick Action Card:** Submission box link, due date, reading requirements (pick 1 from Part 1 and 1 from Part 2), Discussion Leaders board link.
* **Part 1 & Part 2 Cards:** Styled table-cards with reading list and links (`[DOI ↗]` or `[Course Files ↗]`).
* **Footer Navigation:** Links back to Modules and Full Schedule.

### D. Automated Deployment Scripts
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

# Global Agent Guidelines

**Location:** `~/.config/agents/AGENTS.md` (Update this file to persist lessons globally across all projects)

## General Coding Standards
- Write concise, readable code with descriptive naming over short abbreviations.
- Prefer functional paradigms and immutable data structures where practical.
- Always include unit tests when introducing new utility functions or endpoints.

## Git & Workflow
- Format all commit messages using Conventional Commits (`feat:`, `fix:`, `refactor:`).
- Keep changes scoped to the prompt; do not refactor unrelated code.

## Asynchronous & Background Execution Standards (CRITICAL)
- **The Five-Minute Rule**: Any operation, script, or transfer anticipated to take more than 5 minutes (or any operation involving large multi-megabyte/gigabyte file transfers or heavy compilations) **must** be executed as an asynchronous background process (e.g., `nohup ... > /tmp/task.log 2>&1 &`) rather than blocking the foreground interactive shell. This prevents hanging the terminal, exhausting timeouts, or stalling assistant responsiveness.
- **HPC / Hoffman2 Rsync Operations**: Never run large `rsync` transfers synchronously in the interactive terminal. Always launch file transfers from or to Hoffman2 in the background (e.g., `nohup rsync -avz hoffman2:remote_dir/ ./local_dir/ > /tmp/rsync_hoffman.log 2>&1 &`) and monitor progress via log inspection.

## Safety & Boundaries
- Never commit hardcoded secrets, `.env` files, or private keys.
- Always run the repository's test and lint suites before signaling task completion.

## Tool Selection & Execution Architecture: Avoiding Gemini API Turn-Mismatch Errors (CRITICAL)
- **The Turn-Ordering Failure Mode (`Got function response with name '...'`)**:
  - When the assistant runtime is backed by the Google Gemini API (or Google Vertex), the protocol enforces strict turn alternation: every `model` turn containing a `functionCall` must be followed **immediately** by a turn containing the corresponding `functionResponse`. No intermediate messages, console updates, or metadata turns may be interleaved.
  - In Positron, having an active, open interactive REPL (Python or R console session) causes Positron's IDE background worker to periodically and asynchronously poll workspace variables to inject the `<language_session>` state block into the turn buffer.
  - If a background `<language_session>` state update, console stream chunk, or prompt event is injected into the payload while a tool is executing or being packaged, it breaks turn adjacency, triggering the fatal error regardless of which tool was invoked:
    `Error making request: Please ensure that function response turn comes immediately after a function call turn. Got function response with name 'search' (or 'executeCode', etc.).`
- **Root-Cause Resolutions & Prevention Protocol**:
  1. **Close or Clear Active Interactive Consoles in Positron**:
     - The single most effective mitigation when using Gemini in Positron is to close or reset any active Python or R console session when interacting with the assistant. If the console has no active variables or is terminated, Positron ceases polling for `<language_session>` variables, preventing turn-order collisions.
  2. **Switch Backend Provider (Claude / OpenAI)**:
     - This failure mode is unique to Google Gemini's rigid 1:1 message turn alternation.
     - Anthropic (Claude 3.5 / 3.7 Sonnet) and OpenAI (GPT-4o) match tool calls and responses using explicit IDs (`tool_use_id` / `tool_call_id`) rather than strict message order, making them structurally immune to this turn-ordering collision even if Positron injects console state.
  3. **Strict File Operations Isolation (`search`, `read`, `edit`, `write`)**:
     - **NEVER** run ad-hoc R or Python code in `executeCode` to search, inspect, or modify files. Always use the dedicated filesystem tools.
  4. **Standalone Script & Pipeline Execution via `bash`**:
     - For running automation scripts, deployment pipelines, or non-interactive analysis scripts (e.g., `python3 deploy_all_to_canvas.py`, `python3 script.py`, `Rscript script.R`), **always execute them via the `bash` tool** in an isolated subshell.

## Quarto & Reporting Standards
- **Decoupled Compute & Fast Rendering Architecture (CRITICAL)**:
  - **Zero In-Document Model Execution**: Heavy statistical models (e.g., `brmsfit`, large MCMC posteriors, multi-gigabyte datasets) must **never** be loaded or sampled directly inside `.qmd` code chunks during compilation.
  - **Standalone Asset Serialization (`Plots/*.png`)**: Generate all figures via standalone, modular R extraction scripts (e.g., `scripts/generate_plots.R`, `scripts/extract_fixed_effects_stability.R`) and serialize publication-grade PNGs to `Plots/`. Reference them in `.qmd` using native markdown syntax (`![](Plots/my_figure.png){fig-align="center" width="100%"}`). This allows Pandoc to base-64 embed assets in milliseconds under `embed-resources: true` without invoking graphics device loops.
  - **Pre-Compiled Markdown Tables vs. Dynamic Table Engines**: Compute model fit comparisons, parameter estimates, and stability envelopes in extraction scripts, outputting them to `cache/*.csv` and embedding them as clean, static GitHub-flavored markdown tables in the `.qmd`. This completely bypasses heavy runtime HTML widget/table engines (`gt`, `kableExtra`, `DT`).
  - **Pure Pandoc AST Compilation**: Structuring the `.qmd` as pure Markdown/LaTeX without active `{r}` execution blocks allows Quarto to bypass knitr kernel startup, serializing multi-figure, multi-table reports to standalone HTML in under 2 seconds.
  - **Multi-Tiered Cache Architecture (`cache/`)**: Save intermediate tabular summaries and extracted draws to `cache/` (e.g., `fixed_effects_stability_summary.rds`, `random_slopes_stability_summary.rds`) so that modifying a specific figure only re-executes that single script without re-running the entire analytical pipeline.
  - **Decoupled Extraction and Plotting**: Never load heavy models (e.g., 60MB `brms` `.rds` files) directly in plotting scripts (`plot_*.R`). Create dedicated extraction scripts (`extract_*.R`) that load the model ONCE, compute marginal effects, predicted probabilities, or posterior draws, and save lightweight dataframes to `Cache/summaries/`. Plotting scripts must only load these lightweight dataframes.
  - **Parallel Execution**: Once plotting scripts are decoupled from heavy models and rely only on lightweight summaries, execute them concurrently using GNU `parallel` or `xargs -P` rather than sequentially.
- When rendering a Quarto document via the `bash` tool, be hyper-aware that rendering artifacts might trigger ghost file creation if Quarto writes to directories that are currently open in the editor or currently being crawled by background tasks.
- If using `<REPORT>` tags provided by the `report` skill, **do not manually duplicate** the `.qmd` file creation. The `<REPORT>` tags automatically serialize to disk. Mixing `cat > file.qmd` with `<REPORT>` output will create duplicate files (e.g. `file-1.qmd`) that break the `quarto render` logic.
- Avoid using `size` in `ggplot2` for line layers (`geom_line`, `geom_segment`, `geom_errorbar`); always use the modernized `linewidth` aesthetic to prevent deprecation warnings from cluttering the render logs.
- When applying robust standard errors to multi-state categorical models (like `nnet::multinom`), `lmtest::coeftest` struggles to return the structure. Manually extract the `vcovCL` diagonals and calculate the Z-scores and P-values via matrix arithmetic to ensure stable dataframe conversion.

## Overleaf & LaTeX Standards (CRITICAL)
- **Zero Local LaTeX Compilation When Connected to Overleaf**:
  - Whenever a project is connected to an Overleaf repository (via git remote, project URL, or shared repo), **NEVER compile or render `.tex` files locally** using `pdflatex`, `xelatex`, `lualatex`, `latexmk`, or `bibtex`.
  - The author renders, compiles, previews, and debugs PDFs directly online within Overleaf.
  - Running local compilation commands wastes execution time and tokens, generates noisy local build artifacts (`.aux`, `.bbl`, `.blg`, `.log`, `.out`, `.synctex.gz`, etc.) that risk cluttering the working tree, and creates unnecessary friction.
  - Agents must focus exclusively on clean, high-precision editing of source files (`manuscript.tex`, `references.bib`, figures in `Plots/`, tables) and pushing verified commits directly to the remote Overleaf branch (`git push overleaf HEAD:main`).
- **Overleaf Git Remote Push Syntax**:
  - Always push to Overleaf using the explicit refspec `git push overleaf HEAD:main` rather than a bare `git push overleaf main`. This guarantees that commits on the current local working branch (whether named `master`, `main`, or another working branch) are properly pushed and synchronized to Overleaf's expected `main` branch.
- **Document Class for Formal Journal Submissions (`article` vs. Quarto's `scrartcl`)**:
  - Quarto defaults to the KOMA-Script class `\documentclass{scrartcl}`, which imposes modern sans-serif typography (`\sffamily`) onto all section headings, document titles, and captions.
  - For formal sociology and network science journal submissions (e.g., *Social Networks*, *ASR*, *AJS*), standard LaTeX `article` (11pt Latin Modern font, 1.0-inch margins) or `\setkomafont{disposition}{\rmfamily}` must be used to preserve classic serif typography across all headings, titles, and running prose.
- **Modern TeX Live (2024/2025) & Overleaf Preamble Robustness**:
  - *Suppressing Kernel Warnings*: Newer TeX Live engines (2024+) emit noisy `\showhyphens` and kernel deprecation warnings during compilation. Load `\usepackage{silence}` and targeted warning filters in the preamble to suppress benign engine notifications and keep Overleaf logs clean.
  - *Preventing Bibliography Overfull Margin Spills*: Long DOIs, URLs, and multi-author citations in `thebibliography` frequently trigger overfull `\hbox` errors where references bleed past the 1.0-inch right margin into the page edge. Inject `\sloppy` directly into the bibliography environment via `etoolbox`'s `\apptocmd{\thebibliography}{\sloppy}{}{}`. This loosens line-breaking tolerances strictly within the reference list without relaxing tolerances in the body text.
- **Manuscript Revision Lifecycle & Historical Baseline Preservation (CRITICAL)**:
  - During journal revision cycles (e.g., Major or Minor Revision / R&R), **NEVER overwrite or edit `manuscript.tex`**.
  - `manuscript.tex` must remain permanently frozen as the historical, as-submitted baseline version originally evaluated by the editor and reviewers.
  - All active revisions, text overhauls, and new models belong exclusively in revision-tagged files (e.g., `manuscript_R1.tex`, `manuscript_R2.tex`).
  - This preserves the exact baseline needed for git history, automated diffing (`latexdiff manuscript.tex manuscript_R1.tex`), and verifying reviewer quotation line numbers without destructive rollbacks.

## Supercomputing & HPC Integration (UCLA Hoffman2)
The local machine is fully configured to deploy computationally intensive R jobs (e.g., Bayesian mixture models, large simulations) to the **UCLA Hoffman2 Cluster**.

### Deployment Workflow
When asked to run a model on Hoffman2, you must do the following from the bash tool:
1. **Create the Project Directory on Hoffman2:**
   `ssh -o BatchMode=yes hoffman2 "mkdir -p my_project/Scripts my_project/dta"`
2. **Write the `.sh` Submit Script Locally:** (Use a standard Grid Engine `qsub` template)

### Grid Engine (.sh) Script Template
When writing `.sh` SGE submission scripts to run models on the cluster from scratch, ALWAYS use this exact structure to guarantee the toolchain compiles CmdStan flawlessly across array tasks and stays under the 24-hour limit:

```bash
#!/bin/bash
#$ -cwd
#$ -j y
#$ -o output_job.log
#$ -l h_rt=23:50:00   # CRITICAL: Always bound to just under 24 hours
#$ -l h_data=4G       # Tightly restrict RAM per core (e.g. 4G per core)
#$ -pe shared 4       # Number of cores

# CRITICAL: Must initialize the module system first in non-interactive Grid Engine shells
source /u/local/Modules/default/init/bash

# Must load modern GCC before R
module load gcc/10.2.0
module load R

# Pass allocated cores to R
export CMDSTANR_CORES=$NSLOTS
export cmdstanr_no_ver_check=TRUE

# Stagger concurrent array tasks by 15 mins to avoid compile races
if [ ! -z "$SGE_TASK_ID" ] && [ "$SGE_TASK_ID" -eq 2 ]; then
  sleep 900
fi

# Example R command:
Rscript Scripts/your_model.R
```
3. **Sync Data and Scripts via rsync:**
   `rsync -avz my_data.dta hoffman2:my_project/dta/`
   `rsync -avz Scripts/my_model.R Scripts/submit_job.sh hoffman2:my_project/Scripts/`
4. **Submit the Job via SSH:**
   `ssh -o BatchMode=yes hoffman2 "cd my_project && qsub Scripts/submit_job.sh"`

### Hoffman2 Best Practices & Gotchas
- **Cluster Hygiene (CRITICAL)**: Never run `qdel` on Hoffman2 unless you explicitly created the job ID yourself during your current session, or the user explicitly commands you to kill a specific ID. The user runs multiple concurrent jobs for different projects that must not be disrupted.
- **Queue Optimization (Avoiding Indefinite Waits & "Forever Queues")**: 
  - Hoffman2's maximum time limit for the general campus base pool is **24 hours**. Requesting `h_rt > 24:00:00` automatically traps the job in a permanent queue unless you have dedicated physical node hardware (`highp` queues). 
  - To maximize compute time while guaranteeing the fair-share backfill scheduler places your job:
    1. **Always bound time to just under the limit** (e.g., `#$ -l h_rt=23:50:00`).
    2. **Tightly restrict memory to exactly what is needed per core** (e.g., `#$ -l h_data=3G` when using 16 cores) to ensure the total footprint doesn't block the scheduler.
  - *Note on Checkpointing:* While standard jobs can checkpoint and resume, `brms` (NUTS sampler) cannot resume NUTS adaptation mid-warmup. Thus, you must allocate sufficient cores (`threading(4)`) to ensure the model finishes within the 24-hour limit.
- **Array Job Strategies**: For iterating across independent datasets or running sequential model blocks rapidly, use Array Jobs (e.g., `#$ -t 1-N` or `run_on_hoffman script.R 8 12 4G 1-10`). This slices large requests into smaller chunks that backfill through the queue instantly.
  - *Staggering Locks*: When submitting an Array Job to a fresh environment, concurrent tasks will race to write to the `renv/library` directory, causing a `00LOCK-renv` crash. Always add a bash `sleep` stagger in the submit script (e.g., `sleep $(( (SGE_TASK_ID - 1) * 600 ))`) so Task 1 can finish building the library before subsequent tasks wake up.
- **Bypassing Obscure renv Compilation Crashes & CmdStan Linker Errors**: When restoring a massive project lockfile from source on Hoffman2, obscure downstream dependencies (like `QuickJSR`, `bslib`, or HTML widgets) often fail to compile and crash the entire pipeline. For raw modeling runs, bypass `renv::restore()` in the SGE script entirely. Instead, use base R to manually `install.packages('brms')` and `cmdstanr`. **CRITICALLY**, if you see Intel TBB linker errors (`undefined reference to tbb::interface...`) during model compilation, it means a stale `~/.cmdstan` directory was compiled under a different toolchain. Force a native compilation with `overwrite = TRUE` so CmdStan links against the currently loaded `gcc/10.2.0` and `tbb` modules. **However, in an Array Job, NEVER let all tasks run this concurrently** (they will overwrite and delete each other's source files). Wrap the call so only Task 1 performs the installation (`if(as.integer(Sys.getenv("SGE_TASK_ID", 1)) == 1) { cmdstanr::install_cmdstan(...) }`), and ensure the bash `sleep` stagger for subsequent tasks is at least 10 minutes (`600` seconds) so compilation finishes.
- **C++ Compilation Errors**: Hoffman2's default `R` module uses an outdated 2015 compiler (`gcc-4.8.5`). If you manually install packages on the cluster (or if `renv::restore()` is running), you *must* load a modern compiler (e.g., `module load gcc/10.2.0`) before loading R. Also load `module load cmake` to prevent `RcppParallel` installation failures. The `run_on_hoffman` script handles this automatically, preventing notorious C++11 literal spacing errors (e.g., `operator""_xl`) when compiling packages like `tidyr`, `dplyr`, or `brms`.
- **Bulletproof Hoffman2 SGE Template for brms**: When writing `.sh` SGE submission scripts to run models on the cluster from scratch, ALWAYS use this exact structure to guarantee the toolchain compiles CmdStan flawlessly across array tasks:
  ```bash
  # Must load modern GCC before R
  source /u/local/Modules/default/init/bash
  module load gcc/10.2.0
  module load R

  # CRITICAL: Prevent Hoffman's global TBB module from overriding CmdStan's internal TBB
  
  
  # CRITICAL: Stagger concurrent tasks by at least 15 minutes (900 seconds) 
  # so Task 1 can cleanly compile both CmdStan AND the first brms C++ model 
  # without Task 2 racing it to delete shared temporary compiler objects (e.g. main_threads.o)
  if [ "$SGE_TASK_ID" -eq 2 ]; then
    sleep 900
  fi
  
  # Pass allocated cores to R
  export CMDSTANR_CORES=$NSLOTS
  
  # CRITICAL: DO NOT export CMDSTAN in bash! If the directory is missing/empty, 
  # cmdstanr's .onLoad sequence crashes with an obscure `endsWith()` error.
  # Instead, export only the version check skip, and set the path safely inside R.
  export cmdstanr_no_ver_check=TRUE
  
  # Ensure ONLY Task 1 installs the CmdStan backend natively. 
  # Pin version to 2.33.1 to avoid the stanc --name bug with brms.
  # Force overwrite to avoid TBB linker crashes from stale builds.
  Rscript -e "
    options(repos = c(CRAN = 'https://cloud.r-project.org'))
    if (!requireNamespace('brms', quietly = TRUE)) install.packages('brms')
    if (!requireNamespace('cmdstanr', quietly = TRUE)) install.packages('cmdstanr', repos = c('https://mc-stan.org/r-packages/', getOption('repos')))
    
    # Load library FIRST, then set the path safely inside R
    library(cmdstanr)
    cmdstanr::set_cmdstan_path('~/.cmdstan/cmdstan-2.33.1')
    
    if(as.integer(Sys.getenv('SGE_TASK_ID', 1)) == 1) { 
      cmdstanr::install_cmdstan(version = '2.33.1', cores = Sys.getenv('NSLOTS', unset = 4), overwrite = TRUE) 
    }
  "
  Rscript Scripts/your_model.R
  ```
- **Dynamic Threads**: R scripts submitted to Hoffman must dynamically read `$NSLOTS` (e.g., `Sys.getenv("CMDSTANR_CORES")`) and calculate `threads_per_chain = floor(NSLOTS / 4)` to ensure `brms` fully utilizes the allocated node without sitting idle.
- **Authentication & SSH Config**: Passwordless SSH is fully configured for Hoffman2. The config file is located at `~/.ssh/config` (which sets the `hoffman2` alias, username `olizardo`, and keep-alive intervals). It relies on the `ed25519` cryptographic keys in the same `~/.ssh/` directory. AI agents MUST seamlessly use `ssh -o BatchMode=yes hoffman2 "command"` to directly interact with the cluster without prompting the user. Do not alter this configuration.

## R & Bayesian Modeling Practices
- **mclogit & mblogit**: 
  - When specifying crossed random effects in `mclogit::mblogit`, you **must** pass them as a list (e.g., `random = list(~ 1|individual_id, ~ 1|cluster_id)`). Using the `lme4` syntax (`~ 1|id + 1|cluster_id`) will crash with a `model frame and formula mismatch in model.matrix()` error.
- **Handling `renv` Sync Issues**:
  - When using Quarto/RMarkdown documents that require external compilation engines (like `rmarkdown` or `knitr`), ensure those packages are explicitly installed and snapshotted (`renv::install("rmarkdown"); renv::snapshot()`). Even if the scripts don't directly `library(rmarkdown)`, the `renv` environment requires them to render documents properly.
  - **Implicit Dependencies (e.g., `cmdstanr`)**: If a package is only passed as a string argument (e.g., `backend = "cmdstanr"` in a `brms::brm()` call), `renv`'s dependency discovery will miss it. Always add `library(cmdstanr)` explicitly at the top of your script before running `renv::snapshot()`. Otherwise, remote cluster runs using `renv::restore()` will fail because the package is absent from the lockfile.
- **Bayesian Mixture Models (brms)**:
  - **Label Switching**: Finite mixture models in Stan suffer from "label switching." Always apply ordered constraints (e.g., `order = "mu"`) when defining the mixture families to ensure chains converge to the same latent classes.
  - **Posterior Collapse (Random vs. Fixed Effects)**: Be extremely careful when using crossed random effects (`(1 | category)`) inside latent mixture distributions. Highly dense parameter spaces can cause the sampler to "give up" (shrink variance to zero), leading to posterior collapse and erasing group heterogeneity. Switching group-level variables to **fixed effects with interactions** (`category + time:category`) drastically improves stability and trajectory identification, despite increasing run times.
  - **Model Comparison (LOO vs WAIC & Socket Timeouts)**: While LOO-CV (`add_criterion(fit, "loo")`) is theoretically preferred over WAIC or information criteria (AIC/BIC) for finite mixture models (as the mathematical proofs for AIC/BIC break down in bounded mixture spaces), computing exact or approximate LOO-CV on complex models with many cores (e.g., 16) causes `parallel::makePSOCKcluster()` to crash with network socket timeouts on HPC nodes, destroying the model object *after* sampling completes but *before* saving. 
    - **Crucial Rule:** If you must use LOO, strictly limit it to `cores = 4` or fewer (e.g., `add_criterion(fit, "loo", cores = min(num_cores, 4))`). Alternatively, fall back to `WAIC` (`add_criterion(fit, "waic")`) to drastically reduce memory usage and completely bypass parallel socket timeouts.
    - **Crucial Rule 2 (Atomic Saving & Wall Limits):** NEVER chain NUTS sampling and `add_criterion()` in memory on HPC clusters. ALWAYS use the `file = "..."` argument natively inside `brm()` so the multi-hour posterior samples are immediately and atomically serialized to disk the second sampling finishes. Only *after* `brm()` saves the file should you call `add_criterion()` to compute fit statistics. This ensures that if the LOO/WAIC calculation crashes or hits an HPC 24h wall limit, the raw posterior draws are perfectly preserved.
  - **Adjacent Category Dispersion**: When fitting `brms` Adjacent Category models (`family = acat()`) that model variance/dispersion (`disc ~ ...`), the response variable *must* be an explicit `ordered` factor (e.g., `ordered(y)`). Unordered factors or integers will cause `brms` to crash during internal Stan data compilation.
- **Local vs Remote Execution**: Never accidentally include HPC-bound heavy models (like variance/dispersion SGE jobs) in local background queues (e.g., `systemd`). This will silently hang or starve the local machine. Strictly separate local queues from Hoffman submission scripts.

## Longitudinal Panel & Network Modeling Practices

### 1. Counterfactual Predicted Probabilities from Fixed-Effects Logit via Root-Finding (`uniroot`)
- **The Challenge**: In conditional logit and fixed-effects binary logit models (`survival::clogit` or `fixest::feglm`), unit-specific intercepts $\alpha_i$ are conditioned out of the likelihood via sufficient statistics ($m_i = \sum_{t=1}^{n_i} y_{it}$). Consequently, the model does not estimate unit intercepts directly, making it impossible to compute raw predicted probabilities or marginal effects on the $0\text{--}1$ probability scale using standard `predict()` methods.
- **The Solution**: For each informative unit $i$ (where $0 < m_i < n_i$), solve for the latent fixed effect $\hat{\alpha}_i$ by finding the root that sets the sum of predicted probabilities equal to the observed event count:
  $$\sum_{t=1}^{n_i} \frac{1}{1 + \exp(-(\hat{\alpha}_i + \mathbf{x}_{it}\hat{\boldsymbol{\beta}}))} = m_i$$
- **R Implementation**:
  ```r
  alpha_map <- df_varying %>%
    group_by(egoid) %>%
    summarise(
      m_i = first(m_i),
      alpha = uniroot(
        function(a) sum(plogis(a + eta_no_alpha)) - m_i,
        interval = c(-20, 20)
      )$root,
      .groups = "drop"
    )
  ```
- **Counterfactual Scenarios**: Merging $\hat{\alpha}_i$ back onto the cohort allows generating valid within-unit counterfactual probability plots and marginal effects across continuous or categorical predictors holding unobserved individual heterogeneity fixed.

### 2. Defending Trait and Disposition Durability (ICCs & Test-Retest Reliability)
- **Reviewer Critique**: In panel studies examining cultural tastes, psychological traits, or survey attributes as predictors of relational survival or behavioral outcomes, reviewers regularly challenge the assumption of temporal stability, suggesting traits may be endogenous or transient states.
- **Methodological Defense**: Before modeling relational outcomes, estimate two-level random intercept models (`lme4::lmer`) across panel waves to decompose total variance into between- vs. within-individual components:
  1. **Intraclass Correlation Coefficients (ICCs)**: Explicitly report that trait variance is predominantly stable between individuals ($ICC > 0.50\text{--}0.80$) rather than noisy within-individual fluctuation.
  2. **Test-Retest Stability Matrices**: Report correlation coefficients ($r$) across 1-, 2-, and 3-year observation windows.
  3. **Scale-Step Stability Percentages**: Calculate the proportion of repeated measurements that remain within $\pm 1$ scale step on ordinal items (typically $>95\%$), establishing empirical durability.

### 3. Decomposing Dyadic Homophily: Positive Enthusiasm vs. Mutual Disinterest
- **Reviewer Critique**: In homophily, matching, or social selection research, reviewers often question whether "matching" reflects shared cultural capital or merely mutual disinterest / shared negative avoidance (e.g., both partners checking "never/dislike").
- **Methodological Defense**: Disaggregate composite matching indices into mutually exclusive or competing dimensions:
  1. *Positive exact matching* (both partners actively participating or liking).
  2. *Strong positive enthusiasm* (both partners reporting maximum engagement/taste).
  3. *Shared disinterest / mutual dislikes* (both partners reporting zero engagement or disliking).
- Present sensitivity specifications demonstrating that tie retention, selection, or closeness is driven by positive cultural capital and shared enthusiasm rather than shared absence or disinterest.

### 4. Auditing Alter-Ego Crossover & Two-Way Dyadic Clustering in Egocentric Panels
- **The Issue**: In egocentric network panels sampled from bounded communities (e.g., university cohorts, schools, organizations), alters nominated by one ego may appear across multiple ego networks or may be egos themselves. Reviewers frequently challenge standard single-level or ego-clustered models for ignoring cross-dyad dependence.
- **Methodological Protocol**:
  1. Audit sample overlap directly: report the exact count and percentage of crossed alters and dyad-periods.
  2. Estimate **cross-classified multilevel models** (`(1 | egoid) + (1 | alterid)`) alongside undirected dyadic clustering.
  3. Demonstrate that the crossed alter random variance is negligible and that substantive fixed-effect estimates are invariant to dyadic cross-classification.

### 5. Tie Sequence Combinatorics & Rekindling in Discrete-Time Hazard Models
- **The Issue**: Panel network studies often exhibit non-contiguous tie sequences (e.g., $1 \to 0 \to 1$, where a tie decays and later rekindles). Counting rekindled ties as continued "survival" invalidates discrete-time event-history risk-set assumptions.
- **Methodological Protocol**:
  1. **Audit Combinatorics**: Enumerate and report the empirical distribution of binary tie histories across all waves (e.g., how many of the $2^W$ theoretically possible sequences occur; proportion contiguous vs. intermittent).
  2. **Formalize Risk Sets**: Specify that tie decay terminates the initial spell. Any subsequent rekindling initiates a new spell with re-measured baseline covariates.
  3. **Absorbing First-Dissolution Sensitivity**: Supplement full multi-spell models with an absorbing first-decay model restricted to the initial spell ($N_{\text{first spell}}$) to confirm that findings do not depend on rekindling dynamics.


## Google Drive & Word Manuscript Table / Figure Synchronization
For projects where manuscripts, tables, and figures are synced with Google Drive / Microsoft Word (`.docx`):

### 0. Strict Non-Interactive & Autonomous Execution Protocol (CRITICAL)
- **Zero Permission Prompts (`dangerReason`)**: For routine file reading, script execution, data loading, or executing the `sync_*.R` / `sync_*.py` pipeline, **NEVER** set the `dangerReason` parameter in `executeCode` or `bash` tool calls. Populating `dangerReason` triggers unnecessary UI confirmation modals.
- **Immediate Turnkey Execution**: Whenever asked to revise, edit, add to, or update a Google Doc manuscript, do not hesitate, prompt for permissions, or perform destructive pandoc conversions. Immediately execute the established in-place OpenXML round-trip pipeline (`Rscript Scripts/sync_*.R`).
- **Seamless Authentication**: `googledrive::drive_auth(email = "omarlizardo@gmail.com")` uses cached tokens in `~/.cache/gargle/` and runs non-interactively without user prompts.

### 1. The Google Drive In-Place Injection Pipeline (CRITICAL)
- **Zero Style Disruption**: To completely preserve the live manuscript's typography, fonts, heading hierarchy, margins, line spacing, track changes, and collaborator comments, **never re-upload or overwrite the whole document via Pandoc conversion**.
- **The Drive Round-Trip Protocol**:
  1. Download the live draft via `googledrive::drive_download(as_id(DOC_ID), path = "draft_live.docx", overwrite = TRUE)`.
  2. Perform surgical XML injection on `word/document.xml`, `word/_rels/document.xml.rels`, and `word/media/` locally.
  3. Upload the updated document directly back to Drive via `googledrive::drive_update(as_id(DOC_ID), media = "draft_updated.docx")`.
- **Two Update Modes (Initial Insertion vs. Automatic Re-Sync)**:
  - **Initial Tag Injection**: Authors place tags wrapped in double curly braces where assets belong (e.g., `{{TABLE_1}}` or `{{PLOT_FOREST_M7}}`). The script replaces the tag paragraph with the native OpenXML table or plot image.
  - **Automated Caption-Anchored Updates (No Re-Tagging Required)**: Once a table or figure is in the document, subsequent model/data updates do **not** require re-inserting tags. The script automatically matches standard captions (e.g., `Table 1.`, `Figure 2.`) and replaces the adjacent `<w:tbl>` or `<w:drawing>` in-place with the latest version.

### 2. OpenXML Schema Compliance & Character Escaping Rules (Preventing 400 Bad Request)
- **Mandatory XML Character Escaping**: All text inserted into table cells, headers, or captions **must** be XML-escaped (`<` to `&lt;`, `>` to `&gt;`, `&` to `&amp;`, `"` to `&quot;`). For example, unescaped p-values like `<0.001` produce `<w:t><0.001</w:t>`, which corrupts the XML syntax and causes Google Drive's import filter to fail with `400 Bad Request`.
- **Strict ECMA-376 Tag Ordering**:
  - Inside `<w:pPr>`: `<w:suppressAutoHyphens/>` -> `<w:spacing/>` -> `<w:ind/>` -> `<w:jc/>`. (Out-of-order elements violate XML schemas and trigger upload errors).
  - Inside `<w:tcPr>`: `<w:tcW/>` -> `<w:tcBorders/>` -> `<w:noWrap/>`.

### 3. Figure Injection, Relationship Mapping & Exact Aspect Ratios
- **Strict Relationship Tracing**: In OpenXML, drawing elements (`<w:drawing>`) reference image files via relationship IDs (`r:embed="rIdX"`). Never assume the order of `rId`s matches the order of `imageX.png` files or figure appearance. Always parse `word/_rels/document.xml.rels` to map `rIdX` -> `media/imageY.png` and confirm with the adjacent caption text (`Figure 1.`, `Figure 2.`).
- **Dual DrawingML Extent Synchronization**: When updating an image, **both** `<wp:extent cx="..." cy="..."/>` and `<a:ext cx="..." cy="..."/>` in `word/document.xml` **must** be updated simultaneously to match the image's exact native aspect ratio:
  - Width is set to full printable text width ($6.5 \text{ inches} = 5,943,600 \text{ EMUs}$).
  - Height in EMUs: $\text{height\_EMU} = \text{round}(5,943,600 \times (\text{pixel\_height} / \text{pixel\_width}))$.
  - Failing to synchronize extents causes Google Docs to stretch/squish images into old container dimensions.

### 4. Universal APA Table Style & Formatting Standards (MANDATORY)
All manuscript tables injected into Google Docs / Word documents across all projects must strictly conform to these formatting specifications:

1. **Width & Proportional Column Allocations**:
   - Total table width must scale to full **6.5-inch printable portrait width** (`w:w="9360" w:type="dxa"`).
   - **Column 1 (Row Labels / Models)**: Must be allocated wider space (~36–42% of total table width = 3,400–3,900 dxa) to prevent multi-line text wrapping.
   - **Numeric / Statistic Columns**: Remaining table width (58–64%) is divided proportionally across subsequent columns.
   - Define exact `<w:gridCol w:w="..."/>` in `<w:tblGrid>` and `<w:tcW w:w="..." w:type="dxa"/>` on each table cell.

2. **Column Title Length & Anti-Squish Budgeting**:
   - For narrow columns (under 0.8 in / 1,100 dxa), keep column headers concise (e.g., `Model`, `Under`, `Over`, `Both`, `Par`, `WAIC (SE)`, `ΔWAIC`) to fit without line breaks or column squishing.
   - Embed standard errors in parentheses in the same cell (`WAIC (SE)` or `Est (SE)`) rather than creating separate SE columns.

3. **Anti-Word-Break & Hyphenation Controls (Fit Whole Words to Columns)**:
   - **No Mid-Word Splitting**: Every paragraph inside table cells must include `<w:suppressAutoHyphens/>` in `<w:pPr>` to strictly prevent words from breaking or hyphenating mid-word across lines.
   - **No Wrap on Numbers**: Include `<w:noWrap/>` in `<w:tcPr>` for all numeric/statistic cells so numbers, estimates, and confidence intervals stay strictly on a single line.

4. **Horizontal Text Alignment**:
   - **Column 1 (Row Labels)**: Strictly left-justified (`<w:jc w:val="left"/>`).
   - **All Other Columns (Estimates, Statistics, Percentages)**: Strictly center-justified (`<w:jc w:val="center"/>`).

5. **Pagination & Page Break Controls**:
   - **Row Protection**: Every table row (`<w:trPr>`) must include `<w:cantSplit/>` to prevent individual rows from being sliced across page breaks.
   - **Repeating Header Rows**: The header row must include `<w:tblHeader/>` so column headers repeat automatically when a table spans multiple pages.

6. **Paragraph Indentations & Spacing**:
   - **Zero Indentation**: Strip all paragraph indentations from inside the table environment (`<w:ind w:left="0" w:right="0" w:firstLine="0" w:hanging="0"/>`).
   - **Tight Vertical Spacing**: Set zero before/after paragraph spacing (`<w:spacing w:before="0" w:after="0"/>`).

7. **Decimal Precision & Number Formatting**:
   - **Percentages**: Format strictly to **1 decimal place** (e.g., `79.9%`, `65.4%`).
   - **Model Estimates & Odds Ratios**: Format strictly to **2 decimal places** (e.g., `0.35`, `1.42`, `-0.18`), with directional significance bolding where appropriate.
   - **Standard Errors / Confidence Intervals**: Format strictly to **2 decimal places** (e.g., `(0.04)`).
   - **Sample Sizes (N, J)**: Format as integers with comma separators (e.g., `10,695`).

8. **APA 7th Horizontal Borders & Cell Padding**:
   - **Horizontal Rules**: 1pt top border (`sz="8"`), 0.5pt header-bottom border (`sz="4"`), 1pt table-bottom border (`sz="8"`).
   - **Zero Vertical Borders**: Set vertical and interior vertical borders to `w:val="none"`.
   - **Cell Padding (Margins)**: Top and bottom padding set to `120` dxa (6pt); left and right padding set to `160` dxa (8pt).

9. **Cross-Group & Multi-Category Layout (Vertically Stacked Panels vs. Horizontal Compression)**:
   - **Avoid Horizontal Squeezing**: When comparing multiple groups across several categorical levels, avoid side-by-side columns (which creates 7–11 narrow columns under 0.6 inches wide).
   - **Vertically Stacked Panels**: Stack groups vertically as distinct panels (*Panel A: Group 1*, *Panel B: Group 2*) sharing the same top column headers. Use a full-width spanning section header row (`<w:gridSpan w:val="N"/>`) with bold/italic title (`<w:b/><w:i/>`), and indent sub-item row labels in Column 1 (`<w:ind w:left="140"/>`).

10. **Model Parameter Column Naming**:
    - In model fit and specification comparison tables, standardly name the parameter count column **`Par`** or **`N. Par`** (rather than `Params` or `Parameters`) to maintain concise, consistent APA presentation.

### 5. Authentication
- Use `googledrive::drive_auth(email = "omarlizardo@gmail.com")`. Cached gargle tokens in `~/.cache/gargle/` provide seamless, non-interactive authentication.

### 6. Intermediate File Cleanup & Workspace Hygiene (MANDATORY)
- **Zero Artifact Leaks**: The Drive download, OpenXML DOM injection, and Pandoc text verification pipeline generates transient `.docx`, `.txt`, and `.json` scratch files (e.g., `draft_live.docx`, `draft_updated.docx`, `draft_verify.txt`, `replacements.json`). These files must never be allowed to accumulate in the workspace.
- **Automated `on.exit()` / `try...finally` Cleanup**:
  - In R master driver scripts (`sync_manuscript.R`), register an `on.exit()` handler to ensure scratch files are purged even if an error occurs mid-execution:
    ```r
    on.exit({
      unlink(Sys.glob("draft_*.docx"))
      unlink(Sys.glob("draft_*.txt"))
      unlink(Sys.glob("*.tmp"))
      unlink("replacements.json")
    }, add = TRUE)
    ```
  - In Python scripts (`sync_manuscript.py`), implement a `cleanup_scratch_files()` utility with a `--cleanup` command-line argument:
    ```python
    import glob, os

    def cleanup_scratch_files(keep=None):
        keep_set = set(os.path.abspath(f) for f in (keep or []) if os.path.exists(f))
        patterns = ["draft_*.docx", "draft_*.txt", "replacements.json", "*.tmp"]
        for pat in patterns:
            for f in glob.glob(pat):
                if os.path.abspath(f) not in keep_set:
                    try: os.remove(f)
                    except OSError: pass
    ```
- **Standard `.gitignore` Rules**:
  Every project implementing this workflow must include the following rules in `.gitignore`:
  ```gitignore
  # Drive sync intermediate files
  draft_*.docx
  draft_*.txt
  draft.docx
  draft_updated.docx
  draft_live.docx
  replacements.json
  *.tmp
  .tmp/
  ```

### 8. Document Styles, Paragraphs, Headings, and Caption Normalization Standards (MANDATORY)
When updating manuscripts in Google Docs / Word via OpenXML injection, scripts must maintain total stylistic fidelity with the document's native styles (`docDefaults` and `styles.xml`). Never allow disparate paragraphs or heading levels to diverge:

1. **Title Page Protection Standards (Title, Subtitle, Author, Date, Spacing)**:
   - **Preserve Title Styles**: Any paragraph styled with `Title` or `Subtitle` (or representing the document title) must retain `<w:pStyle w:val="Title"/>` or `Subtitle`, zero first-line indent (`<w:ind w:left="0" w:right="0" w:firstLine="0" w:hanging="0"/>`), and center alignment (`<w:jc w:val="center"/>`).
   - **Protect Title Page Metadata**: All cover page metadata paragraphs preceding the first substantive section heading (`Heading1` / `Introduction`)—including author names, affiliations, survey team labels, dates, and empty vertical spacing paragraphs—must retain center alignment (`jc="center"`) and zero first-line indent (`firstLine="0"`), strictly preventing standard 0.5-inch body indentation from corrupting the title page layout.
   - **Section Page Break Preservation**: Section breaks (`<w:br w:type="page"/>` or `<w:pageBreakBefore/>`) separating the title page from the main body must never be deleted.

2. **Surgical Additions & Terminological Precision vs. Zero Wholesale Rewriting (CRITICAL)**:
   - Once a manuscript is live on Google Drive and authors are actively revising, scripts and agents must treat the downloaded document as the canonical source of truth for text, subheadings, and structure.
   - **Zero Text Rewriting During Formatting Passes (MANDATORY)**: When performing formatting, style normalization, typography adjustments, line spacing changes, or indentation fixes, **NEVER alter, rewrite, condense, summarize, or replace the underlying prose text** of the manuscript or abstract. Formatting scripts must operate strictly on paragraph properties (`<w:pPr>`), run properties (`<w:rPr>`), styles (`styles.xml`), and document defaults (`docDefaults`), leaving the textual content of `<w:t>` elements 100% untouched. Any substantive text revisions or length reductions (such as shortening the abstract) must be handled through explicit user review or direct author drafting, never bundled into automated style or formatting passes.
   - **Targeted Additions & Terminological Adjustments Are Encouraged**: It is fully appropriate to insert newly requested literature citations, targeted conceptual arguments, hypotheses, table/figure references, and surgical terminological adjustments (e.g., standardizing *beliefs in objective taste* or *experiences of beauty*, and purging improper uses of *democratic*).
   - **Zero Wholesale Rewriting**: Never replace entire existing paragraph blocks with hardcoded local versions that discard or revert the author's live prose edits, phrasing choices, or stylistic revisions in the live Google Doc. Operate in surgical, non-destructive in-place mode.

3. **Body Prose Paragraphs (Normal Document Style Fidelity)**:
   - **Inherit Native Defaults**: All standard body prose paragraphs must follow the document's **Normal** style (`<w:pStyle w:val="Normal"/>` or default).
   - **Mutual Exclusivity of First-Line Indent and Hanging Indent in OpenXML (CRITICAL)**:
     - In ECMA-376 OpenXML schema and Google Docs' Word import engine, `w:firstLine` (positive first-line indentation) and `w:hanging` (hanging indentation) are mutually exclusive opposites.
     - When enforcing a first-line indent (`<w:ind w:left="0" w:right="0" w:firstLine="720"/>`), **NEVER specify `w:hanging="0"`**. If `w:hanging` is present in the XML element attributes, it must be explicitly deleted (`del ind.attrib['{...}hanging']`). If both `firstLine` and `hanging="0"` are present, Google Docs treats the paragraph as having a 0-width hanging indent, which completely wipes out and zeroes the first-line indent (`firstLine="0"`).
     - Conversely, for references requiring a hanging indent, specify strictly `<w:ind w:left="720" w:hanging="720" w:right="0"/>` without any `w:firstLine` attribute.
   - **Strip Hardcoded Paragraph Overrides**: Remove direct paragraph-level overrides on `w:ind` (e.g., `firstLine="0"`) and `w:spacing` (e.g., `line="240"`, `spacing after="180"` or `spacing before="180"`), so that body paragraphs cleanly inherit the local document style defaults (typically 11pt/12pt font, 1.5 line spacing `line="360"`, 0.5-inch first-line indent `firstLine="720"`, full justification `jc="both"`).
   - **Clean Direct Run Overrides**: Strip accidental whole-paragraph bolding (`<w:b/>`), stray shading/highlighting (`<w:shd>`), stray baseline alignments (`<w:vertAlign w:val="baseline"/>`), and hardcoded font/size/color overrides (`<w:rFonts>`, `<w:sz>`, `<w:color>`) so body text renders in the document's default font family and color (e.g., `Alegreya Sans`, 11pt, `#222222`).
   - **Preserve Genuine Semantic Markup**: Always preserve inline semantic formatting (such as `<i>` for italics like *ex ante*, quotes, book/work titles, and statistical symbols $N$, $J$, $\text{WAIC}$, $\text{OR}$, $\Delta$, $p < 0.001$).

4. **Heading Level & Style Hierarchy**:
   - **Level 1 Headings (`Heading1`)**: Style: `<w:pStyle w:val="Heading1"/>`, `<w:ind w:left="0" w:right="0" w:firstLine="0" w:hanging="0"/>`, left-aligned. Strip direct run-level bold/color/size overrides so headings cleanly inherit the document's native `Heading1` style definition (e.g., 16pt, dark red `#943634`, spacing before 300 / after 60).
   - **Level 2 Headings (`Heading2`)**: Style: `<w:pStyle w:val="Heading2"/>`, `<w:ind w:left="0" w:right="0" w:firstLine="0" w:hanging="0"/>`, left-aligned. Strip direct run-level bold/color/size overrides so headings cleanly inherit the native `Heading2` style definition (e.g., 14pt, dark red `#943634`, spacing before 200).
   - **Level 3 Headings (`Heading3`)**: Style: `<w:pStyle w:val="Heading3"/>`, `<w:ind w:left="0" w:right="0" w:firstLine="0" w:hanging="0"/>`, left-aligned. Strip direct run-level bold/size/color overrides (inheriting 12pt italic dark red `#943634`, spacing before 60).
   - **Purge Ghost/Empty Heading Paragraphs**: Automatically scan for and delete any blank/empty paragraphs styled with `Heading1`, `Heading2`, or `Heading3`.

5. **Table Captions, Figure Captions, and Notes**:
   - **Table Captions (`Table X. ...`)**: Zero first-line indent (`<w:ind w:left="0" w:right="0" w:firstLine="0" w:hanging="0"/>`), bold caption text (`<w:b/>`), left-aligned, tight vertical spacing (e.g., `before="60" after="80"`).
   - **Figure Captions (`Figure X. ...`)**: Captions must be **purely descriptive** of what the figure displays without interpretative results claims. Styled with zero first-line indent (`<w:ind w:left="0" w:right="0" w:firstLine="0" w:hanging="0"/>`), bold caption text (`<w:b/>`), left or justified alignment (`jc="both"` or `jc="left"`), tight vertical spacing (`before="60" after="160"`), and document house font (`Alegreya Sans`, 12pt).
   - **Table & Figure Notes (`Note: ...`)**: Style strictly as `Heading4` (footnote size 11pt/10pt) with zero first-line indent (`<w:ind w:left="0" w:right="0" w:firstLine="0" w:hanging="0"/>`), bold `Note:` prefix label, and tight vertical spacing (`before="60" after="120"`).
   - **Figure Drawings (`<w:drawing>`)**: Zero first-line indent, centered alignment (`jc="center"`), tight spacing before/after.

### 7. Turnkey Setup & Workflow Architecture for Any Project
To implement this synchronization workflow in any project, establish the following standardized structure:

#### Project Directory Convention:
```
project/
├── Scripts/
│   ├── sync_manuscript.R        # Master R driver (run via: Rscript Scripts/sync_manuscript.R)
│   ├── sync_manuscript.py       # DOM-based OpenXML table & figure injector
│   └── generate_md_tables.R     # Pre-computes markdown tables to cache/
├── cache/                       # Intermediate markdown tables (table1_wald.md, etc.)
└── Plots/                       # Publication-grade PNG figures (6.5 inches wide, 300 DPI)
```

#### Template 1: Master Driver Script (`Scripts/sync_manuscript.R`)
```r
#!/usr/bin/env Rscript
library(googledrive)

# 1. Configuration: Note document ID from Google Doc URL
doc_id <- "YOUR_GOOGLE_DOC_ID_HERE"
live_docx <- "draft_live.docx"
updated_docx <- "draft_updated.docx"

message("[1/4] Ensuring table summaries in cache/ are up to date...")
if (file.exists("Scripts/generate_md_tables.R")) {
  source("Scripts/generate_md_tables.R")
}

message("[2/4] Downloading live manuscript from Google Drive...")
drive_auth(email = "omarlizardo@gmail.com")
drive_download(as_id(doc_id), path = live_docx, overwrite = TRUE)

message("[3/4] Performing in-place XML injection of tables and figures...")
exit_code <- system2("python3", args = c("Scripts/sync_manuscript.py", live_docx, updated_docx))
if (exit_code != 0) {
  stop("Error during in-place XML injection.")
}

message("[4/4] Uploading updated manuscript back to Google Drive...")
drive_update(as_id(doc_id), media = updated_docx)

# Clean up local temporary files
if (file.exists(live_docx)) unlink(live_docx)
if (file.exists(updated_docx)) unlink(updated_docx)
message("Synchronization complete! Google Doc updated successfully.")
```

#### Template 2: OpenXML DOM Injector (`Scripts/sync_manuscript.py`)
```python
import os
import re
import sys
import zipfile
import struct
import xml.etree.ElementTree as ET

def xml_escape(s):
    if s is None: return ""
    return str(s).replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;").replace('"', "&quot;")

def get_png_dimensions(image_path):
    with open(image_path, "rb") as f:
        data = f.read(24)
        if len(data) >= 24 and data.startswith(b'\x89PNG\r\n\x1a\n'):
            return struct.unpack('>II', data[16:24])
    return 1950, 1200

def parse_markdown_table(file_path):
    with open(file_path, "r", encoding="utf-8") as f:
        lines = [line.strip() for line in f if line.strip()]
    table_lines = [line for line in lines if line.startswith("|") and line.endswith("|")]
    if len(table_lines) < 3: return [], []
    headers = [c.strip() for c in table_lines[0].strip("|").split("|")]
    rows = []
    for line in table_lines[2:]:
        row = [c.strip() for c in line.strip("|").split("|")]
        rows.append(row)
    return headers, rows

def create_apa_table_xml(headers, rows_data, col_widths=None):
    total_w = 9360  # 6.5 in portrait width in dxa
    num_cols = len(headers)
    if col_widths is None:
        col1_w = int(total_w * 0.40)
        rem_w = total_w - col1_w
        sub_w = int(rem_w / (num_cols - 1))
        col_widths = [col1_w] + [sub_w] * (num_cols - 2)
        col_widths.append(total_w - sum(col_widths))
        
    xml = [f'<w:tbl xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main"><w:tblPr><w:tblW w:w="{total_w}" w:type="dxa"/><w:tblBorders><w:top w:val="single" w:sz="8" w:space="0" w:color="000000"/><w:left w:val="none"/><w:bottom w:val="single" w:sz="8" w:space="0" w:color="000000"/><w:right w:val="none"/><w:insideH w:val="none"/><w:insideV w:val="none"/></w:tblBorders><w:tblCellMar><w:top w:w="120" w:type="dxa"/><w:bottom w:w="120" w:type="dxa"/><w:left w:w="160" w:type="dxa"/><w:right w:w="160" w:type="dxa"/></w:tblCellMar></w:tblPr><w:tblGrid>']
    for w in col_widths: xml.append(f'<w:gridCol w:w="{w}"/>')
    xml.append('</w:tblGrid>')
    
    # Header Row
    xml.append('<w:tr><w:trPr><w:tblHeader/><w:cantSplit/></w:trPr>')
    for i, h in enumerate(headers):
        align = "left" if i == 0 else "center"
        escaped_h = xml_escape(h)
        xml.append(f'<w:tc><w:tcPr><w:tcW w:w="{col_widths[i]}" w:type="dxa"/><w:tcBorders><w:bottom w:val="single" w:sz="4" w:space="0" w:color="000000"/></w:tcBorders><w:noWrap/></w:tcPr><w:p><w:pPr><w:suppressAutoHyphens/><w:spacing w:before="0" w:after="0"/><w:ind w:left="0" w:right="0" w:firstLine="0" w:hanging="0"/><w:jc w:val="{align}"/></w:pPr><w:r><w:rPr><w:b/></w:rPr><w:t>{escaped_h}</w:t></w:r></w:p></w:tc>')
    xml.append('</w:tr>')
    
    # Data Rows
    for row in rows_data:
        xml.append('<w:tr><w:trPr><w:cantSplit/></w:trPr>')
        for i, val in enumerate(row):
            align = "left" if i == 0 else "center"
            escaped_val = xml_escape(val)
            xml.append(f'<w:tc><w:tcPr><w:tcW w:w="{col_widths[i]}" w:type="dxa"/><w:noWrap/></w:tcPr><w:p><w:pPr><w:suppressAutoHyphens/><w:spacing w:before="0" w:after="0"/><w:ind w:left="0" w:right="0" w:firstLine="0" w:hanging="0"/><w:jc w:val="{align}"/></w:pPr><w:r><w:t>{escaped_val}</w:t></w:r></w:p></w:tc>')
        xml.append('</w:tr>')
    xml.append('</w:tbl>')
    return "".join(xml)

def generate_table_xmls():
    # Map each manuscript table caption to parsed markdown table and column widths
    tables = {}
    # Example:
    # h1, r1 = parse_markdown_table("cache/table1_results.md")
    # tables["Table 1"] = create_apa_table_xml(["Model", "Estimate", "SE", "p"], r1, [3860, 1800, 1800, 1900])
    return tables

def sync_docx(in_docx, out_docx):
    with zipfile.ZipFile(in_docx, "r") as zin:
        xml_bytes = zin.read("word/document.xml")
        rels_bytes = zin.read("word/_rels/document.xml.rels")
        all_files = {item.filename: zin.read(item.filename) for item in zin.infolist()}
    
    root_rels = ET.fromstring(rels_bytes)
    rid_to_target = {e.get('Id'): e.get('Target') for e in root_rels if e.get('Id')}
    
    # Map caption prefixes to figure images
    figure_map = {
        # "Figure 1.": "Plots/fig1.png",
        # "Figure 2.": "Plots/fig2.png"
    }
    
    ET.register_namespace('w', 'http://schemas.openxmlformats.org/wordprocessingml/2006/main')
    ET.register_namespace('a', 'http://schemas.openxmlformats.org/drawingml/2006/main')
    ET.register_namespace('r', 'http://schemas.openxmlformats.org/officeDocument/2006/relationships')
    ET.register_namespace('wp', 'http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing')
    
    doc_tree = ET.fromstring(xml_bytes)
    ns = {
        'w': 'http://schemas.openxmlformats.org/wordprocessingml/2006/main',
        'a': 'http://schemas.openxmlformats.org/drawingml/2006/main',
        'r': 'http://schemas.openxmlformats.org/officeDocument/2006/relationships',
        'wp': 'http://schemas.openxmlformats.org/drawingml/2006/wordprocessingDrawing'
    }
    body = doc_tree.find('w:body', ns)
    
    # 1. Update Figures in-place
    if figure_map:
        body_list = list(body)
        for i, elem in enumerate(body_list):
            text = ''.join(elem.itertext()).strip()
            for fig_caption, img_path in figure_map.items():
                if fig_caption in text and os.path.exists(img_path):
                    for offset in range(0, 4):
                        idx = i - offset
                        if 0 <= idx < len(body_list):
                            candidate = body_list[idx]
                            blips = candidate.findall('.//{http://schemas.openxmlformats.org/drawingml/2006/main}blip')
                            if blips:
                                for blip in blips:
                                    rid = blip.attrib.get('{http://schemas.openxmlformats.org/officeDocument/2006/relationships}embed')
                                    if rid and rid in rid_to_target:
                                        target_media = "word/" + rid_to_target[rid]
                                        with open(img_path, "rb") as f_img:
                                            all_files[target_media] = f_img.read()
                                        
                                        pw, ph = get_png_dimensions(img_path)
                                        cx = 5943600  # 6.5 in portrait width in EMUs
                                        cy = int(round(5943600 * (ph / pw)))
                                        for wp_ext in candidate.findall('.//wp:extent', ns):
                                            wp_ext.set('cx', str(cx))
                                            wp_ext.set('cy', str(cy))
                                        for a_ext in candidate.findall('.//a:ext', ns):
                                            a_ext.set('cx', str(cx))
                                            a_ext.set('cy', str(cy))
                                break

    # 2. Update Tables in-place (DOM Sibling Search)
    tables = generate_table_xmls()
    if tables:
        body_list = list(body)
        for i, elem in enumerate(body_list):
            text = ''.join(elem.itertext()).strip()
            for caption_key, tbl_xml in tables.items():
                if text.startswith(caption_key) or f'{caption_key}:' in text or f'{caption_key}.' in text:
                    for j in range(i + 1, min(len(body_list), i + 6)):
                        sibling = body_list[j]
                        if sibling.tag.endswith('tbl'):
                            new_tbl_elem = ET.fromstring(tbl_xml)
                            idx_in_body = list(body).index(sibling)
                            body.remove(sibling)
                            body.insert(idx_in_body, new_tbl_elem)
                            break
                            
    all_files["word/document.xml"] = ET.tostring(doc_tree, encoding="utf-8", xml_declaration=True)
    
    with zipfile.ZipFile(out_docx, "w", compression=zipfile.ZIP_DEFLATED) as zout:
        for fname, data in all_files.items():
            zout.writestr(fname, data)

if __name__ == "__main__":
    if len(sys.argv) >= 3:
        sync_docx(sys.argv[1], sys.argv[2])
```

### 9. Critical Operational Lessons & Edge-Case Safeguards for Live Document Syncing
Across collaborative manuscripts where authors actively edit online while agents perform programmatic updates, agents must strictly enforce the following safeguards:

1. **Canonical Source of Truth & Safe Two-Way Syncing (Never Blind-Overwrite User Edits)**:
   - Once a manuscript is live on Google Drive, human authors frequently perform direct prose edits, merge paragraphs, eliminate figures/tables, and customize table cells directly online.
   - **Crucial Rule:** The downloaded `draft_live.docx` is the canonical source of truth for text, headings, and table structures.
   - **Decoupled Data Injection (`--inject-tables`)**: Do NOT automatically overwrite user-edited tables with local markdown files unless explicit table injection is requested. Formatting, image refreshes, and figure renumbering passes must preserve user table edits and text revisions intact.

2. **Font Contamination, Math Markup & Run-Level Override Sanitization (The "Alien Font" Trap)**:
   - External paste operations and equation editors often insert direct run-level font declarations (e.g., `<w:rFonts w:ascii="Nova Mono"/>`, `<w:rFonts w:ascii="Cardo"/>`) or Office Math markup (`<m:oMath>`, `<m:oMathPara>`) into the OpenXML tree.
   - Because run-level `<w:rFonts>` and `<m:oMath>` override the document's default `docDefaults` / `Normal` style, Google Docs and Word will render numbers, Greek symbols ($\Delta$, $\theta$), and equations in alien serif (`Cardo`, `Cambria Math`) or monospace typefaces.
   - **Crucial Rule:** Always execute a normalization pass on `word/document.xml` that: (a) converts all `<m:oMath>`/`<m:oMathPara>` blocks to standard text runs (`<w:r>`); (b) strips all direct `<w:rFonts>`, stray `<w:shd>`, `<w:highlight>`, `<w:color>`, and `<w:sz>` tags so that all numbers, variables ($N$, $p$, $J$, $\text{WAIC}$), and symbols render cleanly in the document house font (`Alegreya Sans`); and (c) sanitizes `word/fontTable.xml` to remap foreign fonts to `Alegreya Sans`.

3. **Page Breaks & Section Boundary Preservation in Appendices**:
   - In manuscripts with extensive appendices (e.g., Appendix Sections A through E), explicit page breaks (`<w:br w:type="page"/>` or `<w:pageBreakBefore/>`) define clean section transitions.
   - **Crucial Rule:** Never strip or collapse empty paragraphs that carry `<w:br w:type="page"/>`. Verify that appendix section breaks remain intact across sync cycles.

4. **Atomic Sequential Figure & Table Renumbering (Preventing Cascading Regex Collisions)**:
   - When a figure or table is removed (e.g., Figure 4 deleted, so Figure 5 becomes Figure 4, Figure 6 becomes Figure 5), applying sequential linear replacements (`replace("Figure 5", "Figure 4"); replace("Figure 6", "Figure 5")`) causes cascading collisions (Figure 6 -> Figure 5 -> Figure 4).
   - **Crucial Rule:** Always use atomic token/lambda replacement with word boundaries:
     ```python
     mapping = {'5': '4', '6': '5', '7': '6'}
     text = re.sub(
         r'\bFigure\s+([567])\b',
         lambda m: f'Figure {mapping.get(m.group(1), m.group(1))}',
         text,
     )
     ```
   - Update both the caption paragraphs (`Figure X. ...`) AND all corresponding in-text narrative references (`As shown in Figure X...`).

5. **Immediate Parity between Live Document and Local Accounting**:
   - Whenever figures or tables are added, removed, or renumbered in the Google Doc, immediately synchronize:
     - The project's `sync_manuscript.py` `figure_mappings` list
     - The project memory (`AGENTS.md` Figure & Table taxonomy)
     - `Scripts/Plotting_and_QA/README.md`
   - This ensures any future agent or subagent working in the repo operates from the exact same numbering and asset inventory.

### 10. Surgical Live Document Updating & Anti-Collision Protocols (CRITICAL - ALL AGENTS)
When collaborating on live manuscripts hosted on Google Drive where authors actively write and revise online, all programmatic edits and sync scripts must enforce these anti-collision safeguards:

1. **The Human In-Flight Collaboration Rule (The Live Document Is King)**:
   - When manuscripts are live on Google Drive, human authors perform continuous edits (refining prose, fine-tuning paragraph spacing, reordering lines, customizing table cells, adding comments, and resolving track changes).
   - Never assume local files, templates, or hardcoded scripts represent current prose. Every sync cycle must start with a fresh download (`googledrive::drive_download()`) of the live draft.

2. **Idempotent Structural Restructuring (The "Check-Before-Inject" Pattern)**:
   - When asked to restructure a section (e.g., inlining hypotheses into subsections, reordering headings), the script must check whether the target structure is *already present* in the document (e.g., `has_inlined_hypotheses = any(...)` or checking for heading titles).
   - If the structure is already present, the script **must skip section re-injection** and enter non-destructive pass-through mode. This prevents re-injecting hardcoded templates over author edits made after the initial restructuring.

3. **Surgical Single-Node / Run-Level DOM Targeting**:
   - For targeted text revisions (e.g., updating a hypothesis citation, fixing a typo, removing a duplicated sentence):
     - Locate the exact `<w:p>` node via text matching in `.itertext()`.
     - **Preserve the `<w:pPr>` block intact** (inheriting existing paragraph styles, spacing, indents, and alignment).
     - Modify only the targeted text inside `<w:r>` runs.
     - Never regenerate or overwrite surrounding paragraphs or the broader section.

4. **Preserving Inline Semantic Markup (`<w:rPr>`)**:
   - Word OpenXML stores italics (`<w:i/>`), bold (`<w:b/>`), subscripts, and superscripts at the run level (`<w:r>`).
   - Plain string replacement on paragraph text will strip formatting tags. When constructing replacement text, use a helper that tokenizes inline markup (e.g., `<i>...</i>`, `<b>...</b>`) and builds corresponding `<w:r>` runs so italicized terms, book titles, and statistical symbols ($p < 0.001$, $N$, $\Delta$) are never flattened into plain text.

5. **Decoupled Asset Replacement (Zero Prose Side-Effects)**:
   - Tables and figures must be updated strictly via DOM sibling search and relationship ID mapping.
   - Replacing Table 3 or Figure 2 must never touch, reflow, or alter adjacent introductory or discussion paragraphs.

6. **Preserving Paragraph Spacing, Blank Lines, and Page Breaks**:
   - Authors use deliberate paragraph spacing (`before`, `after`, line spacing) and page breaks (`<w:br w:type="page"/>`).
   - Sync scripts must never indiscriminately purge "empty" paragraphs that contain page breaks, and must never strip custom `pPr` spacing definitions from body paragraphs unless explicitly cleaning alien overrides.

7. **The Two-Way Local Markdown Synchronization Rule**:
   - Immediately after applying a surgical edit to the live document, update `current_text.md` to reflect the change so that the local markdown mirror remains 100% faithful to the live document.

8. **Remote Formatting & Style Preservation Standard (MANDATORY - ZERO OVERWRITING)**:
   - **Preserve All Author Styling & Formatting**: Any formatting, typography, styling, font choices, line spacing, margins, color palettes, heading hierarchies, paragraph alignments, track changes, or layout adjustments made by human authors remotely in Google Docs / Word must be **100% preserved** across all future sync cycles and draft updates.
   - **Never Overwrite with Pandoc Re-compilation**: Once a document is live on Google Drive, scripts and agents must **never** overwrite it by uploading a fresh Pandoc-compiled `.docx` file. Pandoc re-compilation wipes out custom remote styles, comments, track changes, and typography.
   - **Strict In-Place DOM Injection**: All future updates must operate strictly via the in-place OpenXML round-trip protocol (`draft_live.docx` -> surgical DOM injection -> `draft_updated.docx`). Only targeted `<w:tbl>` (table) and `<w:drawing>` (figure) nodes are modified in `word/document.xml`, leaving all remote paragraph properties (`<w:pPr>`), font properties (`<w:rPr>`), section styles, and document defaults completely intact.

### 11. Marked Copy Generation & Text Highlighting Standards (Editor Revisions)
When journal editors mandate a marked version showing revisions (e.g., *"Please also highlight the changes to your manuscript within the document by using bold, highlighted or coloured text"*):
1. **Professional Royal Blue Formatting (`#004FB9`)**:
   - Apply OpenXML font color `<w:color w:val="004FB9"/>` to all substantive additions, newly added theoretical sections, comparative syntheses, critical discussions, and new reference entries.
   - Retain baseline, carried-over text from previous rounds in standard black.
   - Avoid solid yellow background highlighting across full pages, as dense yellow blocks cause severe visual fatigue for reviewers reading long documents.
2. **Reviewer/Editor Banner**:
   - Insert an explanatory header banner immediately before the Abstract:
     `[Marked Copy: All substantive additions, theoretical expansions, comparative syntheses, and conceptual developments in this revised manuscript are highlighted in blue text.]`
3. **Dual Document Synchronization**:
   - Maintain both a Clean Copy (`manuscript.docx`) and a Marked Copy (`manuscript-marked.docx`) synchronized directly with distinct Google Drive documents.

### 12. Social Science Table Relocation & In-Text Placeholder Protocols
When submitting to journals that require data and synthesis tables to appear at the end of the manuscript rather than in-line:
1. **In-Text Centered Placeholder**:
   - Insert a centered, bolded placeholder paragraph at the exact narrative location where the table was discussed:
     `<w:p><w:pPr><w:jc w:val="center"/><w:spacing w:before="240" w:after="240"/></w:pPr><w:r><w:rPr><w:b/><w:sz w:val="24"/></w:rPr><w:t>[Table 1 about here]</w:t></w:r></w:p>`
2. **End-of-Manuscript Placement & Page Break**:
   - Position the table at the very end of the manuscript body (immediately following the References section, right before `<w:sectPr>`).
   - Precede the table with a page-break paragraph (`<w:p><w:pPr><w:pageBreakBefore/></w:pPr></w:p>`).
3. **APA 7th Two-Line Caption & Table Note**:
   - Above Table: Line 1: `Table 1` (bold), Line 2: `Title` (italic).
   - Below Table: `Note.` (italic) followed by regular text explaining corpus criteria, abbreviations, or sample parameters.

## Bayesian Model Visualization Standards (`ggdist` & `ggplot2`)

When visualizing Bayesian posterior distributions, marginal effects, random coefficients, and ordinal transitions from `brms` or `cmdstanr` models across any project, all agents must strictly adhere to these unified publication standards:

### 1. Directional Credibility Criterion (95% Posterior Mass)
- **Mathematical Definition**: An effect or contrast is statistically credible if at least **95% of the posterior probability mass** falls strictly on one side of zero ($P(\theta > 0) \ge 0.95$ or $P(\theta < 0) \ge 0.95$).
- **Extraction Code Standard**:
  ```r
  credibility <- plot_draws %>%
    group_by(Condition, Threshold) %>%
    summarise(
      conf_low  = quantile(draw, 0.025),
      conf_high = quantile(draw, 0.975),
      p_pos     = mean(draw > 0),
      p_neg     = mean(draw < 0),
      .groups   = "drop"
    ) %>%
    mutate(
      Credible = ifelse(p_pos >= 0.95 | p_neg >= 0.95, "Credible Shift", "No Credible Shift")
    )
  ```
  *(Note: Checking `conf_low > 0` on a standard two-tailed 95% interval enforces a 97.5% one-tailed threshold; computing `p_pos >= 0.95` correctly evaluates directional hypothesis credibility).*

### 2. Dual-Interval Multi-Width Thickness in `stat_halfeye`
- Always supply `.width = c(0.80, 0.95)` to represent both the narrow **80% credible interval** and the wider **95% credible interval**.
- **CRITICAL Sizing Rule**: Always specify `interval_size_range = c(0.75, 1.9)` (or `c(1.5, 4.0)`).
- **NEVER pass a single fixed scalar** like `interval_size = 1.2`. Passing a single scalar overrides `ggdist` interval scaling and collapses both the 80% and 95% bars into the identical line thickness, contradicting figure captions that reference "thick and thin bars".

### 3. Marker & Color Mapping for Statistical Credibility (Grayscale Standard)
- **Credible Shift (≥ 95% Mass)**: Render as a solid circle (`shape = 16` / default point) filled with the condition or directional color (Deep Blue `#0072B2` for positive, Vermillion `#D55E00` for negative).
- **No Credible Shift / Spanning Zero (< 95% Mass)**: Render in **grayscale** (`"gray60"`), using solid markers (`shape = 16` / default point).
- **Grayscale Consistency Rule**: Across all plots (single-series, multi-condition, and directional transitions), non-credibility is indicated consistently via **grayscale color/fill** rather than hollow/white-filled circles.
- **Standard Sizing**: Set `point_size = 4.8` (or `3.2` on dense multi-facet plots).

### 4. Half-Eye Density Slab Standards
- Set `slab_alpha = 0.15` (or `0.20`) with `scale = 0.65` so the posterior density cloud provides a subtle, aesthetic backdrop without cluttering the intervals and points.

### 5. Standardized `stat_halfeye` Code Template
```r
stat_halfeye(
  point_interval = median_qi,
  .width = c(0.80, 0.95),
  point_size = 4.8,
  interval_size_range = c(0.75, 1.9),
  slab_alpha = 0.15,
  scale = 0.65,
  position = position_dodge(width = 0.6)
) +
scale_color_manual(values = COLOR_CREDIBILITY, name = "Directional Credibility") +
scale_fill_manual(values = COLOR_CREDIBILITY, name = "Directional Credibility")
```

### 6. Responsive Legend & Subtitle Layout (Preventing Horizontal Clipping)
- **Controlled Row Guide Strips**: Enforce `nrow = 1` for short single-series legends, or wrap into multiple rows (`nrow = 2` or `nrow = 3`, `byrow = TRUE`) for multi-category or vertically stacked facet plots on standard 6.5-inch canvases to prevent horizontal margin overflow or clipping:
  ```r
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE, override.aes = list(shape = 16, size = 4.5, linetype = 0)),
    fill  = "none",
    shape = guide_legend(nrow = 1, override.aes = list(color = "black", fill = "white", size = 4.5, stroke = 1.5))
  )
  ```
- **Legend Centering**: Use `theme(legend.box = "horizontal", legend.box.just = "center", legend.spacing.x = unit(0.4, "cm"), legend.margin = margin(t = 4, b = 2))`.
- **Subtitle & Caption Wrapping**: Ensure long subtitles and captions include explicit line breaks (`\n`) or use `stringr::str_wrap(subtitle, width = 75)` so text never clips at the plot margins.

## Visualization & Table Presentation Standards
- **Standard 6.5-Inch Image Width**: Export all publication plots at `width = 6.5` inches (300 DPI) to match the exact printable text width of a standard 1.0-inch margin portrait page.
- **Credible / Not-Credible Plotting Strategy (Grayscale Standards)**:
  - **Credibility Criterion**: An effect is statistically credible if >= 95% of the posterior probability mass falls strictly to the right or left of zero (Q2.5 > 0 or Q97.5 < 0).
  - **Credible Effects**: Rendered with **solid filled markers** (`shape = 16` / default point) and directional color (Deep Blue `#0072B2` for positive / upper tier, Vermillion `#D55E00` for negative / comparison tier).
  - **Not Credible (Spanning Zero)**: Rendered in **grayscale** (`"gray60"`) with standard solid markers.
- **Domain Axis Ordering Standard (Beauty Encounters)**:
  - In all Bayesian posterior plots for beauty encounters, order the domain y-axis according to the **overall correlation between the binary domain indicator and educational attainment** (ranked from lowest correlation at the bottom to highest correlation at the top):
    1. **Literature** (Top, $r \approx +0.15$)
    2. **Visual Arts** ($r \approx +0.12$)
    3. **Built Places** ($r \approx +0.12$)
    4. **Music** ($r \approx +0.06$)
    5. **Screen Media** ($r \approx +0.06$)
    6. **Food & Drink** ($r \approx +0.03$)
    7. **Fashion & Style** ($r \approx +0.025$)
    8. **Crafts & DIY** (Bottom, $r \approx +0.015$)
    *(Factor levels in R: `c("Crafts & DIY", "Fashion & Style", "Food & Drink", "Screen Media", "Music", "Built Places", "Visual Arts", "Literature")`)*.
- **Vertical Panel Stacking & Common X-Axis Standard (`ncol = 1`)**:
  - For multi-category demographic plots (e.g. Educational Capital, Political Ideology, and Age Cohorts), **stack panels vertically in a single column (`facet_wrap(~ Category, ncol = 1)`) on a common fixed x-axis** rather than side-by-side or 2x2 grids with independent `free_x` scales.
  - **Visual Advantages**:
    - **Direct Top-Down Scanning**: Facilitates natural vertical eye movement across ordinal progressions (e.g. *Bachelor's* $\rightarrow$ *Postgraduate*, *Right* $\rightarrow$ *Center* $\rightarrow$ *Left* $\rightarrow$ *Very Left*, *35–44* $\rightarrow$ *65+*).
    - **Continuous Vertical Reference Line**: The dashed zero line ($x = 0$) aligns unbroken down the entire column.
    - **Elimination of Scale Distortion**: Every unit of shift represents the exact same physical horizontal distance across all panels, ensuring accurate cross-category visual comparisons.
  - **Standard Sizing**:
    - 2-panel vertical stacks: `width = 6.5` in, `height = 7.2` in.
    - 4-panel vertical stacks: `width = 6.5` in, `height = 9.8` in.
- **Demographic Disparity Dumbbells**:
  - For natural probability scale visualizations (0%-100%), use connected horizontal dumbbell charts to display the baseline prevalence alongside the disparity span between demographic poles (e.g., Men vs. Women, No Degree vs. Postgrad, Very Right vs. Very Left, 65+ vs. 18-34, Minimizing vs. Maximizing profiles).
  - **Geometric Shape Differentiation**: Use distinct solid geometric shapes (e.g., **Solid Circles `shape = 16`** for Reference / Minimizing profiles and **Solid Triangles `shape = 17`** for Comparison / Maximizing profiles) with uniform point size (`size = 3.6`) and country palette coloring (`#D55E00` for US, `#0072B2` for UK) rather than subtle size or open-stroke variations.
  - Label the percentage point gap directly next to each dumbbell (`+24 pp`, `0 pp`, etc.).
- **Controlled Legend Rows & Margins**:
  - Multi-item legends must explicitly use `guide_legend(nrow = 2, byrow = TRUE)` or `nrow = 1` with generous margins to prevent horizontal canvas overflow.
  - Facet strips must be concise, centered (`hjust = 0.5`), with generous panel spacing (`panel.spacing = unit(1.8, "lines")`).
- **Concise Embedded Plot Headers**: Keep plot-embedded titles and subtitles concise (e.g., `< 55` characters) so they never wrap awkwardly or clip horizontally at 6.5 inches.
- **Figure Notes at Bottom**: Place figure titles and notes at the bottom of the figure block. In notes, describe graphical elements (slopes definition, probability densities, median points, 80%/95% intervals, and color coding) without raw code variables or narrative effect-size claims.
- **Simplified Regression Tables**:
  - Omit wide bracketed ranges `[Q2.5, Q97.5]` from cells in favor of clean point estimates with directional credibility bolding/asterisks (e.g., `<b>0.251***</b>`).
  - Strip technical/range metadata from row labels (e.g., `Variable Name` instead of `Variable Name (1-4)`).
  - Embed sample sizes ($N_{\text{obs}}$, $N_{\text{respondents}}$, $J_{\text{clusters}}$), priors, and model fit diagnostics ($\text{WAIC}, \Delta\text{WAIC}$) directly into bottom summary rows of the regression table.
- **Factor Level & Condition Ordering Synchronization Across Figures and Tables**:
  - Establish a standardized semantic sorting order for multi-arm conditions (e.g., all positive/facilitation conditions grouped together, ordered from generalized to status gradients, followed by all negative/veto conditions in the exact same hierarchical order).
  - Enforce that **identical top-down order** across both the y-axes of horizontal forest/bar plots and the row sequence of summary/regression tables.
  - Never allow arbitrary discrepancies (e.g., alphabetical in tables vs. effect-size ordered in figures), which forces readers to repeatedly re-orient themselves across exhibits.

## LaTeX Table Construction Standards (Publication-Grade APA Architecture)

When designing, formatting, and refining regression, model fit, and summary tables in LaTeX for sociology and network science manuscripts, all agents must adhere to these structural standards to prevent column crowding, margin overflow, and awkward text wrapping:

### 1. Full-Width Elastic Tables (`tabular*` vs. `tabularx`)
- **The Pitfall of `tabularx` with `l` + `X`**: In standard 12pt single-column manuscripts with 1.0-inch margins (printable text width = 6.5 inches / `\linewidth`), specifying `\begin{tabularx}{\linewidth}{lXXXXXX}` forces Column 1 to take the natural width of its longest unbroken text string (often 3.5 to 4.0 inches for detailed model descriptions). The remaining $6.5 - 4.0 = 2.5$ inches must then be split equally across the remaining 5 to 7 `X` columns, forcing numeric cells into narrow 0.35–0.45 inch slots where column headers (e.g. `\textbf{LRT ($\boldsymbol{\chi}^2$)}` or `\textbf{\textit{p}-value}`) break into multi-line text, overlap, or collide.
- **The `tabular*` Solution**: Use `\begin{tabular*}{\linewidth}{@{\extracolsep{\fill}}lcccccc@{}}` paired with calibrated inter-column padding `\setlength{\tabcolsep}{4.5pt}` (or 4.0pt). The `@{\extracolsep{\fill}}` directive automatically distributes elastic spacing evenly across the exact 6.5-inch page width without forcing artificial column wrapping or cell compression.

### 2. Vertically Stacked Substantive Panel Hierarchy
- **Avoid Repetitive Labels**: Do not repeat verbose parenthetical suffixes on every row (e.g., avoid repeating `Model 1a: Family SES Only (Income, Mom College)`).
- **Spanning Panel Headers**: Group rows into substantive developmental blocks or specification tiers using full-width spanning headers:
  ```latex
  \multicolumn{K}{l}{\textit{\textbf{Panel Title}}} \\
  \quad Model 1a: Family SES (Income, Mother's College) & ... \\
  \addlinespace[3pt]
  ```
- **Indentation**: Indent sub-items and models with `\quad`. This provides clear visual hierarchy and keeps Column 1 concise.

### 3. Concise Anti-Squish Column Headers & Parameter Reporting
- Keep statistic column headers concise to ensure they fit cleanly on a single line:
  - Use `\textbf{LL}` instead of `\textbf{Log-Likelihood}`
  - Use `\textbf{Par}` or `\textbf{N. Par}` instead of `\textbf{Parameters}`
  - Use `$\boldsymbol{\chi}^2$` or `\textbf{LRT}` instead of `\textbf{LRT ($\boldsymbol{\chi}^2$)}`
  - Use `\textbf{\textit{p}}` instead of `\textbf{\textit{p}-value}`
  - Use `$\Delta$\textbf{BIC}` instead of `\textbf{Delta BIC}`
- Embed standard errors or confidence intervals in parentheses/brackets within the same cell (`Est (SE)` or `$\text{OR} [95\% \text{ CI}]$`) rather than expanding into separate columns.

### 4. Minipage Table Note Boundary Synchronization
- Never use unconstrained text paragraphs directly beneath a LaTeX table. Always wrap table notes in a minipage matched to `\linewidth`:
  ```latex
  \begin{minipage}{\linewidth}
  \vspace{4pt}
  \footnotesize
  \textit{Note:} Likelihood Ratio Tests ($\chi^2$) evaluated against Model 0 baseline. All models estimated on complete-case cohort ($N = 433$ egos, 2,456 observations).
  \end{minipage}
  ```
  This ensures notes wrap strictly to the table's 6.5-inch boundary without overflowing into the page margins.

### 5. Evaluating Hypotheses via Block Fit Comparison & Marginal Probability Plots
- Rather than overwhelming readers with dense 30-row tables of individual multinomial logit coefficients (which depend on arbitrary reference categories and log-odds metrics), evaluate hypotheses directly through:
  1. **Block Model Fit Hierarchy Tables**: Likelihood Ratio Tests ($\chi^2, df, p$), AIC, and BIC evaluating substantive predictor blocks (e.g., SES vs. Demographics vs. Psychological Dispositions).
  2. **Model-Implied Marginal Class Probability Plots**: Translating parameters into intuitive 0%–100% probabilities across continuous and categorical predictors with 95% simulation confidence bands.

## Experimental Design & Causal Inference Standards

When designing, analyzing, or reporting experimental and quasi-experimental quantitative studies, all agents must adhere to these causal and design standards:

### 1. Terminology Disambiguation in Repeated-Measures & Difference-in-Differences (DiD)
- **Baseline vs. Control Condition**: Reserve the term **"baseline"** strictly for the temporal pre-treatment measurement point ($t_0$ / Trial 1). Never refer to the unexposed comparison group as "the baseline group"; explicitly designate it the **"Control Condition"**.
- **Counterfactual Exposure Drift**: Differentiate the counterfactual test-retest shift observed in the unexposed control condition as **"exposure drift"** (or "temporal drift") rather than treating it as an active intervention effect or confounding it with pre-treatment scores.
- **Net Causal Effect**: Clearly define the Difference-in-Differences estimator as the net shift in the treatment condition relative to the counterfactual exposure drift in the Control Condition: $\text{DiD} = (\bar{Y}_{T, \text{post}} - \bar{Y}_{T, \text{pre}}) - (\bar{Y}_{C, \text{post}} - \bar{Y}_{C, \text{pre}})$.

### 2. Inverse Probability Weighting (IPW): Strict Exogeneity of Conditioning Covariates
- **Pre-Treatment Exogeneity Requirement**: In propensity score weighting (`WeightIt`, `cobalt`), conditioning covariates must be restricted strictly to **exogenous pre-treatment background characteristics** (e.g., age, gender, race/ethnicity, parental socioeconomic status).
- **Exclusion of Downstream Mediators and Colliders**: Explicitly exclude post-treatment adult socioeconomic variables (e.g., adult personal income, adult occupation, current geographic region) that are downstream consequences or potential mediators/colliders of the primary stratification variable (e.g., educational attainment). Conditioning on post-treatment socioeconomic attainments induces collider stratification bias or artificially blocks causal mediation pathways.
- **Standardized Mean Differences**: Verify that IPW weights compress standardized mean differences (SMDs) across all comparison arms below $0.10$ before proceeding to outcome models, and document balance via a Love plot.

### 3. Mitigating Right-Censoring in Educational Attainment (The Age $\ge 24$/25 Cutoff)
- In adult general population surveys (e.g., SSI, GSS, Pew, ANES) where completed educational attainment (e.g., BA degree, graduate credentials) is a primary explanatory variable or stratification axis, the 18–23 college-age cohort suffers severe right-censoring because many respondents are actively enrolled and have not completed their highest credential.
- **Primary Analytic Sample**: Restrict the primary analytic sample to completed-education adults ($\text{Age} \ge 24$ or $\ge 25$) to eliminate right-censoring in educational status.
- **Full-Sample Sensitivity Preservation**: Retain the full unrestricted sample (including ages 18–23) as an explicit appendix robustness check, verifying that all empirical findings and hypothesis verdicts remain invariant across the full age spectrum.

### 4. Pre-Pooling Equivalence Tests for Factorial Vignette Designs
- When complex factorial vignette designs (e.g., high-dimensional grids distinguishing multiple alter profiles, such as Economic Specialists vs. Cultural Specialists) threaten statistical power across dozens of interaction cells, conduct formal contrast / equivalence tests ($F$-tests or Wald tests) between closely related sub-arms.
- If responses to the sub-arms are statistically indistinguishable across valence conditions (e.g., $p > 0.50$), collapse them into a unified status tier with formal statistical justification before estimating moderated interaction models. This preserves statistical power while eliminating arbitrary ad-hoc pooling concerns.

### 5. Procedural Safeguards Against Demand Characteristics & Reactivity
When evaluating repeated-measures survey experiments against potential experimenter demand, consistency biases, or Hawthorne-type reactivity, methodically document four structural safeguards:
1. **Intervening Cognitive Buffer**: Incorporate an extensive substantive battery (e.g., 15–20 minutes of unrelated socio-demographic, cultural, or occupational modules) between Trial 1 (pre-test) and Trial 2 (post-test) to decay active short-term memory of initial scores.
2. **Neutral Verbatim Prompts**: Use identical, neutral rating prompts at Trial 1 and Trial 2 without reminding participants of their earlier score or framing the post-test as a "re-evaluation" or "test of consistency."
3. **Unidirectional Navigation**: Restrict survey flow to forward-only navigation, preventing participants from flipping back to previous pages to inspect or align prior answers.
4. **Naturalistic Aggregate Framing**: Frame experimental cues as aggregate social patterns from prior respondents (e.g., *"Our survey so far indicates..."*) rather than normative or authoritative demands to change opinion.

### 6. Decomposing Continuous Multi-Way Interactions into Discrete Behavioral Choice Models
- When continuous regression models with high-order multi-way interactions (e.g., 24- or 36-cell status $\times$ condition continuous grids) become unwieldy, uninterpretable, or dilute narrative focus, relegate the exhaustive continuous matrix table to the Appendix.
- In the main text, model the underlying behavioral decisions directly as discrete multinomial choices (e.g., *Stay* [inertial stability], *Conform* [directional alignment], and *React* [oppositional divergence]; or *Adopt*, *Resist*, *Abandon*).
- Estimate Average Marginal Effects (AMEs) via multinomial logistic regression (`nnet::multinom` or `mclogit::mblogit`), providing intuitive percentage-point shifts that communicate the substantive behavioral story clearly.

## Scientific Paper Structuring Standards (Mensh & Kording Ten Simple Rules)
All manuscript drafting, section outlining, paragraph composition, and structural revisions must adhere to the principles established by Mensh & Kording (2017, *PLOS Computational Biology*):

### 1. Core Principles (Rules 1–4)
- **Rule 1: Focus on a Single Central Contribution (The "Rule of One")**:
  - Every paper must communicate one primary conceptual message or discovery, crystallized in the title.
  - Avoid diluting the manuscript with competing primary claims; secondary findings and methodological innovations must directly support or bridge to the central takeaway.
- **Rule 2: Write for Naïve, Non-Specialist Readers**:
  - Assume an intelligent reader who is unfamiliar with the project's micro-jargon, abbreviations, and specific institutional context.
  - Define technical terms upon first use, purge unneeded acronyms, and minimize cognitive friction so readers never have to flip backward to understand terminology.
- **Rule 3: Enforce the Context-Content-Conclusion (C-C-C) Scheme Across All Scales**:
  - The C-C-C structure must operate fractally across the entire manuscript, each major section, each subsection, and each individual paragraph:
    * **Context**: Ground the problem, establish common ground, or state the specific question/gap (prevents *"Why am I being told this?"*).
    * **Content**: Deliver the substantive empirical evidence, methodological innovation, or narrative progression.
    * **Conclusion**: Articulate the immediate takeaway, implication, or bridge to the next step (prevents *"So what?"*).
- **Rule 4: Optimize Logical Flow: Avoid Zig-Zagging & Use Strict Parallelism**:
  - **Zero Zig-Zagging**: Cover each sub-topic in a single, dedicated location. Only the central contribution should be revisited across sections. Never bounce back and forth between topics.
  - **Strict Parallelism**: When comparing conditions, groups, hypotheses, or analytical blocks, keep syntactic structures, terminology, and sequential ordering identical across text, tables, and figures.

### 2. Structural Section Goals (Rules 5–8)
- **Rule 5: Tell a Complete, Standalone Story in the Abstract**:
  - The abstract must follow the C-C-C progression within standard word limits (< 250 words):
    1. *Context*: Broad importance of the field + specific knowledge gap.
    2. *Content*: Core approach + executive summary of primary empirical discoveries ("Here we show...").
    3. *Conclusion*: Theoretical significance and broader scientific takeaway.
- **Rule 6: Funnel the Introduction from Broad Importance to the Specific Knowledge Gap**:
  - Structure as an inverted pyramid: start with broad domain importance $\rightarrow$ narrow down to what the literature currently lacks (the specific knowledge gap) $\rightarrow$ state the paper's contribution and preview how it resolves the gap.
- **Rule 7: Deliver Results as a Progressive Sequence of Declarative Findings**:
  - Structure the Results section as a series of logical declarative statements supported by figures and tables.
  - Subsection headers should be concise and informative, signaling the empirical finding rather than generic methodological operations.
  - Figures and tables must tell the central story in an unbroken, cumulative progression.
- **Rule 8: Structure the Discussion to Mirror the Introduction (Gap $\rightarrow$ Findings $\rightarrow$ Caveats $\rightarrow$ Broad Relevance)**:
  - Open by stating directly how the original gap identified in the introduction was filled.
  - Walk through key results and theoretical mechanisms.
  - Provide an honest, thorough treatment of limitations and scope conditions.
  - Conclude by expanding back out to the broader discipline, highlighting future directions and lasting conceptual implications.

### 3. Writing Process & Workflow Heuristics (Rules 9–10)
- **Rule 9: Allocate Effort Disproportionately to High-Leverage Assets**:
  - Prioritize time on the elements that 95% of readers encounter first: Title, Abstract, Figures, Tables, and the structural Outline before drafting long prose.
- **Rule 10: Ruthlessly Refactor, Cut, and Iterate ("Kill Your Darlings")**:
  - Treat writing as an optimization problem: do not hesitate to discard or rewrite full paragraphs if logical flow or clarity can be improved.

## Global Academic Writing & Style Guidelines
- **Authorial Voice: First-Person Singular ("I", "my") for Sole-Authored Manuscripts**:
  - For sole-authored manuscripts, book chapters, essays, and response-to-reviewers memos, strictly maintain first-person singular (*"I"*, *"my"*, *"mine"*) throughout the narrative prose.
  - Never default to the royal plural (*"we model"*, *"our findings"*, *"we thank the reviewer"*) or awkward passive circumlocutions (*"it is demonstrated that"*, *"the author argues"*) in single-authored texts. First-person singular projects an authoritative, direct, and authentic scholarly voice.
- **Plain Language for Educational Stage & Cohort Settings (Strict Prohibition of "Collegiate")**:
  - Strictly use **"college"** or **"undergraduate"** (e.g., *"college careers"*, *"college peer networks"*, *"undergraduate trajectory"*, *"college students"*, *"post-college"*) rather than Latinate/inflated adjectives.
  - **Prohibited Variants**: Do **NOT** use *"collegiate"* (e.g., *"collegiate careers"*, *"collegiate trajectory"*, *"collegiate setting"*) or *"post-collegiate"*.
- **Concise, Informative Section Headings & Titles (MANDATORY)**:
  - When structuring and drafting academic manuscripts, reports, and documentation, strictly use **short, informative, and non-wordy titles** for all section headers (Level 1, Level 2, and Level 3).
  - Avoid overly complex, verbose, or pedantic phrasing (e.g., prefer *“Network Layers”* or *“Layer Decomposition”* over *“The Collegiate Network Landscape: Decomposing Dunbar Cognitive Layers”*; prefer *“Trajectory Mixture Analysis”* over *“Trivariate Latent Class Trajectory Analysis across Dunbar Circles”*; prefer *“Discussion”* over *“Discussion and Conclusion”*).
  - Strive for clean, direct 2-to-4 word phrases that clearly orient the reader without cluttering tables of contents, document outlines, or running headers.
- **Quotation Marks Standard (Double Typographic Quotes vs. Single Quotes)**:
  - Strictly use **double quotation marks** (“...”) for quotations, named concepts, coined phrases, and colloquial terms (e.g., “omnivorous generation”, “cultural omnivore”, “high art”, “inclusive elitists”) rather than single quotes (‘...’ or '...').
  - Reserve single quotation marks (‘...’) strictly for quotations or terms nested inside other quotations.
  - Avoid using straight single quotes (`'...'`) in running manuscript prose, abstracts, titles, and captions for general terminology or quotation.
- **Prohibition of "Democratic" for Egalitarian / Pervasive Distribution**:
  - Strictly do **NOT** use the word *"democratic"*, *"democratically"*, or *"democratization"* when what is meant is *egalitarian*, *equally distributed*, *widely distributed*, *broad-based*, *pervasive*, or *widespread*.
  - "Democratic" is a specific term from political theory and political science denoting a particular kind of political regime or system of governance, not cultural prevalence, widespread adoption, or evaluative egalitarianism.
  - Strictly use **"widely distributed"**, **"broadly distributed"**, **"egalitarian"**, **"pervasive"**, **"broad-based"**, or **"equalization"** instead (e.g., *“aesthetic experience is widely distributed across the general public”* rather than *“democratically distributed”*; *“egalitarian tolerance”* rather than *“democratic tolerance”*).
- **Abstract and Title Page Preservation (Zero Author Overwriting - CRITICAL)**:
  - Once an author makes edits to the Abstract, Title, or Title Page metadata in the live Google Doc, sync scripts and agents must **NEVER** blind-overwrite, replace, or revert the author's live prose with hardcoded text strings.
  - Every sync cycle must preserve 100% of the live author edits to the Abstract and Title Page, applying only non-destructive OpenXML DOM styling (inheriting Alegreya Sans, 0-indent justified body, centered metadata) without modifying the underlying text.
- **Author and Scholar Naming Conventions**:
  - When citing or referring to particular scholars or theorists by name in narrative text, strictly use only their **last name** (e.g., **Bourdieu** instead of *Pierre Bourdieu*, **Dewey** instead of *John Dewey*, **Lamont** instead of *Michèle Lamont*), except where full names are explicitly required to disambiguate scholars sharing a surname or within full bibliographic entries.
- **Hypothesis Formatting Standards**:
  - When stating or listing formal hypotheses in text, do **not** attach parenthetical titles or labels to the hypothesis prefix (e.g., use strictly **Hypothesis 1:** or *Hypothesis 1: ...* without adding parenthetical names like *(Democratic Distribution)*, *(Cultural Stratification)*, or *(Aesthetic Omnivorousness)*).
- **Gender & Demographic Terminology Standards**:
  - Avoid using 'male' and 'female' as nouns to refer to people when writing up results; strictly use **'men'** and **'women'**.
  - Always use the term **'Gender Identity'** (or 'Gender') rather than **'Sex'** across all write-ups, variable definitions, table row/column headers, and figure labels.
- **Per-Figure Analytical Discussion Standard**:
  - In empirical reports and manuscripts featuring multi-figure visualization sequences, **every visual figure must be accompanied by its own dedicated discussion paragraph** (or introductory prose) situated immediately beneath its subsection heading and preceding/following the figure asset.
  - Discussion paragraphs must walk through specific domain estimates, directional shifts, credible contrasts, and substantive takeaways for that figure rather than grouping all commentary into a single generic section note.
- **Strict Anti-Hyperbole & Empirical Tone Standards**:
  - Strictly prohibit hyperbolic, dramatic, or sensationalist terms (e.g., *'massive'*, *'gigantic'*, *'profound'*, *'dramatic'*, *'stark'*, *'breakthrough'*, *'monumental'*, *'unprecedented'*, *'complete neutrality'*, *'without exception'*).
  - Use measured, precise empirical descriptions (e.g., *'substantial'*, *'largest'*, *'gradient'*, *'difference'*, *'attenuated'*, *'limited differentiation'*).
  - Always hedge and qualify analytical claims (e.g., *'suggests'*, *'indicates'*, *'is associated with'* rather than *'proves'*, *'confirms that'*, *'confers'*).
- **Word Choice Standards ("Show" vs. "Demonstrate", "Use" vs. "Utilize", Prohibition of "pp"):**
  - **Absolute Hardcoded Prohibition on "Demonstrate":** Strictly and absolutely NEVER use the words **"demonstrate"**, **"demonstrates"**, **"demonstrating"**, or **"demonstrated"** when you can use **"show"**, **"shows"**, **"showing"**, or **"shown"** across all analytical prose, empirical write-ups, abstracts, introductions, and theoretical discussions. Zero exceptions.
  - Strictly use **"use"**, **"uses"**, **"using"**, and **"used"** rather than **"utilize"**, **"utilizes"**, **"utilizing"**, **"utilized"**, or **"utilization"**.
  - **Absolute Prohibition on "pp" for Percentage Points:** Strictly do **NOT** use the abbreviation **"pp"** or the phrase "percentage points" for changes or differences. Instead, speak directly in terms of **percentages and proportions** (using **"%"** or **"percent"**, e.g., *"+24.0%"* or *"expanded by 24.0 percent"* rather than *"+24.0 pp"*).
- **Prohibition of "Archetypes" / "Archetype"**:
  - Don't use the word "archetypes" or "archetype" when you could use words such as **class**, **classes**, **types**, **categories**, **clusters**, **profiles**, or **pathways**.
  - Strictly use **"classes"**, **"latent classes"**, **"types"**, **"categories"**, **"profiles"**, **"typologies"**, or **"developmental pathways"** instead (e.g., *“distinct developmental pathways”* rather than *“developmental archetypes”*; *“latent classes”* rather than *“latent archetypes”*).
- **Prohibition of "Modern" for Statistical Methods and Approaches**:
  - Strictly do **NOT** use the word **"modern"** to refer to statistical methods, models, frameworks, techniques, or analytical approaches.
  - Strictly use **"recent"** or **"recently developed"** instead (e.g., *“recent age-period-cohort methods”*, *“recently developed partial identification frameworks”*, *“recent APC bounding techniques”*).
- **Avoidance of "Robust" as a Generic Evaluative Adjective**:
  - Strictly avoid using **"robust"**, **"robustly"**, or **"robustness"** as generic evaluative praise, vague intensifiers, or stylistic filler (e.g., avoid *“robust findings”*, *“robust typologies”*, *“robust evidence”*, *“robust relationship”*, *“robust patterns”*).
  - Use precise, descriptive alternatives instead (e.g., *“consistent”*, *“stable”*, *“reliable”*, *“persistent”*, *“well-supported”*, *“pronounced”*, *“systematic”*).
  - Reserve **"robust"** strictly for formal, recognized methodological or statistical techniques where it is an established technical term (e.g., *“robust standard errors”*, *“Huber-White robust covariance”*, *“robust regression”*, *“cluster-robust variance estimation”*).
- **Vernacular English & Reduction of Latinisms**:
  - Write in natural, direct, and accessible vernacular English rather than stiff, inflated Latinate vocabulary or bureaucratic abstractions (e.g., prefer *use* over *utilize*, *show* over *demonstrate*, *help* over *facilitate*, *start/begin* over *commence/initiate*, *run/do* over *effectuate*, *before* over *prior to*, *after* over *subsequent to*, *about/roughly* over *approximately*, *part* over *component*, *since/because* over *inasmuch as*).
  - Avoid unnecessary Latin phrases and idioms in running text (e.g., avoid *inter alia*, *ex ante*, *ceteris paribus*, *qua*, *vis-à-vis*) in favor of plain English equivalents, reserving Latin strictly for standard bibliographic citations (e.g., *et al.*) or verbatim historical/philosophical maxims explicitly under discussion.
- **Software and Package Citations Standard (R and Primary R Packages - MANDATORY)**:
  - Always include formal in-text citations and complete bibliographic entries for **R itself** (citing the R Core Team) and all primary **R packages** used in data ingestion, statistical modeling, estimation, and visualization (e.g., `flexmix`, `nnet`, `lme4`, `brms`, `ggplot2`, `splines`).
  - Never mention software packages casually in running text without formal bibliographic citations (e.g., write *“using the \texttt{flexmix} package \citep{grun2008flexmix} in \textsf{R} \citep{Rmanual}”*).
- **Terminological Consistency Standards (Cultural Matching & Ego-Network Dynamics)**:
  - In projects examining cultural tastes and egocentric tie dynamics (such as the *Cultural Matching Egonets* project):
    1. **Outcome Terminology**: Strictly refer to the relational outcome as **"tie decay"** (or **"protection from tie decay"**, **"protecting ties from decay"**, **"hazard of tie decay"**). Do **NOT** use *"tie dissolution"*, *"tie survival"*, or *"tie retention"* to describe the dependent variable or relational outcome.
    2. **Predictor Terminology**: Strictly and exclusively use the theoretical term **"cultural matching"** (e.g., *"closed-form cultural matching"*, *"open-ended activity matching"*, *"protection from tie decay through cultural matching"*). Do **NOT** use *"cultural alignment"*, *"cultural affinity"*, *"cultural resonance"*, *"cultural compatibility"*, *"shared tastes"*, or *"shared cultural tastes"*.
    3. **Italics for In-Text Emphasis (MANDATORY)**: Strictly use italics (`	extit{...}` or `\emph{...}`) rather than boldface (`	extbf{...}`) for emphasizing terms, theoretical concepts, hypotheses (e.g., *Hypothesis 1*), and variables in running text. Reserve boldface strictly for section headings, table captions, and table header rows.
- **Discussion Section Structural Architecture (The CUA Tripartite Standard)**:
  All quantitative sociology manuscripts and empirical research reports should structure the concluding Discussion section into three distinct, cohesive subsections:
  1. **`Summary of Key Results`**:
     - *Bird's-Eye Synthesis*: Open with a high-level framing linking the core findings back to the original theoretical puzzle.
     - *Systematic Walking of Findings*: Dedicate sequential narrative paragraphs to each major empirical result, hypothesis test, and methodological advance, walking through exact empirical estimates, model comparisons, and trajectory morphologies.
     - *Concluding Transition*: Provide a smooth transition linking empirical patterns to methodological caveats and theoretical implications.
  2. **`Limitations and Suggestions for Future Work`**:
     - *Prose Standard*: Write strictly in cohesive, full academic prose paragraphs. Avoid using numbered lists or bullet points.
     - *Methodical Domain Coverage*: Methodically cover five essential analytical dimensions in sequence:
       1. *Operationalization & Measurement Constraints*: Evaluation of survey instruments, bounding limits (e.g., alter nomination caps), self-report subjectivity, and proposals for multimodal or passive sensing integration.
       2. *Threats to Causal Inference & Contextual Confounding*: Evaluation of unmeasured variables, omitted variable bias, and environmental/organizational opportunity structures.
       3. *Directionality, Selection & Feedback Loops*: Evaluation of bidirectional dynamics, reciprocal causality, and recommendations for continuous-time SEM, cross-lagged panel models, or experimental primes.
       4. *Temporal Granularity & Panel Attrition*: Assessment of observation intervals (e.g., semester rhythms vs. micro-churn), panel attrition over long horizons, and proposals for ecological momentary assessment (EMA) or micro-surveys.
       5. *Institutional Scope Conditions & Generalizability*: Identification of specific sample populations, organizational settings, and demographic boundaries, with concrete directions for comparative cross-national or cross-institutional extensions.
  3. **`Implications: [Substantive Conceptual Subtitle]`**:
     - *Theoretical Stakes*: Revisit foundational debates in the subdiscipline (e.g., structural capital vs. evolutionary cognitive constraints; cultural omnivorousness vs. domain-contingent sorting).
     - *Resolving Zero-Sum Paradoxes*: Use the empirical findings to transcend simplistic dichotomies, showing how seemingly contradictory phenomena coexist through specialized divisions of labor, social signatures, or domain-contingent mechanisms.
     - *Broader Sociological Synthesis*: Synthesize lasting theoretical contributions and articulate the enduring takeaways for sociology, network science, or cultural analysis.
- **Redundant Asset Pruning**:
  - Eliminate standalone visual plots whose data is already exhaustively detailed in an accompanying APA summary table (e.g., omitting WAIC forest progression plots when the full information criterion hierarchy is already presented in a model comparison table).
- Use clear, active, concise academic prose.
- Adhere strictly to Quarto markdown formatting conventions.
- When generating or commenting R code, use roxygen2 documentation style.
- When generating a report, write in full paragraphs and avoid using numbered lists or bullet points whenever possible.
- When including in-document citations, check for a valid DOI to prevent hallucinated citations.

- **Prohibition of Significance Asterisks in Narrative Text vs. Tables**:
  - Significance asterisks and daggers (`*`, `**`, `***`, `\dagger`) belong strictly in formal regression tables and figure notes.
  - Strictly do **NOT** append significance asterisks to numbers, estimates, or test statistics in the running narrative text (e.g., write *“the treatment effect was positive ($\beta = 0.25, SE = 0.08, t = 3.12, p = 0.002$)”* rather than *“the treatment effect was positive ($0.25^{**}$)”* or *“$p < 0.01^{***}$”*).
  - Report exact point estimates, standard errors, test statistics, and $p$-values in academic prose, preserving asterisks exclusively for quick-scan table layouts.
- **Hypothesis Architecture and the Summary Table Pattern (Table 2 Pattern)**:
  - In empirical manuscripts testing multiple hypotheses, enforce strict structural parallelism across three foundational anchors:
    1. **Theory / Introduction**: Introduce hypotheses sequentially ($H1, H2, \dots$) stated in clear declarative sentences without parenthetical naming prefixes.
    2. **Results Section**: Walk through each hypothesis in dedicated sequential subsections or topic paragraphs, referencing specific model coefficients or contrast tests.
    3. **Discussion Summary Table**: Include a concise, standardized summary table (conventionally Table 2) synthesizing the empirical outcomes. The table should contain columns for:
       - *Hypothesis Number* ($H1, H2, \dots$)
       - *Formal Theoretical Expectation* (brief 1-sentence prediction)
       - *Primary Model / Estimator* (e.g., Difference-in-Differences, IPW Mixed Model, Multinomial Logit)
       - *Empirical Estimate & Statistics* (exact coefficient, SE, $t$/$z$, $p$-value)
       - *Final Empirical Verdict* (*Supported*, *Rejected*, *Null*)

### 7. Manuscript Anonymization & Double-Blind Peer Review Standards (CRITICAL)
For journals operating under double-blind peer review (e.g., *Current Sociology*, *American Sociological Review*, *American Journal of Sociology*, *Social Forces*), manuscripts must be completely free of any direct or indirect identification:
1. **Self-Citation Year Censoring**:
   - In-text self-citations must censor the publication year using `Author (XXXX)` or, for multiple self-citations, alphabetical suffixing: `Author (XXXXa)`, `Author (XXXXb)`, `Author et al. (XXXXc)`.
   - Never leave explicit publication years on self-citations (e.g., replace `Author (2017)` with `Author (XXXXa)`).
2. **Multi-Authored Self-Citations**:
   - When citing a multi-authored paper or book where the author is one of several co-authors, **never leave the full author list in the bibliography or in-text citation**. Censor the in-text citation as `(Author et al., XXXXc)` and censor the reference list entry as `Author et al. (XXXXc).` with all book/journal titles and co-author names stripped.
3. **References Section Sanitization**:
   - Under the References section, self-citations must be strictly stripped of sensitive titles, journal names, book titles, page numbers, and publishers. List them purely as:
     - `Author. (XXXXa).`
     - `Author. (XXXXb).`
     - `Author et al. (XXXXc).`
4. **Metadata & Acknowledgments Purge**:
   - The main manuscript file must contain zero occurrences of author names, co-author names, institutional affiliations (e.g., "UCLA", "University of California"), email addresses, phone numbers, grant numbers, course names, or thanking notes.
   - All identifying metadata must be segregated strictly into a standalone Title Page file (`title-page.docx`) uploaded separately as a Title Page / Cover Document.

## Response to Reviewers Memo Standards (LaTeX & Academic Memo Architecture)

When preparing, drafting, or revising point-by-point Response to Reviewers memos (in LaTeX `response_to_reviewers.tex` or Markdown `response_to_reviewers.md`) across any project, all agents must strictly adhere to these unified standards:

### 1. Document Architecture & Minimal Header Standard
- **Clean Header Block**:
  - Main Document Title: `{\LARGE 	extbf{Response to Reviewers and Editor}}\[6pt]`
  - Minimal Metadata: Include only `	extbf{Manuscript Number:}` and `	extbf{Date:} 	oday`. Avoid cluttered lists of authors, journal titles, or editor-in-chief names unless explicitly requested by the author.
- **Overview & Letter to the Editor**:
  - Begin with a formal, professional salutation to the Editor (`Dear Dr.~[Editor's Last Name],`).
  - Open with sincere gratitude for the opportunity to revise and for the constructive feedback of the editors and reviewers.
  - Synthesize the core revisions in full academic prose paragraphs (or concise in-line numbered items: `(1)~	extit{...}`, `(2)~	extit{...}`) rather than fragmented bullet lists.
  - Conclude the overview with a smooth transition into the point-by-point responses.

### 2. Reviewer Comment Callouts (`tcolorbox`)
- Render all verbatim editor and reviewer comments inside a dedicated, styled `tcolorbox` environment:
  ```latex
  
ewtcolorbox{reviewercomment}[1][]{
      enhanced,
      breakable,
      colback=revgray,        % e.g., rgb(0.96, 0.96, 0.98)
      colframe=revborder,     % e.g., rgb(0.75, 0.78, 0.85)
      coltitle=black,
      arc=2mm,
      boxrule=0.8pt,
      left=12pt,
      right=12pt,
      top=9pt,
      bottom=9pt,
      fontupper=\itshape,
      before skip=10pt,
      after skip=10pt,
      #1
  }
  ```
- **Section Hierarchy**:
  - `\section*{Response to the Editor (Dr.~[Editor Name])}`
  - `\section*{Response to Reviewer 1}`
  - `\section*{Response to Reviewer 2}`
  - Subsections for each comment/point: `\subsection*{1. [Short Descriptive Topic]}` or `\subsection*{Point 1: [Short Descriptive Topic]}`.

### 3. Point Response Structure (MANDATORY FORMATTING)
Every response block must strictly follow this clean two-element metadata structure:
1. **`	extbf{Location in Revised Manuscript:}`**:
   - Provide the precise section numbers, titles, figure labels, and table numbers where changes were enacted (e.g., `	extbf{Location in Revised Manuscript:} Section 2.6 (Difference-in-Differences Analytic Strategy), Section 3.1, Figures 3--4, Section 4.1 (Table 2), and Appendix Tables A.2--A.3`).
   - **MANDATORY AUDIT**: Cross-references must be actively checked and verified against the current working manuscript file (`manuscript_R1.tex`, `manuscript.docx`). Never allow stale section numbers, phantom subsections (e.g., Section 3.4 when Section 3 stops at 3.3), misnumbered equations, or out-of-date appendix counters.
2. **`	extbf{Response:}`**:
   - Use the simple, direct label `	extbf{Response:}`.
   - **NO Bug-Tracker Status Tags**: Never include software-tracker lines such as `	extbf{Status:} 	extcolor{forestgreen}{	extbf{[Addressed]}}` or `	extbf{Response / Actions Taken:}`. These clutter the document and undermine the scholarly tone.

### 4. Prose Style: Full Paragraphs Over Bullet Points (MANDATORY)
- **Eliminate Fragmented Bullet Lists**: Write responses in continuous, well-structured, academic prose paragraphs with explicit topic sentences, methodological explanations, and transitional logic.
- **Rely Less on Bullets**: Do not rely on nested `itemize` or `enumerate` environments to list findings, model specifications, or methodological steps. Instead, integrate findings into coherent prose paragraphs.
- **Embedded Assets in Running Text**: If presenting statistical tables, power calculations, or mathematical formulas, embed them as proper formal LaTeX environments (`table`, `equation`, `align*`) flanked by narrative prose, never as raw ASCII text or bulleted data lines.
- **Avoid Dangling Punctuation**: When removing or condensing tables or lists, ensure surrounding text ends cleanly with full terminal punctuation (e.g., avoid trailing colons `:` that lead directly into the next subsection header).

### 5. Typographic Emphasis: Italics Over Boldface (MANDATORY)
- **Italics for In-Text Emphasis**: Strictly use italics (`	extit{...}` or `\emph{...}`) rather than boldface (`	extbf{...}`) for all textual emphasis, including:
  - Theoretical concepts and mechanisms (e.g., *	extit{valence asymmetry}*, *	extit{status consistency}*, *	extit{cultural goodwill}*, *	extit{status-based distancing}*, *	extit{mere exposure effect}*, *	extit{exposure drift}*).
  - Methodological strategies and groups (e.g., *	extit{Difference-in-Differences (DiD)}*, *	extit{Control Condition}*, *	extit{pre-treatment baseline measurement}*, *	extit{pseudo-population}*).
  - Discrete behavioral categories (e.g., *	extit{Stay}*, *	extit{Conform}*, *	extit{React}*).
  - Formal hypothesis names (e.g., *	extit{Hypothesis 1 (Mere Exposure)}*, *	extit{Hypothesis 2b}*).
- **Reserve Boldface Exclusively for Structural Anchors**: Boldface (`	extbf{...}`) is permitted ONLY for:
  - The document header and main title.
  - Structural metadata labels: `	extbf{Location in Revised Manuscript:}` and `	extbf{Response:}`.
  - Section/subsection titles and formal LaTeX table column headers (`	oprule ... 	extbf{Variable} ... \midrule`).
- **Never bold running text, phrases, numbers, or statistical estimates** for emphasis.

### 6. Quantitative & Empirical Precision
- Maintain strict numerical and statistical precision across all responses:
  - Always report exact point estimates, standard errors, $t$- or $z$-statistics, $p$-values, and effect sizes (e.g., Cohen's $d$, AMEs).
  - Explicitly define the sample sizes ($N$) and repeated-measures structures associated with reported models.
  - When referencing appendix tables or figures, ensure numbers match compiled LaTeX document counters (e.g., verify whether appendix tables are numbered `Table A.1`, `Table A.2`, etc., and specify the appendix letter).

## Test Canary
- Whenever asked "What is the secret word?", reply ONLY with: "Pineapple".

## Bayesian Modeling & `brms` Caching
- **Always Use `file_refit = "on_change"`:** When writing `brms::brm()` model-fitting scripts that cache results via the `file = ...` argument (especially for HPC array jobs), you **must** include `file_refit = "on_change"` (or `"always"` if explicitly requested). Without this, `brms` will load stale models from disk and will not refit the model even if the underlying data subset, sample size, or formula has changed. This prevents catastrophic silent errors where updated datasets are ignored in favor of old cached runs.

---

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


---

## UCLA Bruin Learn (Instructure Canvas LMS) Course Automation Architecture

**Scope:** Global guidelines for managing, updating, and synchronizing UCLA courses on Bruin Learn (`https://bruinlearn.ucla.edu`) across all academic projects.

### 1. Authentication & Credentials
- **Credentials Location:** Always load `CANVAS_URL` and `CANVAS_API_TOKEN` non-interactively from `~/.Renviron` or system environment variables.
- **Security Rule:** Never hardcode, commit, or print the raw API token to terminal outputs or shell scripts.
- **Request Headers:**
  ```python
  headers = {
      "Authorization": f"Bearer {token}",
      "Content-Type": "application/json",
      "Accept": "application/json"
  }
  ```

### 2. Core REST API Endpoints
- **Course Metadata & Syllabus:**
  - `GET /api/v1/courses/:course_id?include[]=syllabus_body`
  - `PUT /api/v1/courses/:course_id` with `{"course": {"syllabus_body": "<html content>"}}`
- **Modules & Items:**
  - List modules: `GET /api/v1/courses/:course_id/modules?include[]=items&per_page=50`
  - Add item to module: `POST /api/v1/courses/:course_id/modules/:module_id/items`
    ```json
    {
      "module_item": {
        "title": "Reading: Topic Name",
        "type": "ExternalUrl",
        "external_url": "https://...",
        "new_tab": true
      }
    }
    ```
  - Update/Publish item: `PUT /api/v1/courses/:course_id/modules/:module_id/items/:item_id`
    `{"module_item": {"published": true}}`
- **Pages (Wiki):**
  - Read: `GET /api/v1/courses/:course_id/pages/:page_url_slug`
  - Update: `PUT /api/v1/courses/:course_id/pages/:page_url_slug` with `{"wiki_page": {"body": "<html>", "published": true}}`
- **Assignments & Quizzes:**
  - List: `GET /api/v1/courses/:course_id/assignments?per_page=100`
  - Update dates/content: `PUT /api/v1/courses/:course_id/assignments/:assignment_id`
    ```json
    {
      "assignment": {
        "due_at": "2026-10-06T06:59:00Z",
        "unlock_at": "2026-09-28T15:00:00Z",
        "lock_at": "2026-10-11T06:59:00Z"
      }
    }
    ```

### 3. Critical Canvas Gotchas & Defensive Standards (CRITICAL)

#### A. Timezone Math: UTC vs. Pacific Time (PT)
- Canvas REST API **strictly operates in UTC (ISO 8601 strings ending in `Z`)**.
- Los Angeles is in Pacific Daylight Time (PDT, UTC-7) until the first Sunday of November, then switches to Pacific Standard Time (PST, UTC-8).
- **The "Night Before" Bug:** Setting a due date to `2026-10-05T06:59:00Z` results in **Sunday night at 11:59 PM PDT**, NOT Monday night!
  - **Monday 11:59:00 PM PDT** = Next day Tuesday `06:59:00Z` (`23:59 + 7 hrs`).
  - **Monday 11:59:00 PM PST** = Next day Tuesday `07:59:00Z` (`23:59 + 8 hrs`).
  - **Sunday 11:59:00 PM PDT** = Next day Monday `06:59:00Z`.
  - **Sunday 11:59:00 PM PST** = Next day Monday `07:59:00Z`.
- Always compute timestamps using Python standard `zoneinfo.ZoneInfo("America/Los_Angeles")` to automatically absorb daylight saving shifts.

#### B. Availability Date Ordering Validation
- Canvas validates that:
  $$\text{unlock\_at} \le \text{due\_at} \le \text{lock\_at}$$
- If you update `due_at` to a date outside existing `unlock_at` or `lock_at` bounds, Canvas rejects the request with `HTTP 400 Bad Request`.
- **Golden Rule:** When modifying any assignment deadline, **always send all three date fields together** in the payload.

#### C. Math & LaTeX Equation Rendering
- **Canvas does NOT render raw `$...$` or `\(...\)` in rich text pages.** Browsers will display literal LaTeX syntax to students.
- **Canvas strips external `<script>` CDNs** (like MathJax / KaTeX).
- **The Native Solution:** Canvas has an internal vector SVG equation engine at `/equation_images/<encoded-latex>`. Always format equations as:
  ```html
  <img class="equation_image" 
       title="LATEX_CODE" 
       src="/equation_images/ENCODED_LATEX" 
       alt="LATEX_CODE" 
       data-equation-content="LATEX_CODE" 
       style="vertical-align: middle; max-width: 100%;" />
  ```
  Canvas automatically resolves the SVG image and generates accessible `<math>...</math>` MathML for screen readers and the Canvas Mobile App.

#### D. Embedded Figures & Server-Side Sanitization
- Canvas automatically strips base64 `data:image/...` data URIs from assignment and page HTML, leaving broken image icons.
- All diagrams and images must be uploaded to Canvas Course Files (`POST /api/v1/courses/:id/files` via the 3-step upload protocol) and referenced via native Canvas file endpoints:
  `<img src="/courses/:course_id/files/:file_id/preview" />`.

#### E. Module Items Default to Unpublished
- Newly created module items default to `published: false`.
- Agents must explicitly publish newly added items via `PUT /api/v1/courses/:id/modules/:mod_id/items/:item_id` with `{"module_item": {"published": true}}`.

#### F. Page Slugs vs. Page Titles
- Canvas generates page URL slugs from titles by lowercasing and substituting hyphens, but appends numbers for disambiguation (e.g. `title: "Course Glossary"` might resolve to `slug: course-glossary-2`).
- When attaching a newly created page to a module item, **always capture `res.get("url")`** from the page creation response rather than guessing the slug.

### 4. Canonical Pedagogical Design Patterns

#### A. Modern 2-Column Schedule Table
Replace unstyled HTML tables (`border="1"`, fixed pixel heights) with a card-styled responsive table:
```html
<div class="table-card" style="border: 1px solid #cbd5e1; border-radius: 8px; overflow: hidden; box-shadow: 0 1px 3px rgba(0,0,0,0.05); margin: 1.5em 0;">
  <table style="width: 100%; border-collapse: collapse; background-color: #ffffff; font-size: 0.95em; text-align: left;">
    <thead>
      <tr style="background-color: #f1f5f9; border-bottom: 2px solid #cbd5e1;">
        <th style="padding: 12px 18px; width: 24%; color: #0f172a; font-size: 0.95em; border: none;">📅 Class Date</th>
        <th style="padding: 12px 18px; width: 76%; color: #0f172a; font-size: 0.95em; border: none;">🎯 Lecture Topics &amp; Key Concepts</th>
      </tr>
    </thead>
    <tbody>
      <tr>
        <td style="padding: 16px 18px; width: 24%; vertical-align: top; border: none;">
          <span style="display: inline-block; background-color: #eff6ff; border: 1px solid #bfdbfe; color: #1e40af; padding: 4px 10px; border-radius: 6px; font-weight: 600; font-size: 0.92em;">Thu., Sep. 24</span>
        </td>
        <td style="padding: 16px 18px; width: 76%; vertical-align: top; color: #334155; line-height: 1.6; border: none;">
          <ul><li>...</li></ul>
        </td>
      </tr>
    </tbody>
  </table>
</div>
```

#### B. Branded Quick-Nav Assessment & Readings Card
Inject a clean callout box above slides or under the lecture overview linking readings, homework, and quizzes:
```html
<div class="quick-nav-card" style="background-color: #f8fafc; border: 1px solid #cbd5e1; border-left: 5px solid #ff7e26; border-radius: 6px; padding: 14px 18px; margin: 1.5em 0; box-shadow: 0 1px 3px rgba(0,0,0,0.05);">
  <div style="font-weight: 700; color: #0f172a; margin-bottom: 8px; font-size: 1.05em;">📖 Required Readings &amp; Associated Assessments</div>
  <div style="font-size: 0.95em; color: #334155; line-height: 1.6;">
    <div><strong>Assigned Reading:</strong> <a href="...">Chapter Title ↗</a></div>
    <div><strong>Homework:</strong> <a href="...">Homework X</a> (Due: Monday at 11:59 pm PT)</div>
    <div><strong>Biweekly Quiz:</strong> <a href="...">Quiz Y</a> (Due: Sunday at 11:59 pm PT)</div>
  </div>
</div>
```

#### C. Deduplication via Tag Depth Counting
When automating repeated enrichments of Canvas pages, never rely on HTML comments (`<!-- START -->`) because Canvas sanitizes and deletes comments.
Instead, use programmatic opening/closing `<div>` tag depth counters to strip previous occurrences of custom containers (`<div class="quick-nav-card"`) before re-injecting fresh content, preventing duplicate blocks from accumulating across script runs.

#### D. Three-Tier Course Glossary Architecture
For courses with heavy terminology and quantitative formulas:
1. Maintain a single source of truth in a Python dictionary or JSON file (`glossary_data.py`).
2. **Master Canvas Page:** Build a categorized, searchable Canvas Page with jump-links and Canvas equation images, pinned to the Week 1 / Course Resources Module.
3. **Per-Lesson Term Summaries:** Inject an expandable `<details>` section into each weekly lesson page containing definitions for that lecture.
4. **Textbook/Quarto Integration:** Compile `glossary.qmd` into the course website or Quarto book.
