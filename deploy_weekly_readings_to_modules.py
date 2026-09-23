import os, json, urllib.request, re
from pathlib import Path

def get_credentials():
    token = os.environ.get('CANVAS_API_TOKEN')
    url = os.environ.get('CANVAS_URL')
    renviron = Path.home() / '.Renviron'
    if renviron.exists():
        with open(renviron) as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith('#'): continue
                if line.startswith('CANVAS_API_TOKEN=') and not token:
                    token = line.split('=', 1)[1].strip('\"\' ')
                elif line.startswith('CANVAS_URL=') and not url:
                    url = line.split('=', 1)[1].strip('\"\' ')
    return (url or 'https://bruinlearn.ucla.edu').rstrip('/'), token

url, token = get_credentials()
headers = {
    'Authorization': f'Bearer {token}',
    'Content-Type': 'application/json',
    'Accept': 'application/json'
}
course_id = 239524
GDRIVE_URL = 'https://bruinlearn.ucla.edu/courses/239524/files'

# Data for all 10 weeks
WEEKS_DATA = [
    {
        'week': 1,
        'title': 'Networks As Theory and Perspective',
        'date_str': 'Mon., Sep. 28',
        'full_date': 'Monday, September 28, 2026 (3:00–5:50 pm)',
        'location': 'Public Affairs Building Room 2325',
        'module_id': 1941251,
        'memo_id': 2041357,
        'memo_due': 'Sunday, Sep. 27 at 5:00 pm PT',
        'part1_title': 'Mathematical Foundations & Relational Manifesto',
        'part1_readings': [
            'Wellman, B. (1988). Structural Analysis: From Method and Metaphor To Theory and Substance. In B. Wellman & S. D. Berkowitz (Eds.), <em>Social Structures: A Network Approach</em> (pp. 19–61). Cambridge University Press.',
            'Martin, J. L. (2009). <em>Social Structures</em>. Princeton University Press (selections).',
            'Borgatti, S. P., & Halgin, D. S. (2011). On Network Theory. <em>Organization Science</em>, 22(5), 1168–1181.',
            'Rivera, M. T., Soderstrom, S. B., & Uzzi, B. (2010). Dynamics of Dyads in Social Networks: Assortative, Relational, and Proximity Mechanisms. <em>Annual Review of Sociology</em>, 36(1), 91–115.'
        ],
        'part2_title': 'Formalism, Culture, and Agency',
        'part2_readings': [
            'Erikson, E. (2013). Formalist and Relationalist Theory In Social Network Analysis. <em>Sociological Theory</em>, 31(3), 219–242.',
            'Mische, A. (2011). Relational Sociology, Culture, and Agency. In J. Scott & P. J. Carrington (Eds.), <em>The Sage Handbook of Social Network Analysis</em> (pp. 80–97). Sage.',
            'Fuhse, J. (2020). Theories of Social Networks. In R. Light & J. Moody (Eds.), <em>The Oxford Handbook of Social Networks</em> (pp. 34–49). Oxford University Press.'
        ]
    },
    {
        'week': 2,
        'title': 'Networks and the Economy and Organizations',
        'date_str': 'Mon., Oct. 5',
        'full_date': 'Monday, October 5, 2026 (3:00–5:50 pm)',
        'location': 'Public Affairs Building Room 2325',
        'module_id': 1941251,
        'memo_id': 2041280,
        'memo_due': 'Sunday, Oct. 4 at 5:00 pm PT',
        'part1_title': 'Embeddedness & Market Social Structure',
        'part1_readings': [
            'Granovetter, M. (1985). Economic Action and Social Structure: The Problem of Embeddedness. <em>American Journal of Sociology</em>, 91(3), 481–510.',
            'Uzzi, B. (1996). The Sources and Consequences of Embeddedness for the Economic Performance of Organizations: The Network Effect. <em>American Sociological Review</em>, 61(4), 674–698.',
            'Baker, W. E. (1984). The Social Structure of a National Securities Market. <em>American Journal of Sociology</em>, 89(4), 775–811.'
        ],
        'part2_title': 'Network Forms of Organization & Market Status',
        'part2_readings': [
            'Powell, W. W. (1990). Neither Market Nor Hierarchy: Network Forms of Organization. <em>Research in Organizational Behavior</em>, 12, 295–336.',
            'Podolny, J. M. (2001). Networks as the Pipes and Prisms of the Market. <em>American Journal of Sociology</em>, 107(1), 33–60.',
            'Uzzi, B. (1997). Social Structure and Competition in Interfirm Networks: The Paradox of Embeddedness. <em>Administrative Science Quarterly</em>, 42(1), 35–67.'
        ]
    },
    {
        'week': 3,
        'title': 'Brokerage and Intermediation',
        'date_str': 'Mon., Oct. 12',
        'full_date': 'Monday, October 12, 2026 (3:00–5:50 pm)',
        'location': 'Public Affairs Building Room 2325',
        'module_id': 1941252,
        'memo_id': 2041281,
        'memo_due': 'Sunday, Oct. 11 at 5:00 pm PT',
        'part1_title': 'Structures and Processes of Mediation',
        'part1_readings': [
            'Granovetter, M. S. (1973). The Strength of Weak Ties. <em>American Journal of Sociology</em>, 78(3), 1360–1380.',
            '<em>(Optional)</em> Rejection letter from <em>American Sociological Review</em> of the first (1969) version of the paper. <a href="https://scatter.wordpress.com/wp-content/uploads/2014/10/granovetter-rejection.pdf" target="_blank" rel="noopener noreferrer" style="color: #2563eb; font-size: 0.88em; font-weight: 600; text-decoration: underline;">[PDF ↗]</a>',
            '<em>(Optional)</em> Granovetter, M. S. (1969). Alienation Reconsidered: The Strength of Weak Ties. Reprinted in <em>Connections</em> 5(2): 4–16. <a href="https://assets.noviams.com/novi-file-uploads/insna/Connections_Archive/1982_Volume_5__Issue_2-b2c89e8d.pdf" target="_blank" rel="noopener noreferrer" style="color: #2563eb; font-size: 0.88em; font-weight: 600; text-decoration: underline;">[PDF ↗]</a>',
            'Kim, M., & Fernandez, R. M. (2023). What Makes Weak Ties Strong? <em>Annual Review of Sociology</em>, 49(1), 177–193.',
            'Gould, R. V., & Fernandez, R. M. (1989). Structures of Mediation: A Formal Approach To Brokerage In Transaction Networks. <em>Sociological Methodology</em>, 89–126.',
            'Obstfeld, D., Borgatti, S. P., & Davis, J. (2014). Brokerage As a Process: Decoupling Third Party Action From Social Network Structure. <em>Research In The Sociology of Organizations</em>, 40, 135–159.',
            'Stovel, K., & Shaw, L. (2012). Brokerage. *Annual Review of Sociology*, 38, 139–158.'
        ],
        'part2_title': 'Structural Holes and Diversity-Bandwidth Tradeoffs',
        'part2_readings': [
            'Burt, R. S. (2004). Structural Holes and Good Ideas. <em>American Journal of Sociology</em>, 110(2), 349–399.',
            'Goldberg, A., Srivastava, S. B., Manian, V. G., Monroe, W., & Potts, C. (2016). Fitting In Or Standing Out? The Tradeoffs of Structural and Cultural Embeddedness. <em>American Sociological Review</em>, 81(6), 1190–1222.',
            'Aral, S., & Van Alstyne, M. (2011). The diversity-bandwidth trade-off. <em>American Journal of Sociology</em>, 117(1), 90–171.'
        ]
    },
    {
        'week': 4,
        'title': 'Networks In Science',
        'date_str': 'Mon., Oct. 19',
        'full_date': 'Monday, October 19, 2026 (3:00–5:50 pm)',
        'location': 'Public Affairs Building Room 2325',
        'module_id': 1941252,
        'memo_id': 2041282,
        'memo_due': 'Sunday, Oct. 18 at 5:00 pm PT',
        'part1_title': 'Diffusion, Specialization, and Disruption in Science',
        'part1_readings': [
            'Cheng, M., Smith, D. S., Ren, X., Cao, H., Smith, S., & McFarland, D. A. (2023). How new ideas diffuse in science. <em>American Sociological Review</em>, 88(3), 522–561.',
            'Heiberger, R. H., Munoz-Najar Galvez, S., & McFarland, D. A. (2021). Facets of specialization and its relation to career success: An analysis of US sociology, 1980 to 2015. <em>American Sociological Review</em>, 86(6), 1164–1192.',
            'Leahey, E., Lee, J., & Funk, R. J. (2023). What types of novelty are most disruptive?. <em>American Sociological Review</em>, 88(3), 562–597.'
        ],
        'part2_title': 'Tradition, Innovation, and Scientific Curation',
        'part2_readings': [
            'Foster, J. G., Rzhetsky, A., & Evans, J. A. (2015). Tradition and Innovation In Scientists’ Research Strategies. <em>American Sociological Review</em>, 80(5), 875–908.',
            'McMahan, P., & McFarland, D. A. (2021). Creative Destruction: The Structural Consequences of Scientific Curation. <em>American Sociological Review</em>, 86(2), 341–376.'
        ]
    },
    {
        'week': 5,
        'title': 'Collaboration, Creativity, and Field Dynamics',
        'date_str': 'Mon., Oct. 26',
        'full_date': 'Monday, October 26, 2026 (3:00–5:50 pm)',
        'location': 'Public Affairs Building Room 2325',
        'module_id': 1941252,
        'memo_id': 2041283,
        'memo_due': 'Sunday, Oct. 25 at 5:00 pm PT',
        'part1_title': 'Small Worlds and Structural Folds',
        'part1_readings': [
            'Vedres, B., & Stark, D. (2010). Structural Folds: Generative Disruption In Overlapping Groups. <em>American Journal of Sociology</em>, 115(4), 1150–1190.',
            'Uzzi, B., & Spiro, J. (2005). Collaboration and Creativity: The Small World Problem. <em>American Journal of Sociology</em>, 111, 447–504.'
        ],
        'part2_title': 'Field Evolution and Team Spillovers',
        'part2_readings': [
            'Powell, W. W., White, D. R., Koput, K. W., & Owen-Smith, J. (2005). Network Dynamics and Field Evolution: The Growth of Interorganizational Collaboration In The Life Sciences. <em>American Journal of Sociology</em>, 110(4), 1132–1205.',
            'Rossman, G., Esparza, N., & Bonacich, P. (2010). I’d Like To Thank The Academy, Team Spillovers, and Network Centrality. <em>American Sociological Review</em>, 75(1), 31–51.'
        ]
    },
    {
        'week': 6,
        'title': 'Networks and Culture and Culture in Networks',
        'date_str': 'Mon., Nov. 2',
        'full_date': 'Monday, November 2, 2026 (3:00–5:50 pm)',
        'location': 'Public Affairs Building Room 2325',
        'module_id': 1941253,
        'memo_id': 2041284,
        'memo_due': 'Sunday, Nov. 1 at 5:00 pm PT',
        'part1_title': 'Dualities of Culture and Structure',
        'part1_readings': [
            'Breiger, R. L. (2010). Dualities of culture and structure: Seeing through cultural holes. Pp. 37–47 in <em>Relationale soziologie</em>. VS Verlag für Sozialwissenschaften.',
            'Lizardo, O. (2023). Culture and Networks. Pp. 188–201 in <em>The SAGE Handbook of Social Network Analysis</em>. SAGE Publications Ltd.',
            'Lewis, K., & Kaufman, J. (2018). The Conversion of Cultural Tastes Into Social Network Ties. <em>American Journal of Sociology</em>, 123(6), 1684–1742.',
            'Fuhse, J. A., & Gondal, N. (2024). Networks from culture: Mechanisms of tie-formation follow institutionalized rules in social fields. <em>Social Networks</em>, 77, 43–54.'
        ],
        'part2_title': 'Meaning Structure, Publics, and Relational Sociology',
        'part2_readings': [
            'Fuhse, J. A. (2009). The Meaning Structure of Social Networks. <em>Sociological Theory</em>, 27(1), 51–73.',
            'Ikegami, E. (2000). A Sociological Theory of Publics: Identity and Culture As Emergent Properties In Networks. <em>Social Research</em>, 989–1029.',
            'Mützel, S., & Breiger, R. (2021). Duality Beyond Persons and Groups. In <em>The Oxford Handbook of Social Networks</em>. Oxford University Press.',
            'Fuhse, J., & Mische, A. (2024). Relational Sociology: Networks, Culture and Interaction. In J. McLevey, J. Scott, & P. J. Carrington (Eds.), <em>The SAGE Handbook of Social Network Analysis</em> (2nd ed., pp. 55–71). SAGE.'
        ]
    },
    {
        'week': 7,
        'title': 'Diffusion In Networks',
        'date_str': 'Mon., Nov. 9',
        'full_date': 'Monday, November 9, 2026 (3:00–5:50 pm)',
        'location': 'Public Affairs Building Room 2325',
        'module_id': 1941253,
        'memo_id': 2041285,
        'memo_due': 'Sunday, Nov. 8 at 5:00 pm PT',
        'part1_title': 'Complex Contagion, Clustering, and Spatial Diffusion',
        'part1_readings': [
            'Centola, D., & Macy, M. (2007). Complex Contagions and the Weakness of Long Ties. <em>American Journal of Sociology</em>, 113(3), 702–734.',
            'DellaPosta, D., Shi, Y., & Macy, M. (2015). Why Do Liberals Drink Lattes? <em>American Journal of Sociology</em>, 120(5), 1473–1511.',
            'Centola, D. (2015). The social origins of networks and diffusion. <em>American Journal of Sociology</em>, 120(5), 1295–1338.',
            'Becker, S. O., Hsiao, Y., Pfaff, S., & Rubin, J. (2020). Multiplex Network Ties and the Spatial Diffusion of Radical Innovations: Martin Luther\'s Leadership In The Early Reformation. <em>American Sociological Review</em>, 85(5), 857–894.'
        ],
        'part2_title': 'Associative Diffusion and Cultural Currents',
        'part2_readings': [
            'Goldberg, A., & Stein, S. K. (2018). Beyond Social Contagion: Associative Diffusion and The Emergence of Cultural Variation. <em>American Sociological Review</em>, 83(5), 897–932.',
            'Bail, C. A., Brown, T. W., & Mann, M. (2017). Channeling hearts and minds: Advocacy organizations, cognitive-emotional currents, and public conversation. <em>American Sociological Review</em>, 82(6), 1188–1213.'
        ]
    },
    {
        'week': 8,
        'title': 'Networks In History',
        'date_str': 'Mon., Nov. 16',
        'full_date': 'Monday, November 16, 2026 (3:00–5:50 pm)',
        'location': 'Public Affairs Building Room 2325',
        'module_id': 1941253,
        'memo_id': 2041286,
        'memo_due': 'Sunday, Nov. 15 at 5:00 pm PT',
        'part1_title': 'Robust Action and Historical Mobilization',
        'part1_readings': [
            'Padgett, J. F., & Ansell, C. K. (1993). Robust Action and the Rise of the Medici, 1400–1434. <em>American Journal of Sociology</em>, 98(6), 1259–1319.',
            'Gould, R. V. (1991). Multiple Networks and Mobilization In The Paris Commune, 1871. <em>American Sociological Review</em>, 56(6), 716–729.',
            'Erikson, E., & Feltham, E. (2021). Historical Network Research. In <em>The Oxford Handbook of Social Networks</em>. Oxford University Press.'
        ],
        'part2_title': 'Global Trade, Malfeasance, and Historical Social Science',
        'part2_readings': [
            'Erikson, E., & Bearman, P. (2006). Malfeasance and the Foundations For Global Trade: The Structure of English Trade In The East Indies, 1601–1833. <em>American Journal of Sociology</em>, 112(1), 195–230.',
            'Bearman, P., Faris, R., & Moody, J. (1999). Blocking The Future: New Solutions For Old Problems In Historical Social Science. <em>Social Science History</em>, 23(4), 501–533.'
        ]
    },
    {
        'week': 9,
        'title': 'Networks and Inequality',
        'date_str': 'Mon., Nov. 23',
        'full_date': 'Monday, November 23, 2026 (3:00–5:50 pm)',
        'location': 'Public Affairs Building Room 2325',
        'module_id': 1941254,
        'memo_id': 2041287,
        'memo_due': 'Sunday, Nov. 22 at 5:00 pm PT',
        'part1_title': 'Origins of Status Hierarchies & Inherited Inequality',
        'part1_readings': [
            'McPherson, M., Smith-Lovin, L., & Cook, J. M. (2001). Birds of a feather: Homophily in social networks. <em>Annual Review of Sociology</em>, 27(1), 415–444.',
            'Gould, R. V. (2002). The Origins of Status Hierarchies: A Formal Theory and Empirical Test. <em>American Journal of Sociology</em>, 107(5), 1143–1178.',
            'Gondal, N. (2015). Inequality preservation through uneven diffusion of Cultural materials across stratified groups. <em>Social Forces</em>, 93(3), 1109–1137.',
            'Thomas, R. J., & Mark, N. P. (2013). Population size, network density, and the emergence of inherited inequality. <em>Social Forces</em>, 92(2), 521–544.'
        ],
        'part2_title': 'Academic Caste Systems & Faculty Hiring Networks',
        'part2_readings': [
            'Burris, V. (2004). The Academic Caste System: Prestige Hierarchies In PhD Exchange Networks. <em>American Sociological Review</em>, 69, 239–264.',
            'Fowler, J. H., Grofman, B., & Masuoka, N. (2007). Social networks in political science: Hiring and placement of Ph.Ds, 1960–2002. <em>PS: Political Science & Politics</em>, 40(4), 729–739.',
            'Clauset, A., Arbesman, S., & Larremore, D. B. (2015). Systematic inequality and hierarchy in faculty hiring networks. <em>Science Advances</em>, 1(1), e1400005.',
            'Gondal, N. (2018). Duality of departmental specializations and PhD exchange: A Weberian analysis of status in interaction using multilevel exponential random graph models (mERGM). <em>Social Networks</em>, 55, 202–212.'
        ]
    },
    {
        'week': 10,
        'title': 'Networks and The Micro/Macro Link',
        'date_str': 'Mon., Nov. 30',
        'full_date': 'Monday, November 30, 2026 (3:00–5:50 pm)',
        'location': 'Public Affairs Building Room 2325',
        'module_id': 1941254,
        'memo_id': 2041279,
        'memo_due': 'Sunday, Nov. 29 at 5:00 pm PT',
        'part1_title': 'Network Ecology & Romantic/Sexual Networks',
        'part1_readings': [
            'McFarland, D. A., Moody, J., Diehl, D., Smith, J. A., & Thomas, R. J. (2014). Network Ecology and Adolescent Social Structure. <em>American Sociological Review</em>, 79(6), 1088–1121.',
            'Bearman, P. S., Moody, J., & Stovel, K. (2004). Chains of Affection: The Structure of Adolescent Romantic and Sexual Networks. <em>American Journal of Sociology</em>, 110(1), 44–91.'
        ],
        'part2_title': 'Urban Mobility, Migration, and Spatial Disadvantage',
        'part2_readings': [
            'Levy, B. L., Phillips, N. E., & Sampson, R. J. (2020). Triple Disadvantage: Neighborhood Networks of Everyday Urban Mobility and Violence In US Cities. <em>American Sociological Review</em>, 85(6), 925–956.',
            'Papachristos, A. V. (2009). Murder By Structure: Dominance Relations and the Social Structure of Gang Homicide. <em>American Journal of Sociology</em>, 115(1), 74–128.',
            'Huang, P., & Butts, C. T. (2023). Rooted America: Immobility and Segregation of the Intercounty Migration Network. <em>American Sociological Review</em>, 88(6), 1031–1065.',
            'Cheng, S., & Park, B. (2020). Flows and Boundaries: A Network Approach to Studying Occupational Mobility in the Labor Market. <em>American Journal of Sociology</em>, 126(3), 577–631.'
        ]
    }
]

def make_weekly_page_html(w):
    p1_items = ''.join([f'<li style="margin-bottom: 8px;">{r} <a href="{GDRIVE_URL}" target="_blank" style="color: #2563eb; font-size: 0.88em; font-weight: 600; text-decoration: none;">[PDF ↗]</a></li>' for r in w['part1_readings']])
    p2_items = ''.join([f'<li style="margin-bottom: 8px;">{r} <a href="{GDRIVE_URL}" target="_blank" style="color: #2563eb; font-size: 0.88em; font-weight: 600; text-decoration: none;">[PDF ↗]</a></li>' for r in w['part2_readings']])
    
    html = f'''<link rel="stylesheet" href="https://instructure-uploads.s3.amazonaws.com/account_148090000000000001/attachments/14738291/dp_app.css">

<div style="max-width: 900px; margin: 0 auto; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; color: #1e293b; line-height: 1.6;">

  <!-- Week Banner -->
  <div style="background: linear-gradient(135deg, #1e3a8a 0%, #2563eb 100%); color: #ffffff; padding: 24px 28px; border-radius: 10px; margin-bottom: 20px; box-shadow: 0 4px 6px rgba(0,0,0,0.08);">
    <div style="font-size: 0.85em; font-weight: 700; text-transform: uppercase; letter-spacing: 0.05em; color: #bfdbfe; margin-bottom: 6px;">Week {w['week']} &bull; Seminar Overview &amp; Assigned Readings</div>
    <h1 style="margin: 0 0 10px 0; font-size: 1.85em; font-weight: 800; color: #ffffff; letter-spacing: -0.01em;">{w['title']}</h1>
    <div style="font-size: 0.95em; color: #e2e8f0;">📅 {w['full_date']} &bull; 📍 {w['location']}</div>
  </div>

  <!-- Quick Action Nav Card -->
  <div class="quick-nav-card" style="background-color: #f8fafc; border: 1px solid #cbd5e1; border-left: 5px solid #2563eb; border-radius: 6px; padding: 14px 18px; margin: 16px 0 24px 0; box-shadow: 0 1px 3px rgba(0,0,0,0.04);">
    <div style="font-weight: 700; color: #0f172a; margin-bottom: 6px; font-size: 1.02em;">📝 Weekly Action Items &amp; Submissions</div>
    <div style="font-size: 0.92em; color: #334155; line-height: 1.6;">
      <div><strong>Weekly Analytic Memo:</strong> <a href="/courses/239524/assignments/{w['memo_id']}" style="color: #2563eb; font-weight: 600;">Week {w['week']} Analytic Memo Submission Box &rarr;</a> (Due: <strong>{w['memo_due']}</strong>)</div>
      <div><strong>Reading Requirement:</strong> Pick at least <em>one reading from Part 1</em> and at least <em>one reading from Part 2</em> to put in conversation with each other.</div>
      <div><strong>Discussion Leaders:</strong> Post at least two questions per assigned reading to the <a href="/courses/239524/discussion_topics/1466035" style="color: #2563eb; font-weight: 600;">Discussion Leaders Summary &amp; Questions Board &rarr;</a></div>
      <div><strong>PDF Access:</strong> All readings are available in the <a href="{GDRIVE_URL}" target="_blank" rel="noopener noreferrer" style="color: #2563eb; font-weight: 600; text-decoration: underline;">Course Files Folder ↗</a></div>
    </div>
  </div>

  <!-- Part 1 Readings -->
  <div class="table-card" style="border: 1px solid #cbd5e1; border-radius: 8px; overflow: hidden; box-shadow: 0 1px 3px rgba(0,0,0,0.04); margin-bottom: 22px;">
    <div style="background-color: #f1f5f9; border-bottom: 2px solid #cbd5e1; padding: 12px 18px;">
      <h2 style="margin: 0; font-size: 1.15em; color: #0f172a; font-weight: 700;">📖 Part 1: {w['part1_title']}</h2>
    </div>
    <div style="padding: 16px 20px; background-color: #ffffff;">
      <ul style="margin: 0; padding-left: 20px; color: #334155;">
        {p1_items}
      </ul>
    </div>
  </div>

  <!-- Part 2 Readings -->
  <div class="table-card" style="border: 1px solid #cbd5e1; border-radius: 8px; overflow: hidden; box-shadow: 0 1px 3px rgba(0,0,0,0.04); margin-bottom: 24px;">
    <div style="background-color: #f1f5f9; border-bottom: 2px solid #cbd5e1; padding: 12px 18px;">
      <h2 style="margin: 0; font-size: 1.15em; color: #0f172a; font-weight: 700;">📖 Part 2: {w['part2_title']}</h2>
    </div>
    <div style="padding: 16px 20px; background-color: #ffffff;">
      <ul style="margin: 0; padding-left: 20px; color: #334155;">
        {p2_items}
      </ul>
    </div>
  </div>

  <!-- Footer Navigation -->
  <div style="display: flex; justify-content: space-between; border-top: 1px solid #e2e8f0; padding-top: 14px; margin-top: 24px;">
    <a href="/courses/239524/modules" style="color: #2563eb; font-size: 0.92em; font-weight: 600; text-decoration: none;">&larr; Back to Modules</a>
    <a href="/courses/239524/pages/sociol-208b-reading-schedule-fall-2026" style="color: #2563eb; font-size: 0.92em; font-weight: 600; text-decoration: none;">View Full 10-Week Schedule &rarr;</a>
  </div>

</div>'''
    return html

# Create weekly pages and add to Canvas
def deploy():
    # Update Module 1 subheader for Week 2
    try:
        sub_req = urllib.request.Request(
            f'{url}/api/v1/courses/{course_id}/modules/1941251/items/8303279',
            data=json.dumps({'module_item': {'title': 'Week 2: Networks and the Economy and Organizations (Mon., Oct. 5)'}}).encode('utf-8'),
            headers=headers,
            method='PUT'
        )
        with urllib.request.urlopen(sub_req) as resp:
            print('Updated Week 2 subheader in Module 1')
    except Exception as e:
        print('Error updating subheader:', e)

    page_slugs = {}
    for w in WEEKS_DATA:
        wnum = w['week']
        wtitle = w['title']
        slug = f'week-{wnum}-readings-and-overview'
        page_title = f'Week {wnum}: {wtitle} (Readings & Overview)'
        page_html = make_weekly_page_html(w)
        
        # 1. Create or update page
        page_payload = {
            'wiki_page': {
                'title': page_title,
                'body': page_html,
                'published': True
            }
        }
        
        req = urllib.request.Request(f'{url}/api/v1/courses/{course_id}/pages/{slug}', data=json.dumps(page_payload).encode('utf-8'), headers=headers, method='PUT')
        try:
            with urllib.request.urlopen(req) as resp:
                res = json.loads(resp.read().decode('utf-8'))
                final_slug = res.get('url')
                print(f'Page updated via PUT: {final_slug}')
        except urllib.error.HTTPError as e:
            if e.code == 404:
                req2 = urllib.request.Request(f'{url}/api/v1/courses/{course_id}/pages', data=json.dumps(page_payload).encode('utf-8'), headers=headers, method='POST')
                with urllib.request.urlopen(req2) as resp2:
                    res = json.loads(resp2.read().decode('utf-8'))
                    final_slug = res.get('url')
                    print(f'Page created via POST: {final_slug}')
            else:
                raise
        
        page_slugs[wnum] = final_slug
        
        # 2. Add page to module
        mod_id = w['module_id']
        mod_item_payload = {
            'module_item': {
                'title': page_title,
                'type': 'Page',
                'page_url': final_slug,
                'indent': 0
            }
        }
        item_req = urllib.request.Request(f'{url}/api/v1/courses/{course_id}/modules/{mod_id}/items', data=json.dumps(mod_item_payload).encode('utf-8'), headers=headers, method='POST')
        try:
            with urllib.request.urlopen(item_req) as resp:
                item_res = json.loads(resp.read().decode('utf-8'))
                item_id = item_res['id']
                # Publish item
                pub_req = urllib.request.Request(f'{url}/api/v1/courses/{course_id}/modules/{mod_id}/items/{item_id}', data=json.dumps({'module_item': {'published': True}}).encode('utf-8'), headers=headers, method='PUT')
                with urllib.request.urlopen(pub_req):
                    print(f'   Added and published item {item_id} in Module {mod_id}')
        except Exception as e:
            print(f'   Error adding item to module: {e}')

    return page_slugs

if __name__ == '__main__':
    slugs = deploy()
    print('All weekly pages deployed:', slugs)
