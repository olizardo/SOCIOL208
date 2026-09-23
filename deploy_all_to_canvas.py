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

doi_map = {
    'Wellman, B. (1988)': None,
    'Martin, J. L. (2009)': None,
    'Borgatti, S. P., & Halgin': '10.1287/orsc.1100.0641',
    'Borgatti, S. P., &amp; Halgin': '10.1287/orsc.1100.0641',
    'Rivera, M. T.': '10.1146/annurev.soc.34.040507.134743',
    'Erikson, E. (2013)': '10.1177/0735275113501998',
    'Mische, A. (2011)': '10.4135/9781446294413.n7',
    'Fuhse, J. (2020)': '10.1093/oxfordhb/9780190251765.013.5',

    # Week 2 (Brokerage)
    'Granovetter, M. S. (1973)': '10.1086/225469',
    'Kim, M., & Fernandez': '10.1146/annurev-soc-030921-034152',
    'Kim, M., &amp; Fernandez': '10.1146/annurev-soc-030921-034152',
    'Gould, R. V., & Fernandez': '10.2307/270949',
    'Gould, R. V., &amp; Fernandez': '10.2307/270949',
    'Obstfeld, D.': '10.1108/S0733-558X(2014)0000040007',
    'Stovel, K.': '10.1146/annurev-soc-081309-150054',
    'Burt, R. S. (2004)': '10.1086/421787',
    'Goldberg, A., Srivastava': '10.1177/0003122416671873',
    'Aral, S.': '10.1086/661238',

    # Week 3 (Economy)
    'Granovetter, M. (1985)': '10.1086/228311',
    'Uzzi, B. (1996)': '10.2307/2096399',
    'Baker, W. E. (1984)': '10.1086/227944',
    'Powell, W. W. (1990)': None,
    'Podolny, J. M. (2001)': '10.1086/323038',
    'Uzzi, B. (1997)': '10.2307/2393808',

    # Week 4
    'Smith, D., Kennard': '10.1177/00031224251362254',
    'Moody, J. (2004)': '10.1177/000312240406900204',
    'Shwed, U.': '10.1177/0003122410388488',
    'Moody, J., & Light': '10.1007/s12108-006-1006-8',
    'Moody, J., &amp; Light': '10.1007/s12108-006-1006-8',
    'Foster, J. G.': '10.1177/0003122415601618',
    'McMahan, P.': '10.1177/0003122421996323',

    # Week 5
    'Vedres, B.': '10.1086/649497',
    'Uzzi, B., & Spiro': '10.1086/432782',
    'Uzzi, B., &amp; Spiro': '10.1086/432782',
    'De Vaan, M.': '10.1086/681213',
    'Lutter, M. (2015)': '10.1177/0003122414568788',
    'Powell, W. W., White': '10.1086/421508',
    'Rossman, G.': '10.1177/0003122409359164',

    # Week 6
    'Breiger, R. L. (2010)': '10.1007/978-3-531-92402-1_2',
    'Lizardo, O. (2023)': '10.4135/9781529614695.n13',
    'Lewis, K.': '10.1086/697525',
    'Fuhse, J. A., & Gondal': '10.1016/j.socnet.2021.12.005',
    'Fuhse, J. A., &amp; Gondal': '10.1016/j.socnet.2021.12.005',
    'Fuhse, J. A. (2009)': '10.1111/j.1467-9558.2009.00338.x',
    'Ikegami, E. (2000)': None,
    'Mützel, S.': '10.1093/oxfordhb/9780190251765.013.9',
    'M&uuml;tzel, S.': '10.1093/oxfordhb/9780190251765.013.9',
    'Fuhse, J., & Mische': '10.4135/9781529614695.n5',
    'Fuhse, J., &amp; Mische': '10.4135/9781529614695.n5',

    # Week 7
    'Centola, D., & Macy': '10.1086/521848',
    'Centola, D., &amp; Macy': '10.1086/521848',
    'DellaPosta, D.': '10.1086/681254',
    'Centola, D. (2015)': '10.1086/681275',
    'Becker, S. O.': '10.1177/0003122420948059',
    'Goldberg, A., & Stein': '10.1177/0003122418797576',
    'Goldberg, A., &amp; Stein': '10.1177/0003122418797576',
    'Bail, C. A.': '10.1177/0003122417733673',

    # Week 8
    'Padgett, J. F.': '10.1086/230190',
    'Gould, R. V. (1991)': '10.2307/2096251',
    'Erikson, E., & Feltham': '10.1093/oxfordhb/9780190251765.013.40',
    'Erikson, E., &amp; Feltham': '10.1093/oxfordhb/9780190251765.013.40',
    'Erikson, E., & Bearman': '10.1086/502694',
    'Erikson, E., &amp; Bearman': '10.1086/502694',
    'Bearman, P., Faris': '10.1017/s0145553200021854',

    # Week 9
    'McPherson, M.': '10.1146/annurev.soc.27.1.415',
    'Gould, R. V. (2002)': '10.1086/341744',
    'Gondal, N. (2015)': '10.1093/sf/sou101',
    'Thomas, R. J.': '10.1093/sf/sot080',
    'Burris, V. (2004)': '10.1177/000312240406900205',
    'Fowler, J. H.': '10.1017/s104909650707117x',
    'Clauset, A.': '10.1126/sciadv.1400005',
    'Gondal, N. (2018)': '10.1016/j.socnet.2018.07.005',

    # Week 10
    'McFarland, D. A.': '10.1177/0003122414554001',
    'Bearman, P. S., Moody': '10.1086/386272',
    'Levy, B. L.': '10.1177/0003122420972323',
    'Papachristos, A. V.': '10.1086/597791'
}

def get_doi_for_citation(c):
    for k, v in doi_map.items():
        if k in c:
            return v
    return None

# Update front-page.html to use [DOI ↗]
with open('front-page.html') as f:
    fp_text = f.read()

fp_text = fp_text.replace('[link ↗]', '[DOI ↗]')
with open('front-page.html', 'w') as f:
    f.write(fp_text)
with open('bruinlearn_course_materials/pages/front-page.html', 'w') as f:
    f.write(fp_text)

# Update sociol-208b-reading-schedule-fall-2026.html to use [DOI ↗]
with open('bruinlearn_course_materials/pages/sociol-208b-reading-schedule-fall-2026.html') as f:
    sched_text = f.read()

sched_text = sched_text.replace('>[link]</a>', '>[DOI ↗]</a>')
sched_text = sched_text.replace('>[Course Files]</a>', '>[Course Files ↗]</a>')
with open('bruinlearn_course_materials/pages/sociol-208b-reading-schedule-fall-2026.html', 'w') as f:
    f.write(sched_text)
with open('schedule-208B.html', 'w') as f:
    f.write(sched_text)

print('Updated local HTML templates with [DOI ↗] labels.')

# 1. Push Front Page to Canvas
print('\n--- Deploying Front Page to Canvas ---')
put_front = urllib.request.Request(
    f'{url}/api/v1/courses/{course_id}/pages/sociol-208b-social-network-methods',
    data=json.dumps({
        'wiki_page': {
            'title': 'SOCIOL 208B: Social Network Methods',
            'body': fp_text,
            'published': True
        }
    }).encode('utf-8'),
    headers=headers,
    method='PUT'
)
with urllib.request.urlopen(put_front) as resp:
    res = json.loads(resp.read().decode('utf-8'))
    print('Front Page updated on Canvas successfully! Slug:', res.get('url'))

# Ensure it is set as front page
set_fp = urllib.request.Request(
    f'{url}/api/v1/courses/{course_id}',
    data=json.dumps({
        'course': {
            'default_view': 'wiki'
        }
    }).encode('utf-8'),
    headers=headers,
    method='PUT'
)
with urllib.request.urlopen(set_fp):
    print('Course default view verified as wiki')

# 2. Push Reading Schedule to Canvas
print('\n--- Deploying Reading Schedule Page to Canvas ---')
put_sched = urllib.request.Request(
    f'{url}/api/v1/courses/{course_id}/pages/sociol-208b-reading-schedule-fall-2026',
    data=json.dumps({
        'wiki_page': {
            'title': 'SOCIOL 208B Reading Schedule (Fall 2026)',
            'body': sched_text,
            'published': True
        }
    }).encode('utf-8'),
    headers=headers,
    method='PUT'
)
with urllib.request.urlopen(put_sched) as resp:
    res = json.loads(resp.read().decode('utf-8'))
    print('Reading Schedule Page updated on Canvas successfully! Slug:', res.get('url'))

# 2b. Push Syllabus Page to Canvas
print('\n--- Deploying Syllabus Page to Canvas ---')
with open('bruinlearn_course_materials/pages/sociol-208b-syllabus-fall-2026.html') as f:
    syl_text = f.read()

put_syl = urllib.request.Request(
    f'{url}/api/v1/courses/{course_id}/pages/sociol-208b-syllabus-fall-2026',
    data=json.dumps({
        'wiki_page': {
            'title': 'SOCIOL 208B Syllabus (Fall 2026)',
            'body': syl_text,
            'published': True
        }
    }).encode('utf-8'),
    headers=headers,
    method='PUT'
)
with urllib.request.urlopen(put_syl) as resp:
    res = json.loads(resp.read().decode('utf-8'))
    print('Syllabus Page updated on Canvas successfully! Slug:', res.get('url'))

# 3. Deploy all 10 Weekly Pages to Canvas with DOI links
print('\n--- Deploying Weekly Overview Pages to Canvas ---')

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
        'slug': 'week-1-networks-as-theory-and-perspective-readings-and-overview',
        'part1_title': 'Structural Foundations & Network Theory',
        'part1_readings': [
            ('Wellman, B. (1988). Structural Analysis: From Method and Metaphor To Theory and Substance. In B. Wellman & S. D. Berkowitz (Eds.), <em>Social Structures: A Network Approach</em> (pp. 19–61). Cambridge University Press.', None),
            ('Martin, J. L. (2009). <em>Social Structures</em>. Princeton University Press (selections).', None),
            ('Borgatti, S. P., & Halgin, D. S. (2011). On Network Theory. <em>Organization Science</em>, 22(5), 1168–1181.', '10.1287/orsc.1100.0641'),
            ('Rivera, M. T., Soderstrom, S. B., & Uzzi, B. (2010). Dynamics of Dyads in Social Networks: Assortative, Relational, and Proximity Mechanisms. <em>Annual Review of Sociology</em>, 36(1), 91–115.', '10.1146/annurev.soc.34.040507.134743')
        ],
        'part2_title': 'Relational Sociology, Culture, and Agency',
        'part2_readings': [
            ('Erikson, E. (2013). Formalist and Relationalist Theory In Social Network Analysis. <em>Sociological Theory</em>, 31(3), 219–242.', '10.1177/0735275113501998'),
            ('Mische, A. (2011). Relational Sociology, Culture, and Agency. In J. Scott & P. J. Carrington (Eds.), <em>The Sage Handbook of Social Network Analysis</em> (pp. 80–97). Sage.', '10.4135/9781446294413.n7'),
            ('Fuhse, J. (2020). Theories of Social Networks. In R. Light & J. Moody (Eds.), <em>The Oxford Handbook of Social Networks</em> (pp. 34–49). Oxford University Press.', '10.1093/oxfordhb/9780190251765.013.5')
        ]
    },
    {
        'week': 2,
        'title': 'Brokerage and Intermediation',
        'date_str': 'Mon., Oct. 5',
        'full_date': 'Monday, October 5, 2026 (3:00–5:50 pm)',
        'location': 'Public Affairs Building Room 2325',
        'module_id': 1941251,
        'memo_id': 2041280,
        'memo_due': 'Sunday, Oct. 4 at 5:00 pm PT',
        'slug': 'week-2-brokerage-and-intermediation-readings-and-overview',
        'part1_title': 'Weak Ties, Brokerage Roles, and Mediation',
        'part1_readings': [
            ('Granovetter, M. S. (1973). The Strength of Weak Ties. <em>American Journal of Sociology</em>, 78(3), 1360–1380.', '10.1086/225469'),
            ('Rejection letter from <em>American Sociological Review</em> of the first (1969) version of the paper.', None),
            ('Granovetter, M. S. (1969). Alienation Reconsidered: The Strength of Weak Ties. Reprinted in <em>Connections</em> 5(2): 4–16.', None),
            ('Kim, M., & Fernandez, R. M. (2023). What Makes Weak Ties Strong? <em>Annual Review of Sociology</em>, 49(1), 177–193.', '10.1146/annurev-soc-030921-034152'),
            ('Gould, R. V., & Fernandez, R. M. (1989). Structures of Mediation: A Formal Approach To Brokerage In Transaction Networks. <em>Sociological Methodology</em>, 89–126.', '10.2307/270949'),
            ('Obstfeld, D., Borgatti, S. P., & Davis, J. (2014). Brokerage As a Process: Decoupling Third Party Action From Social Network Structure. <em>Research In The Sociology of Organizations</em>, 40, 135–159.', '10.1108/S0733-558X(2014)0000040007'),
            ('Stovel, K., & Shaw, L. (2012). Brokerage. <em>Annual Review of Sociology</em>, 38, 139–158.', '10.1146/annurev-soc-081309-150054')
        ],
        'part2_title': 'Structural Holes, Embeddedness, and Information Bandwidth',
        'part2_readings': [
            ('Burt, R. S. (2004). Structural Holes and Good Ideas. <em>American Journal of Sociology</em>, 110(2), 349–399.', '10.1086/421787'),
            ('Goldberg, A., Srivastava, S. B., Manian, V. G., Monroe, W., & Potts, C. (2016). Fitting In Or Standing Out? The Tradeoffs of Structural and Cultural Embeddedness. <em>American Sociological Review</em>, 81(6), 1190–1222.', '10.1177/0003122416671873'),
            ('Aral, S., & Van Alstyne, M. (2011). The diversity-bandwidth trade-off. <em>American Journal of Sociology</em>, 117(1), 90–171.', '10.1086/661238')
        ]
    },
    {
        'week': 3,
        'title': 'Networks and the Economy and Organizations',
        'date_str': 'Mon., Oct. 12',
        'full_date': 'Monday, October 12, 2026 (3:00–5:50 pm)',
        'location': 'Public Affairs Building Room 2325',
        'module_id': 1941252,
        'memo_id': 2041281,
        'memo_due': 'Sunday, Oct. 11 at 5:00 pm PT',
        'slug': 'week-3-networks-and-the-economy-and-organizations-readings-and-overview',
        'part1_title': 'Embeddedness, Price Setting, and Market Dynamics',
        'part1_readings': [
            ('Uzzi, B. (1996). The Sources and Consequences of Embeddedness for the Economic Performance of Organizations: The Network Effect. <em>American Sociological Review</em>, 61(4), 674–698.', '10.2307/2096399'),
            ('Fernandez-Mateo, I. (2007). Who Pays the Price of Brokerage? Transferring Constraint through Price Setting in the Staffing Sector. <em>American Sociological Review</em>, 72(2), 291–317.', '10.1177/000312240707200208'),
            ('Duxbury, S. W., & Haynie, D. L. (2021). Shining a Light on the Shadows: Endogenous Trade Structure and the Growth of an Online Illegal Market. <em>American Journal of Sociology</em>, 127(3), 787–827.', '10.1086/718197')
        ],
        'part2_title': 'Market Status, Interfirm Networks, and Hybrid Economic Forms',
        'part2_readings': [
            ('Podolny, J. M. (2001). Networks as the Pipes and Prisms of the Market. <em>American Journal of Sociology</em>, 107(1), 33–60.', '10.1086/323038'),
            ('Uzzi, B. (1997). Social Structure and Competition in Interfirm Networks: The Paradox of Embeddedness. <em>Administrative Science Quarterly</em>, 42(1), 35–67.', '10.2307/2393808'),
            ('Mani, D., & Moody, J. (2014). Moving beyond Stylized Economic Network Models: The Hybrid World of the Indian Firm Ownership Network. <em>American Journal of Sociology</em>, 119(6), 1629–1669.', '10.1086/676040')
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
        'slug': 'week-4-networks-in-science-readings-and-overview',
        'part1_title': 'Specialization, Novelty, and Idea Diffusion',
        'part1_readings': [
            ('Smith, D., Kennard, N., Du, T., & McFarland, D. (2025). How Values and Uncertainty Shape Scientific Advance in Peer Review. <em>American Sociological Review</em>, 90(5), 879–915.', '10.1177/00031224251362254'),
            ('Heiberger, R. H., Munoz-Najar Galvez, S., & McFarland, D. A. (2021). Facets of specialization and its relation to career success: An analysis of US sociology, 1980 to 2015. <em>American Sociological Review</em>, 86(6), 1164–1192.', '10.1177/00031224211056267'),
            ('Leahey, E., Lee, J., & Funk, R. J. (2023). What types of novelty are most disruptive?. <em>American Sociological Review</em>, 88(3), 562–597.', '10.1177/00031224231168074')
        ],
        'part2_title': 'Innovation Strategies, Scientific Consensus, and Curation',
        'part2_readings': [
            ('Foster, J. G., Rzhetsky, A., & Evans, J. A. (2015). Tradition and Innovation In Scientists’ Research Strategies. <em>American Sociological Review</em>, 80(5), 875–908.', '10.1177/0003122415601618'),
            ('McMahan, P., & McFarland, D. A. (2021). Creative Destruction: The Structural Consequences of Scientific Curation. <em>American Sociological Review</em>, 86(2), 341–376.', '10.1177/0003122421996323'),
            ('Shwed, U., & Bearman, P. S. (2010). The Temporal Structure of Scientific Consensus Formation. <em>American Sociological Review</em>, 75(6), 817–840.', '10.1177/0003122410388488')
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
        'slug': 'week-5-collaboration-creativity-and-field-dynamics-readings-and-overview',
        'part1_title': 'Structural Folds & Small Worlds of Creativity',
        'part1_readings': [
            ('Vedres, B., & Stark, D. (2010). Structural Folds: Generative Disruption In Overlapping Groups. <em>American Journal of Sociology</em>, 115(4), 1150–1190.', '10.1086/649497'),
            ('Uzzi, B., & Spiro, J. (2005). Collaboration and Creativity: The Small World Problem. <em>American Journal of Sociology</em>, 111, 447–504.', '10.1086/432782'),
            ('De Vaan, M., Stark, D., & Vedres, B. (2015). Game Changer: The Topology of Creativity. <em>American Journal of Sociology</em>, 120(4), 1144–1194.', '10.1086/681213'),
            ('Lutter, M. (2015). Do Women Suffer from Network Closure? The Moderating Effect of Social Capital on Gender Inequality in a Project-Based Labor Market, 1929 to 2010. <em>American Sociological Review</em>, 80(2), 329–358.', '10.1177/0003122414568788')
        ],
        'part2_title': 'Interorganizational Fields & Team Spillovers',
        'part2_readings': [
            ('Powell, W. W., White, D. R., Koput, K. W., & Owen-Smith, J. (2005). Network Dynamics and Field Evolution: The Growth of Interorganizational Collaboration In The Life Sciences. <em>American Journal of Sociology</em>, 110(4), 1132–1205.', '10.1086/421508'),
            ('Rossman, G., Esparza, N., & Bonacich, P. (2010). I’d Like To Thank The Academy, Team Spillovers, and Network Centrality. <em>American Sociological Review</em>, 75(1), 31–51.', '10.1177/0003122409359164')
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
        'slug': 'week-6-networks-and-culture-and-culture-in-networks-readings-and-overview',
        'part1_title': 'Dualities of Culture & Structure and Tie Formation',
        'part1_readings': [
            ('Breiger, R. L. (2010). Dualities of culture and structure: Seeing through cultural holes. Pp. 37–47 in <em>Relationale soziologie</em>. VS Verlag für Sozialwissenschaften.', '10.1007/978-3-531-92402-1_2'),
            ('Lizardo, O. (2023). Culture and Networks. Pp. 188–201 in <em>The SAGE Handbook of Social Network Analysis</em>. SAGE Publications Ltd.', '10.4135/9781529614695.n13'),
            ('Lewis, K., & Kaufman, J. (2018). The Conversion of Cultural Tastes Into Social Network Ties. <em>American Journal of Sociology</em>, 123(6), 1684–1742.', '10.1086/697525'),
            ('Fuhse, J. A., & Gondal, N. (2024). Networks from culture: Mechanisms of tie-formation follow institutionalized rules in social fields. <em>Social Networks</em>, 77, 43–54.', '10.1016/j.socnet.2021.12.005')
        ],
        'part2_title': 'Meaning Structures, Relational Sociology, and Publics',
        'part2_readings': [
            ('Fuhse, J. A. (2009). The Meaning Structure of Social Networks. <em>Sociological Theory</em>, 27(1), 51–73.', '10.1111/j.1467-9558.2009.00338.x'),
            ('Ikegami, E. (2000). A Sociological Theory of Publics: Identity and Culture As Emergent Properties In Networks. <em>Social Research</em>, 989–1029.', None),
            ('Mützel, S., & Breiger, R. (2021). Duality Beyond Persons and Groups. In <em>The Oxford Handbook of Social Networks</em>. Oxford University Press.', '10.1093/oxfordhb/9780190251765.013.9'),
            ('Fuhse, J., & Mische, A. (2024). Relational Sociology: Networks, Culture and Interaction. In J. McLevey, J. Scott, & P. J. Carrington (Eds.), <em>The SAGE Handbook of Social Network Analysis</em> (2nd ed., pp. 55–71). SAGE.', '10.4135/9781529614695.n5')
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
        'slug': 'week-7-diffusion-in-networks-readings-and-overview',
        'part1_title': 'Complex Contagion, Clustering, and Spatial Diffusion',
        'part1_readings': [
            ('Centola, D., & Macy, M. (2007). Complex Contagions and the Weakness of Long Ties. <em>American Journal of Sociology</em>, 113(3), 702–734.', '10.1086/521848'),
            ('DellaPosta, D., Shi, Y., & Macy, M. (2015). Why Do Liberals Drink Lattes? <em>American Journal of Sociology</em>, 120(5), 1473–1511.', '10.1086/681254'),
            ('Centola, D. (2015). The social origins of networks and diffusion. <em>American Journal of Sociology</em>, 120(5), 1295–1338.', '10.1086/681275'),
            ('Cheng, M., Smith, D. S., Ren, X., Cao, H., Smith, S., & McFarland, D. A. (2023). How new ideas diffuse in science. <em>American Sociological Review</em>, 88(3), 522–561.', '10.1177/00031224231166955'),
            ('Becker, S. O., Hsiao, Y., Pfaff, S., & Rubin, J. (2020). Multiplex Network Ties and the Spatial Diffusion of Radical Innovations: Martin Luther\'s Leadership In The Early Reformation. <em>American Sociological Review</em>, 85(5), 857–894.', '10.1177/0003122420948059'),
            ('Keuchenius, A., Törnberg, P., & Uitermark, J. (2021). Adoption and Adaptation: A Computational Case Study of the Spread of Granovetter\'s Weak Ties Hypothesis. <em>Social Networks</em>, 66, 10–25.', '10.1016/j.socnet.2021.01.001')
        ],
        'part2_title': 'Associative Diffusion & Cultural Currents',
        'part2_readings': [
            ('Goldberg, A., & Stein, S. K. (2018). Beyond Social Contagion: Associative Diffusion and The Emergence of Cultural Variation. <em>American Sociological Review</em>, 83(5), 897–932.', '10.1177/0003122418797576'),
            ('Bail, C. A., Brown, T. W., & Mann, M. (2017). Channeling hearts and minds: Advocacy organizations, cognitive-emotional currents, and public conversation. <em>American Sociological Review</em>, 82(6), 1188–1213.', '10.1177/0003122417733673')
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
        'slug': 'week-8-networks-in-history-readings-and-overview',
        'part1_title': 'Robust Action, Mobilization, and Historical Networks',
        'part1_readings': [
            ('Padgett, J. F., & Ansell, C. K. (1993). Robust Action and the Rise of the Medici, 1400–1434. <em>American Journal of Sociology</em>, 98(6), 1259–1319.', '10.1086/230190'),
            ('Gould, R. V. (1991). Multiple Networks and Mobilization In The Paris Commune, 1871. <em>American Sociological Review</em>, 56(6), 716–729.', '10.2307/2096251')
        ],
        'part2_title': 'Global Trade, Malfeasance, and Historical Sequences',
        'part2_readings': [
            ('Erikson, E., & Bearman, P. (2006). Malfeasance and the Foundations For Global Trade: The Structure of English Trade In The East Indies, 1601–1833. <em>American Journal of Sociology</em>, 112(1), 195–230.', '10.1086/502694'),
            ('Bearman, P., Faris, R., & Moody, J. (1999). Blocking The Future: New Solutions For Old Problems In Historical Social Science. <em>Social Science History</em>, 23(4), 501–533.', '10.1017/s0145553200021854')
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
        'slug': 'week-9-networks-and-inequality-readings-and-overview',
        'part1_title': 'Homophily, Elite Kinship, and Intergroup Network Inequalities',
        'part1_readings': [
            ('DiMaggio, P., & Garip, F. (2011). How network externalities can exacerbate intergroup inequality. <em>American Journal of Sociology</em>, 116(6), 1887–1933.', '10.1086/659653'),
            ('Leszczensky, L., & Pink, S. (2019). What drives ethnic homophily? A relational approach on how ethnic identification moderates preferences for same-ethnic friends. <em>American Sociological Review</em>, 84(3), 394–419.', '10.1177/0003122419846665'),
            ('O’Brien, S. (2026). Kinship Interlocks: How the Intimate Exchange of Wealth, Status, and Power Generates Upper-Class Persistence. <em>American Sociological Review</em>, 91(2), 191–226.', '')
        ],
        'part2_title': 'Academic Status Hierarchies, Network Segregation, and Migration',
        'part2_readings': [
            ('Gondal, N. (2018). Duality of departmental specializations and PhD exchange: A Weberian analysis of status in interaction using multilevel exponential random graph models (mERGM). <em>Social Networks</em>, 55, 202–212.', '10.1016/j.socnet.2018.07.005'),
            ('Zhao, L. (2025). Uneven Mixing, Network Segregation, and Immigrant Integration. <em>American Sociological Review</em>, 90(3), 521–559.', ''),
            ('Leal, D. F. (2021). Network inequalities and international migration in the Americas. <em>American Journal of Sociology</em>, 126(5), 1067–1126.', '10.1086/713745')
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
        'slug': 'week-10-networks-and-the-micro-slash-macro-link-readings-and-overview',
        'part1_title': 'Network Ecology & Romantic/Sexual Networks',
        'part1_readings': [
            ('McFarland, D. A., Moody, J., Diehl, D., Smith, J. A., & Thomas, R. J. (2014). Network Ecology and Adolescent Social Structure. <em>American Sociological Review</em>, 79(6), 1088–1121.', '10.1177/0003122414554001'),
            ('Bearman, P. S., Moody, J., & Stovel, K. (2004). Chains of Affection: The Structure of Adolescent Romantic and Sexual Networks. <em>American Journal of Sociology</em>, 110(1), 44–91.', '10.1086/386272')
        ],
        'part2_title': 'Spatial Mobility, Segregation, and Labor Markets',
        'part2_readings': [
            ('Levy, B. L., Phillips, N. E., & Sampson, R. J. (2020). Triple Disadvantage: Neighborhood Networks of Everyday Urban Mobility and Violence In US Cities. <em>American Sociological Review</em>, 85(6), 925–956.', '10.1177/0003122420972323'),
            ('Papachristos, A. V. (2009). Murder By Structure: Dominance Relations and the Social Structure of Gang Homicide. <em>American Journal of Sociology</em>, 115(1), 74–128.', '10.1086/597791'),
            ('Huang, P., & Butts, C. T. (2023). Rooted America: Immobility and Segregation of the Intercounty Migration Network. <em>American Sociological Review</em>, 88(6), 1031–1065.', '10.1177/00031224231212679'),
            ('Cheng, S., & Park, B. (2020). Flows and Boundaries: A Network Approach to Studying Occupational Mobility in the Labor Market. <em>American Journal of Sociology</em>, 126(3), 577–631.', '10.1086/712399')
        ]
    }
]

def make_weekly_page_html(w):
    p1_lis = []
    for r, doi in w['part1_readings']:
        if doi:
            link = f' <a href="https://doi.org/{doi}" target="_blank" rel="noopener noreferrer" style="color: #2563eb; font-size: 0.88em; font-weight: 600; text-decoration: underline;">[DOI ↗]</a>'
        else:
            link = f' <a href="{GDRIVE_URL}" target="_blank" rel="noopener noreferrer" style="color: #2563eb; font-size: 0.88em; font-weight: 600; text-decoration: underline;">[Course Files ↗]</a>'
        p1_lis.append(f'<li style="margin-bottom: 8px;">{r}{link}</li>')
    p1_items = ''.join(p1_lis)

    p2_lis = []
    for r, doi in w['part2_readings']:
        if doi:
            link = f' <a href="https://doi.org/{doi}" target="_blank" rel="noopener noreferrer" style="color: #2563eb; font-size: 0.88em; font-weight: 600; text-decoration: underline;">[DOI ↗]</a>'
        else:
            link = f' <a href="{GDRIVE_URL}" target="_blank" rel="noopener noreferrer" style="color: #2563eb; font-size: 0.88em; font-weight: 600; text-decoration: underline;">[Course Files ↗]</a>'
        p2_lis.append(f'<li style="margin-bottom: 8px;">{r}{link}</li>')
    p2_items = ''.join(p2_lis)
    
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
      <div><strong>Reading Requirement:</strong> Make reference to at least <em>four readings total</em>, including at least <em>one reading from Part 1</em> and at least <em>one reading from Part 2</em> to put in conversation with each other.</div>
      <div><strong>Discussion Leaders:</strong> Post at least two questions per assigned reading to the <a href="/courses/239524/discussion_topics/1466035" style="color: #2563eb; font-weight: 600;">Discussion Leaders Summary &amp; Questions Board &rarr;</a></div>
      <div><strong>Readings Access:</strong> Articles with DOIs link directly via <strong>[DOI ↗]</strong>. Readings without DOIs are available in the <a href="{GDRIVE_URL}" target="_blank" rel="noopener noreferrer" style="color: #2563eb; font-weight: 600; text-decoration: underline;">Course Files Folder ↗</a></div>
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

for w in WEEKS_DATA:
    slug = w['slug']
    page_title = f"Week {w['week']}: {w['title']} (Readings & Overview)"
    page_html = make_weekly_page_html(w)
    
    put_req = urllib.request.Request(
        f'{url}/api/v1/courses/{course_id}/pages/{slug}',
        data=json.dumps({
            'wiki_page': {
                'title': page_title,
                'body': page_html,
                'published': True
            }
        }).encode('utf-8'),
        headers=headers,
        method='PUT'
    )
    with urllib.request.urlopen(put_req) as resp:
        res = json.loads(resp.read().decode('utf-8'))
        print(f"Week {w['week']} page updated on Canvas: {res.get('url')}")

print('\nALL 10 Weekly Pages successfully deployed to Canvas with direct DOI links!')
