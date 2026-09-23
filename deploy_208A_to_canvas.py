"""
deploy_208A_to_canvas.py

Consolidated deployment script for SOCIOL 208A on Bruin Learn (Canvas), mirroring
the structure of deploy_all_to_canvas.py (the 208B script). Single source of truth
is schedule-208A.qmd; this script:

  1. Parses schedule-208A.qmd into structured week/section/reading data.
  2. Regenerates front-page-208A.html (hero banner, quick-access bar, 4 thematic
     module cards with per-week reading disclosures + lecture-note chips), reusing
     the hand-maintained resource cards in front-page-208A-resources-partial.html
     (Lecture Slides, Reference Works, Textbooks, Data Repositories, Organizations,
     Journals, Resources) rather than regenerating them from scratch.
  3. Regenerates the flat reading-schedule HTML from the same source file.
  4. Pushes both pages to Canvas (front page + reading-schedule page), looking up
     current slugs dynamically rather than hardcoding them, since Canvas
     regenerates a page's URL slug whenever its title changes (see AGENTS.md
     Section 7.D for this quirk).
  5. Rebuilds the 10 per-week Canvas Modules (one per week), each bundling:
       - a Page item linking to the reading-schedule page,
       - an Assignment item for that week's Data Analysis Exercise,
       - one ExternalUrl item per lecture-note file for that week.

Data Analysis Exercise assignment descriptions are intentionally NOT regenerated
here (they contain hand-written, exercise-specific instructions) -- edit those
directly via the Canvas UI or a targeted one-off script, same convention as 208B.

Usage:
    python3 deploy_208A_to_canvas.py
"""
import json
import os
import re
import urllib.error
import urllib.request
from pathlib import Path

# ---------------------------------------------------------------------------
# Credentials
# ---------------------------------------------------------------------------

def get_credentials():
    token = os.environ.get('CANVAS_API_TOKEN')
    url = os.environ.get('CANVAS_URL')
    renviron = Path.home() / '.Renviron'
    if renviron.exists():
        with open(renviron) as f:
            for line in f:
                line = line.strip()
                if not line or line.startswith('#'):
                    continue
                if line.startswith('CANVAS_API_TOKEN=') and not token:
                    token = line.split('=', 1)[1].strip('"\' ')
                elif line.startswith('CANVAS_URL=') and not url:
                    url = line.split('=', 1)[1].strip('"\' ')
    return (url or 'https://bruinlearn.ucla.edu').rstrip('/'), token


CANVAS_URL, TOKEN = get_credentials()
HEADERS = {
    'Authorization': f'Bearer {TOKEN}',
    'Content-Type': 'application/json',
    'Accept': 'application/json',
}
COURSE_ID = 232537
BASE = f'{CANVAS_URL}/api/v1/courses/{COURSE_ID}'
GH = 'https://olizardo.github.io/SOCIOL208'

SCHEDULE_QMD = Path('schedule-208A.qmd')
RESOURCES_PARTIAL = Path('front-page-208A-resources-partial.html')
FRONT_PAGE_LOCAL = Path('front-page-208A.html')

CSS_LINK = '<link rel="stylesheet" href="https://instructure-uploads.s3.amazonaws.com/account_148090000000000001/attachments/14738291/dp_app.css">'
JS_TAG = '<script src="https://instructure-uploads.s3.amazonaws.com/account_148090000000000001/attachments/24600424/2026-05-14-cidi-updates-mobile-theme.js"></script>'

# Exercise assignment IDs -- single source of truth for week <-> exercise mapping.
EXERCISES = {
    1: (1956037, 'Data Analysis Exercise I: Basic Network Statistics'),
    2: (1956039, 'Data Analysis Exercise II: Centrality'),
    3: (1956044, 'Data Analysis Exercise III: Ego Networks'),
    4: (1956040, 'Data Analysis Exercise IV: Status and Prestige'),
    5: (1956041, 'Data Analysis Exercise V: Role Equivalence and Structural Similarity'),
    6: (1956042, 'Data Analysis Exercise VI: Community Structure/Detection'),
    7: (2044561, 'Data Analysis Exercise VII: Advanced Community Detection'),
    8: (1956043, 'Data Analysis Exercise VIII: Two-Mode Networks'),
    9: (2044562, 'Data Analysis Exercise IX: QAP and Permutation-Based Null Models'),
    10: (2044563, 'Data Analysis Exercise X: Exponential Random Graph Models'),
}

# Thematic module groupings for the front page's 2x2 card grid and for naming
# the 10 Canvas Modules is NOT grouped -- Canvas Modules stay one-per-week.
FRONT_PAGE_MODULES = [
    {'title': 'Foundations', 'weeks': [1, 2], 'gradient': '#1e3a8a 0%, #3b82f6 100%', 'label_color': '#bfdbfe'},
    {'title': 'Structural Positions &amp; Similarity', 'weeks': [3, 4, 5], 'gradient': '#065f46 0%, #059669 100%', 'label_color': '#a7f3d0'},
    {'title': 'Community Detection', 'weeks': [6, 7], 'gradient': '#7c2d12 0%, #ea580c 100%', 'label_color': '#fed7aa'},
    {'title': 'Two-Mode &amp; Statistical Network Models', 'weeks': [8, 9, 10], 'gradient': '#4c1d95 0%, #7c3aed 100%', 'label_color': '#ddd6fe'},
]


# ---------------------------------------------------------------------------
# Small HTTP helpers
# ---------------------------------------------------------------------------

def api_get(path):
    req = urllib.request.Request(f'{BASE}{path}', headers=HEADERS)
    with urllib.request.urlopen(req, timeout=30) as resp:
        return json.loads(resp.read().decode())


def api_post(path, payload):
    data = json.dumps(payload).encode()
    req = urllib.request.Request(f'{BASE}{path}', data=data, headers=HEADERS, method='POST')
    with urllib.request.urlopen(req, timeout=30) as resp:
        return json.loads(resp.read().decode())


def api_put(path, payload):
    data = json.dumps(payload).encode()
    req = urllib.request.Request(f'{BASE}{path}', data=data, headers=HEADERS, method='PUT')
    with urllib.request.urlopen(req, timeout=30) as resp:
        return json.loads(resp.read().decode())


def api_delete(path):
    req = urllib.request.Request(f'{BASE}{path}', headers=HEADERS, method='DELETE')
    with urllib.request.urlopen(req, timeout=30) as resp:
        return resp.read()


# ---------------------------------------------------------------------------
# Parse schedule-208A.qmd (single source of truth) into structured data
# ---------------------------------------------------------------------------

def extract_link(text):
    """If a markdown link sits at the very end of a line, split it off."""
    m = re.search(r'\[([^\]]+)\]\(([^)]+)\)\s*$', text.strip())
    if m:
        label, link_url = m.group(1), m.group(2)
        rest = text[:m.start()].rstrip()
        return rest, label, link_url
    return text, None, None


def inline_md_to_html(text):
    text = re.sub(r'\*\*(.+?)\*\*', r'<strong>\1</strong>', text)
    text = re.sub(r'\*(.+?)\*', r'<em>\1</em>', text)
    return text


def parse_schedule(path=SCHEDULE_QMD):
    qmd = path.read_text(encoding='utf-8')
    lines = qmd.splitlines()
    start = 0
    if lines and lines[0].strip() == '---':
        for i in range(1, len(lines)):
            if lines[i].strip() == '---':
                start = i + 1
                break

    weeks = []
    cur_week = None
    cur_section = None

    for raw in lines[start:]:
        line = raw.rstrip()
        if not line.strip():
            continue
        if line.startswith('## '):
            cur_week = {'title': line[3:].strip(), 'sections': []}
            weeks.append(cur_week)
            cur_section = None
        elif line.startswith('### '):
            cur_section = {'heading': line[4:].strip(), 'items': []}
            cur_week['sections'].append(cur_section)
        elif line.startswith('- '):
            item_text = line[2:].strip()
            rest, label, link_url = extract_link(item_text)
            cur_section['items'].append({
                'text': inline_md_to_html(rest),
                'label': label,
                'url': link_url,
            })
        # other lines (intro paragraph, live-doc notice) are ignored

    return weeks


def week_num(title):
    return int(title.split(':', 1)[0].replace('Week', '').strip())


# ---------------------------------------------------------------------------
# Reading-schedule page (flat HTML, mirrors schedule-208A.qmd structure 1:1)
# ---------------------------------------------------------------------------

def build_reading_schedule_html(path=SCHEDULE_QMD):
    qmd = path.read_text(encoding='utf-8')
    lines = qmd.splitlines()
    start = 0
    if lines and lines[0].strip() == '---':
        for i in range(1, len(lines)):
            if lines[i].strip() == '---':
                start = i + 1
                break

    def inline_md(text):
        text = re.sub(r'\*\*(.+?)\*\*', r'<strong>\1</strong>', text)
        text = re.sub(r'\*(.+?)\*', r'<em>\1</em>', text)
        text = re.sub(r'\[([^\]]+)\]\(([^)]+)\)', r'<a href="\2">\1</a>', text)
        return text

    html_parts = [CSS_LINK]
    in_list = False
    for raw in lines[start:]:
        line = raw.rstrip()
        if not line.strip():
            continue
        if line.startswith('## '):
            if in_list:
                html_parts.append('</ul>')
                in_list = False
            html_parts.append(f'<h2>{inline_md(line[3:].strip())}</h2>')
        elif line.startswith('### '):
            if in_list:
                html_parts.append('</ul>')
                in_list = False
            html_parts.append(f'<h3>{inline_md(line[4:].strip())}</h3>')
        elif line.startswith('**') and 'LIVE DOCUMENT' in line:
            html_parts.append(f'<p>{inline_md(line)}</p>')
        elif line.startswith('- '):
            if not in_list:
                html_parts.append('<ul>')
                in_list = True
            html_parts.append(f'<li>{inline_md(line[2:].strip())}</li>')
        else:
            if in_list:
                html_parts.append('</ul>')
                in_list = False
            html_parts.append(f'<p>{inline_md(line.strip())}</p>')
    if in_list:
        html_parts.append('</ul>')

    return '\n'.join(html_parts) + JS_TAG


# ---------------------------------------------------------------------------
# Front page (hero banner + quick-access bar + 4 module cards + resources)
# ---------------------------------------------------------------------------

def render_item(item):
    text = item['text']
    text = re.sub(
        r'\[([^\]]+)\]\(([^)]+)\)',
        r'<a href="\2" target="_blank" rel="noopener noreferrer" style="color:#2563eb;font-weight:600;">\1</a>',
        text,
    )
    if not text and item.get('label') and item.get('url'):
        return f'<a href="{item["url"]}" target="_blank" rel="noopener noreferrer" style="color:#2563eb;font-weight:600;">{item["label"]}</a>'
    if item.get('label') and item.get('url'):
        badge_label = 'DOI' if 'doi.org' in item['url'] else item['label'].upper()
        badge = f' <a href="{item["url"]}" target="_blank" rel="noopener noreferrer" style="color:#2563eb;font-weight:600;">[{badge_label} \u2197]</a>'
        return text + badge
    return text


def render_week_card(week):
    n = week_num(week['title'])
    topic = week['title'].split(':', 1)[1].strip()
    reading_sections = [s for s in week['sections'] if s['heading'] != 'Lecture Notes']
    lecture_section = next((s for s in week['sections'] if s['heading'] == 'Lecture Notes'), None)
    total_readings = sum(len(s['items']) for s in reading_sections)

    parts = [
        '<div style="margin-bottom: 14px;">',
        '<div style="display: flex; justify-content: space-between; align-items: baseline;">',
        f'<strong style="color: #0f172a; font-size: 0.95em;">Week {n}: {topic}</strong>',
        '</div>',
        '<details style="margin-top: 4px; cursor: pointer;">',
        '<summary style="font-size: 0.88em; color: #2563eb; font-weight: 600; outline: none; user-select: none;">',
        f'View Readings ({total_readings} items)',
        '</summary>',
        '<div style="background-color: #f8fafc; border: 1px solid #e2e8f0; border-radius: 6px; padding: 10px 14px; margin-top: 6px; font-size: 0.86em; color: #334155;">',
    ]
    for si, sec in enumerate(reading_sections):
        margin = 'margin: 0 0 10px 0;' if si < len(reading_sections) - 1 else 'margin: 0;'
        parts.append(f'<div style="font-weight: 700; color: #1e293b; margin-bottom: 4px; border-bottom: 1px solid #e2e8f0; padding-bottom: 2px;">\U0001F4D6 {sec["heading"]}</div>')
        parts.append(f'<ul style="{margin} padding-left: 18px; line-height: 1.5;">')
        for it in sec['items']:
            parts.append(f'<li style="margin-bottom: 5px;">{render_item(it)}</li>')
        parts.append('</ul>')
    parts.append('</div>')
    parts.append('</details>')

    if lecture_section:
        parts.append('<div style="margin-top: 8px; background-color: #eef2ff; border: 1px solid #c7d2fe; border-radius: 6px; padding: 8px 10px;">')
        parts.append('<div style="font-size: 0.82em; font-weight: 700; color: #3730a3; margin-bottom: 5px;">\U0001F393 Lecture Notes</div>')
        parts.append('<div style="display: flex; flex-wrap: wrap; gap: 6px;">')
        for it in lecture_section['items']:
            parts.append(
                f'<a href="{it["url"]}" target="_blank" rel="noopener noreferrer" '
                f'style="font-size: 0.8em; background-color: #ffffff; border: 1px solid #a5b4fc; color: #4338ca; '
                f'padding: 3px 8px; border-radius: 12px; text-decoration: none; font-weight: 600;">{it["label"]} \u2197</a>'
            )
        parts.append('</div>')
        parts.append('</div>')

    ex_id, ex_name = EXERCISES[n]
    parts.append(
        f'<div style="margin-top: 8px; text-align: right;">'
        f'<a href="/courses/{COURSE_ID}/assignments/{ex_id}" style="font-size: 0.82em; color: #b45309; font-weight: 600; text-decoration: none;">'
        f'\U0001F4DD {ex_name.replace("Data Analysis ", "")} &rarr;</a></div>'
    )
    parts.append('</div>')
    return '\n'.join(parts)


def build_module_grid(weeks):
    weeks_by_num = {week_num(w['title']): w for w in weeks}
    module_cards = []
    for mi, mod in enumerate(FRONT_PAGE_MODULES, start=1):
        wk_lo, wk_hi = mod['weeks'][0], mod['weeks'][-1]
        span = f'Week {wk_lo}' if wk_lo == wk_hi else f'Weeks {wk_lo}\u2013{wk_hi}'
        card = [
            '<div style="background: #ffffff; border: 1px solid #cbd5e1; border-radius: 10px; overflow: hidden; box-shadow: 0 2px 4px rgba(0,0,0,0.04); display: flex; flex-direction: column;">',
            f'<div style="background: linear-gradient(135deg, {mod["gradient"]}); color: #ffffff; padding: 14px 18px;">',
            f'<div style="font-size: 0.8em; font-weight: 700; text-transform: uppercase; letter-spacing: 0.05em; color: {mod["label_color"]};">Module {mi} &bull; {span}</div>',
            f'<h3 style="margin: 4px 0 0 0; font-size: 1.18em; font-weight: 700; color: #ffffff;">{mod["title"]}</h3>',
            '</div>',
            '<div style="padding: 18px; flex: 1;">',
        ]
        for n in mod['weeks']:
            card.append(render_week_card(weeks_by_num[n]))
        card.append('</div>')
        card.append('</div>')
        module_cards.append('\n'.join(card))

    return (
        '<div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(440px, 1fr)); gap: 20px; margin-bottom: 38px;">\n'
        + '\n'.join(module_cards)
        + '\n</div>'
    )


def build_front_page_html(weeks, reading_schedule_slug):
    module_grid = build_module_grid(weeks)
    resources_html = RESOURCES_PARTIAL.read_text(encoding='utf-8')

    return f'''{CSS_LINK}

<div style="max-width: 1020px; margin: 0 auto; font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Helvetica, Arial, sans-serif; color: #1e293b; line-height: 1.6;">

  <!-- Hero Banner -->
  <div style="background: linear-gradient(135deg, #0f172a 0%, #1e3a8a 60%, #2563eb 100%); color: #ffffff; padding: 32px; border-radius: 12px; margin-bottom: 24px; box-shadow: 0 10px 15px -3px rgba(0, 0, 0, 0.1);">
    <div>
      <div style="display: inline-block; background-color: rgba(255, 255, 255, 0.15); border: 1px solid rgba(255, 255, 255, 0.25); color: #f8fafc; padding: 4px 12px; border-radius: 20px; font-weight: 600; font-size: 0.85em; margin-bottom: 12px; letter-spacing: 0.05em; text-transform: uppercase;">
        UCLA Sociology &bull; Graduate Methods Seminar
      </div>
      <h1 style="margin: 0 0 10px 0; font-size: 2.2em; font-weight: 800; color: #ffffff; letter-spacing: -0.02em; line-height: 1.2;">
        SOCIOL 208A: Social Network Methods
      </h1>
      <p style="font-size: 1.15em; color: #bfdbfe; margin: 0 0 16px 0; font-weight: 400;">
        A Graduate Seminar on Data-Analytic Techniques in Social Network Analysis
      </p>
      <div style="display: flex; flex-wrap: wrap; gap: 10px; font-size: 0.9em; color: #e2e8f0;">
        <span style="background-color: rgba(15, 23, 42, 0.4); padding: 4px 10px; border-radius: 6px;">\U0001F4C5 Wednesdays, 9:00&ndash;11:50 am</span>
        <span style="background-color: rgba(15, 23, 42, 0.4); padding: 4px 10px; border-radius: 6px;">\U0001F4CD Haines A37</span>
        <span style="background-color: rgba(15, 23, 42, 0.4); padding: 4px 10px; border-radius: 6px;">\U0001F464 Prof. Omar Lizardo</span>
      </div>
    </div>
  </div>

  <!-- Quick Access Navigation Bar -->
  <div style="display: grid; grid-template-columns: repeat(auto-fit, minmax(180px, 1fr)); gap: 12px; margin-bottom: 30px;">
    <a href="/courses/{COURSE_ID}/assignments/syllabus" style="text-decoration: none; display: block; background: #ffffff; border: 1px solid #cbd5e1; border-left: 4px solid #2563eb; padding: 14px 16px; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.05);">
      <div style="font-weight: 700; color: #0f172a; font-size: 0.98em;">\U0001F4CB Course Syllabus</div>
      <div style="font-size: 0.82em; color: #64748b; margin-top: 2px;">Policies, grading &amp; requirements</div>
    </a>
    <a href="/courses/{COURSE_ID}/pages/{reading_schedule_slug}" style="text-decoration: none; display: block; background: #ffffff; border: 1px solid #cbd5e1; border-left: 4px solid #059669; padding: 14px 16px; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.05);">
      <div style="font-weight: 700; color: #0f172a; font-size: 0.98em;">\U0001F4C5 Reading Schedule</div>
      <div style="font-size: 0.82em; color: #64748b; margin-top: 2px;">Weekly topics &amp; readings</div>
    </a>
    <a href="/courses/{COURSE_ID}/modules" style="text-decoration: none; display: block; background: #ffffff; border: 1px solid #cbd5e1; border-left: 4px solid #7c3aed; padding: 14px 16px; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.05);">
      <div style="font-weight: 700; color: #0f172a; font-size: 0.98em;">\U0001F4E6 Canvas Modules</div>
      <div style="font-size: 0.82em; color: #64748b; margin-top: 2px;">Weekly overview &amp; exercises</div>
    </a>
    <a href="{GH}/" target="_blank" rel="noopener noreferrer" style="text-decoration: none; display: block; background: #ffffff; border: 1px solid #cbd5e1; border-left: 4px solid #d97706; padding: 14px 16px; border-radius: 8px; box-shadow: 0 1px 3px rgba(0,0,0,0.05);">
      <div style="font-weight: 700; color: #0f172a; font-size: 0.98em;">\U0001F393 Lecture Notes \u2197</div>
      <div style="font-size: 0.82em; color: #64748b; margin-top: 2px;">Full companion course website</div>
    </a>
  </div>

  <!-- Section: Core Seminar Modules -->
  <div style="display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 14px; border-bottom: 2px solid #e2e8f0; padding-bottom: 8px;">
    <h2 style="margin: 0; font-size: 1.4em; color: #0f172a; font-weight: 700;">\U0001F4E6 Core Seminar Modules</h2>
    <a href="/courses/{COURSE_ID}/modules" style="font-size: 0.9em; color: #2563eb; font-weight: 600; text-decoration: none;">View All in Modules Tool &rarr;</a>
  </div>

  <p style="font-size: 0.95em; color: #475569; margin-bottom: 20px;">
    The seminar is structured into four thematic modules spanning the 10-week quarter. Click the triangle next to any week to open its assigned readings; lecture notes for each week are linked separately below the readings.
  </p>

  {module_grid}

  <!-- Section: Resources -->
  <div style="display: flex; justify-content: space-between; align-items: baseline; margin-bottom: 14px; border-bottom: 2px solid #e2e8f0; padding-bottom: 8px;">
    <h2 style="margin: 0; font-size: 1.4em; color: #0f172a; font-weight: 700;">\U0001F5C2\uFE0F Resources &amp; Research Hubs</h2>
  </div>

  {resources_html}

</div>{JS_TAG}'''


# ---------------------------------------------------------------------------
# Canvas page deployment (looks up slugs dynamically to survive title-driven
# slug regeneration -- see AGENTS.md Section 7.D)
# ---------------------------------------------------------------------------

def find_front_page_slug():
    pages = api_get('/pages?per_page=100')
    for p in pages:
        if p.get('front_page'):
            return p['url'], p['title']
    raise RuntimeError('No page flagged front_page=true was found for this course.')


def find_reading_schedule_slug():
    pages = api_get('/pages?per_page=100')
    for p in pages:
        if 'reading-schedule' in p['url'] or p.get('title', '').lower().endswith('reading schedule'):
            return p['url'], p['title']
    raise RuntimeError('Could not find a reading-schedule page for this course.')


def deploy_front_page(weeks, reading_schedule_slug):
    slug, title = find_front_page_slug()
    body = build_front_page_html(weeks, reading_schedule_slug)
    FRONT_PAGE_LOCAL.write_text(body, encoding='utf-8')
    # NOTE: deliberately omit 'title' from the payload on updates so Canvas does
    # not regenerate the URL slug on every run (see AGENTS.md Section 7.D).
    resp = api_put(f'/pages/{slug}', {'wiki_page': {'body': body, 'published': True}})
    print(f'Front page updated: {resp.get("url")} (title: {title})')
    return resp['url']


def deploy_reading_schedule():
    slug, title = find_reading_schedule_slug()
    body = build_reading_schedule_html()
    resp = api_put(f'/pages/{slug}', {'wiki_page': {'body': body, 'published': True}})
    print(f'Reading schedule page updated: {resp.get("url")} (title: {title})')
    return resp['url']


# ---------------------------------------------------------------------------
# Canvas Modules (10, one per week)
# ---------------------------------------------------------------------------

def rebuild_modules(weeks, reading_schedule_slug):
    weeks_by_num = {week_num(w['title']): w for w in weeks}

    existing = api_get('/modules?per_page=100')
    for m in existing:
        api_delete(f'/modules/{m["id"]}')
        print(f'Deleted pre-existing module {m["id"]} ({m["name"]})')

    for n in range(1, 11):
        week = weeks_by_num[n]
        topic = week['title'].split(':', 1)[1].strip()
        mod_name = f'Week {n}: {topic}'
        mod = api_post('/modules', {'module': {'name': mod_name, 'position': n}})
        mod_id = mod['id']

        item = api_post(f'/modules/{mod_id}/items', {
            'module_item': {
                'title': f'Week {n} Overview & Reading Schedule',
                'type': 'Page',
                'page_url': reading_schedule_slug,
                'position': 1,
            }
        })
        api_put(f'/modules/{mod_id}/items/{item["id"]}', {'module_item': {'published': True}})

        ex_id, ex_name = EXERCISES[n]
        item = api_post(f'/modules/{mod_id}/items', {
            'module_item': {
                'title': ex_name,
                'type': 'Assignment',
                'content_id': ex_id,
                'position': 2,
            }
        })
        api_put(f'/modules/{mod_id}/items/{item["id"]}', {'module_item': {'published': True}})

        lecture_section = next((s for s in week['sections'] if s['heading'] == 'Lecture Notes'), None)
        pos = 3
        if lecture_section:
            for it in lecture_section['items']:
                item = api_post(f'/modules/{mod_id}/items', {
                    'module_item': {
                        'title': it['label'],
                        'type': 'ExternalUrl',
                        'external_url': it['url'],
                        'new_tab': True,
                        'position': pos,
                    }
                })
                api_put(f'/modules/{mod_id}/items/{item["id"]}', {'module_item': {'published': True}})
                pos += 1

        api_put(f'/modules/{mod_id}', {'module': {'published': True}})
        print(f'Created module {mod_id}: {mod_name} ({pos - 1} items, published)')


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    print(f'--- Deploying SOCIOL 208A (course {COURSE_ID}) ---')
    weeks = parse_schedule()
    print(f'Parsed {len(weeks)} weeks from {SCHEDULE_QMD}')

    print('\n--- Reading Schedule Page ---')
    reading_schedule_url = deploy_reading_schedule()

    print('\n--- Front Page ---')
    deploy_front_page(weeks, reading_schedule_url)

    print('\n--- Canvas Modules ---')
    rebuild_modules(weeks, reading_schedule_url)

    print('\nDone. Front page, reading schedule, and 10 Canvas Modules are live.')


if __name__ == '__main__':
    main()
