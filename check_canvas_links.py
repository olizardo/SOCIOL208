#!/usr/bin/env python3
"""
Audit all links across UCLA Bruin Learn (Canvas LMS) for SOCIOL 208B Fall 2026 (course 239524):
- Syllabus
- Module items (External URLs, etc.)
- Pages (content body)
- Assignments (descriptions)
- Quizzes (descriptions)
- Discussions (messages)
Adapted from the SOCIOL 111 link auditor.
"""

import os
import sys
import re
import json
import urllib.request
import urllib.error
import urllib.parse
import html.parser
from concurrent.futures import ThreadPoolExecutor

COURSE_ID = "239524"
DEFAULT_CANVAS_URL = "https://bruinlearn.ucla.edu"

USER_AGENT = (
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) "
    "WebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
)

def load_credentials():
    url = os.environ.get("CANVAS_URL", DEFAULT_CANVAS_URL).rstrip("/")
    token = os.environ.get("CANVAS_API_TOKEN")
    renviron = os.path.expanduser("~/.Renviron")
    if not token and os.path.exists(renviron):
        with open(renviron) as f:
            for line in f:
                if line.startswith("CANVAS_API_TOKEN="):
                    token = line.strip().split("=", 1)[1].strip("\"'")
                elif line.startswith("CANVAS_URL="):
                    url = line.strip().split("=", 1)[1].strip("\"'").rstrip("/")
    if not token:
        sys.exit("Error: CANVAS_API_TOKEN not found in environment or ~/.Renviron.")
    return url, token

class LinkExtractor(html.parser.HTMLParser):
    def __init__(self):
        super().__init__()
        self.links = []  # list of (tag, attr, url, text)
        self.current_tag = None
        self.current_href = None
        self.current_text = []

    def handle_starttag(self, tag, attrs):
        attrs_dict = dict(attrs)
        if tag == "a" and "href" in attrs_dict:
            self.current_tag = tag
            self.current_href = attrs_dict["href"]
            self.current_text = []
        elif tag == "iframe" and "src" in attrs_dict:
            self.links.append((tag, "src", attrs_dict["src"], "[iframe embed]"))
        elif tag == "img" and "src" in attrs_dict:
            alt = attrs_dict.get("alt", "[image]")
            self.links.append((tag, "src", attrs_dict["src"], alt))

    def handle_data(self, data):
        if self.current_tag == "a":
            self.current_text.append(data)

    def handle_endtag(self, tag):
        if tag == "a" and self.current_tag == "a":
            text = "".join(self.current_text).strip()
            self.links.append(("a", "href", self.current_href, text or "[link]"))
            self.current_tag = None
            self.current_href = None
            self.current_text = []

def extract_html_links(html_text):
    if not html_text:
        return []
    parser = LinkExtractor()
    try:
        parser.feed(html_text)
    except Exception as e:
        pass
    return parser.links

def canvas_api_get(endpoint, token, url_base):
    results = []
    separator = "&" if "?" in endpoint else "?"
    next_url = f"{url_base}/api/v1/courses/{COURSE_ID}/{endpoint}{separator}per_page=100"

    headers = {
        "Authorization": f"Bearer {token}",
        "Accept": "application/json",
        "User-Agent": USER_AGENT,
    }

    while next_url:
        req = urllib.request.Request(next_url, headers=headers)
        try:
            with urllib.request.urlopen(req) as resp:
                data = json.loads(resp.read().decode())
                if isinstance(data, list):
                    results.extend(data)
                else:
                    return data

                link_header = resp.headers.get("Link", "")
                next_url = None
                for part in link_header.split(","):
                    match = re.search(r'<([^>]+)>;\s*rel="next"', part)
                    if match:
                        next_url = match.group(1)
                        break
        except Exception as e:
            print(f"Error fetching API endpoint {next_url}: {e}", file=sys.stderr)
            break
    return results

def check_single_url(target_url, canvas_url, token, timeout=12):
    """Checks a single URL and returns (status_code, error_message, final_url)"""
    parsed = urllib.parse.urlparse(target_url)
    is_canvas = (parsed.netloc == urllib.parse.urlparse(canvas_url).netloc) or target_url.startswith("/")

    if target_url.startswith("/"):
        full_url = f"{canvas_url}{target_url}"
    else:
        full_url = target_url

    headers = {
        "User-Agent": USER_AGENT,
        "Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
    }
    if is_canvas:
        headers["Authorization"] = f"Bearer {token}"

    # Try HEAD request first, fall back to GET if 403, 404, or 405
    methods = ["HEAD", "GET"]
    last_err = None
    final_url = full_url

    for method in methods:
        req = urllib.request.Request(full_url, headers=headers, method=method)
        try:
            with urllib.request.urlopen(req, timeout=timeout) as resp:
                status = resp.status
                final_url = resp.geturl()
                return (status, None, final_url)
        except urllib.error.HTTPError as e:
            last_err = e
            # If HEAD failed with 403, 404, 405, try GET before declaring dead
            if method == "HEAD" and e.code in (403, 404, 405):
                continue
            return (e.code, str(e.reason), getattr(e, "url", full_url))
        except urllib.error.URLError as e:
            return (0, str(e.reason), full_url)
        except Exception as e:
            return (0, str(e), full_url)

    return (getattr(last_err, "code", 0), str(getattr(last_err, "reason", "Unknown")), final_url)

def main():
    canvas_url, token = load_credentials()
    print(f"Auditing Canvas Course {COURSE_ID} ({canvas_url})...\n")

    # Collect items to inspect: list of dicts: {'source': ..., 'type': tag, 'text': text, 'url': url}
    all_links = []

    # 1. Syllabus
    print("1. Fetching Course Syllabus...")
    course_info = canvas_api_get("?include[]=syllabus_body", token, canvas_url)
    syllabus_body = course_info.get("syllabus_body", "") if isinstance(course_info, dict) else ""
    for tag, attr, u, text in extract_html_links(syllabus_body):
        all_links.append({
            "source_type": "Syllabus",
            "source_title": "Syllabus Body",
            "tag": tag,
            "text": text,
            "raw_url": u,
        })
    print(f"   Found {len([l for l in all_links if l['source_type'] == 'Syllabus'])} links/embeds in Syllabus.")

    # 2. Modules & Module Items
    print("2. Fetching Modules and Items...")
    modules = canvas_api_get("modules?include[]=items", token, canvas_url)
    for m in modules:
        m_name = m.get("name", "Unnamed Module")
        for item in m.get("items", []):
            i_type = item.get("type")
            i_title = item.get("title", "")
            if i_type == "ExternalUrl":
                ext_url = item.get("external_url")
                if ext_url:
                    all_links.append({
                        "source_type": "Module Item (ExternalUrl)",
                        "source_title": f"{m_name} > {i_title}",
                        "tag": "a",
                        "text": i_title,
                        "raw_url": ext_url,
                    })
    print(f"   Found {len([l for l in all_links if l['source_type'].startswith('Module')])} ExternalUrl items in Modules.")

    # 3. Pages
    print("3. Fetching Course Pages...")
    pages_list = canvas_api_get("pages", token, canvas_url)
    for p in pages_list:
        p_url = p.get("url")
        p_title = p.get("title", p_url)
        # Fetch full page to get body
        page_detail = canvas_api_get(f"pages/{p_url}", token, canvas_url)
        if isinstance(page_detail, dict):
            body = page_detail.get("body", "")
            for tag, attr, u, text in extract_html_links(body):
                all_links.append({
                    "source_type": "Page",
                    "source_title": p_title,
                    "tag": tag,
                    "text": text,
                    "raw_url": u,
                })
    print(f"   Processed {len(pages_list)} pages.")

    # 4. Assignments
    print("4. Fetching Assignments...")
    assignments = canvas_api_get("assignments", token, canvas_url)
    for a in assignments:
        a_title = a.get("name", "Unnamed Assignment")
        desc = a.get("description", "")
        for tag, attr, u, text in extract_html_links(desc):
            all_links.append({
                "source_type": "Assignment",
                "source_title": a_title,
                "tag": tag,
                "text": text,
                "raw_url": u,
            })
    print(f"   Processed {len(assignments)} assignments.")

    # 5. Quizzes
    print("5. Fetching Quizzes...")
    quizzes = canvas_api_get("quizzes", token, canvas_url)
    for q in quizzes:
        q_title = q.get("title", "Unnamed Quiz")
        desc = q.get("description", "")
        for tag, attr, u, text in extract_html_links(desc):
            all_links.append({
                "source_type": "Quiz",
                "source_title": q_title,
                "tag": tag,
                "text": text,
                "raw_url": u,
            })
    print(f"   Processed {len(quizzes)} quizzes.")

    # 6. Discussions
    print("6. Fetching Discussions / Announcements...")
    discussions = canvas_api_get("discussion_topics", token, canvas_url)
    for d in discussions:
        d_title = d.get("title", "Unnamed Discussion")
        msg = d.get("message", "")
        for tag, attr, u, text in extract_html_links(msg):
            all_links.append({
                "source_type": "Discussion",
                "source_title": d_title,
                "tag": tag,
                "text": text,
                "raw_url": u,
            })
    print(f"   Processed {len(discussions)} discussions.\n")

    # Filter links to test
    valid_links = []
    for item in all_links:
        u = item["raw_url"].strip()
        if not u or u.startswith("#") or u.startswith("mailto:") or u.startswith("javascript:") or u.startswith("data:"):
            continue
        item["clean_url"] = u
        valid_links.append(item)

    unique_urls = sorted(list(set(item["clean_url"] for item in valid_links)))
    print(f"Total links found: {len(all_links)}")
    print(f"Total actionable URLs: {len(valid_links)} (across {len(unique_urls)} unique targets)\n")

    print(f"Checking {len(unique_urls)} unique URLs using 8 threads...")

    url_results = {}
    with ThreadPoolExecutor(max_workers=8) as executor:
        futures = {
            executor.submit(check_single_url, u, canvas_url, token): u
            for u in unique_urls
        }
        for future in futures:
            u = futures[future]
            try:
                status, err, final_url = future.result()
                url_results[u] = (status, err, final_url)
            except Exception as e:
                url_results[u] = (0, str(e), u)

    # Classify results
    ok_urls = {}
    dead_urls = {}
    suspicious_urls = {} # e.g. 403 or redirects

    for u, (status, err, final_url) in url_results.items():
        if 200 <= status < 400:
            ok_urls[u] = status
        elif status == 404 or status == 410:
            dead_urls[u] = (status, err)
        elif status == 0 or status >= 500:
            dead_urls[u] = (status, err)
        else:
            # 401, 403, etc.
            suspicious_urls[u] = (status, err)

    print("\n" + "="*80)
    print("AUDIT RESULTS SUMMARY")
    print("="*80)
    print(f"Active & Verified (2xx/3xx): {len(ok_urls)}")
    print(f"Dead / Broken (404/5xx/Connection Error): {len(dead_urls)}")
    print(f"Blocked / Auth-Restricted (401/403): {len(suspicious_urls)}")

    if dead_urls:
        print("\n" + "!"*80)
        print("DEAD / BROKEN LINKS DETECTED:")
        print("!"*80)
        for u, (status, err) in sorted(dead_urls.items()):
            print(f"\n[Status {status}] {u}")
            print(f"Reason: {err}")
            print("Found in:")
            for item in valid_links:
                if item["clean_url"] == u:
                    print(f"  - [{item['source_type']}] {item['source_title']} (Text/Alt: '{item['text']}')")

    if suspicious_urls:
        print("\n" + "?"*80)
        print("RESTRICTED / BLOCKED LINKS (Status 401 / 403):")
        print("?"*80)
        for u, (status, err) in sorted(suspicious_urls.items()):
            print(f"\n[Status {status}] {u}")
            print(f"Reason: {err}")
            print("Found in:")
            for item in valid_links:
                if item["clean_url"] == u:
                    print(f"  - [{item['source_type']}] {item['source_title']} (Text/Alt: '{item['text']}')")

    # Save detailed JSON report
    report = {
        "summary": {
            "total_links": len(valid_links),
            "unique_urls": len(unique_urls),
            "ok": len(ok_urls),
            "dead": len(dead_urls),
            "suspicious": len(suspicious_urls)
        },
        "dead_links": [
            {
                "url": u,
                "status": status,
                "error": err,
                "occurrences": [
                    {
                        "source_type": item["source_type"],
                        "source_title": item["source_title"],
                        "tag": item["tag"],
                        "text": item["text"]
                    }
                    for item in valid_links if item["clean_url"] == u
                ]
            }
            for u, (status, err) in dead_urls.items()
        ],
        "suspicious_links": [
            {
                "url": u,
                "status": status,
                "error": err,
                "occurrences": [
                    {
                        "source_type": item["source_type"],
                        "source_title": item["source_title"],
                        "tag": item["tag"],
                        "text": item["text"]
                    }
                    for item in valid_links if item["clean_url"] == u
                ]
            }
            for u, (status, err) in suspicious_urls.items()
        ]
    }
    with open("link_audit_report_208B.json", "w") as f:
        json.dump(report, f, indent=2)
    print("\nDetailed report saved to 'link_audit_report_208B.json'.")

if __name__ == "__main__":
    main()
