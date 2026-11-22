#!/usr/bin/env python3
# THE RULE INDEX for the Discord bot's /rule command (good-game/Rules.scala): every numbered rule of the Law of Root, from the
# Root Database's copy (https://www.therootdatabase.com/law-of-root/, kept up to date with Leder's), with the link to it there.
# Run it again when the Law of Root changes:
#   python3 haunt-roll-fail/tools/law-index.py haunt-roll-fail/webp2/lfg/rules-root.tsv
# It writes: number, title, chapter, text, link. Only the official chapters (1 to 18 and the appendices), not the fan ones.
import html, re, sys, urllib.request

SITE = "https://www.therootdatabase.com"

def get(url):
    return urllib.request.urlopen(urllib.request.Request(url, headers={"User-Agent": "hrf-rules-index"}), timeout=60).read().decode("utf-8")

def plain(s):
    s = re.sub(r'<img[^>]*>', '', s)
    # the small capitals are spans inside a word ("<span class='large-char'>C</span>ARDS")
    s = re.sub(r"</?span[^>]*>", "", s)
    return re.sub(r"\s+", " ", html.unescape(re.sub(r"<[^>]+>", " ", s))).strip()

def entries(path, chapter):
    s = get(SITE + path)
    rows = []
    # the faction icons the site puts after a rule, which its copied text names ("... (4.1). LizardFrog")
    icons = sorted({a.capitalize() for a in re.findall(r'<img[^>]*alt="([^"]*)"[^>]*class="inline-icon"', s)}, key=len, reverse=True)
    # each rule: its copy-text icon, and before it the last block id="law-N" with its heading (h2/h4) or its bold title
    for m in re.finditer(r'data-link="(?:&gt;|>) (.*?)"\s*title="Copy text"', s, re.S):
        start = s.rfind('id="law-', 0, m.start())
        lid = re.match(r'id="law-(\d+)"', s[start:]).group(1)
        block, copied = s[start:m.start()], plain(m.group(1)).replace("**", "")
        num = copied.split(" - ", 1)[0].strip()
        rest = copied.split(" - ", 1)[1] if " - " in copied else ""
        h = re.search(r"<h[1-6][^>]*>(.*?)</h[1-6]>", block, re.S)
        st = re.search(r"<strong>(.*?)</strong>", block, re.S)
        title = plain((h or st).group(1)) if (h or st) else ""
        title = re.sub(r"^[0-9A-Z]+(\.[0-9]+)*\s+", "", title).rstrip(". ")
        if title.isupper():
            title = " ".join(w if w in ("and", "of", "the", "to", "in", "or", "a") else w.capitalize() for w in title.lower().split())
            title = title[0].upper() + title[1:]
        text = rest
        if title and text.lower().startswith(title.lower()):
            text = text[len(title):].lstrip(". ").strip()
        if icons:
            text = re.sub(r"(\s*(" + "|".join(icons) + r"))+$", "", text)
        if not re.match(r"^([0-9]+|[A-Z])(\.[0-9]+)*(\.[IVX]+)?$", num):
            continue
        rows.append((num, title, chapter, text, SITE + path + "?highlight_law=" + lid))
    return rows

def main():
    out = sys.argv[1] if len(sys.argv) > 1 else "rules-root.tsv"
    idx = get(SITE + "/law-of-root/")
    idx = idx[:idx.find("Law of Rootbotics")]
    rows, seen = [], set()
    for m in re.finditer(r'href="(/law/[^/]+/en/)\?highlight_law=\d+"[^>]*>(.*?)</a>', idx, re.S):
        name = plain(m.group(2))
        code, rest = name.split(". ", 1)
        known = ["Golden Rules", "Key Concepts", "Key Actions", "Setup", "Victory"]
        chapter = next((k for k in known if rest.startswith(k)), rest)
        chapter = re.split(r" (This|Terms|If) ", chapter)[0]
        for r in entries(m.group(1), code + ". " + chapter):
            if r[0] not in seen:
                seen.add(r[0])
                rows.append(r)
    with open(out, "w", encoding="utf-8") as f:
        f.write("number\ttitle\tchapter\ttext\tlink\n")
        for r in rows:
            f.write("\t".join(x.replace("\t", " ").replace("\n", " ") for x in r) + "\n")
    print("law-index:", len(rows), "rules")

if __name__ == "__main__":
    main()
