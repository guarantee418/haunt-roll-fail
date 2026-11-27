#!/usr/bin/env python3
# Cuts the factions' emblems out of their first Building Board (the board's top left corner, over the title bar's rope frame and
# the tan panel) for the Company of Heroes card on the site's main page: webp2/tactics/images/emblem/<faction>-home.webp, 160x160,
# transparent around the emblem. The boards share one frame, so the frame without an emblem is rebuilt from the same board: the
# title bar row by row from its columns right of the emblem, the left rope column by column from the rows under it. What differs
# from it by more than the threshold is the emblem; under the frame only what goes on straight down from it is kept (the tips).
#
#   home-emblems.py <dir> <out>    <dir> holds the boards as tactics-art.py fetches them (named by their hash), <out> is
#                                  webp2/tactics/images
#
# Needs numpy, Pillow and OpenCV (pip install opencv-python-headless).

import sys, os
import numpy as np, cv2
from PIL import Image

BOARDS = {
    'us': '3961DCD195A7A7AC3F2A97E8A669E60C33EEB00F',
    'wehrmacht': 'C4E7F9A31FAE610FEDD3E7654533A51A9E64CE9D',
    'british': '156D1FA3195668036B8582EC87652A3D8EF13ADE',
    'soviet': '589860CCDAE2DCE85C7031752A49E29237A82C98',
    'okw': 'F11D27745BE7AB692E122C87360F61B1EB44E6E0',
}
H, W, CUT, T = 340, 400, 306, 45    # on the 1350x1650 board

def largest(m):
    n, lab, st, _ = cv2.connectedComponentsWithStats(m)
    k = 1 + np.argmax(st[1:, cv2.CC_STAT_AREA])
    return np.where(lab == k, 255, 0).astype(np.uint8)

def cut(path):
    img = Image.open(path).convert('RGB')
    img = img.resize((1350, 1650), Image.LANCZOS) if img.size != (1350, 1650) else img
    b = np.array(img).astype(np.int16)
    reg = b[:H, :W]
    rowt = np.median(b[:H, 345:410], axis=1)
    colt = np.median(b[360:460, :W], axis=0)
    diff = np.abs(reg - rowt[:, None, :]).max(axis=2)
    dc = np.abs(reg - colt[None, :, :]).max(axis=2)
    diff[:, :110] = np.minimum(diff[:, :110], dc[:, :110])
    diff[reg.max(axis=2) < 30] = 0
    m = (diff > T).astype(np.uint8) * 255
    m = cv2.morphologyEx(m, cv2.MORPH_OPEN, np.ones((3, 3), np.uint8))
    m = cv2.morphologyEx(m, cv2.MORPH_CLOSE, np.ones((9, 9), np.uint8))
    m = largest(m)
    inv = 255 - m
    n, lab, st, _ = cv2.connectedComponentsWithStats(inv)
    for i in range(1, n):
        x, y, ww, hh, a = st[i]
        if x > 0 and y > 0 and x + ww < W and y + hh < H:
            m[lab == i] = 255
    m = cv2.morphologyEx(m, cv2.MORPH_OPEN, np.ones((5, 5), np.uint8))
    raw = m.copy()
    for y in range(CUT, H):
        above = cv2.erode(m[y - 1:y], np.ones((1, 3), np.uint8)) if y > CUT + 2 else m[y - 1:y]
        m[y] = np.where(above[0] > 0, raw[y], 0)
    m = largest(m)
    m = cv2.GaussianBlur(m, (3, 3), 0)
    e = Image.fromarray(np.dstack([reg.astype(np.uint8), m]))
    x0, y0, x1, y1 = e.getbbox()
    s = max(x1 - x0, y1 - y0) + 8
    sq = Image.new('RGBA', (s, s), (0, 0, 0, 0))
    sq.alpha_composite(e.crop((x0, y0, x1, y1)), ((s - (x1 - x0)) // 2, (s - (y1 - y0)) // 2))
    return sq.resize((160, 160), Image.LANCZOS)

if __name__ == '__main__':
    d, out = sys.argv[1], sys.argv[2]
    for f, h in BOARDS.items():
        cut(os.path.join(d, h)).save(os.path.join(out, 'emblem', f + '-home.webp'), 'WEBP', quality=90, method=6)
        print(f)
