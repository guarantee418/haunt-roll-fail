#!/usr/bin/env python3
# Cuts the seven Corruption track spaces of Chancellor's Utopia out of its boards, for the track on its player panel
# (`Utopia` in factionStatus, root/ui.scala): faction/utopia/cu-track-<board>-<n> (the space as printed, emptied) and
# cu-track-<board>-<n>-m (the same with a morality token on it), for cu-board-v1 (CU) and cu-board-v3 (CUv2).
# The morality token is drawn here (the boards don't show one): a lilac disc with the track's scales, in the space's
# top left corner, so the printed icon of what emptying the space does stays visible.
#
#   python3 utopia-track.py
import os
from PIL import Image, ImageDraw
import numpy as np

dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), '..', '..', 'webp2', 'root', 'images', 'faction', 'utopia')

R = 82          # space radius on the board
SIZE = 100      # output size

def space(board, k):
    cx = round(1173 + 159.6 * (k - 1))
    cy = 715 if k % 2 == 1 else 788
    c = board.crop((cx - R, cy - R, cx + R, cy + R))
    mask = Image.new('L', (4 * 2 * R, 4 * 2 * R), 0)
    ImageDraw.Draw(mask).ellipse((0, 0, 4 * 2 * R - 1, 4 * 2 * R - 1), fill=255)
    c.putalpha(mask.resize(c.size, Image.LANCZOS))
    return c.resize((SIZE, SIZE), Image.LANCZOS)

def scales(board):
    # the light scales printed on space 1, as a mask
    c = np.asarray(board.crop((1173 - R, 715 - R, 1173 + R, 715 + R)).convert('RGB')).astype(int)
    light = (c[..., 0] > 245) & (c[..., 1] > 228) & (c[..., 2] > 200)
    yy, xx = np.mgrid[0:2 * R, 0:2 * R]
    light &= (xx - R) ** 2 + (yy - R) ** 2 < (R - 6) ** 2
    return Image.fromarray((light * 255).astype(np.uint8))

def morality(board):
    S = 4 * 58
    t = Image.new('RGBA', (S, S), (0, 0, 0, 0))
    d = ImageDraw.Draw(t)
    d.ellipse((6, 6, S - 7, S - 7), fill=(199, 175, 217, 255), outline=(99, 87, 108, 255), width=14)
    m = scales(board).resize((int(S * 0.8), int(S * 0.8)), Image.LANCZOS)
    ink = Image.new('RGBA', m.size, (99, 87, 108, 255))
    off = (S - m.size[0]) // 2
    t.paste(ink, (off, off), m)
    return t.resize((58, 58), Image.LANCZOS)

for name in ['v1', 'v3']:
    board = Image.open(os.path.join(dir, 'cu-board-' + name + '.webp')).convert('RGBA')
    token = morality(board)
    for k in range(1, 8):
        s = space(board, k)
        s.save(os.path.join(dir, 'cu-track-%s-%d.webp' % (name, k)), 'WEBP', quality=90)
        m = s.copy()
        m.alpha_composite(token, (0, 0))
        m.save(os.path.join(dir, 'cu-track-%s-%d-m.webp' % (name, k)), 'WEBP', quality=90)
