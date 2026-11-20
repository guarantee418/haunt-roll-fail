# Makes the player panels' icon for a clan played by a bot (run from haunt-roll-fail/), in the style of panel-icons.py:
# bot.webp, a robot's head with an antenna.
# Drawn 8 times larger then scaled down to 64x64, with a dark outline.
from PIL import Image, ImageDraw, ImageFilter

IMAGES = 'webp2/nort/images/'
K = 8
S = 64 * K

def outlined(shape, out, outline=(40, 30, 20, 255)):
    edge = Image.new('RGBA', shape.size, outline)
    edge.putalpha(shape.getchannel('A').filter(ImageFilter.MaxFilter(2 * K + 1)))
    edge.alpha_composite(shape)
    edge.resize((64, 64), Image.LANCZOS).save(IMAGES + 'ui/' + out, quality=90)

# bot.webp: a steel-blue square head with two glowing eyes, a grille mouth, ears and an antenna
steel, dark, glow = (150, 176, 198, 255), (52, 66, 80, 255), (120, 230, 255, 255)
bot = Image.new('RGBA', (S, S))
d = ImageDraw.Draw(bot)
d.line([(S * 0.50, S * 0.10), (S * 0.50, S * 0.28)], fill=steel, width=4 * K)
d.ellipse((S * 0.43, S * 0.03, S * 0.57, S * 0.17), fill=(230, 80, 60, 255))
d.rounded_rectangle((S * 0.06, S * 0.44, S * 0.18, S * 0.70), radius=S * 0.03, fill=steel)
d.rounded_rectangle((S * 0.82, S * 0.44, S * 0.94, S * 0.70), radius=S * 0.03, fill=steel)
d.rounded_rectangle((S * 0.16, S * 0.26, S * 0.84, S * 0.92), radius=S * 0.10, fill=steel)
for x in (0.36, 0.64):
    d.ellipse((S * (x - 0.10), S * 0.40, S * (x + 0.10), S * 0.60), fill=dark)
    d.ellipse((S * (x - 0.06), S * 0.44, S * (x + 0.06), S * 0.56), fill=glow)
d.rounded_rectangle((S * 0.30, S * 0.70, S * 0.70, S * 0.82), radius=S * 0.02, fill=dark)
for i in range(1, 4):
    x = S * (0.30 + 0.10 * i)
    d.line([(x, S * 0.70), (x, S * 0.82)], fill=steel, width=2 * K)
outlined(bot, 'bot.webp')
