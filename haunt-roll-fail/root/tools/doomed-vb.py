# Cut the Doomed Vagabonds' character art and titles from the PnP card PNGs (Law of Doom D7).
import sys
from PIL import Image, ImageDraw, ImageChops
src, out = sys.argv[1], sys.argv[2]
names = {1: 'berserker', 2: 'bard', 3: 'swindler', 4: 'zealot', 5: 'blacksmith', 6: 'barkeep', 7: 'grouch'}
bg = (0x45, 0x40, 0x3f)
for i, n in names.items():
    card = Image.open(f'{src}/DoomedVBs-CARDfront{i}.png').convert('RGB')
    art = card.crop((130, 170, 698, 738)).convert('RGBA')
    mask = Image.new('L', art.size, 0)
    ImageDraw.Draw(mask).rounded_rectangle((0, 0, art.size[0] - 1, art.size[1] - 1), radius=48, fill=255)
    art.putalpha(mask)
    art.resize((889, 889), Image.LANCZOS).save(f'{out}/vb-char-{n}.webp', quality=88)
    band = card.crop((58, 40, 767, 155)).convert("L")
    w = 920
    h = round(band.size[1] * w / band.size[0])
    band = band.resize((w, h), Image.LANCZOS)
    a = band.point(lambda v: max(0, min(255, (v - 40) * 255 // 200)))
    title = Image.new('RGB', (w, 200), bg)
    white = Image.new('RGB', (w, h), (255, 255, 255))
    title.paste(white, (0, (200 - h) // 2), a)
    title.save(f'{out}/vb-title-{n}.webp', quality=90)
