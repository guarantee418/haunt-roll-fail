# Makes the five Uncharted Horizons map tiles tile/horizon-<1-4|bridge>.webp from their textures in the Tabletop
# Simulator mod 3597126237 (Custom_Model tiles; 1221x2564 textures, front on top). Each tile square is cut out,
# sized like the other tiles (948x944) and its colours brought closer to them: the scans are darker and much
# yellower (almost no blue in the grass), so the white dashes would not pass for dashes (tile-masks.py). The
# Bridge's impassable lines are a dull brown-orange in the scan; they get the Wilderness orange, which the tools
# and the map look for.
#
#   python3 horizon-tiles.py <directory with the five textures> <webp2/nort/images/tile>
#
# The textures are the DiffuseURLs of the five tiles stacked at (-8.7, 39.2) in the mod's save, in this order:
#   horizon-1       14515901207219808589/6321BF858FE594DF80545504B93AF4FF52AF5AA9
#   horizon-2       17352490810686560367/8534700D07571A1351622249B46166F00B1660EF
#   horizon-3       9519328652871587011/46F7A899E8E2A21B478DCF463D4B5BE83E4E908E
#   horizon-4       11904398015728264999/A02712CA01A1326FFCA25EDF3E5AA5CCEEDF19F1
#   horizon-bridge  15277547784910253796/8AF307A0A757F7B26523D381AB6B1B2B242B755C
# (under https://steamusercontent-a.akamaihd.net/ugc/), saved as <name>.jpg.
#
# Needs Pillow, numpy and scipy.

import sys
import numpy as np
from PIL import Image
from scipy import ndimage as ndi

names = ['horizon-1', 'horizon-2', 'horizon-3', 'horizon-4', 'horizon-bridge']

src, out = sys.argv[1], sys.argv[2]

for n in names:
    t = Image.open(src + '/' + n + '.jpg').convert('RGB').crop((30, 35, 1190, 1195)).resize((948, 944), Image.LANCZOS)
    a = np.asarray(t).astype(float)
    a[..., 0] *= 1.1
    a[..., 1] *= 1.1
    a[..., 2] = 45 + a[..., 2] * 0.9
    a = np.clip(a, 0, 255)

    if n == 'horizon-bridge':
        r, g, b = a[..., 0], a[..., 1], a[..., 2]
        line = (r > 165) & (g > 100) & (g < 165) & (b < 115) & (r - b > 95) & (r - g > 40)
        line = ndi.binary_closing(line, iterations=2)
        lab, k = ndi.label(line)
        sizes = ndi.sum(line, lab, range(1, k + 1))
        line = np.isin(lab, [i + 1 for i, s in enumerate(sizes) if s > 300])
        soft = ndi.gaussian_filter(ndi.binary_dilation(line, iterations=1).astype(float), 1.0)[..., None]
        # The orange of the Wilderness peaks
        target = np.array([235.0, 140.0, 36.0])
        fixed = np.clip(a * (target / np.median(a[line], 0)), 0, 255)
        a = a * (1 - soft) + fixed * soft

    Image.fromarray(a.astype(np.uint8)).save(out + '/' + n + '.webp', quality=88, method=6)
