import bottleneck as bn
import numpy as np
import pathlib
path = 'Home/syed/data/raw'


def rollavg_bottlneck(a, n):
    return bn.move_mean(a, window=n, min_count=None)


sek = 10
SF = 100
win = SF*sek

files = list(pathlib.Path(path).glob('**/*.npy'))

for f in files:
    print(f)
    fstr = str(f)
    fstr = fstr.replace("npy", "csv")
    print(fstr)
    data = np.load(f)
    res = rollavg_bottlneck(data[:, 0], win)
    res = res[win::win]
    np.savetxt(fstr, res, fmt='%d')


# test = np.load(r"C:\Users\eskovgaard\Desktop\nonwear\Syed\data\GT3X\36265_0002043208\nw_vector.npy")
# np.savetxt(r"C:\Users\eskovgaard\Desktop\nonwear\Syed\data\GT3X\36265_0002043208\36265.csv", test, delimiter=",")
