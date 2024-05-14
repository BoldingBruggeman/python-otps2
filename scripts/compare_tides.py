import sys
import datetime
import numpy as np

from timeit import default_timer

start = default_timer()

from matplotlib import pyplot

IGOTM_DIR = r"C:\Users\jornb\OneDrive\Code\igotm"

sys.path.append(IGOTM_DIR)

import tpxo9

import pyTMD.predict

m = tpxo9.TPXO()

r = m.get(-4.148, 50.366)

comp_ids = []
comp_re = []
comp_im = []
for c, (re, im) in r.components.items():
    comp_ids.append(c)
    comp_re.append(re)
    comp_im.append(im)
comp_re = np.array(comp_re)
comp_im = np.array(comp_im)

comp_vals = np.ma.array([complex(r, i) for r, i in zip(comp_re, comp_im)], mask=False)

amp = np.abs(comp_vals)[np.newaxis, :]
ph = np.arctan2(-np.imag(comp_vals), np.real(comp_vals))[np.newaxis, :]
ph = ph * 180.0 / np.pi
ph[ph < 0] += 360.0

# calculate complex phase in radians for Euler's
cph = -1j * ph * np.pi / 180.0
# calculate constituent oscillation
hc = amp * np.exp(cph)


start = datetime.datetime(2024, 1, 1)
timestep = 600.0
n = 30 * 24 * int(3600 / timestep)

nrep = 5

print("OTPS...", end="")
start_timer = default_timer()
for _ in range(nrep):
    h = r.predict(start, n, timestep)
print(f" done: {default_timer() - start_timer:.3f} s")

dt_ref = datetime.datetime(1992, 1, 1)
t = (np.arange(n) * timestep + (start - dt_ref).total_seconds()) / 86400.0
print("pyTMD...", end="")
start_timer = default_timer()
for _ in range(nrep):
    h_alt = pyTMD.predict.time_series(t, hc, comp_ids)
    minor = pyTMD.predict.infer_minor(t, hc, comp_ids)
    h_alt.data[:] += minor.data[:]
print(f" done: {default_timer() - start_timer:.3f} s")

fig, (ax1, ax2) = pyplot.subplots(figsize=(15, 5), ncols=2)
ax1.plot(h, color="k", label="OTPS")
ax1.plot(h_alt, ls="--", color="r", label="pyTMD")
ax1.legend()
ax1.grid()
ax1.set_ylabel("elevation (mm)")
ax2.plot(h_alt - h)
ax2.grid()
ax2.set_ylabel("OTPS - pyTMD elevation difference (mm)")
pyplot.show()
