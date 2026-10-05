import numpy as np
from scipy import optimize, stats

# Corrigendum (Sanger et al. 2021) Table 1, transcribed.
# cols: study, n_e, n_c, pre_c_m, pre_c_sd, pre_e_m, pre_e_sd, post_c_m, post_c_sd, post_e_m, post_e_sd
# e = eCBT (intervention), c = face-to-face (control)
tab = [
 ("Luxton 2016",      62, 59, 29.69, 11.74, 26.65, 11.80, 11.74, 12.08, 13.63, 12.47),
 ("Mohr 2012",       163,162, 22.83,  4.60, 22.83,  4.60, 12.51,  8.41, 13.58,  7.55),
 ("Choi 2014",        56, 63, 27.75,  6.59, 23.54,  6.44, 14.08,  7.46, 13.68,  7.48),
 ("Wagner 2014",      32, 30, 23.41,  7.63, 22.96,  6.07, 12.33,  8.77, 12.41, 10.03),
 ("Sethi 2013",       23, 21, 21.42,  6.80, 17.65,  5.03,  7.80,  3.09, 12.17,  3.12),
 ("Sethi 2010",        9, 10, 19.00,  5.10, 16.40,  9.20,  7.20,  3.10, 15.70,  4.20),
 ("Wright 2005",      15, 15, 24.40,  6.80, 31.10,  8.20,  9.70,  8.50, 10.70,  8.10),
 ("Himelhoch 2013",   16, 18, 24.60,  5.30, 22.60,  5.20, 15.90,  7.20, 16.30,  8.60),
 ("Poppelaars 2016",  51, 50, 63.35, 10.39, 62.61, 11.97, 59.33, 13.27, 57.88, 12.57),
 ("Glueckauf 2012",    7,  7, 11.80,  7.40, 12.67,  8.91,  9.00,  5.70,  4.67,  2.07),
 ("Kalapatapu 2014",  50, 53, 21.80,  3.80, 22.20,  4.70, 11.80,  7.20, 12.80,  9.20),
 ("Nelson 2003",      14, 14, 13.57,  8.75, 14.36,  9.85, 11.64, 11.63,  6.71,  4.78),
 ("Kay-Lambkin 2009", 32, 35, 34.91,  9.70, 28.57,  9.89, 16.65, 10.63, 17.09, 12.14),
 ("Andersson 2013",   33, 36, 24.10,  5.00, 23.60,  4.80, 17.10,  8.00, 13.60,  9.80),
]

# Corrigendum Fig 3 values (Mean "SD" for eCBT and f2f) for the check
fig3 = {
 "Luxton 2016":(13,2.2,18,2.2), "Mohr 2012":(9.25,0.7,10.35,0.8), "Choi 2014":(9.86,1.3,13.67,1.25),
 "Wagner 2014":(10.6,2.1,11.1,2.1), "Sethi 2013":(5.5,1.2,13.6,1.3), "Sethi 2010":(0.7,3.4,11.8,1.9),
 "Wright 2005":(20.4,2.9,14.7,2.8), "Himelhoch 2013":(6.3,2.5,8.7,2.1), "Poppelaars 2016":(4.7,2.4,4,2.4),
 "Glueckauf 2012":(9.4,3.5,2.8,3.5), "Kalapatapu 2014":(9.4,1.5,10,1.1), "Nelson 2003":(7.7,2.9,1.9,3.9),
 "Kay-Lambkin 2009":(11.5,2.8,18.3,2.4), "Andersson 2013":(10,1.9,7,1.6),
}

def hedges_g(m1, sd1, n1, m2, sd2, n2):
    sp = np.sqrt(((n1-1)*sd1**2 + (n2-1)*sd2**2)/(n1+n2-2))
    d = (m1-m2)/sp
    J = 1 - 3/(4*(n1+n2)-9)
    g = J*d
    v = (n1+n2)/(n1*n2) + g**2/(2*(n1+n2))
    return g, v

def re_dl(y, v):
    w = 1/v; yw = np.sum(w*y)/np.sum(w)
    Q = np.sum(w*(y-yw)**2); k = len(y)
    C = np.sum(w) - np.sum(w**2)/np.sum(w)
    tau2 = max(0, (Q-(k-1))/C)
    return tau2, Q

def re_reml(y, v):
    def nll(t2):
        w = 1/(v+t2); mu = np.sum(w*y)/np.sum(w)
        return 0.5*(np.sum(np.log(v+t2)) + np.log(np.sum(w)) + np.sum(w*(y-mu)**2))
    r = optimize.minimize_scalar(nll, bounds=(0, 50), method="bounded")
    return r.x

def pool(y, v, method="REML", hksj=False):
    tau2 = re_reml(y, v) if method=="REML" else re_dl(y, v)[0]
    w = 1/(v+tau2); mu = np.sum(w*y)/np.sum(w); se = np.sqrt(1/np.sum(w))
    k = len(y)
    if hksj:
        q = np.sum(w*(y-mu)**2)/(k-1); se = se*np.sqrt(max(q, 1))  # truncated HKSJ
        crit = stats.t.ppf(0.975, k-1)
    else:
        crit = 1.96
    Q = np.sum((1/v)*(y-np.sum(y/v)/np.sum(1/v))**2)
    I2 = max(0, (Q-(k-1))/Q)*100
    return mu, mu-crit*se, mu+crit*se, tau2, I2

print("=== 1. Does the corrigendum's 'SD' column reproduce as SE of change with r = 0? ===")
print(f"{'study':18s} {'chg_e':>6s} {'fig_e':>6s} {'seSD_e':>7s} {'fig_e':>6s}   {'chg_c':>6s} {'fig_c':>6s} {'seSD_c':>7s} {'fig_c':>6s}")
for (s, ne, nc, pcm, pcs, pem, pes, qcm, qcs, qem, qes) in tab:
    chg_e = pem-qem; chg_c = pcm-qcm
    se_e = np.sqrt(pes**2/ne + qes**2/ne); se_c = np.sqrt(pcs**2/nc + qcs**2/nc)
    fe, fse, fc, fsc = fig3[s]
    flag = "" if abs(chg_e-fe)<0.06 and abs(chg_c-fc)<0.06 else "  <-- mean mismatch"
    print(f"{s:18s} {chg_e:6.2f} {fe:6.2f} {se_e:7.2f} {fse:6.2f}   {chg_c:6.2f} {fc:6.2f} {se_c:7.2f} {fsc:6.2f}{flag}")

print("\n=== 2. Their analysis: 'SMD' = change diff / pooled SE (what RevMan did with their input) ===")
ys, vs = [], []
for (s, *_ ) in tab:
    fe, fse, fc, fsc = fig3[s]
    ne, nc = [t for t in tab if t[0]==s][0][1:3]
    g, v = hedges_g(fe, fse, ne, fc, fsc, nc); ys.append(g); vs.append(v)
ys, vs = np.array(ys), np.array(vs)
for m in ["DL","REML"]:
    mu, lo, hi, t2, I2 = pool(ys, vs, m)
    print(f"  {m:4s}: {mu:6.2f} [{lo:6.2f}, {hi:6.2f}]  tau2={t2:.2f} I2={I2:.0f}%")

print("\n=== 3. Correct analyses from the SAME Table 1 ===")
# 3a. Post-test SMD (Hedges g), sign: positive = eCBT higher depression score = favours f2f
print("-- 3a. Post-test Hedges g (positive favours face-to-face) --")
yp, vp = [], []
for (s, ne, nc, pcm, pcs, pem, pes, qcm, qcs, qem, qes) in tab:
    g, v = hedges_g(qem, qes, ne, qcm, qcs, nc); yp.append(g); vp.append(v)
    print(f"  {s:18s} g={g:6.2f} [{g-1.96*np.sqrt(v):6.2f}, {g+1.96*np.sqrt(v):6.2f}]")
yp, vp = np.array(yp), np.array(vp)
for m, h in [("DL",False),("REML",False),("REML",True)]:
    mu, lo, hi, t2, I2 = pool(yp, vp, m, h)
    print(f"  {m:4s}{'+HKSJ' if h else '     '}: {mu:6.2f} [{lo:6.2f}, {hi:6.2f}]  tau2={t2:.2f} I2={I2:.0f}%")
mask = np.array([s not in ("Sethi 2010","Sethi 2013") for (s,*_) in tab])
mu, lo, hi, t2, I2 = pool(yp[mask], vp[mask], "REML")
print(f"  REML w/o both Sethi trials: {mu:6.2f} [{lo:6.2f}, {hi:6.2f}]  tau2={t2:.2f} I2={I2:.0f}%")

# 3b. SMC: standardized mean change difference with imputed pre-post r; sign flipped so positive favours f2f
print("-- 3b. Difference in standardized mean change (SMCR, Morris-type), positive favours face-to-face --")
def smcr(m_pre, m_post, sd_pre, n, r):
    d = (m_post-m_pre)/sd_pre
    J = 1 - 3/(4*(n-1)-1)
    g = J*d
    v = 2*(1-r)/n + g**2/(2*n)
    return g, v
for r in [0.3, 0.5, 0.7, 0.9]:
    yc, vc = [], []
    for (s, ne, nc, pcm, pcs, pem, pes, qcm, qcs, qem, qes) in tab:
        ge, ve = smcr(pem, qem, pes, ne, r); gc, vcc = smcr(pcm, qcm, pcs, nc, r)
        yc.append(ge-gc); vc.append(ve+vcc)   # eCBT change minus f2f change; less negative = less reduction in eCBT
    yc, vc = np.array(yc), np.array(vc)
    for m in ["DL","REML"]:
        mu, lo, hi, t2, I2 = pool(yc, vc, m)
        print(f"  r={r:.1f} {m:4s}: {mu:6.2f} [{lo:6.2f}, {hi:6.2f}]  tau2={t2:.2f} I2={I2:.0f}%")

# 3c. change-score SMD with SD of change imputed from r (Cochrane 6.5.2.8) and pooled across arms
print("-- 3c. SMD of change scores, SD_change from imputed r (positive favours face-to-face) --")
for r in [0.5, 0.7]:
    yc, vc = [], []
    for (s, ne, nc, pcm, pcs, pem, pes, qcm, qcs, qem, qes) in tab:
        sde = np.sqrt(pes**2+qes**2-2*r*pes*qes); sdc = np.sqrt(pcs**2+qcs**2-2*r*pcs*qcs)
        g, v = hedges_g(pcm-qcm, sdc, nc, pem-qem, sde, ne)  # f2f change minus eCBT change: positive = f2f improved more
        yc.append(g); vc.append(v)
    yc, vc = np.array(yc), np.array(vc)
    for m in ["DL","REML"]:
        mu, lo, hi, t2, I2 = pool(yc, vc, m)
        print(f"  r={r:.1f} {m:4s}: {mu:6.2f} [{lo:6.2f}, {hi:6.2f}]  tau2={t2:.2f} I2={I2:.0f}%")

print("\n=== 4. Baseline imbalance (pre eCBT minus pre f2f, in pooled pre-SD units) ===")
for (s, ne, nc, pcm, pcs, pem, pes, *_ ) in tab:
    g, v = hedges_g(pem, pes, ne, pcm, pcs, nc)
    print(f"  {s:18s} {g:6.2f}")
