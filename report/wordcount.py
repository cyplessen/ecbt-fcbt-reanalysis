# Word count for the JRR body (Goal to Conclusion), excluding figure
# environment (caption), comments and LaTeX commands.
import re, sys
s = open(sys.argv[1] if len(sys.argv) > 1 else "robustness_report.tex").read()
import os
if os.path.exists("numbers.tex"):
    for m in re.finditer(r"\\newcommand\{\\(\w+)\}\{([^}]*)\}", open("numbers.tex").read()):
        s = re.sub(r"\\" + m.group(1) + r"(\{\})?(?![A-Za-z])", m.group(2), s)
s = s.replace("\\%", " percent ")
s = re.sub(r"(?<!\\)%.*", "", s)
s = re.sub(r"\\begin\{(figure|table)\}.*?\\end\{\1\}", " ", s, flags=re.S)
def clean(t):
    t = re.sub(r"\\(label|cite|ref)\*?\{[^}]*\}", " ", t)
    t = re.sub(r"\\[a-zA-Z]+\*?", " ", t)
    t = re.sub(r"[\$\{\}\[\]\\]", " ", t)
    return [w for w in re.split(r"\s+", t) if re.search(r"[A-Za-z0-9]", w)]
secs = ["Goal", "Methods", "Results", "Conclusion"]
pos = [s.index("\\section{%s}" % k) + len("\\section{%s}" % k) for k in secs]
pos.append(s.index("\\section*{\\large{Acknowledgments"))
tot = 0
for i, k in enumerate(secs):
    n = len(clean(s[pos[i]:pos[i+1]])); tot += n; print(f"{k:11s} {n}")
print(f"{'total':11s} {tot}  (limit 500)")
