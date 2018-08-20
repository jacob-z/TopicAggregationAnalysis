from rpy2.robjects import r, pandas2ri
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
from textwrap import wrap

r['load']("python_plotting_data.RData")
dat = pandas2ri.ri2py(r['dat'])
dat.mad_labs = ["\n".join(wrap(l, 20)) for l in dat.mad_labs]

print type(dat)
print dat.head()

f, (ax_box, ax_hist) = plt.subplots(2, sharex=True, 
	gridspec_kw={"height_ratios": (.85, .15)})

fig1 = plt.gcf()
sns.catplot(x="mad_vals", y="mad_labs", kind="box", data=dat, ax=ax_box)
ax_box.yaxis.label.set_visible(False)
ax_box.xaxis.label.set_visible(False)
sns.distplot(dat.mad_vals, kde=False, ax=ax_hist)

plt.show()
# fig1.savefig("mad_dist.png", bbox_inches="tight")
