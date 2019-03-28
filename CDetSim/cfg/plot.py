import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

plt.style.use('seaborn-white')

x = []
y = []

with open("data.dat", "r") as fr:
    for line in fr:
        items = line.split()
        x.append(float(items[0])) 
        y.append(float(items[1]))

plt.figure(figsize=(14,14))
#plt.hexbin(x, y, gridsize=100, cmap='Blues')
#cb = plt.colorbar(label='counts on bin')
#sns.kdeplot(x, y, cmap="Blues", shade=True, shade_lowest=True, bw=.15)
sns.jointplot(x, y, kind='hex')
plt.show()
