import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from copy import deepcopy

plt.style.use('seaborn-white')

plt.figure(1)

xx = []
yy = []
ff = []

f = 20.
deltay = 0.
deltax = 2.
deltaf = 2.
x = 2.

deltasy = {}

while x < 80.:
    y = (x ** 2) / (4. * f) - deltay
    if y > 1.:
        xx.append(x)
        yy.append(0.)
        ff.append(0.)
        x = x + deltax
        f = f + deltaf
        deltay = (x ** 2) / (4. * f)
        deltasy[f] = deltay
        continue    
    xx.append(x)
    yy.append(y)
    ff.append(f)
    x = x + 0.01

y1 = [(x ** 2) / (4 * 26) - deltasy[26] for x in xx]
y2 = [(x ** 2) / (4 * 30) - deltasy[30] for x in xx]   
y3 = [(x ** 2) / (4 * 34) - deltasy[34] for x in xx]   
y4 = [(x ** 2) / (4 * 38) - deltasy[38] for x in xx]   
y5 = [(x ** 2) / (4 * 42) - deltasy[42] for x in xx]   
plt.axis([0, 82, -2, 40])
yyy = list(map(list, zip(*[yy, y1, y2, y3, y4, y5])))
plt.plot(xx, yyy)

plt.show()

for (a, b, c) in zip(xx, yy, ff):
    print('{:.2f} {:.2f} {:.2f}'.format(a*100., b*100., c*100.))
    


#x = []
#y = []
#
#with open("data.dat", "r") as fr:
#    for line in fr:
#        items = line.split()
#        x.append(float(items[0])) 
#        y.append(float(items[1]))
#
#plt.figure(figsize=(14,14))
##plt.hexbin(x, y, gridsize=100, cmap='Blues')
##cb = plt.colorbar(label='counts on bin')
##sns.kdeplot(x, y, cmap="Blues", shade=True, shade_lowest=True, bw=.15)
#sns.jointplot(x, y, kind='hex')
#plt.show()
