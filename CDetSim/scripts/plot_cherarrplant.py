import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

plt.style.use('seaborn-white')

plt.figure(1)

x = np.linspace(0,80,1001)
y=[]

f = 20.
deltay = 0.
show = True
for t in x:
    newy = (t ** 2) / (4 * f) - deltay
    if newy > 1.:
        print(newy)
        show = not show
        y.append(newy - 1 if show else 0)
        f = f + 1.
        deltay = deltay + 1.
        continue
        
    y.append(newy if show else 0)

plt.axis([0, 82, -2, 40])
plt.plot(x, y)

plt.show()

for (a, b) in zip(x, y):
    print('{:.2f} {:.2f}'.format(a*100., b*100.))
    


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
