import pickle
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np
import pprint

cer_data = pickle.load(open('cer_data.pickle','rb'))

plt.imshow(cer_data['xhist20'])
plt.show()

dem = cer_data['xhist10']
ny, nx = dem.shape
extent = [-500., 500., -500., 500.]
x = np.linspace(-500., 500., nx)
y = np.linspace(-500., 500., ny)
xv, yv = np.meshgrid(x, y)

fig = plt.figure()
ax = fig.add_subplot(111)
im = plt.imshow(dem, extent=extent)
cs = ax.contour(xv, yv, dem, cmap='cool')
plt.show()

fig = plt.figure(2)
ax3 = fig.add_subplot(111, projection='3d')
dem = ax3.plot_surface(xv, yv, dem, cmap='plasma')
#ax3.set_title('DEM')
#ax3.set_zlabel('Elevation (x$10^{-2}$ m)')
plt.show()

fig = plt.figure(3)
for k in [1, 2, 5, 10]:
    elem = 'ahist{}'.format(k)
    plt.imshow(cer_data[elem], cmap='Spectral_r')
    plt.show()

nshow = float(len(cer_data['shower_info']))
fig = plt.figure(4)
for spc in ['linear', 'log']:
    for k in [1, 2, 5, 10]:
        n = int(500. // k)
        b = float(k)
        x1 = np.linspace(0., 500.-b, n)
        x2 = np.linspace(b, 500., n)
        x = np.linspace(0., 500.-b, n) + (b*0.5)
        area = 3.141592 * (x2**2 - x1**2)
        elem = 'rhist{}'.format(k)
        plt.yscale(spc)
        dens = (cer_data[elem] / nshow) / area
        plt.plot(x, dens)
    plt.show()


pprint.pprint(cer_data['ahist2'])
