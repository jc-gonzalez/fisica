import json

import numpy as np
import matplotlib.pyplot as plt

with open('results.json', 'r') as fr:
    data = json.load(fr)

for k in data.keys():
    
    x = data[k]['data']['cam']['x']
    y = data[k]['data']['cam']['y']

    plt.style.use('seaborn-white')
    plt.figure(figsize=(17,20))

    d = plt.hexbin(x, y, gridsize=50, vmin=100, vmax=2000, cmap='Blues')
    print(d)
    cb = plt.colorbar(label='counts on bin')
    plt.show()

    prmpt = input('Press any key... ')
