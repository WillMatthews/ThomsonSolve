#! /usr/bin/python3
import math
import matplotlib.pyplot as plt
from ast import literal_eval as make_tuple


with open("./test.txt", "r") as f:
    content = f.readlines()

try:
    content
except NameError:
    print("\n\nOutput file does not exist")
    exit()

dataTup = make_tuple(content[0])

ptX  = [ x[0][0] for x in dataTup[1] ]
ptY  = [ x[0][1] for x in dataTup[1] ]
velX = [ x[1][0] for x in dataTup[1] ]
velY = [ x[1][1] for x in dataTup[1] ]

minL2 = float("inf")
for i,pt1 in enumerate(dataTup[1]):
    for j,pt2 in enumerate(dataTup[1]):
        if i != j:
            l2 = math.sqrt((pt1[0][0]-pt2[0][0])**2 + (pt1[0][1]-pt2[0][1])**2)
            minL2 = min(minL2,l2)

ax = plt.gca()
ax.cla()

circle = plt.Circle((0,0), 1, color='green', fill=False)
ax.add_artist(circle)

for pt in dataTup[1]:
    circle = plt.Circle((pt[0][0], pt[0][1]), minL2/2, color='blue', fill=False)
    ax.add_artist(circle)

print(minL2)

plt.plot(ptX, ptY, 'ro')
#plt.plot(velX, velY, 'ro')
#plt.axis([0, 6, 0, 20])
plt.show()

#writeFile "./test.txt" (show (debugTest 2 16 0.001 4 10000))
