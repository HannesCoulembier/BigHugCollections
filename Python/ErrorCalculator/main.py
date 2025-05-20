import matplotlib.pyplot as pyplot
import numpy as np
import csv

import sys
import os
sys.path.append(os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
import Tools.bhmath as bhmath

with open('Python/ErrorCalculator/data_in.csv', 'r') as file:
    ranges = [bhmath.Interval(float(range[0]), float(range[1]), True, True) for range in csv.reader(file, delimiter=',')]
with open('Python/ErrorCalculator/function.txt') as file:
    func = eval(file.read())

valuesPerRange = 1000
valuesPerRender = 1000
margin = 0.05

rangeBorders = np.concatenate([[range.a,range.b] for range in ranges])
minReached = np.min(rangeBorders)
maxReached = np.max(rangeBorders)
left = minReached - (maxReached-minReached)*margin
right = maxReached + (maxReached-minReached)*margin

xRender = np.linspace(left, right, valuesPerRender)
yRender = np.array(list(map(func, xRender)))
pyplot.plot(xRender, yRender, color="b", label="base function")

result = []
for range in ranges:
    x = np.linspace(range.a,range.b,valuesPerRange, endpoint=True)
    y = np.array(list(map(func, x)))
    result.append(bhmath.Interval(np.min(y), np.max(y), True, True))
    pyplot.plot(x, y, color="r", label="values reached")

pyplot.show()

# If the plot is closed, we return the new ranges
with open('Python/ErrorCalculator/data_out.csv', 'w', newline='') as file:
    writer = csv.writer(file, delimiter=',')
    for range in result:
        writer.writerow([range.a, range.b])