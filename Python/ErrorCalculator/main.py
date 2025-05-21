import matplotlib.pyplot as pyplot
import numpy as np
import csv

def calcRanges(fin, fout, func, divisions):
    # Parse input file into an array of values where every value is an array of parameter ranges -> [[[value1param1Min, value1param1Max], [value1param2Min, value1param2Max], ...], [[value2param1Min, value2param1Max], [value2param2Min, value2param2Max], ...], ...]
    with open(fin, 'r') as file:
        values = np.array([[[float(min(param.split(' '))), float(max(param.split(' ')))] for param in row] for row in csv.reader(file, delimiter=',')])

    result = []
    paramCount = len(values[0])
    for value in values:
        # Create all x values to be evaluated
        xValues = np.stack(np.meshgrid(*[np.linspace(param[0], param[1], divisions) for param in value], indexing='ij'), axis=-1).flatten()
        xValues.shape = (divisions**paramCount, paramCount)

        # Find the min and the max values of f(x) over all x values
        yValues = np.array(list(map(lambda x:func(*x), xValues)))
        result.append([np.min(yValues), np.max(yValues)])

    # Write result into output file
    with open(fout, 'w', newline='') as file:
        writer = csv.writer(file, delimiter=' ')
        for value in result:
            writer.writerow(value)

def plotRanges(fin, func, rangeDivisions, renderDivisions):
    # Parse input file into an array of values where every value is an array of parameter ranges -> [[[value1param1Min, value1param1Max], [value1param2Min, value1param2Max], ...], [[value2param1Min, value2param1Max], [value2param2Min, value2param2Max], ...], ...]
    with open(fin, 'r') as file:
        values = np.array([[[float(min(param.split(' '))), float(max(param.split(' ')))] for param in row] for row in csv.reader(file, delimiter=',')])
        if len(values[0]) != 1: print("Can only plotRanges when function takes exactly one parameter")

    margin = 0.05

    rangeBorders = values.flatten()
    minReached = np.min(rangeBorders)
    maxReached = np.max(rangeBorders)
    left = minReached - (maxReached-minReached)*margin
    right = maxReached + (maxReached-minReached)*margin

    xRender = np.linspace(left, right, renderDivisions)
    yRender = np.array(list(map(func, xRender)))
    pyplot.plot(xRender, yRender, color="b", label="base function")

    for range in values[:,0]:
        x = np.linspace(range[0],range[1],rangeDivisions, endpoint=True)
        y = np.array(list(map(func, x)))
        pyplot.plot(x, y, color="r", label="values reached")

    pyplot.show()

func = lambda x,y:x**2+y
calcRanges('Python/ErrorCalculator/data_in.csv', 'Python/ErrorCalculator/data_out.csv', func, 1000)
# plotRanges('Python/ErrorCalculator/data_in.csv', func, 10, 100)
