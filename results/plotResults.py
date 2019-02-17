import matplotlib.pyplot as plt
import numpy as np

if("PLOT CONVERGENCE PART 1"):
    filenames = ['zeta0.txt', 'mach0.txt']
    for file in filenames:
        data = open(file, 'r')
        header = data.readline()
        nlist = []
        resultlist = []
        for line in data:
            line = line.split()
            nlist.append(int(line[0]))
            resultlist.append(float(line[1]))
        plt.loglog(nlist,resultlist)
    plt.grid()
    plt.legend([file.split('.')[0] for file in filenames]) # Python is the best language 
    plt.title("Convergence plot")
    plt.xlabel(r"$n$")
    plt.ylabel(r"$|\pi_n - \pi|$")
    plt.show()

if("PLOT TIME PART 1"):
    filenames = ['zeta0.txt', 'mach0.txt']
    for file in filenames:
        data = open(file, 'r')
        header = data.readline()
        nlist = []
        resultlist = []
        for line in data:
            line = line.split()
            nlist.append(int(line[0]))
            resultlist.append(float(line[2]))
        plt.loglog(nlist,resultlist)
    plt.grid()
    plt.legend([file.split('.')[0] for file in filenames]) # Python is the best language 
    plt.title("Time plot")
    plt.xlabel(r"$n$")
    plt.ylabel(r"Time in seconds")
    plt.show()

