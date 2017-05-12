import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import re

def plotFitness(xaxis, fitness, fitnessErr):
    plt.errorbar(xaxis, fitness, fitnessErr,lw=3)
    plt.xlabel("Generation", fontsize=17)
    plt.ylabel("Average Fitness", fontsize=17)
    plt.title("Average Fitness", fontsize=17)
    plt.xticks(fontsize=17)
    plt.yticks(fontsize=17)
    plt.tight_layout()
    plt.show()

def plot(xaxis, xlabel, yaxis, ylabel, yerr, plot, col_names):
    plt.errorbar(xaxis, yaxis, yerr,lw=3)
    if plot:
        plt.xlabel(xlabel, fontsize=17)
        plt.ylabel(ylabel, fontsize=17)
        plt.title("Performance Metrics", fontsize=17)
        plt.xticks(fontsize=17)
        plt.yticks(fontsize=17)
        plt.tight_layout()
        plt.legend(col_names, loc='upper left')
        plt.show()

def numFromScientificLisp(col, l):
    for string_num in col.values:
        num = float(re.findall("\d+\.\d+", string_num)[0])
        l.append(num)

def numFromFractionLisp(col, l):
    for string_num in col.values:
        nums = re.findall("\d+", string_num)
        num = float(nums[0]) / float(nums[1])
        l.append(num)

def getGAFitnessMetrics(csv):
    df = pd.read_csv(csv, index_col=False)
    xaxis = df['GENERATIONS'].values
    yaxis = df['AVERAGE-FITNESS'].values
    yerr = []
    numFromScientificLisp(df['FITNESS-ERROR'], yerr)
    print yaxis
    print yerr
    
    plotFitness(xaxis, yaxis, yerr)

def getGAPerformanceMetrics(csv):
    df = pd.read_csv(csv, index_col=False)
    xaxis = df['GENERATIONS'].values
    precision = []
    perr = []
    #numFromFractionLisp(df['AVERAGE-PRECISION'], precision)
    #numFromScientificLisp(df['PRECISION-ERROR'], perr)
    plot(xaxis, "Generations", df['AVERAGE-PRECISION'].tolist(), "Average Performance", df['PRECISION-ERROR'].tolist(), False, False)
    recall = []
    rerr = []
    #numFromFractionLisp(df['AVERAGE-RECALL'], recall)
    #numFromScientificLisp(df['RECALL-ERROR'], rerr)
    plot(xaxis, "Generations", df['AVERAGE-RECALL'].tolist(), "Average Performance", df['RECALL-ERROR'].tolist(), False, False)
    accuracy = []
    aerr = []
    #numFromFractionLisp(df['AVERAGE-ACCURACY'], accuracy)
    #numFromScientificLisp(df['ACCURACY-ERROR'], aerr)
    plot(xaxis, "Generations", df['AVERAGE-ACCURACY'].tolist(), "Average Performance", df['ACCURACY-ERROR'].tolist(), True, ["Average Precision", "Average Recall", "Average Accuracy"])
    
#getGAFitnessMetrics("generations-5.csv")
getGAPerformanceMetrics("generations.csv")
