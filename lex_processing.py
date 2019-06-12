import math
import nltk, re, pprint, string
from nltk import word_tokenize, sent_tokenize
import datetime
import pandas as pd
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
 
#from scipy.stats import zprob

def zTransform(r, n):
    z = np.log((1 + r) / (1 - r)) * (np.sqrt(n - 3) / 2)
    p = zprob(-z)
    return p

def get_julian_datetime(date):
 
    # Ensure correct format
    if not isinstance(date, datetime.datetime):
        raise TypeError('Invalid type for parameter "date" - expecting datetime')
    elif date.year < 1801 or date.year > 2099:
        raise ValueError('Datetime must be between year 1801 and 2099')

    # Perform the calculation
    julian_datetime = 367 * date.year - int((7 * (date.year + int((date.month + 9) / 12.0))) / 4.0) + int(
        (275 * date.month) / 9.0) + date.day + 1721013.5 + (
                          date.hour + date.minute / 60.0 + date.second / math.pow(60,
                                                                                  2)) / 24.0 - 0.5 * math.copysign(
        1, 100 * date.year + date.month - 190002.5) + 0.5

    return julian_datetime

def wordCount(text):
    wordDict = {}
    for sentence in text:
        for entry in sentence:
            keyTitle = entry[1]
            if keyTitle not in wordDict:
                wordDict[keyTitle] = 1
            else:
                count = wordDict[keyTitle]
                count = count + 1
                wordDict[keyTitle] = count
    return wordDict

def meanLengthSentence(text):
    total_length = 0
    for sent in text:
        total_length = total_length+len(sent)
    return total_length / len(text)

def preprocess(text):
    sentences = sent_tokenize(text)
    sentences = [word_tokenize(sent) for sent in sentences]
    sentences = [nltk.pos_tag(sent) for sent in sentences]
    return sentences

def percentage(count, total):
    return 100*count / total

def lexical_diversity(text):
    return len(set(text)) / len(text)

def strip_punctuation(text):
    table = str.maketrans(dict.fromkeys(':,.?'))
    s = text.translate(table)
    return s

def BrunetsIndex (TextLength, UniqueWords):
    w = TextLength**(UniqueWords**-.165)
    return w

def plotter (xs, ys):
    xs = np.asarray(xs)
    trend = np.polyfit(xs, ys, 1) # fit a straight line
    plt.set_xlabel('Article Index')
    plt.set_ylabel(ys)
    plt.plot(xs, ys,'o')
    plt.plot(xs,trend[1]+trend[0]*xs)