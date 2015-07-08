#!/usr/bin/env python
# -*- coding: utf-8 -*-
import re, os, sys, string
from collections import defaultdict
from operator import itemgetter


VocabFile = 'vocab.txt'
OutputFile = 'resources/coocs/output_coocs.txt'

WordFile = "../AIPHES_Data/coocs/deu_news_10M/deu_news_2010_10M-words.txt" # "words.txt"
CoocsFile = "../AIPHES_Data/coocs/deu_news_10M/deu_news_2010_10M-co_s.txt" # "coocs.txt" 

LowerCaseEverything = False
lower = lambda x: x.lower() if LowerCaseEverything else x

print 'Reading vocabulary..'
vocab = set(map(string.rstrip, open(VocabFile, 'rU').readlines()))

wordsFile = open(WordFile, 'rU')
coocsFile = open(CoocsFile, 'rU')
output = open(OutputFile, 'wb')


wordLookup = dict(x.strip().split('\t')[0:2] for x in wordsFile.readlines())
reverseLookup = {v: k for k, v in wordLookup.items()}
print 'Parsed word lookup'

coocs = defaultdict(list)
for line in coocsFile.xreadlines():
	c = line.strip().split()
	coocs[c[0]].append((c[1], c[3]))
print 'Parsed cooc file'


for w in sorted(vocab):
	wId = reverseLookup.get(w)
	if wId:
		cWords = coocs[wId]
		if not cWords:
			print "ERROR: no coocurences found for " + w
		for (cWordId, mi) in cWords:
			cWord = wordLookup.get(cWordId)
			if cWord:
				print >>output, lower(w) + '\t' + lower(cWord) + '\t' + mi
			else:
				print "ERROR: could not look up " + cWordId
	else:
		print "WARING: word not found: " + w


for f in [wordsFile, coocsFile, output]: f.close()
print 'done'