#!/usr/bin/env python
# -*- coding: utf-8 -*-

import BeautifulSoup, glob, codecs
from BeautifulSoup import BeautifulSoup

InFolder = "woxikon"
Outfile = "germeval_woxikon.tsv"
POSFile = '../target-pos.txt'

posFile = codecs.open(POSFile, encoding='utf-8"')
instancePos = dict(x.strip().split() for x in posFile.readlines())

with codecs.open(Outfile, "w", encoding="utf-8") as out:
	for file in glob.glob(InFolder + '/*.html'):
		bs = BeautifulSoup(codecs.open(file, encoding='utf-8"').read())
		word = bs.find('h2', attrs={"class": "inline highlight"}).contents[0]
		syns = []
		for sysContent in bs.findAll('h4', attrs={"class": "synonymsContent"}):
			for a in sysContent.findAll('a'):
					if not len(a.findChildren()):
						if len(a.contents):
							syn = a.contents[0]
							senseId = sysContent.parent.find('span', attrs={"class": "num"}).contents[0]
							syns.append((syn, senseId))
						else: 
							print "WARNING: Found no content in " + str(a) + " in file " + file
		pos = instancePos.get(word, '?')
		for (syn, senseId) in syns:
			relations = ";".join(['woxikon_synonym', 'woxikon_sense'+senseId])
			out.write(word + u'\t' + pos + u'\t' + syn+ u'\t' + relations + u'\n')