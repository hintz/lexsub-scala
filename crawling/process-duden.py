#!/usr/bin/env python
# -*- coding: utf-8 -*-

import BeautifulSoup, glob, codecs
from BeautifulSoup import BeautifulSoup

InFolder = "duden"
Outfile = "germeval_duden.tsv"
POSFile = '../target-pos.txt'

posFile = codecs.open(POSFile, encoding='utf-8"')
instancePos = dict(x.strip().split() for x in posFile.readlines())

def clean(messedUpUnicode):
	return messedUpUnicode.replace(u'\xad', u'')

with codecs.open(Outfile, "w", encoding="utf-8") as out:
	for file in glob.glob(InFolder + '/*.html'):
		bs = BeautifulSoup(codecs.open(file, encoding='utf-8"').read())
		word = bs.find('span', attrs={"class": "lemma"}).contents[0].strip()
		word = clean(word)
		print repr(file), '→', repr(word)
		pos = instancePos.get(word, '?')
		syns = [''.join(x.contents) for x in bs.findAll('a', attrs={"meta-topic" : 'Synonym'})]
		syns = map(clean, syns)
		for syn in syns:
			out.write(word + u'\t' + pos + u'\t' + syn + u'\tduden_synonym' + u'\n')