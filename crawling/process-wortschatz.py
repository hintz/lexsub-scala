#!/usr/bin/env python
# -*- coding: utf-8 -*-

import BeautifulSoup, glob, codecs, re
from BeautifulSoup import BeautifulSoup
from collections import defaultdict

InputDirectory = "wortschatz"
Outfile = "germeval_wortschatz.tsv"
RelationPrefix = "Wortschatz_"

InstanceFile = '../AIPHES_Data/GermEval2015/train-dataset.gold' # for POS
instanceFile = codecs.open(InstanceFile, encoding='utf-8"')
instancePos = dict(x.split()[0].split('.') for x in instanceFile.readlines())

ExcludeRelations = ["Form"] # List of semantic relations to exclude!

# renames the semantic relation scraped from the website
def translateRelation(relationName):
	relationName = re.sub('[^0-9a-zA-ZäÄöÖüÜß\s]+', '', relationName).replace(" ", "_")
	if relationName[0].isdigit(): return RelationPrefix + "Dornseiff" # Dornseiff-Bedeutungsgruppen
	if "synonym" in relationName or "referenced" in relationName: return RelationPrefix + relationName
	return RelationPrefix + "other_" + relationName

def process(filename):
	bs = BeautifulSoup(codecs.open(filename, encoding='utf-8"').read())
	word = bs.find(attrs={"class": "result"}).findChild('p')
	if not word: return []
	word = word.contents[1].strip()
	pos = instancePos.get(word, '?')
	syns = defaultdict(list)
	listItems = bs.findAll('li')

	def extract(li):
		category = translateRelation(li.contents[0].strip())
		words = map(lambda x: x.strip(), sum((x.contents for x in li.findAll('a')), []))
		if not any(exclude in category for exclude in ExcludeRelations):
			for w in words: syns[w].append(category)
	map(extract, listItems)


	lines = [word + u'\t' + pos + u'\t' + syn + u'\t' + u";".join(set(relations)) + '\n' 
			for (syn, relations) in syns.items() if word != syn] # if any(map(lambda x: 'synonym' in x,relations))
	return lines

#process("wortschatz/anklagen.html")
with codecs.open(Outfile, "w", "utf-8") as out:
	for file in glob.glob(InputDirectory + '/*.html'):
		try:
			map(out.write, process(file))
		except Exception as e:
			print 'Error: ' + str(e) + ' in ' + file

