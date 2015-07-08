#!/usr/bin/env python
# -*- coding: utf-8 -*-

import mechanize, sys, time, os, string
from crawler import LexsubCrawler

class WortschatzCrawler(LexsubCrawler):
	def __init__(self): LexsubCrawler.__init__(self, 
		instanceFile = '../targets.txt', 
		outFolder = 'wortschatz',
		initUrl = "http://corpora2.informatik.uni-leipzig.de/cgi-bin/de/wort_www")

	def search(self, keyword):
		br = self.br
		br.select_form(nr=0)
		br['Wort'] = keyword
		br['lang'] = ['de']
		br.form.action = "http://corpora2.informatik.uni-leipzig.de/cgi-bin/de/wort_www"
		result = br.submit(name="Submit")
		## Alternative new version:
		##result = self.br.open("http://corpora.informatik.uni-leipzig.de/res.php?corpusId=deu_newscrawl_2011&word=" + keyword)
		return result.read()

WortschatzCrawler().crawl()