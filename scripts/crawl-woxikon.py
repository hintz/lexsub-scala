#!/usr/bin/env python
# -*- coding: utf-8 -*-

from crawler import LexsubCrawler

class WoxikonCrawler(LexsubCrawler):
	def __init__(self): LexsubCrawler.__init__(self, 
		instanceFile = '../targets.txt', 
		outFolder = 'woxikon',
		initUrl = "http://synonyme.woxikon.de/")

	def search(self, keyword):
		br = self.br
		br.select_form(nr=0)
		br['q'] = keyword
		result = br.submit()
		return result.read()

WoxikonCrawler().crawl()