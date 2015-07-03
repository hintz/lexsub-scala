#!/usr/bin/env python
# -*- coding: utf-8 -*-

from crawler import LexsubCrawler

class DudenCrawler(LexsubCrawler):
	def __init__(self): LexsubCrawler.__init__(self, 
		instanceFile = '../targets.txt', 
		outFolder = 'duden',
		initUrl = "http://www.duden.de/")

	def search(self, keyword):
		br = self.br
		br.select_form(nr=0)
		br['s'] = keyword
		br.submit()
		result = br.follow_link(url_regex=r"rechtschreibung", nr=0)
		return result.read()

DudenCrawler().crawl()