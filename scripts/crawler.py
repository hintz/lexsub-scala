#!/usr/bin/env python
# -*- coding: utf-8 -*-

import mechanize, sys, time, os, string
from mechanize._util import write_file

class LexsubCrawler:
	def __init__(self, instanceFile, outFolder, sleepSeconds = 3, initUrl = False):
		self.outFolder = outFolder
		self.instances = set(map(string.rstrip, open(instanceFile, 'rU').readlines()))
		self.sleepSeconds = sleepSeconds
		self.br = mechanize.Browser()
		self.br.set_handle_robots(False)
		if initUrl: 
			print "Opening init URL " + initUrl
			self.open(initUrl)
		
	def write(self, word, data):
		write_file(self.outFolder + '/' + word + ".html", data)
		
	def search(self, keyword): pass ## implement this!
	
	def open(self, url): return self.br.open(url)
	
	def crawl(self):
		if not os.path.exists(self.outFolder): 
			os.makedirs(self.outFolder)
		for word in self.instances:
			print word + '..'
			try: 
				data = self.search(word)
				self.write(word, data)
			except Exception as e:
				print "ERROR: " + str(e) + " (when processing item '" + word + "')"
			time.sleep(self.sleepSeconds)
