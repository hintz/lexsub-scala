#!/usr/bin/env python
# -*- coding: utf-8 -*-

import mechanize, sys, time
from mechanize._util import write_file

OutFolder = 'wortschatz'
SleepSeconds = 4

InstanceFile = '../AIPHES_Data/GermEval2015/train-dataset.gold'
instanceFile = open(InstanceFile, 'rU')
instances = set(x.split()[0].split('.')[0] for x in instanceFile.readlines())

br = mechanize.Browser()
br.set_handle_robots(False)
br.open("http://corpora.informatik.uni-leipzig.de/cgi-bin/de/wort_www")

def search_and_save(keyword):
	br.select_form(nr=0)
	br['Wort'] = keyword
	br['lang'] = ['de']
	br.form.action = "http://corpora.informatik.uni-leipzig.de/cgi-bin/de/wort_www"
	result = br.submit(name="Submit")
	data = result.read()
	write_file(OutFolder + '/' + keyword + ".html", data)

for word in instances:
	print word + '..'
	search_and_save(word)
	time.sleep(SleepSeconds)