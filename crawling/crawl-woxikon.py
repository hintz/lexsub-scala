#!/usr/bin/env python
# -*- coding: utf-8 -*-

import mechanize, sys, time
from mechanize._util import write_file

OutFolder = 'woxikon'
SleepSeconds = 4

InstanceFile = '../AIPHES_Data/GermEval2015/train-dataset.gold'
instanceFile = open(InstanceFile, 'rU')
instances = set(x.split()[0].split('.')[0] for x in instanceFile.readlines())

br = mechanize.Browser()
br.set_handle_robots(False)
br.open("http://synonyme.woxikon.de/")

def search_and_save(keyword):
	br.select_form(nr=0)
	br['q'] = keyword
	result = br.submit()
	data = result.read()
	write_file(OutFolder + '/' + keyword + ".html", data)

# search_and_save('fesselnd')
# search_and_save('Erleichterung')

for word in instances:
	print word + '..'
	try: 
		search_and_save(word)
	except Exception as e:
		print "ERROR " + e + " for " + word
	time.sleep(SleepSeconds)