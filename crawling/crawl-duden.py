#!/usr/bin/env python
# -*- coding: utf-8 -*-

import mechanize, sys, time
from mechanize._util import write_file

OutFolder = 'download'
SleepSeconds = 3

InstanceFile = '../AIPHES_Data/GermEval2015/train-dataset.gold'
instanceFile = open(InstanceFile, 'rU')
instances = set(x.split()[0].split('.')[0] for x in instanceFile.readlines())

br = mechanize.Browser()
br.open("http://www.duden.de/")

def search_and_save(keyword):
	br.select_form(nr=0)
	br['s'] = keyword
	br.submit()

	result = br.follow_link(url_regex=r"rechtschreibung", nr=0)
	data = result.read()
	write_file(OutFolder + '/' + keyword + ".html", data)


# search_and_save("anspitzen")
# search_and_save("Erleichterung")
for word in instances:
	search_and_save(word)
	time.sleep(SleepSeconds)