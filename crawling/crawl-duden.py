#!/usr/bin/env python
# -*- coding: utf-8 -*-

import mechanize, sys, time, os, string
from mechanize._util import write_file

OutFolder = 'duden'
SleepSeconds = 3

InstanceFile = '../targets.txt'
instances = set(map(string.rstrip, open(InstanceFile, 'rU').readlines()))

br = mechanize.Browser()
br.open("http://www.duden.de/")

def search_and_save(keyword):
	br.select_form(nr=0)
	br['s'] = keyword
	br.submit()

	result = br.follow_link(url_regex=r"rechtschreibung", nr=0)
	data = result.read()
	write_file(OutFolder + '/' + keyword + ".html", data)

if not os.path.exists(OutFolder): os.makedirs(OutFolder)

for word in instances:
	print word + '..'
	search_and_save(word)
	time.sleep(SleepSeconds)