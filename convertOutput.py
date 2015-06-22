#!/usr/bin/env python

import sys
from collections import defaultdict

Input = 'instances.out'
if len(sys.argv) > 1: Input = sys.argv[1]
OOTOutput = Input + '.oot'
BestOutput = Input + '.best'

with open(Input, 'r') as f:
	byId = defaultdict(list)
	
	# parse:
	for line in f.readlines():
		delim = line.split('\t')
		if len(delim) < 5:
			#print >> sys.stderr, 'Ignoring line ' + repr(line)
			continue
		(verb, pos, id, extension, score) = delim
		identifier = verb + '.' + pos.lower() + ' ' + id
		byId[identifier] += [extension]
		
	# output in semeval format:
	with open(OOTOutput, 'wb') as ootFile:
		with open(BestOutput, 'wb') as bestFile:
			for (id, extensions) in byId.items():
				oot = ";".join(extensions[:10])
				best = extensions[0]
				print >>ootFile, id + ' ::: ' + oot
				print >>bestFile, id + ' :: ' + best
