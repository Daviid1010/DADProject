#!/usr/bin/env python
import sys

myList = []
n = 20  # Number of top N records

for line in sys.stdin:
	line.strip()
	if not line.strip():
		continue
	else:
		data = line.split(",")

	suicideCount = data[2]

	myList.append((suicideCount, line))

	myList.sort(reverse=True)

	if len(myList) > n:
		myList = myList[:n]
# Print top N records
for (k, v) in myList:
    print(v)