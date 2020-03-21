#!/usr/bin/env python
import sys

myList = []
n = 20  # Number of top N records

for line in sys.stdin:
    # remove leading and trailing whitespace
    line = line.strip()
    # split data values into list
    data = line.split(",")
    SuicideCount = data[0]
    myList.append((SuicideCount, line))
    myList.sort()
    if len(myList) > n:
        myList = myList[:n]
print(myList)

# Print top N records
for (k, v) in myList:
    print(v)