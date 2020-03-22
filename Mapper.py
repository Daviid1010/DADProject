import sys


myList = []
n = 20  # Number of top N records

for line in sys.stdin:
    line.replace('"','')
    line.strip()
    data = line.split(",")
    data[2].strip('\n')
    suicideCount = float(data[2])

    myList.append((suicideCount, line))
    myList.sort(reverse=True)

    # keep only first N records
    if len(myList) > n:
        myList = myList[:n]

# Print top N records
for (k, v) in myList:
    print(v)