import sys


myList = []
n = 20  # Number of top N records

for line in sys.stdin:
    line.strip()
    data = line.split(",")

    suicideCount = data[2]

    myList.append((suicideCount, line))
    # sort list in reverse order
    myList.sort()

    # keep only first N records
    if len(myList) > n:
        myList = myList[:n]
# print(myList)

# Print top N records
for (k, v) in myList:
    print(v)