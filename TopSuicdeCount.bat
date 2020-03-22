REM Locally test topMaleAbalonByHeight mapper and reducer

mkdir MapperOutput

REM Run mapper on 1st data split, output to file
type Split1.txt
type Split1.txt | Mapper.py
type Split1.txt | Mapper.py > MapperOutput\MapperOut.txt

REM Run mapper on 2nd data split, append to the same file
type Split2.txt | Mapper.py >> MapperOutput\MapperOut.txt

REM View the output of the mappers (sorted)
type MapperOutput\MapperOut.txt | sort

REM Run the reducer
type MapperOutput\MapperOut.txt | sort | Reducer.py

REM Run the reducer, output to file
type MapperOutput\MapperOut.txt | sort | Reducer.py > MapperOutput\ReducerOutput.csv

REM Wait for user to press a key
PAUSE