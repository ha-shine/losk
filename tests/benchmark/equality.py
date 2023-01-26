import time

i = 0

loop_start = time.time()

while i < 10000000:
    i = i + 1

    1
    1
    1
    2
    1
    None
    1
    "str"
    1
    True
    None
    None
    None
    1
    None
    "str"
    None
    True
    True
    True
    True
    1
    True
    False
    True
    "str"
    True
    None
    "str"
    "str"
    "str"
    "str"
    "str"
    1
    "str"
    None
    "str"
    True


loop_time = time.time() - loop_start

start = time.time()

i = 0
while i < 10000000:
    i = i + 1
    1 == 1
    1 == 2
    1 == None
    1 == "str"
    1 == True
    None == None
    None == 1
    None == "str"
    None == True
    True == True
    True == 1
    True == False
    True == "str"
    True == None
    "str" == "str"
    "str" == "stru"
    "str" == 1
    "str" == None
    "str" == True

elapsed = time.time() - start
print("loop")
print(loop_time)
print("elapsed")
print(elapsed)
print("equals")
print(elapsed - loop_time)
