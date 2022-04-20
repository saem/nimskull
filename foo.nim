# DELETE ME!!!
# DELETE ME!!!
# DELETE ME!!!

# compile command:
# ./koch.py temp --lib:lib -u:release -u:danger -d:nimDebugUtils cpp -d:nimCompilerDebugCalltrace -r foo.nim

when false: # original from tests/iter/titervaropenarray.nim
  iterator iterAndZero(a: var openArray[int]): int =
    for i in 0..len(a)-1:
      yield a[i]
      a[i] = 0

  var x = [[1, 2, 3], [4, 5, 6]]
  for y in iterAndZero(x[0]): write(stdout, $y)
  #OUT 123

  write stdout, "\n"

nimCompilerDebugRegion:
  iterator iterAndZero(a: var openArray[int]): int =
    for i in 0..len(a)-1:
      yield a[i]
    #   a[i] = 0

  var x = [1, 2, 3]
  for y in iterAndZero(x):
    echo y
  #OUT 123

# DELETE ME!!!
# DELETE ME!!!
# DELETE ME!!!