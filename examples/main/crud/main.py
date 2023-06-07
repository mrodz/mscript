a = 5

def increment_a():
    global a
    a = a + 5

print(a)
increment_a()
print(a)
increment_a()
print(a)
