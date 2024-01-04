my_list: [[int...]...] = [[10, 20], [100, 200]]

lost = [5, 4, 3]

print my_list[0][1]

print my_list[0]
my_list[0][1] = 555

print my_list[0][1]

a = my_list[0]
print 1 + a[0]