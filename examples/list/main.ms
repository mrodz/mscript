a = 3

const list: [int, bool, int...] = [1, true, a]
print list

my_list = [1, 2, 3]
print my_list

mixed_list = [1, "b", false]
print mixed_list

typed_list: [int...] = [1, 2, 3, 4]
print typed_list

multi_typed_list: [int, str, bool] = [1, "b", false]
print multi_typed_list

const open_typed_list: [int, str, bool, byte...] = [1, "b", false, 0b101, 0b111, 0b1000]
print open_typed_list