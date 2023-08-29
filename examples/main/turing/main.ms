a = true

change_a = fn(new_val: bool) {
    modify a = new_val
}

print a
change_a(false)
print a
a = true
print a
change_a(true)
print a