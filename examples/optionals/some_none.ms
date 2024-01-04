give_name = fn(input: int) -> str? {
    if input == 42 {
        return "Mateo"
    } else {
        return nil
    }
}

x = give_name(30)

print give_name(30)
print give_name(42)
print give_name(1)
print x

# this will crash at runtime
print nil == 5