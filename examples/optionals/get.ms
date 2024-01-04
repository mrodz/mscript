empty: int? = nil

a = get empty or 5

print a

print get empty or 5

print get nil or 5

idk: int = get nil or 5

print idk

give_five = fn() -> int? {
	return nil
}

print (give_five()) or 10