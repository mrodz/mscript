ages = map[str, int] {
	"Mateo": 17,
	"Dylan": 16,
	"Scout": 7,
	"Daniel": 55,
}

assert ages.len() == 4

ages["Mateoo"] = 200

assert ages.len() == 5

ages["Mateo"] = 16

assert ages.len() == 5
assert ages["Mateo"] == 16		

assert ages.contains_key("Mateo")
assert !ages.contains_key("Davey")

assert ages.replace("Davey", 16) == nil
assert ages["Davey"] == 16

assert ages["DDDD"] == nil
assert !ages.contains_key("DDDD")

assert ages.replace("Mateo", 18) == 16
assert ages["Mateo"] == 18

pairs = ages.pairs()

special_sum = 0

print ages
