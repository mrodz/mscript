type Miles int
type Kilometers float

const marathon: Miles = 27
const in_km = (marathon * 1.609).ipart()

const expected: Kilometers = 43.0

assert in_km == expected