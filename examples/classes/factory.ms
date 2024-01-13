class Sim {
	company: str?
	cost: int?
	is_5g: bool?

	fn with_company(self, company: str) -> Self {
		self.company = company
		return self
	}

	fn with_cost(self, cost: int) -> Self {
		self.cost = cost
		return self
	}

	fn with_5g(self, is_5g: bool) -> Self {
		self.is_5g = is_5g
		return self
	}

	fn to_string(self) -> str {
		return "SIM card: the brand is " + self.company + ", it costs $" + self.cost + " per month, and has 5G: " + self.is_5g
	}
}

class Phone {
	name: str?
	memory: int?
	sim: Sim?

	fn with_name(self, name: str) -> Self {
		self.name = name
		return self
	}

	fn with_memory(self, memory: int) -> Self {
		self.memory = memory
		return self
	}

	fn with_sim(self, sim: Sim) -> Self {
		self.sim = sim
		return self
	}

	fn to_string(self) -> str {
		return "Phone {\n\tname: " + self.name + ",\n\tmemory: " + self.memory + ",\n\tsim: " + self.sim.to_string() + "\n}"
	}
}

my_smartphone = (Phone())
		.with_name("Android")
		.with_memory(128)
		.with_sim(
			(Sim()).with_company("Big Phone LTE")
					.with_cost(56)
					.with_5g(false)
		)

print my_smartphone
print my_smartphone.to_string()