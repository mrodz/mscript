MESSAGE = "Hello, "
		GREETER_INSTANCES = 0

		class Greeter {
			name: str

			constructor(self, name: str) {
				self.name = name
				GREETER_INSTANCES += 1
			}

			fn greet(self) -> str {
				return MESSAGE + self.name
			}

			fn number_of_greeters(self) -> int {
				return GREETER_INSTANCES
			}
		}

		hi_mateo = Greeter("Mateo")
		hi_scout = Greeter("Scout")

		print hi_mateo.greet() == "Hello, Mateo"
		print hi_scout.greet() == "Hello, Scout"

		print hi_mateo.number_of_greeters() == hi_scout.number_of_greeters()
		print hi_mateo.number_of_greeters() == GREETER_INSTANCES
		print GREETER_INSTANCES == 2