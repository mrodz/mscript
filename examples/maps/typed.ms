class EnvironmentVariables {
	data: map[str, str]
	constructor(self) {
		self.data = map[str, str]
	}
	fn record(self, key: str, value: str) {
		(self.data)[key] = value
	}
	fn get_value(self, key: str) -> str? {
		return (self.data)[key]
	}
	fn len(self) -> int {
		return data.len()
	}
}

env = EnvironmentVariables()

env.record("CLIENT_ID", "aaaa561Dw60/dWnms01-#wpaBPerj")

assert env.len() == 1