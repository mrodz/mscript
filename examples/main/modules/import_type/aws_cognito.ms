export const VERSION: str = "v0.0.1"

export type CognitoFlags byte

print "aws_cognito library, " + VERSION

export class CognitoUser {
	name: str
	email: str

	constructor(self, name: str, email: str) {
		self.name = name
		self.email = email
	}
}

import take_user from shared

take_user(1)