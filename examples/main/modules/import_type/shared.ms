import type CognitoFlags from aws_cognito

        export take_user: fn(CognitoFlags) = fn(input: CognitoFlags) {
	        print input
        }