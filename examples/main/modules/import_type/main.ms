import CognitoUser, type CognitoFlags, VERSION from aws_cognito

user = CognitoUser("Mateo", "mateo@email.com")

print VERSION

x: CognitoFlags = 0b101

