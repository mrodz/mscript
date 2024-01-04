import CognitoUser, type CognitoFlags, VERSION from aws_cognito
        import shared

        user = CognitoUser("Mateo", "mateo@email.com")

        print VERSION

        x: CognitoFlags = 0b101
        shared.take_user(x)