class AWSDynamoRecord {
	constructor(self, key: str, value: str) {
		# ...
	}
}

class AWSConfig {
	# ...
}

###
Update a DynamoDB table specified in the config with a specified key-value pair record.

[config] AWS credentials and the DynamoDB path.
[record] the key-value pair
###		
send_record_to_aws = fn(config: AWSConfig, record: AWSDynamoRecord) -> bool {
			
	# ...
			
	return true
}

const CONFIG = AWSConfig(### YOUR API KEY HERE ###)
		
send_record_to_aws(CONFIG, AWSDynamoRecord("Mateo", "Rust,Python,Java,C,C++"))