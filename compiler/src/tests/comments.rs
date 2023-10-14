use crate::eval;

#[test]
fn grammar() {
	eval(r####"
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
	"####).unwrap();
}

#[test]
fn no_die() {
	eval(r#"
		###
		print "Hello!"
		assert false
		###
	
		print "World!"
	"#).unwrap();
}

#[test]
fn block_comment_mid_assignment() {
	eval(r#"
		meaning_of_life = ### "To be happy and live righteously" ### 42
	
		assert meaning_of_life == 42
	"#).unwrap();
}

#[test]
fn block_comment_mid_string() {
	eval(r####"
		const LOREM_IPSUM = "###Lorem Ipsum### is a filler text that is used to fill the space between the elements of a web page or a document."
		
		assert LOREM_IPSUM[0] == "#"
		assert LOREM_IPSUM[14] == "#"
		assert LOREM_IPSUM == "###Lorem Ipsum### is a filler text that is used to fill the space between the elements of a web page or a document."
	"####).unwrap();
}

#[test]
fn save_the_world() {
	eval(r#"

		class DeathStar {
			fn build_lazar(self) -> Self {
				return self
			}

			fn generate_power(self) -> Self {
				return self
			}

			fn charge_lazar(self) -> Self {
				return self
			}

			fn destroy_alderaan(self) {
				print "BOOOM!!!!!"
				assert false
			}
		}

		const death_star: DeathStar = DeathStar()

		# Phew! Close call. Good Job Luke ;)
		death_star.build_lazar().generate_power().charge_lazar() #.destroy_alderaan()
	"#).unwrap();
}