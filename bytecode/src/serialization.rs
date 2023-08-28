const TERMINATOR: u8 = 0;

use anyhow::Result;
use std::str::from_utf8;

pub struct SerializedInstructionBuilder {
    code: Option<u8>,
    content: Vec<u8>,
}

impl Drop for SerializedInstructionBuilder {
	fn drop(&mut self) {
		assert!(self.code.is_some(), "Instruction ID not specified")
	}
}

trait SerializeDeserialize: Sized {
	fn serialize(self, buffer: &mut Vec<u8>);
	fn deserialize(input: &[u8]) -> Result<(Self, usize)>;
}

impl SerializeDeserialize for &str {
	fn serialize(self, buffer: &mut Vec<u8>) -> Vec<u8> {
		let str_bytes: &[u8] = self.as_bytes();
        let len_bytes: [u8; 8] = str_bytes.len().to_be_bytes();
		
        buffer.extend_from_slice(&len_bytes);
        buffer.extend_from_slice(str_bytes);
	}

	fn deserialize(input: &[u8]) -> Result<(Self, usize)> {
		let length_bytes: &[u8] = &input[..8];
        let length_usize: usize = usize::from_be_bytes(length_bytes.try_into()?);

        let endpoint = 8 + length_usize;

        let str_bytes: &[u8] = &input[8..endpoint];
        let str_actual: &str = from_utf8(str_bytes).unwrap();

		Ok((str_actual, endpoint))
	}
}

impl SerializeDeserialize for usize {
	fn serialize(self, buffer: &mut Vec<u8>) {
		let number_bytes: [u8; 8] = self.to_be_bytes();
        buffer.extend_from_slice(&number_bytes);
	}

	fn deserialize(input: &[u8]) -> Result<(Self, usize)> {
		let usize_bytes: &[u8] = &input[..8];
        let usize_actual = usize::from_be_bytes(usize_bytes.try_into()?);
		Ok((usize_actual, 8))
	}
}

impl SerializeDeserialize for isize {
	fn serialize(self, buffer: &mut Vec<u8>) {
		let number_bytes: [u8; 8] = self.to_be_bytes();
		buffer.extend_from_slice(&number_bytes);
	}

	fn deserialize(input: &[u8]) -> Result<(Self, usize)> {
		let isize_bytes: &[u8] = &input[..8];
        let isize_actual = isize::from_be_bytes(isize_bytes.try_into()?);
		Ok((isize_actual, 8))
	}
}

impl SerializeDeserialize for bool {
	fn serialize(self, buffer: &mut Vec<u8>) {
        buffer.push(if self { 1 } else { 0 });
	}

	fn deserialize(input: &[u8]) -> Result<(Self, usize)> {
		let bool = input[0];
		Ok((bool != 0, 1))
	}
}

impl SerializeDeserialize for u8 {
	fn serialize(self, buffer: &mut Vec<u8>) {
		buffer.push(self);
	}

	fn deserialize(input: &[u8]) -> Result<(Self, usize)> {
		let byte = input[0];
        Ok((byte, 1))
	}
}

impl SerializedInstructionBuilder {
    pub fn new(code: u8) -> Self {
        Self {
            code: Some(code),
            content: vec![code],
        }
    }

    pub fn build(mut self) -> Vec<u8> {
        assert!(self.code.is_some(), "Instruction without a code");
        self.content.push(TERMINATOR);
        self.content
    }

	pub fn add<T: SerializeDeserialize>(mut self, item: T) -> Self {
		item.serialize(&mut self.content);
		self
	}

	// #[deprecated]
    // pub fn add_str(mut self, str: &str) -> Self {
    //     let str_bytes: &[u8] = str.as_bytes();
    //     let len_bytes: [u8; 8] = str_bytes.len().to_be_bytes();
    //     self.content.extend_from_slice(&len_bytes);
    //     self.content.extend_from_slice(str_bytes);
    //     self
    // }

    // pub fn add_usize(mut self, usize: usize) -> Self {
    //     let number_bytes: [u8; 8] = usize.to_be_bytes();
    //     self.content.extend_from_slice(&number_bytes);
    //     self
    // }

	// pub fn add_isize(mut self, isize: isize) -> Self {
    //     let number_bytes: [u8; 8] = isize.to_be_bytes();
    //     self.content.extend_from_slice(&number_bytes);
    //     self
    // }	

    // pub fn add_bool(mut self, bool: bool) -> Self {
    //     self.content.push(if bool { 1 } else { 0 });
    //     self
    // }
	
	// pub fn add_u8(mut self, u8: u8) -> Self {
	// 	self.content.push(u8);
	// 	self
	// }
}

pub struct InstructionDeserializationFactory<'a> {
    bytes: &'a [u8],
}

impl<'a> InstructionDeserializationFactory<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        Self { bytes: &bytes[1..] }
    }

    pub fn bytes_remaining(&self) -> usize {
        self.bytes.len()
    }

	pub fn next<T: SerializeDeserialize>(&mut self) -> Result<T> {
		let (product, bytes_read) = T::deserialize(self.bytes)?;
		self.bytes = &self.bytes[bytes_read..];
		Ok(product)
	}

    // pub fn next_str(&mut self) -> Result<&str> {
    //     let length_bytes: &[u8] = &self.bytes[..8];
    //     let length_usize: usize = usize::from_be_bytes(length_bytes.try_into()?);

    //     let endpoint = 8 + length_usize;

    //     let str_bytes: &[u8] = &self.bytes[8..endpoint];
    //     let str_actual: &str = from_utf8(str_bytes).unwrap();

    //     self.bytes = &self.bytes[endpoint..];

    //     Ok(str_actual)
    // }

    // pub fn next_usize(&mut self) -> Result<usize> {
    //     let usize_bytes: &[u8] = &self.bytes[..8];
    //     let usize_actual = usize::from_be_bytes(usize_bytes.try_into()?);

    //     self.bytes = &self.bytes[8..];

    //     Ok(usize_actual)
    // }

	// pub fn next_isize(&mut self) -> Result<isize> {
    //     let isize_bytes: &[u8] = &self.bytes[..8];
    //     let isize_actual = isize::from_be_bytes(isize_bytes.try_into()?);

    //     self.bytes = &self.bytes[8..];

    //     Ok(isize_actual)
    // }

    // pub fn next_bool(&mut self) -> Result<bool> {
    //     let bool = self.bytes[0];
    //     self.bytes = &self.bytes[1..];
    //     Ok(bool != 0)
    // }

	// pub fn next_u8(&mut self) -> Result<u8> {
	// 	let byte = self.bytes[0];
    //     self.bytes = &self.bytes[1..];
    //     Ok(byte)
	// }
}

impl Drop for InstructionDeserializationFactory<'_> {
    fn drop(&mut self) {
        assert!(matches!(self.bytes, [TERMINATOR]), "Dropping with data still in the pipe is usually a mistake");
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn supports_multiple_types() {
        let bytes = SerializedInstructionBuilder::new(1)
            .add("Cool Beans")
            .add(true)
            .add::<usize>(505)
            .add("😋😋😋")
            .add::<i32>(1000)
            .add(false)
            .build();

        let mut reader = InstructionDeserializationFactory::new(bytes.as_ref());

        assert_eq!(reader.next::<&str>().unwrap(), "Cool Beans");
        assert!(reader.next::<bool>().unwrap());
        assert_eq!(reader.next::<usize>().unwrap(), 505);
        assert_eq!(reader.next::<&str>().unwrap(), "😋😋😋");
        assert_eq!(reader.next::<i32>().unwrap(), 1000);
        assert!(!reader.next::<bool>().unwrap());
    }
}
pub fn main() -> Result<()> {
    let bytes = SerializedInstructionBuilder::new(1)
        .add_str("Cool Beans")
        .add_bool(true)
        .add_usize(505)
        .add_str("Cool")
        .add_usize(1000)
        .add_bool(false)
        .build();

    let mut reader = InstructionDeserializationFactory::new(bytes.as_ref());

    dbg!(reader.next_str()?);
    dbg!(reader.next_bool()?);
    dbg!(reader.next_usize()?);
    dbg!(reader.next_str()?);
    dbg!(reader.next_usize()?);
    dbg!(reader.next_bool()?);

    Ok(())
    // assert_eq!(reader.next_str(), "Cool");
}
