use crate::execute::Actor;

pub struct OutboxMessages<'a> {
    outbox: &'a [u8],
}

impl<'a> Iterator for OutboxMessages<'a> {
    type Item = &'a [u8];

    fn next(&mut self) -> Option<&'a [u8]> {
        if self.outbox.len() == 0 {
            return None;
        }
        debug_assert!(self.outbox.len() >= 2);

        let length = u16::from_be_bytes([self.outbox[0], self.outbox[1]]);
        if length == 0 {
            return None;
        }

        let end = length as usize + 2;
        let result = &self.outbox[2..end];
        self.outbox = &self.outbox[end..];

        Some(result)
    }
}

pub fn read_outbox<'a>(actor: &'a Actor<'a>) -> OutboxMessages<'a> {
    OutboxMessages {outbox: actor.outbox}
}

pub fn write_outbox_message(dest: &mut [u8], message: &[u8]) -> Result<(), &'static str> {
    if message.len() > u16::MAX as usize {
        return Err("message length is too long");
    }
    if message.len() == 0 {
        // A length header of 0 is already used as the sentinel for end-of-outbox.
        return Err("can't write empty message");
    }
    if dest.len() < message.len() + 2 {
        return Err("not enough room in outbox for message");
    }
    let len_bytes = (message.len() as u16).to_be_bytes();
    dest[0..2].copy_from_slice(&len_bytes);
    dest[2..message.len() + 2].copy_from_slice(message);
    Ok(())
}

pub fn write_outbox_message_tagged(dest: &mut [u8], tag: u8, message: &[u8])
-> Result<(), &'static str> {
    if message.len() > u16::MAX as usize {
        return Err("message length is too long");
    }
    if dest.len() < message.len() + 3 {
        return Err("not enough room in outbox for message");
    }
    let len_bytes = (1 + message.len() as u16).to_be_bytes();
    dest[0..2].copy_from_slice(&len_bytes);
    dest[2] = tag;
    dest[3..message.len() + 3].copy_from_slice(message);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_outbox() {
        let mut r = OutboxMessages {outbox: &[]};
        assert_eq!(r.next(), None);
        assert_eq!(r.next(), None);
    }

    #[test]
    fn empty_message() {
        let outbox = &mut [0, 0][..];
        write_outbox_message(outbox, &[]).unwrap_err();
        assert_eq!(outbox, &[0, 0][..]);

        let mut r = OutboxMessages {outbox};
        assert_eq!(r.next(), None);
        assert_eq!(r.next(), None);
    }

    #[test]
    fn one_message() {
        let outbox = &mut [0; 5][..];
        write_outbox_message(outbox, &[1, 2, 3]).unwrap();
        assert_eq!(outbox, &[0, 3, 1, 2, 3]);

        let mut r = OutboxMessages {outbox};
        assert_eq!(r.next(), Some(&[1, 2, 3][..]));
        assert_eq!(r.next(), None);
        assert_eq!(r.next(), None);
    }

    #[test]
    fn two_messages() {
        let outbox = &mut [0; 9][..];
        write_outbox_message(outbox, &[1, 2, 3]).unwrap();
        write_outbox_message(&mut outbox[5..], &[5, 6]).unwrap();
        assert_eq!(outbox, &[0, 3, 1, 2, 3, 0, 2, 5, 6]);

        let mut r = OutboxMessages {outbox};
        assert_eq!(r.next(), Some(&[1, 2, 3][..]));
        assert_eq!(r.next(), Some(&[5, 6][..]));
        assert_eq!(r.next(), None);
        assert_eq!(r.next(), None);
    }

    #[test]
    fn one_message_actor() {
        let actor = Actor {
            registers: &mut [],
            instructions: &[],
            constants: &[],
            outbox: &mut [0; 5],
        };

        write_outbox_message(actor.outbox, &[1, 2, 3]).unwrap();
        assert_eq!(actor.outbox, &[0, 3, 1, 2, 3]);

        let mut r = read_outbox(&actor);
        assert_eq!(r.next(), Some(&[1, 2, 3][..]));
        assert_eq!(r.next(), None);
        assert_eq!(r.next(), None);
    }

    #[test]
    fn tagged_messages() {
        let actor = Actor {
            registers: &mut [],
            instructions: &[],
            constants: &[],
            outbox: &mut [0; 20],
        };

        write_outbox_message_tagged(actor.outbox, 99, &[1, 2, 3]).unwrap();
        write_outbox_message_tagged(&mut actor.outbox[6..], 111, &[5, 7, 6, 7]).unwrap();
        assert_eq!(&actor.outbox[..13], &[0, 4, 99, 1, 2, 3, 0, 5, 111, 5, 7, 6, 7]);

        let mut r = read_outbox(&actor);
        assert_eq!(r.next(), Some(&[99, 1, 2, 3][..]));
        assert_eq!(r.next(), Some(&[111, 5, 7, 6, 7][..]));
        assert_eq!(r.next(), None);
        assert_eq!(r.next(), None);
    }
}
