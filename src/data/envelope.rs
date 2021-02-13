use super::*;


#[derive(Clone, Debug)]
pub struct Envelope {
    /// Envelope Flags
    pub flags: EnvelopeFlags,

    pub loop_: EnvelopeLoop,

    pub sustain_loop: EnvelopeLoop,

    /// Envelope Node Positions / Values
    pub nodes: Vec<Node>,
}

bitflags! {
    pub struct EnvelopeFlags: u8 {
        /// Envelope on/off, 1 = on, 0 = off
        const ENABLED = 1 << 0;

        /// Loop on/off, 1 = on, 0 = off
        const LOOP = 1 << 1;

        /// SusLoop on/off, 1 = on, 0 = off
        const SUSTAIN = 1 << 2;

        // These are not mentioned in ITTECH.TXT and there are no comment in OpenMPT, documentation
        // here is just our assumption.
        //
        // We assume CARRY is for the carry button in Instrument configuration in OpenMPT.
        const CARRY = 1 << 3;
        // Filter is probably only useful on a pitch envelope and makes it act like a filter
        // envelope instead.
        const FILTER = 1 << 7;
    }
}

#[derive(Clone, Copy, Debug)]
pub struct Node {
    pub value: i8,
    pub tick: u16,
}

// TODO Is the loop an inclusive "interval"? That is, is the node marked as `end` used in the loop?
//      Our guess would be yes, but check with OpenMPT code or interface first.
#[derive(Clone, Debug)]
pub struct EnvelopeLoop {
    /// Start - offset of the node
    pub start: u8,

    /// End - offset of the node
    ///
    /// Must be always `>= start`
    pub end: u8,
}
