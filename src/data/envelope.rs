use bitflags::bitflags;

#[derive(Clone, Debug)]
pub struct Envelope {
    /// Envelope Flags
    pub flags: EnvelopeFlags,

    /// Loop Start
    pub loop_start: u8,

    /// Loop End
    pub loop_end: u8,

    /// Sustain Loop Start
    pub sustain_loop_start: u8,

    /// Sustain Loop End
    pub sustain_loop_end: u8,

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
        // here is just my assumption.
        //
        // I assume CARRY is for the carry button in Instrument configuration in OpenMPT.
        const CARRY = 1 << 3;
        // Filter is probably only useful on a pitch envelope and makes it act like a filter
        // envelope instead.
        const FILTER = 1 << 7;
    }
}

#[derive(Clone, Copy, Debug, Default)]
pub struct Node {
    pub value: i8,
    pub tick: u16,
}
