#[macro_use]
extern crate nom;

pub mod ts {

    use std;
    use std::fmt;
    use std::collections::HashMap;
    use std::str::from_utf8;

    use nom;
    use nom::*;

    #[derive(Debug)]
    pub enum Format {
        RI,
        DB,
        MA,
    }

    impl fmt::Display for Format {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let val = match self {
                &Format::RI => "RI",
                &Format::DB => "DB",
                &Format::MA => "MA",
            };
            write!(f, "{}", val)
        }
    }

    impl Default for Format {
        fn default() -> Format {
            Format::MA
        }
    }

    #[derive(Debug)]
    pub enum FreqUnits {
        Hz,
        KHz,
        MHz,
        GHz,
    }

    impl fmt::Display for FreqUnits {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let val = match self {
                &FreqUnits::Hz => "Hz",
                &FreqUnits::KHz => "KHz",
                &FreqUnits::MHz => "MHz",
                &FreqUnits::GHz => "GHz",
            };
            write!(f, "{}", val)
        }
    }

    impl Default for FreqUnits {
        fn default() -> FreqUnits {
            FreqUnits::GHz
        }
    }

    #[derive(Debug,)]
    pub enum Domain {
        Scattering,
        Transfer,
        Admittance,
        Impedance,
        HybridH,
        HybridG,
    }

    impl fmt::Display for Domain {
        fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
            let val = match self {
                &Domain::Scattering => "S",
                &Domain::Transfer => "T",
                &Domain::Admittance => "Y",
                &Domain::Impedance => "Z",
                &Domain::HybridH => "H",
                &Domain::HybridG => "G",
            };
            write!(f, "{}", val)
        }
    }

    impl Default for Domain {
        fn default() -> Domain {
            Domain::Scattering
        }
    }

    #[derive(Debug)]
    pub struct OptionLine {
        pub freq_units: FreqUnits,
        pub domain: Domain,
        pub format: Format,
        pub system_impedance: Option<f64>,
    }

    impl Default for OptionLine {
        fn default() -> OptionLine {
            OptionLine {
                freq_units: Default::default(),
                domain: Default::default(),
                format: Default::default(),
                system_impedance: Default::default()
            }
        }
    }

    #[allow(non_camel_case_types)]
    #[derive(Debug)]
    pub enum TwoPortOrder {
        TPO_12_21,
        TPO_21_12,
    }

    #[derive(Debug)]
    pub struct NoisePoint {
        pub frequency: f64,
        pub minimum_noise_db: f64,
        pub source_refl_coeff_magnitude: f64,
        pub source_refl_coeff_phase: f64,
        pub effective_noise_resistance: f64,
    }

    #[derive(Debug)]
    pub struct Touchstone2 {
        pub num_ports: i32,
        pub option_line: OptionLine,
        pub two_port_order: Option<TwoPortOrder>,
        pub reference_impedance: Option<Vec<f64>>,
        pub information: HashMap<String, String>,
        pub network_data: Vec<Box<[(f64, f64)]>>,
        pub noise_data: Option<Box<[NoisePoint]>>,
    }

    fn parse<T: std::str::FromStr>(bytes: &[u8]) -> Option<T> {
        from_utf8(bytes).ok().and_then(|s| s.trim().parse().ok())
    }

    named!(
        comment_line<()>,
        value!((), delimited!(char!('['), take_until!("\n"), char!('\n')))
    );

    named!(comment_block<()>, value!((), many0!(comment_line)));

    named!(
        version<(i32, i32)>,
        do_parse!(
            tag_no_case!("[version]") >> many0!(space) >>
            ver: map_opt!(
                separated_pair!(digit, char!('.'), digit),
                |(major, minor)| parse(major).and_then(
                    |mjr| parse(minor).map(|mnr| (mjr, mnr)))
            ) >>
            (ver)
        )
    );

    fn not_whitespace(c: u8) -> bool {
        !(
            c == ' '  as u8 ||
            c == '\t' as u8 ||
            c == '\n' as u8 ||
            c == '\r' as u8
        )
    }

    named!(
        float<f64>,
        map_opt!(take_while!(not_whitespace), parse)
    );

    named!(
        option_line<OptionLine>,
        do_parse!(
            char!('#') >>
            many1!(space) >>
            freq_units: opt!(
                alt!(
                    value!(FreqUnits::Hz, tag_no_case!("hz")) |
                    value!(FreqUnits::KHz, tag_no_case!("khz")) |
                    value!(FreqUnits::MHz, tag_no_case!("mhz")) |
                    value!(FreqUnits::GHz, tag_no_case!("ghz"))
                )
            ) >>
            domain: opt!(
                alt!(
                    value!(Domain::Scattering, tag_no_case!("s")) |
                    value!(Domain::Transfer, tag_no_case!("t")) |
                    value!(Domain::Admittance, tag_no_case!("y")) |
                    value!(Domain::Impedance, tag_no_case!("z")) |
                    value!(Domain::HybridH, tag_no_case!("h")) |
                    value!(Domain::HybridG, tag_no_case!("g"))
                )
            ) >>
            format: opt!(
                alt!(
                    value!(Format::DB, tag_no_case!("db")) |
                    value!(Format::MA, tag_no_case!("ma")) |
                    value!(Format::RI, tag_no_case!("ri"))
                )
            ) >>
            system_impedance: opt!(float) >>
            (OptionLine {
                freq_units: freq_units.unwrap_or(Default::default()),
                domain: domain.unwrap_or(Default::default()),
                format: format.unwrap_or(Default::default()),
                system_impedance: system_impedance
            })
        )
    );

    named!(
        number_of_ports<i32>,
        do_parse!(
            tag_no_case!("[number of ports]") >> many0!(space) >>
            num_ports: map_opt!(take_until!("\n"), parse) >>
            (num_ports)
        )
    );

    named!(
        two_port_order<TwoPortOrder>,
        do_parse!(
            tag_no_case!("[two-port order]") >> many0!(space) >>
            tpo: alt!(
                value!(TwoPortOrder::TPO_12_21, tag!("12_21")) |
                value!(TwoPortOrder::TPO_21_12, tag!("21_12"))
            ) >>
            (tpo)
        )
    );

    named!(
        number_of_frequencies<i32>,
        do_parse!(
            tag_no_case!("[number of frequencies]") >> many0!(space) >>
            num_freqs: map_opt!(take_until!("\n"), parse) >>
            (num_freqs)
        )
    );

    named!(
        number_of_noise_frequencies<i32>,
        do_parse!(
            tag_no_case!("[number of noise frequencies]") >> many0!(space) >>
            num_freqs: map_opt!(take_until!("\n"), parse) >>
            (num_freqs)
        )
    );

    named!(
        reference<Vec<f64>>,
        do_parse!(
            tag_no_case!("[reference]") >> many0!(multispace) >>
            refs: many1!(float) >>
            (refs)
        )
    );

    named!(
        kwarg<(String, String)>,
        do_parse!(
            char!('[') >>
            kw: map_res!(
                take_until!("]"),
                |x| from_utf8(x).map(|s| s.trim().to_string())
            ) >>
            char!(']') >>
            many0!(space) >>
            arg: map_res!(
                take_until_and_consume!("\n"),
                |x| from_utf8(x).map(|s| s.trim().to_string())
            ) >>
            (kw, "".to_string())
        )
    );

    named!(
        information<HashMap<String, String>>,
        do_parse!(
            tag_no_case!("[begin information") >> many0!(space) >> many1!(newline) >>
            vals: map!(
                many0!(kwarg),
                |v: Vec<(String, String)>| v.iter().cloned().collect()
            ) >>
            tag_no_case!("[end information]") >>
            (vals)
        )
    );

    macro_rules! comments {
        ($i:expr, $($args:tt)*) => ({sep!($i, comment_line, $($args)*)})
    }

    named!(
        header<()>,
        do_parse!(
            ver: version >>
        )
    );

    mod test {
        #[test]
        fn test_header() {
            assert_eq!(
                , );
        }
    }
}
