#[macro_use]
extern crate nom;

#[allow(dead_code)]
pub mod ts {

    use std;
    use std::fmt;
    use std::collections::HashMap;
    use std::str::from_utf8;

    use nom::*;

    #[derive(Debug, PartialEq)]
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

    #[derive(Debug, PartialEq)]
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

    #[derive(Debug, PartialEq)]
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

    #[derive(Debug, PartialEq)]
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
    #[derive(Debug, PartialEq)]
    pub enum TwoPortOrder {
        TPO_12_21,
        TPO_21_12,
    }

    #[derive(Debug, PartialEq)]
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

    named!(int<i32>,
        map_res!(
            map_res!(
                ws!(digit),
                from_utf8
            ),
            std::str::FromStr::from_str
        )
    );

    fn is_valid_float_char(c: u8) -> bool {
        (c >= 48u8 && c <= 57) || // is_digit
        (c == 43u8 || c == 45u8 || c == 46u8) || // + or - or .
        (c == 69u8 || c == 101u8) // E or e
    }

    named!(float<f64>,
        map_res!(
            map_res!(
                take_while!(is_valid_float_char),
                from_utf8
            ),
            std::str::FromStr::from_str
        )
    );

    named!(comment_line<()>,
        value!((), delimited!(char!('!'), take_until!("\n"), char!('\n')))
    );
    named!(comment_block<()>,
        value!((), many0!(comment_line))
    );

    named!(version<(i32, i32)>,
        do_parse!(
            tag_no_case!("[version]") >> many0!(space) >>
            ver: separated_pair!(int, char!('.'), int) >>
            opt!(complete!(comment_block)) >>
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

    named!(option_line<OptionLine>,
        do_parse!(
            char!('#') >>
            units: opt!(complete!(do_parse!(
                many1!(space) >>
                v: alt!(
                    value!(FreqUnits::Hz,  tag_no_case!("hz"))  |
                    value!(FreqUnits::KHz, tag_no_case!("khz")) |
                    value!(FreqUnits::MHz, tag_no_case!("mhz")) |
                    value!(FreqUnits::GHz, tag_no_case!("ghz"))
                ) >>
                (v)
            ))) >>
            domain: opt!(complete!(do_parse!(
                many1!(space) >>
                v: alt!(
                    value!(Domain::Scattering, tag_no_case!("s")) |
                    value!(Domain::Transfer,   tag_no_case!("t")) |
                    value!(Domain::Admittance, tag_no_case!("y")) |
                    value!(Domain::Impedance,  tag_no_case!("z")) |
                    value!(Domain::HybridH,    tag_no_case!("h")) |
                    value!(Domain::HybridG,    tag_no_case!("g"))
                ) >>
                (v)
            ))) >>
            format: opt!(complete!(do_parse!(
                many1!(space) >>
                v: alt!(
                    value!(Format::DB, tag_no_case!("db")) |
                    value!(Format::MA, tag_no_case!("ma")) |
                    value!(Format::RI, tag_no_case!("ri"))
                ) >>
                (v)
            ))) >>
            system_impedance: opt!(complete!(do_parse!(
                many1!(space) >>
                tag!("R") >>
                many1!(space) >>
                v: float >>
                (v)
            ))) >>
            opt!(complete!(comment_block)) >>
            (OptionLine {
                freq_units: units.unwrap_or(Default::default()),
                domain: domain.unwrap_or(Default::default()),
                format: format.unwrap_or(Default::default()),
                system_impedance: system_impedance
            })
        )
    );

    named!(number_of_ports<i32>,
        do_parse!(
            tag_no_case!("[number of ports]") >> many0!(space) >>
            num_ports: int >>
            opt!(complete!(comment_block)) >>
            (num_ports)
        )
    );

    named!(two_port_order<TwoPortOrder>,
        do_parse!(
            tag_no_case!("[two-port order]") >> many0!(space) >>
            tpo: alt!(
                value!(TwoPortOrder::TPO_12_21, tag!("12_21")) |
                value!(TwoPortOrder::TPO_21_12, tag!("21_12"))
            ) >>
            opt!(complete!(comment_block)) >>
            (tpo)
        )
    );

    named!(number_of_frequencies<i32>,
        do_parse!(
            tag_no_case!("[number of frequencies]") >> many0!(space) >>
            num_freqs: map_opt!(take_until!("\n"), parse) >>
            opt!(complete!(comment_block)) >>
            (num_freqs)
        )
    );

    named!(number_of_noise_frequencies<i32>,
        do_parse!(
            tag_no_case!("[number of noise frequencies]") >> many0!(space) >>
            num_freqs: map_opt!(take_until!("\n"), parse) >>
            opt!(complete!(comment_block)) >>
            (num_freqs)
        )
    );

    named!(reference<Vec<f64>>,
        do_parse!(
            tag_no_case!("[reference]") >> many0!(alt!(value!((), multispace) | comment_line)) >>
            refs: many1!(ws!(float)) >>
            opt!(complete!(comment_block)) >>
            (refs)
        )
    );

    named!(kwarg<(String, String)>,
        do_parse!(
            char!('[') >>
            kw: map_res!(
                take_until!("]"),
                |x| from_utf8(x).map(|s| s.trim().to_lowercase().to_string())
                                .or_else(|e| Err(format!("{:?}", e)))
                                .and_then(|s|
                                    if s == "end information" {
                                        Err("Reserved keyword".to_string())
                                    } else {
                                        Ok(s)
                                    }
                                )
            ) >>
            char!(']') >>
            many0!(space) >>
            arg: dbg_dmp!(map!(
                many0!(none_of!("!\n")),
                |v: Vec<char>| {
                    let s: String = v.into_iter().collect();
                    s.trim().to_string()
                }
            )) >>
            opt!(complete!(comment_block)) >>
            (kw, arg)
        )
    );

    named!(information<HashMap<String, String>>,
        ws!(delimited!(
            tag_no_case!("[begin information]"),
            map!(
                many0!(ws!(
                    alt!(
                        map!(kwarg, Some) |
                        value!(None, comment_line)
                    )
                )),
                |v: Vec<Option<(_, _)>>| v.into_iter().filter_map(|x| x).collect()
            ),
            tag_no_case!("[end information]")
        ))
    );

    #[allow(dead_code)]
    #[derive(Debug, PartialEq)]
    enum Metadata {
        TwoPortOrder(TwoPortOrder),
        NumberOfFrequencies(i32),
        NumberOfNoiseFrequencies(i32),
        Reference(Vec<f64>),
        Information(HashMap<String, String>),
    }

    named!(metadata<Vec<Metadata>>,
        map!(
            many0!(ws!(alt!(
                map!(two_port_order,              |x| Some(Metadata::TwoPortOrder(x))) |
                map!(number_of_frequencies,       |x| Some(Metadata::NumberOfFrequencies(x))) |
                map!(number_of_noise_frequencies, |x| Some(Metadata::NumberOfNoiseFrequencies(x))) |
                map!(reference,                   |x| Some(Metadata::Reference(x))) |
                map!(information,                 |x| Some(Metadata::Information(x))) |
                value!(None, comment_line)
            ))),
            |v: Vec<Option<Metadata>>| v.into_iter().filter_map(|i| i).collect::<Vec<_>>()
        )
    );

    named!(header<((i32, i32), OptionLine, i32, Vec<Metadata>)>,
        ws!(do_parse!(
            opt!(complete!(comment_block)) >>
            ver: version >>
            opt!(complete!(comment_block)) >>
            opt_line: option_line >>
            opt!(complete!(comment_block)) >>
            num_ports: number_of_ports >>
            opt!(complete!(comment_block)) >>
            metadata: metadata >>
            opt!(complete!(comment_block)) >>
            (ver, opt_line, num_ports, metadata)
        ))
    );

    #[allow(unused_imports)]
    mod test {
        use super::*;
        use nom;

        fn done<'a, T>(val: T) -> nom::IResult<&'a [u8], T> {
            nom::IResult::Done(&b""[..], val)
        }

        #[test]
        fn test_comment_line() {
            assert_eq!(comment_line(b"!test\n"), done(()));
        }

        #[test]
        fn test_version() {
            assert_eq!(version(b"[version] 2.0"), done((2, 0)));
            assert_eq!(version(b"[version] 2.0 ! test\n"), done((2, 0)));
        }

        #[test]
        fn test_option_line() {
            assert_eq!(option_line(b"#"), done(Default::default()));
            assert_eq!(option_line(b"#!/usr/bin/bash\n"), done(Default::default()));
        }

        #[test]
        fn test_number_of_ports() {
            assert_eq!(number_of_ports(b"[number of ports] 1"), done(1));
            assert_eq!(number_of_ports(b"[number of ports] 1!\n"), done(1));
        }

        #[test]
        fn test_information() {
            fn build_hashmap(kvs: Vec<(&str, &str)>) -> HashMap<String, String> {
                kvs.into_iter().map(|(k, v)| (k.to_string(), v.to_string())).collect()
            }

            assert_eq!(
                information(b"\
                    [Begin Information]\n\
                    [End Information]\
                    "
                ),
                done(HashMap::new())
            );
            assert_eq!(
                information(b"\
                    [Begin Information]\n\
                    [Manufacturer] Keysight\n\
                    [Model] E5071C\n\
                    [End Information]\
                    "
                ),
                done(build_hashmap(vec![
                    ("manufacturer", "Keysight"),
                    ("model", "E5071C")
                ]))
            );
            assert_eq!(
                information(b"\
                    [Begin Information]\n\
                    ! FooBarBaz\n\
                    [Manufacturer] Keysight\n\
                    [Model] E5071C ! Can I put a comment here?\n\
                    [End Information]\
                    "
                ),
                done(build_hashmap(vec![
                    ("manufacturer", "Keysight"),
                    ("model", "E5071C")
                ]))
            );
            assert_eq!(
                information(b"\
                    [Begin Information]\n\
                    ! FooBarBaz\n\
                    [Manufacturer] Keysight\n\
                    [Model] E5071C ! Can I put a comment here?\n\
                    [End Info]\n\
                    [Begin Information]  ! I'm just mean to myself\n\
                    [End Information]\
                    "
                ),
                done(build_hashmap(vec![
                    ("manufacturer", "Keysight"),
                    ("model", "E5071C"),
                    ("end info", ""),
                    ("begin information", "")
                ]))
            );
        }

        #[test]
        fn test_header() {
            assert_eq!(
                header(b"\
                    [version] 2.0\n\
                    # Hz DB R 25\n\
                    [number of ports] 4\n\
                    [number of frequencies] 20\n\
                    [two-port order] 12_21\n\
                    [Number of noise frequenCIES] 1\n\
                    [Reference]\n\
                    25.0 50 5.0e1 100\n\
                "),
                done((
                    (2, 0),
                    OptionLine {
                        freq_units: FreqUnits::Hz,
                        domain: Domain::Scattering,
                        format: Format::DB,
                        system_impedance: Some(25.0),
                    },
                    4,
                    vec![
                        Metadata::NumberOfFrequencies(20),
                        Metadata::TwoPortOrder(TwoPortOrder::TPO_12_21),
                        Metadata::NumberOfNoiseFrequencies(1),
                        Metadata::Reference(vec![25.0, 50.0, 50.0, 100.0])
                    ]
                ))
            );
            assert_eq!(
                header(b"\
                    !\n\
                    [version] 2.0\n\
                    # Hz DB R 25!foobar\n\
                    [number of ports] 4! = 24\n\
                    ! test\n\
                    [number of frequencies] 20\n\
                    ! test\n\
                    [two-port order] 12_21\n\
                    ! test\n\
                    [Number of noise frequenCIES] 1\n\
                    ! test\n\
                    [Reference]\n\
                    ! This one is just mean\n\
                    25.0 50 5.0e1 100\n\
                    ! test\n\
                "),
                done((
                    (2, 0),
                    OptionLine {
                        freq_units: FreqUnits::Hz,
                        domain: Domain::Scattering,
                        format: Format::DB,
                        system_impedance: Some(25.0),
                    },
                    4,
                    vec![
                        Metadata::NumberOfFrequencies(20),
                        Metadata::TwoPortOrder(TwoPortOrder::TPO_12_21),
                        Metadata::NumberOfNoiseFrequencies(1),
                        Metadata::Reference(vec![25.0, 50.0, 50.0, 100.0])
                    ]
                ))
            );
            assert_eq!(
                header(b"\
                    ! An example Touchstone header\n\
                    [version] 2.0\n\
                    # MHz T RI R 50.0\n\
                    ! Technically you can't have more than 100,\n\
                    ! but this is just a test\n\
                    [Number Of Ports] 1024\n\
                    [number of frequencies] 2000\n\
                    [number of noise frequencies] 2000\n\
                    [Begin Information]\n\
                    [Manufacturer] Keysight\n\
                    [Model] E5071C\n\
                    [End Information]\n\
                    ! Will it blend?\n\
                "),
                done((
                    (2, 0),
                    OptionLine {
                        freq_units: FreqUnits::MHz,
                        domain: Domain::Transfer,
                        format: Format::RI,
                        system_impedance: Some(50.0),
                    },
                    1024,
                    vec![
                        Metadata::NumberOfFrequencies(2000),
                        Metadata::NumberOfNoiseFrequencies(2000),
                        Metadata::Information(
                            vec![
                                ("model", "E5071C"),
                                ("manufacturer", "Keysight")
                            ].into_iter().map(|(k, v)|
                                (k.to_string(), v.to_string())
                            ).collect::<HashMap<_, _>>())
                    ])
                )
            );
        }
    }
}
