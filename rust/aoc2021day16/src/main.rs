use std::slice::Iter;

#[derive(Debug)]
enum Content {
    Literal(usize),
    Subpackets(Vec<Packet>),
}

#[derive(Debug)]
struct Packet {
    packet_version: usize,
    packet_type: usize,
    content: Content,
}

fn apply_operator(type_id: usize, subs: &Vec<Packet>) -> usize {
    match type_id {
        0 => subs.iter().map(|p| eval_packet(p)).sum(),
        1 => subs.iter().map(|p| eval_packet(p)).product(),
        2 => subs.iter().map(|p| eval_packet(p)).min().unwrap(),
        3 => subs.iter().map(|p| eval_packet(p)).max().unwrap(),
        // 4 = literal
        5 => (eval_packet(&subs[0]) > eval_packet(&subs[1])) as usize,
        6 => (eval_packet(&subs[0]) < eval_packet(&subs[1])) as usize,
        7 => (eval_packet(&subs[0]) == eval_packet(&subs[1])) as usize,
        _ => panic!("invalid op"),
    }
}

fn eval_packet(packet: &Packet) -> usize {
    match &packet.content {
        Content::Literal(lit) => *lit,
        Content::Subpackets(subs) => apply_operator(packet.packet_type, subs),
    }
}

fn parse_subpackets(bits: &mut Iter<usize>) -> (usize, Content) {
    let length_type_id = bits.next().unwrap();
    return match length_type_id {
        0 => {
            let content_bits = parse_num(15, bits);
            let mut n = 0;
            let mut packets = vec![];
            while n < content_bits {
                let (bits_read, subpacket) = parse_packet(bits);
                packets.push(subpacket);
                n += bits_read;
            }
            if n != content_bits {
                panic!("content_bits mismatch");
            }
            (1 + 15 + content_bits, Content::Subpackets(packets))
        }
        1 => {
            let count = parse_num(11, bits);
            let mut packets = Vec::new();
            let mut n = 0;
            for _i in 0..count {
                let (bits_read, packet) = parse_packet(bits);
                packets.push(packet);
                n += bits_read
            }
            (1 + 11 + n, Content::Subpackets(packets))
        }
        _ => panic!("invalid length type id"),
    };
}

fn parse_num(n: usize, bits: &mut Iter<usize>) -> usize {
    let mut value = 0;
    let mut exp = 1;
    let literal_bits: Vec<_> = bits.take(n).collect();
    for x in literal_bits.iter().rev() {
        value += *x * exp;
        exp = exp * 2;
    }
    return value;
}

fn parse_literal_value(bits: &mut Iter<usize>) -> (usize, Content) {
    let mut literal_bits: Vec<_> = vec![];
    let mut bits_consumed = 0;
    let mut group_first_bit = 1;
    while group_first_bit == 1 {
        group_first_bit = *bits.next().unwrap();
        for _i in 0..4 {
            literal_bits.push(*bits.next().unwrap());
        }
        bits_consumed += 5;
    }
    let mut it = literal_bits.iter();
    return (
        bits_consumed,
        Content::Literal(parse_num(literal_bits.len(), &mut it)),
    );
}

fn parse_packet(bits: &mut Iter<usize>) -> (usize, Packet) {
    let version = parse_num(3, bits);
    let ptype = parse_num(3, bits);
    let (n, content) = match ptype {
        4 => parse_literal_value(bits),
        _ => parse_subpackets(bits),
    };
    return (
        3 + 3 + n,
        Packet {
            packet_version: version,
            packet_type: ptype,
            content: content,
        },
    );
}

fn hex_to_it(hex: &str) -> Vec<usize> {
    return hex
        .chars()
        .flat_map(|c| match c {
            '0' => vec![0, 0, 0, 0],
            '1' => vec![0, 0, 0, 1],
            '2' => vec![0, 0, 1, 0],
            '3' => vec![0, 0, 1, 1],
            '4' => vec![0, 1, 0, 0],
            '5' => vec![0, 1, 0, 1],
            '6' => vec![0, 1, 1, 0],
            '7' => vec![0, 1, 1, 1],
            '8' => vec![1, 0, 0, 0],
            '9' => vec![1, 0, 0, 1],
            'A' => vec![1, 0, 1, 0],
            'B' => vec![1, 0, 1, 1],
            'C' => vec![1, 1, 0, 0],
            'D' => vec![1, 1, 0, 1],
            'E' => vec![1, 1, 1, 0],
            'F' => vec![1, 1, 1, 1],
            _ => panic!("invalid hex"),
        })
        .collect();
}

fn version_sum(p: &Packet) -> usize {
    return match &p.content {
        Content::Literal(_) => 0,
        Content::Subpackets(subs) => subs.iter().map(|s| version_sum(s)).sum(),
    } + p.packet_version;
}

fn packet(hex: &str) -> Packet {
    let b = hex_to_it(hex);
    let mut it3 = b.iter();
    return parse_packet(&mut it3).1;
}

fn main() {
    let examples = vec![
        "D2FE28",
        "38006F45291200",
        "EE00D40C823060",
              "38006F45291200",
        "8A004A801A8002F478",
        "A0016C880162017C3686B18A3D4780",
        "620080001611562C8802118E34",
        "C0015000016115A2E0802F182340",
        "220D6448300428021F9EFE668D3F5FD6025165C00C602FC980B45002A40400B402548808A310028400C001B5CC00B10029C0096011C0003C55003C0028270025400C1002E4F19099F7600142C801098CD0761290021B19627C1D3007E33C4A8A640143CE85CB9D49144C134927100823275CC28D9C01234BD21F8144A6F90D1B2804F39B972B13D9D60939384FE29BA3B8803535E8DF04F33BC4AFCAFC9E4EE32600C4E2F4896CE079802D4012148DF5ACB9C8DF5ACB9CD821007874014B4ECE1A8FEF9D1BCC72A293A0E801C7C9CA36A5A9D6396F8FCC52D18E91E77DD9EB16649AA9EC9DA4F4600ACE7F90DFA30BA160066A200FC448EB05C401B8291F22A2002051D247856600949C3C73A009C8F0CA7FBCCF77F88B0000B905A3C1802B3F7990E8029375AC7DDE2DCA20C2C1004E4BE9F392D0E90073D31634C0090667FF8D9E667FF8D9F0C01693F8FE8024000844688FF0900010D8EB0923A9802903F80357100663DC2987C0008744F8B5138803739EB67223C00E4CC74BA46B0AD42C001DE8392C0B0DE4E8F660095006AA200EC198671A00010E87F08E184FCD7840289C1995749197295AC265B2BFC76811381880193C8EE36C324F95CA69C26D92364B66779D63EA071008C360098002191A637C7310062224108C3263A600A49334C19100A1A000864728BF0980010E8571EE188803D19A294477008A595A53BC841526BE313D6F88CE7E16A7AC60401A9E80273728D2CC53728D2CCD2AA2600A466A007CE680E5E79EFEB07360041A6B20D0F4C021982C966D9810993B9E9F3B1C7970C00B9577300526F52FCAB3DF87EC01296AFBC1F3BC9A6200109309240156CC41B38015796EABCB7540804B7C00B926BD6AC36B1338C4717E7D7A76378C85D8043F947C966593FD2BBBCB27710E57FDF6A686E00EC229B4C9247300528029393EC3BAA32C9F61DD51925AD9AB2B001F72B2EE464C0139580D680232FA129668"
    ];

    for ex in examples {
        println!("\nexample {:?}", ex);
        //        println!("{:?}", packet(ex));
        println!("version_sum: {:?}", version_sum(&packet(ex)));
        println!("apply_operator       : {:?}", eval_packet(&packet(ex)));
    }
}
