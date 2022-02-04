#![feature(test)]

pub mod primitives;

pub mod rust;

pub mod shadowing;

pub mod enums;

#[cfg(test)]
mod tests {
    use chrono::naive::NaiveDate;
    use chrono::{Date, DateTime, Utc};

    use std::collections::HashMap;

    use theta::avro::{FromAvro, ToAvro};

    use super::enums::*;
    use super::rust::*;
    use super::primitives::*;

    use super::shadowing;

    // Primitives and Containers from primitives.theta

    #[test]
    fn test_round_trip_primitives() {
        let example = primitives::Primitives {
            bool: true,
            bytes: vec![0xC0, 0xFF, 0xEE],
            int: 37i32,
            long: 42i64,
            float: 0.05,
            double: 0.12,
            string: "blarg".to_string(),
            date: Date::from_utc(NaiveDate::from_ymd(20, 12, 23), Utc),
            datetime: DateTime::from_utc(NaiveDate::from_ymd(10, 11, 12).and_hms(5, 0, 0), Utc),
        };
        assert!(check_encoding(example));
    }

    #[test]
    fn test_round_trip_containers() {
        let mut hashmap = HashMap::new();
        hashmap.insert("key".to_string(), true);

        let mut nested_hashmap = HashMap::new();
        nested_hashmap.insert("key".to_string(), vec![Some(true), None]);

        let example = primitives::Containers {
            array: vec![true, false],
            map: hashmap,
            optional: None,
            nested: nested_hashmap,
        };
        assert!(check_encoding(example));
    }

    // Foo, Recursive and RecursiveVariant from rust.theta

    #[test]
    fn test_round_trip_foo() {
        let example = rust::Foo {
            input: 10,
            nested: vec![vec![1, 2], vec![3, 4i64]],
        };
        assert!(check_encoding(example));
    }

    #[test]
    fn test_round_trip_recursive() {
        let example_inner = rust::Recursive {
            recurse: Box::new(None),
        };
        let example = rust::Recursive {
            recurse: Box::new(Some(example_inner)),
        };
        assert!(check_encoding(example));
    }

    #[test]
    fn test_round_trip_recursive_variant() {
        let example_inner = rust::RecursiveVariant::A {
            recurse: Box::new(None),
        };
        let example_a = rust::RecursiveVariant::A {
            recurse: Box::new(Some(example_inner)),
        };
        let example_b = rust::RecursiveVariant::B {
            recurse: Box::new(Some(example_a)),
        };
        assert!(check_encoding(example_b));
    }

    #[test]
    fn test_round_trip_recursive_container() {
        let example_inner = rust::Recursive {
            recurse: Box::new(None),
        };
        let example = rust::Recursive {
            recurse: Box::new(Some(example_inner)),
        };

        let mut example_map = HashMap::new();
        example_map.insert("foo".to_string(), example.clone());

        assert!(check_encoding(vec![example.clone()]));
        assert!(check_encoding(example_map));
        assert!(check_encoding(Some(example.clone())));
    }

    #[test]
    fn test_round_trip_shadowing() {
        let shadowed_record = shadowing::primitives::Primitives {
            bool: true,
            bytes: vec![0xC0, 0xFF, 0xEE],
            int: 37i32,
            long: 42i64,
            float: 0.05,
            double: 0.12,
            string: "blarg".to_string(),
            date: Date::from_utc(NaiveDate::from_ymd(20, 12, 23), Utc),
            datetime: DateTime::from_utc(NaiveDate::from_ymd(10, 11, 12).and_hms(5, 0, 0), Utc),
        };
        let shadowing_record = shadowing::shadowing::Primitives {
            underlying: shadowed_record.clone(),
        };

        assert!(check_encoding(shadowed_record));
        assert!(check_encoding(shadowing_record));
    }

    // Enums
    #[test]
    fn test_round_trip_simple_enum() {
        assert!(check_encoding(enums::SimpleEnum::SymbolA));
        assert!(check_encoding(enums::SimpleEnum::SymbolB));
    }

    #[test]
    fn test_round_trip_tricky_enum() {
        assert!(check_encoding(enums::TrickyEnum::Sym));
        assert!(check_encoding(enums::TrickyEnum::Sym_));
        assert!(check_encoding(enums::TrickyEnum::Sym__));
    }

    #[test]
    fn test_round_trip_enum_record() {
        assert!(check_encoding(enums::EnumRecord {
            enum_field: enums::TrickyEnum::Sym,
            int_field: 1,
        }));
    }
    
    fn check_encoding<A: ToAvro + FromAvro + PartialEq>(value: A) -> bool {
        match FromAvro::from_avro(value.to_avro().as_slice()) {
            Ok(([], result)) => value == result,
            Ok((_, _)) => false, // did not consume all input
            Err(_) => false,
        }
    }
}
