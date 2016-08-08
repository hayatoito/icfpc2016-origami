// use euclid::point::Point2D;
// use num::complex::Complex;
// type Point = Complex<BigRational>;
// pub type Point = Point2D<f64>;

mod rest {
    use std::io::prelude::*;

    use hyper;
    use hyper::header::{Headers, ContentType, AcceptEncoding, Encoding, Authorization, qitem};
    use hyper::Client;

    use hyper::mime;
    use hyper::mime::Mime;

    // https://github.com/alexcrichton/flate2-rs
    // use flate2::Compression;
    // use flate2::write::ZlibEncoder;
    use flate2::read::GzDecoder;

    header! { (XApiKey, "X-API-Key") => [String] }

    fn headers(key: &str) -> Headers {

        let mut headers = Headers::new();
        // let mime: Mime = "application/json".parse().unwrap();
        headers.set(
            AcceptEncoding(vec![
                qitem(Encoding::Gzip),
            ])
        );
        headers.set(XApiKey(key.to_string()));
        headers
    }

    pub fn hello_world() {
        // % curl --compressed -L -H Expect: -H 'X-API-Key: 84-192db86cfbc5c403a46efdbec067922f' 'http://2016sv.icfpcontest.org/api/hello'
        let endpoint = "http://2016sv.icfpcontest.org/api/hello";
        // let endpoint = "http://www.google.com";
        info!("endpoint: {}", endpoint);
        let headers = headers(key);
        let client = Client::new();
        let mut res = client.get(endpoint)
            .headers(headers)
            // .body(&message_json)
            .send()
            .unwrap();
        assert_eq!(res.status, hyper::Ok);

        let mut bytes: Vec<u8> = vec![];
        res.read(&mut bytes).unwrap();
        // server does not return?

        // info!("response (gziped): size: {}" , bytes.len());
        // let mut d = GzDecoder::new(bytes.as_slice()).unwrap();
        // let mut s = String::new();
        // d.read_to_string(&mut s).unwrap();
        // println!("response: {}", s)
    }
}
