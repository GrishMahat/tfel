use super::system::expect_string_arg;
use super::*;
use std::io::{Read, Write};
use std::net::{TcpStream, ToSocketAddrs};
use std::process::{Command, Stdio};
use std::time::Duration;

impl Evaluator {
    pub(super) fn eval_builtin_http_request(
        &mut self,
        args: Vec<Value>,
    ) -> Result<Value, EvalError> {
        if !self.options.permissions.allow_net {
            return Err(EvalError::new(
                "network access is disabled (pass --allow-net to enable)",
            ));
        }

        if args.len() < 2 || args.len() > 3 {
            return Err(EvalError::new(format!(
                "__http_request expected 2 or 3 argument(s), got {}",
                args.len()
            )));
        }

        let method = expect_string_arg(&args[0], "__http_request", 1)?;
        let url = expect_string_arg(&args[1], "__http_request", 2)?;
        let body = if args.len() == 3 {
            expect_string_arg(&args[2], "__http_request", 3)?
        } else {
            String::new()
        };

        let response = send_http_request(&method, &url, &body)?;
        Ok(Value::Array(vec![
            Value::Number(response.status as f64),
            Value::String(response.body),
        ]))
    }
}

#[derive(Debug)]
struct ParsedHttpUrl {
    scheme: String,
    host: String,
    port: u16,
    path: String,
}

#[derive(Debug)]
struct HttpResponse {
    status: u16,
    body: String,
}

fn send_http_request(method: &str, url: &str, body: &str) -> Result<HttpResponse, EvalError> {
    let method = method.trim().to_uppercase();
    if method.is_empty() || !method.chars().all(|ch| ch.is_ascii_uppercase()) {
        return Err(EvalError::new(format!(
            "unsupported HTTP method '{}'",
            method
        )));
    }

    let parsed = parse_http_url(url)?;
    match parsed.scheme.as_str() {
        "http" => send_http_plain(&method, &parsed, body),
        "https" => send_https_via_openssl(&method, &parsed, body),
        _ => Err(EvalError::new(format!(
            "unsupported URL scheme '{}'",
            parsed.scheme
        ))),
    }
}

fn parse_http_url(url: &str) -> Result<ParsedHttpUrl, EvalError> {
    let (scheme, rest) = url
        .split_once("://")
        .ok_or_else(|| EvalError::new(format!("invalid URL '{}': missing scheme", url)))?;
    let scheme = scheme.to_lowercase();
    if scheme != "http" && scheme != "https" {
        return Err(EvalError::new(format!(
            "invalid URL '{}': scheme must be http or https",
            url
        )));
    }

    let (host_port, path) = match rest.split_once('/') {
        Some((host_port, tail)) => (host_port, format!("/{}", tail)),
        None => (rest, "/".to_string()),
    };

    if host_port.is_empty() || host_port.contains('@') {
        return Err(EvalError::new(format!(
            "invalid URL '{}': unsupported authority segment",
            url
        )));
    }

    let (host, port) = match host_port.rsplit_once(':') {
        Some((host, raw_port))
            if !host.contains(']') && raw_port.chars().all(|ch| ch.is_ascii_digit()) =>
        {
            let port = raw_port.parse::<u16>().map_err(|_| {
                EvalError::new(format!("invalid port '{}' in URL '{}'", raw_port, url))
            })?;
            (host.to_string(), port)
        }
        _ => {
            let default_port = if scheme == "https" { 443 } else { 80 };
            (host_port.to_string(), default_port)
        }
    };

    if host.is_empty() {
        return Err(EvalError::new(format!(
            "invalid URL '{}': missing host",
            url
        )));
    }

    Ok(ParsedHttpUrl {
        scheme,
        host,
        port,
        path,
    })
}

fn send_http_plain(
    method: &str,
    parsed: &ParsedHttpUrl,
    body: &str,
) -> Result<HttpResponse, EvalError> {
    let address = format!("{}:{}", parsed.host, parsed.port);
    let socket_addr = address
        .to_socket_addrs()
        .map_err(|err| EvalError::new(format!("failed to resolve '{}': {}", address, err)))?
        .next()
        .ok_or_else(|| EvalError::new(format!("failed to resolve '{}': no addresses", address)))?;

    let mut stream = TcpStream::connect_timeout(&socket_addr, Duration::from_secs(10))
        .map_err(|err| EvalError::new(format!("failed to connect to '{}': {}", address, err)))?;
    stream
        .set_read_timeout(Some(Duration::from_secs(10)))
        .map_err(|err| EvalError::new(format!("failed to set read timeout: {}", err)))?;
    stream
        .set_write_timeout(Some(Duration::from_secs(10)))
        .map_err(|err| EvalError::new(format!("failed to set write timeout: {}", err)))?;

    let request = build_http_request(method, parsed, body);
    stream
        .write_all(request.as_bytes())
        .map_err(|err| EvalError::new(format!("failed to write HTTP request: {}", err)))?;
    stream
        .flush()
        .map_err(|err| EvalError::new(format!("failed to flush HTTP request: {}", err)))?;

    let mut raw = String::new();
    stream
        .read_to_string(&mut raw)
        .map_err(|err| EvalError::new(format!("failed to read HTTP response: {}", err)))?;

    parse_http_response(&raw)
}

fn send_https_via_openssl(
    method: &str,
    parsed: &ParsedHttpUrl,
    body: &str,
) -> Result<HttpResponse, EvalError> {
    let connect_target = format!("{}:{}", parsed.host, parsed.port);
    let request = build_http_request(method, parsed, body);

    let mut child = Command::new("openssl")
        .arg("s_client")
        .arg("-quiet")
        .arg("-connect")
        .arg(&connect_target)
        .arg("-servername")
        .arg(&parsed.host)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|err| {
            EvalError::new(format!(
                "failed to start openssl for HTTPS request: {} (is 'openssl' installed?)",
                err
            ))
        })?;

    {
        let stdin = child
            .stdin
            .as_mut()
            .ok_or_else(|| EvalError::new("failed to open stdin for openssl process"))?;
        stdin
            .write_all(request.as_bytes())
            .map_err(|err| EvalError::new(format!("failed to send HTTPS request: {}", err)))?;
    }

    let output = child
        .wait_with_output()
        .map_err(|err| EvalError::new(format!("failed to read HTTPS response: {}", err)))?;
    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(EvalError::new(format!(
            "openssl HTTPS request failed: {}",
            stderr.trim()
        )));
    }

    let raw = String::from_utf8(output.stdout)
        .map_err(|_| EvalError::new("HTTPS response contained non-utf8 data"))?;
    parse_http_response(&raw)
}

fn build_http_request(method: &str, parsed: &ParsedHttpUrl, body: &str) -> String {
    let host_header = if (parsed.scheme == "http" && parsed.port == 80)
        || (parsed.scheme == "https" && parsed.port == 443)
    {
        parsed.host.clone()
    } else {
        format!("{}:{}", parsed.host, parsed.port)
    };

    let mut request = format!(
        "{method} {} HTTP/1.1\r\nHost: {host_header}\r\nUser-Agent: tfel/0.1\r\nAccept: */*\r\nConnection: close\r\n",
        parsed.path
    );
    if !body.is_empty() {
        request.push_str("Content-Type: text/plain; charset=utf-8\r\n");
        request.push_str(&format!("Content-Length: {}\r\n", body.len()));
    }
    request.push_str("\r\n");
    request.push_str(body);
    request
}

fn parse_http_response(raw: &str) -> Result<HttpResponse, EvalError> {
    let trimmed = if let Some(idx) = raw.find("HTTP/") {
        &raw[idx..]
    } else {
        raw
    };

    let (header_text, body) = if let Some((h, b)) = trimmed.split_once("\r\n\r\n") {
        (h, b.to_string())
    } else if let Some((h, b)) = trimmed.split_once("\n\n") {
        (h, b.to_string())
    } else {
        return Err(EvalError::new(
            "invalid HTTP response: missing header/body separator",
        ));
    };

    let mut lines = header_text.lines();
    let status_line = lines
        .next()
        .ok_or_else(|| EvalError::new("invalid HTTP response: missing status line"))?;
    let status = status_line
        .split_whitespace()
        .nth(1)
        .ok_or_else(|| EvalError::new("invalid HTTP response: malformed status line"))?
        .parse::<u16>()
        .map_err(|_| EvalError::new("invalid HTTP response: status code is not a number"))?;

    Ok(HttpResponse { status, body })
}
