use std::net::SocketAddr;
use std::process::Stdio;

use axum::extract::ws::{Message, WebSocket};
use axum::extract::WebSocketUpgrade;
use axum::response::Response;
use axum::{response::Html, routing::get, Router};
use serde::{Deserialize, Serialize};
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tokio::process::Command;

#[derive(Deserialize)]
struct SocketRequest {
    tag: String,
    color: Option<String>,
    command: String,
}

#[derive(Serialize)]
struct SocketResponse {
    tag: String,
    color: Option<String>,
    body: String,
}

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/", get(handler))
        .route("/ws", get(ws_handler));

    let addr = SocketAddr::from(([127, 0, 0, 1], 3000));
    println!("listening on {}", addr);
    axum::Server::bind(&addr)
        .serve(app.into_make_service())
        .await
        .unwrap();
}

async fn handler() -> Html<&'static str> {
    Html("<h1>Hello, World!</h1>")
}

async fn ws_handler(ws: WebSocketUpgrade) -> Response {
    ws.on_upgrade(|socket| handle_socket(socket))
}

async fn handle_socket(mut ws: WebSocket) {
    let mut cmd = Command::new("gnugo")
        .args(&["--mode", "gtp"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("Failed to create process.");

    while let Some(message) = ws.recv().await {
        if let Ok(msg) = message {
            match msg {
                Message::Text(text) => {
                    let req = serde_json::from_str::<SocketRequest>(&text).unwrap();
                    let _ = cmd
                        .stdin
                        .as_mut()
                        .unwrap()
                        .write_all(format!("{}\n", req.command).as_bytes())
                        .await;

                    let mut stdout_buffer = BufReader::new(cmd.stdout.as_mut().unwrap()).lines();
                    let mut command_responses: Vec<String> = vec![];
                    while let Ok(Some(line)) = stdout_buffer.next_line().await {
                        if line.len() == 0 {
                            break;
                        }
                        command_responses.push(line);
                    }
                    let res = SocketResponse {
                        tag: req.tag,
                        color: req.color,
                        body: command_responses.join("\n"),
                    };

                    if ws
                        .send(Message::from(serde_json::to_string(&res).unwrap()))
                        .await
                        .is_err()
                    {
                        break;
                    }
                }
                Message::Close(_) => break,
                _ => { /* no-op */ }
            }
        } else {
            break;
        };
    }

    let _ = cmd.kill().await;
}
