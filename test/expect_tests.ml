open Core
open Language_server_protocol

let%expect_test "DidCloseTextDocument" =
  let open DidCloseTextDocument in
  { jsonrpc = "2.0"
  ; method_ = "textDocument/didClose"
  ; params = Some {
        textDocument =
          { uri = "foo.txt"
          ; version = None
          }
      }
  }
  |> DidCloseTextDocument.to_yojson
  |> Yojson.Safe.pretty_to_string
  |> print_string;
  [%expect_exact {|{
  "jsonrpc": "2.0",
  "method": "textDocument/didClose",
  "params": { "textDocument": { "uri": "foo.txt" } }
}|}]
