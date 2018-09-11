open Core
open Language_server_protocol

let%expect_test "DidCloseTextDocument" =
  let open DidCloseTextDocumentParams in
  let params =
    { textDocument =
        { uri = "foo.txt"
        ; version = None
        }
    }
  in
  DidCloseTextDocument.create ~params
  |> DidCloseTextDocument.to_yojson
  |> Yojson.Safe.pretty_to_string
  |> print_string;
  [%expect_exact "{
  \"method\": \"textDocument/didClose\",
  \"params\": { \"textDocument\": { \"uri\": \"foo.txt\" } }
}"]
