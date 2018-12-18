(** Copyright (c) 2016-present, Facebook, Inc.
    Modified work Copyright (c) 2018 Rijnard van Tonder

    This source code is licensed under the MIT license found in the
    LICENSE file in the root directory of this source tree. *)

(** Types for Basic JSON structures. *)

(** The Parameters module represents serializing and deserializing an OCaml type
    [t] to a "any" JSON data used in LSP parameters *)
module type Any = sig
  type t
  val to_yojson : t -> Yojson.Safe.json
  val of_yojson : Yojson.Safe.json -> (t, string) result
end

module type Method = sig
  val name : string
end

(** The Null module represents a `Null type of Any *)
module Null : Any = struct
  type t = unit
  let to_yojson _ = `Null
  let of_yojson = function
    | `Null -> Ok ()
    | json -> Error (Format.asprintf "Invalid JSON %s" (Yojson.Safe.to_string json))
end

module Void : Any = struct
  type t = unit
  let to_yojson _ = `Null
  let of_yojson _ = Ok ()
end

module Experimental = Void


(** {1 Base Protocol JSON structures}

    Types for protocol messages. There are three kinds of messages: A Request,
    Response, and a Notification. *)

module Message = struct
  let create ~json =
    let json_string =
      Yojson.Safe.Util.to_assoc json
      |> List.cons ("jsonrpc", `String "2.0")
      |> (fun json -> `Assoc json)
      |> Yojson.Safe.to_string
    in
    let length = String.length json_string in
    Format.sprintf
      "Content-Length: %d\r\n\
       \r\n\
       %s"
      length
      json_string
end


(** {2 Request Message} *)

module RequestMessage = struct
  module type S = sig
    type t =
      { id: int
      ; method_: string
      ; params: parameters option
      }
    and parameters
    [@@deriving yojson]

    val create : id:int -> params:parameters -> t
  end

  module Make (Method : Method) (Parameters: Any): S
    with type parameters = Parameters.t = struct
    type t =
      { id: int
            [@key "id"]
      ; method_: string
            [@key "method"]
      ; params: parameters option
            [@key "params"]
            [@default None]
      }
    and parameters = Parameters.t
    [@@deriving yojson]

    let create ~id ~params =
      { id
      ; method_ = Method.name
      ; params = Some params
      }
  end
end


module ResponseError = struct
  module type S = sig
    type t =
      { code: int
      ; message: string
      ; data: data option
      }
    and data
    [@@deriving yojson]
  end

  module Make (AnyData: Any): S with type data = AnyData.t = struct
    type t =
      { code: int
            [@key "code"]
      ; message: string
            [@key "message"]
      ; data: data option
            [@key "data"]
            [@default None]
      }
    and data = AnyData.t
    [@@deriving yojson]
  end
end


(** {2 Response Message} *)

module ResponseMessage = struct
  module type S = sig
    type t =
      { jsonrpc: string
      ; id: int
      ; result: result option
      ; error: error option
      }
    and result
    and error
    [@@deriving yojson]
  end

  (** A response message is parameterized by a result which can be [Any] option,
      and a response error which can contain an [Any] option data *)
  module Make (AnyResult: Any) (ResponseError: ResponseError.S): S
    with type result = AnyResult.t = struct
    type t =
      { jsonrpc: string
            [@key "jsonrpc"]
      ; id: int
            [@key "id"]
      ; result: result option
            [@key "result"]
            [@default None]
      ; error: error option
            [@key "error"]
            [@default None]
      }
    and result = AnyResult.t
    and error = ResponseError.t
    [@@deriving yojson]
  end
end


(** {2 Notification Message} *)

module NotificationMessage = struct
  module type S = sig
    type t =
      { method_: string
      ; params: parameters option
      }
    and parameters
    [@@deriving yojson]

    val create : params:parameters -> t
  end

  module Make (Method : Method) (Parameters : Any) : S
    with type parameters = Parameters.t = struct
    type t =
      { method_: string
            [@key "method"]
      ; params: parameters option
            [@key "params"]
            [@default None]
      }
    and parameters = Parameters.t
    [@@deriving yojson]

    let create ~params =
      {  method_ = Method.name
      ; params = Some params
      }
  end
end


(** {1 Language Server Protocol} *)

(** {2 Basic JSON Structures} *)

module DocumentUri = struct
  type t = string
  [@@deriving yojson]
end


module Position = struct
  type t =
    { line: int
          [@key "line"]
    ; character: int
          [@key "character"]
    }
  [@@deriving yojson]
end


module Range = struct
  type t =
    { start: Position.t
          [@key "start"]
    ; end_: Position.t
          [@key "end"]
    }
  [@@deriving yojson]
end


module Location = struct
  type t =
    { uri: DocumentUri.t
          [@key "uri"]
    ; range: Range.t
          [@key "range"]
    }
  [@@deriving yojson]
end


module WorkspaceFolder = struct
  type t =
    { uri: string
          [@key "uri"]
    ; name: string
          [@key "name"]
    }
  [@@deriving yojson]
end


module DiagnosticSeverity = struct
  type t =
    | Error
    | Warning
    | Information
    | Hint

  let to_yojson : t -> Yojson.Safe.json = function
    | Error -> `Int 1
    | Warning -> `Int 2
    | Information -> `Int 3
    | Hint -> `Int 4

  let of_yojson : Yojson.Safe.json -> (t, string) Result.result = function
    | `Int 1 -> Result.Ok Error
    | `Int 2 -> Result.Ok Warning
    | `Int 3 -> Result.Ok Information
    | `Int 4 -> Result.Ok Hint
    | _ -> Result.Error "Invalid JSON"
end


module Diagnostic = struct
  type t =
    { range: Range.t
          [@key "range"]
    ; severity: DiagnosticSeverity.t option
          [@key "severity"]
          [@default None]
    ; code: int option
          [@key "code"]
          [@default None]
    ; source: string option
          [@key "source"]
          [@default None]
    ; message: string
          [@key "message"]
    }
  [@@deriving yojson]
end


module TextDocumentIdentifier = struct
  type t =
    { uri: DocumentUri.t
    ; version: int option [@default None]
    }
  [@@deriving yojson]
end


module TextDocumentItem = struct
  type t =
    { uri: DocumentUri.t
    ; languageId: string
    ; version: int
    ; text: string
    }
  [@@deriving yojson]
end


module VersionedTextDocumentIdentifier = struct
  type t =
    { uri: DocumentUri.t
    ; version: int
    }
  [@@deriving yojson]
end


module TextDocumentPositionParams = struct
  type t =
    { textDocument: TextDocumentIdentifier.t
    ; position: Position.t
    }
  [@@deriving yojson]
end


module CodeActionParams = struct

  module CodeActionKind = struct
    type t =
      | QuickFix
      | Refactor
      | RefactorExtract
      | RefactorInline
      | RefactorRewrite
      | Source
      | SourceOrganizeImports

    let to_yojson : t -> Yojson.Safe.json = function
      | QuickFix -> `String "quickfix"
      | Refactor -> `String "refactor"
      | RefactorExtract -> `String "refactor.extract"
      | RefactorInline -> `String "refactor.inline"
      | RefactorRewrite -> `String "refactor.rewrite"
      | Source -> `String "source"
      | SourceOrganizeImports -> `String "source.organizeImports"

    let of_yojson : Yojson.Safe.json -> (t, string) Result.result = function
      | `String "quickfix" -> Ok QuickFix
      | `String "refactor" -> Ok Refactor
      | `String "refactor.extract" -> Ok RefactorExtract
      | `String "refactor.inline" -> Ok RefactorInline
      | `String "refactor.rewrite" -> Ok RefactorRewrite
      | `String "source" -> Ok Source
      | `String "source.organizeImports" -> Ok SourceOrganizeImports
      | _ -> Result.Error "Invalid JSON"

  end

  module CodeActionContext = struct
    type t =
      { diagnostics: Diagnostic.t list
            [@key "diagnostics"]
      ; only: CodeActionKind.t list option
            [@key "only"]
            [@default None]
      }
    [@@deriving yojson]
  end

  type t =
    { textDocument: TextDocumentIdentifier.t
    ; range: Range.t
    ; context: CodeActionContext.t
    }
  [@@deriving yojson]
end


module DidCloseTextDocumentParams = struct
  type t =
    { textDocument: TextDocumentIdentifier.t }
  [@@deriving yojson]
end


module DidSaveTextDocumentParams = struct
  type t =
    { textDocument: TextDocumentIdentifier.t
    ; text: string option [@default None]
    }
  [@@deriving yojson]
end


module DidOpenTextDocumentParams = struct
  type t = { textDocument: TextDocumentItem.t }
  [@@deriving yojson]
end


module ShowMessageParams = struct
  type messageType =
    | ErrorMessage
    | WarningMessage
    | InfoMessage
    | LogMessage


  let messageTypeNumber = function
    | ErrorMessage -> 1
    | WarningMessage -> 2
    | InfoMessage -> 3
    | LogMessage -> 4


  type t =
    { messageType: int [@key "type"]
    ; message: string
    }
  [@@deriving yojson]
end


module SaveOptions = struct
  type t =
    { include_text: bool option
          [@key "includeText"]
          [@default None]
    }
  [@@deriving yojson]
end


module TextDocumentSyncOptions = struct
  module Kind = struct
    type t =
      | None_
      | Full
      | Incremental


    let get_change = function
      | None_ -> 0
      | Full -> 1
      | Incremental -> 2
  end


  type t =
    { open_close: bool option
          [@key "openClose"]
          [@default None]
    ; change: int option
          [@key "change"]
          [@default None]
    ; will_save: bool option
          [@key "willSave"]
          [@default None]
    ; will_save_wait_until: bool option
          [@key "willSaveWaitUntil"]
          [@default None]
    ; save: SaveOptions.t option
          [@key "save"]
          [@default None]
    }
  [@@deriving yojson]
end


module CompletionOptions = struct
  type t =
    { resolve_provider: bool option
          [@key "resolveProvider"]
          [@default None]
    ; trigger_characters: string list option
          [@key "triggerCharacters"]
          [@default None];
    }
  [@@deriving yojson]
end


module SignatureHelpOptions = struct
  type t =
    { trigger_characters: string list option
          [@key "triggerCharacters"]
          [@default None]
    }
  [@@deriving yojson]
end


module CodeLensOptions = struct
  type t =
    { resolve_provider: bool option
          [@key "resolveProvider"]
          [@default None]
    }
  [@@deriving yojson]
end


module DocumentOnTypeFormattingOptions = struct
  type t =
    { first_trigger_character: string
          [@key "firstTriggerCharacter"]
    ; more_trigger_character: string list option
          [@key "moreTriggerCharacter"]
          [@default None]
    }
  [@@deriving yojson]
end


module DocumentLinkOptions = struct
  type t =
    { resolve_provider: bool option
          [@key "resolveProvider"]
          [@default None]
    }
  [@@deriving yojson]
end


module ExecuteCommandOptions = struct
  type t =
    { commands: string list
          [@key "commands"]
    }
  [@@deriving yojson]
end


module ServerCapabilities = struct
  module type S = sig
    type t =
      { text_document_sync: TextDocumentSyncOptions.t option
      ; hover_provider: bool option
      ; completion_provider: CompletionOptions.t option
      ; signature_help_provider: SignatureHelpOptions.t option
      ; definition_provider: bool option
      ; references_provider: bool option
      ; document_highlight_provider: bool option
      ; document_symbol_provider: bool option
      ; workspace_symbol_provider: bool option
      ; code_action_provider: bool option
      ; code_lens_provider: CodeLensOptions.t option
      ; document_formatting_provider: bool option
      ; document_range_formatting_provider: bool option
      ; document_on_type_formatting_provider: DocumentOnTypeFormattingOptions.t option
      ; rename_provider: bool option
      ; document_link_provider: DocumentLinkOptions.t option
      ; execute_command_provider: ExecuteCommandOptions.t option
      ; experimental: experimental option
      }
    and experimental
    [@@deriving yojson]
  end

  module Make (AnyExperimental: Any): S
    with type experimental = AnyExperimental.t = struct
    type t =
      { text_document_sync: TextDocumentSyncOptions.t option
            [@key "textDocumentSync"]
            [@default None]
      ; hover_provider: bool option
            [@key "hoverProvider"]
            [@default None]
      ; completion_provider: CompletionOptions.t option
            [@key "completionProvider"]
            [@default None]
      ; signature_help_provider: SignatureHelpOptions.t option
            [@key "signatureHelpProvider"]
            [@default None]
      ; definition_provider: bool option
            [@key "definitionProvider"]
            [@default None]
      ; references_provider: bool option
            [@key "referencesProvider"]
            [@default None]
      ; document_highlight_provider: bool option
            [@key "documentHighlightProvider"]
            [@default None]
      ; document_symbol_provider: bool option
            [@key "documentSymbolProvider"]
            [@default None]
      ; workspace_symbol_provider: bool option
            [@key "workspaceSymbolProvider"]
            [@default None]
      ; code_action_provider: bool option
            [@key "codeActionProvider"]
            [@default None]
      ; code_lens_provider: CodeLensOptions.t option
            [@key "codeLensProvider"]
            [@default None]
      ; document_formatting_provider: bool option
            [@key "documentFormattingProvider"]
            [@default None]
      ; document_range_formatting_provider: bool option
            [@key "documentRangeFormattingProvider"]
            [@default None]
      ; document_on_type_formatting_provider: DocumentOnTypeFormattingOptions.t option
            [@key "documentOnTypeFormattingProvider"]
            [@default None]
      ; rename_provider: bool option
            [@key "renameProvider"]
            [@default None]
      ; document_link_provider: DocumentLinkOptions.t option
            [@key "documentLinkProvider"]
            [@default None]
      ; execute_command_provider: ExecuteCommandOptions.t option
            [@key "executeCommandProvider"]
            [@default None]
      ; experimental: experimental option
            [@key "experimental"]
            [@default None]
      }
    and experimental = AnyExperimental.t
    [@@deriving yojson]
  end
end


module ClientCapabilities = struct
  module DynamicRegistration = struct
    type t =
      { dynamic_registration: bool option
            [@key "dynamicRegistration"]
            [@default None]
      }
    [@@deriving yojson { strict = false }]
  end

  module WorkspaceClientCapabilities = struct
    type t =
      { apply_edit: bool option
            [@key "applyEdit"]
            [@default None]
      ; workspace_edit: DynamicRegistration.t option
            [@key "workspaceEdit"]
            [@default None]
      ; did_change_configuration: DynamicRegistration.t option
            [@key "didChangeConfiguration"]
            [@default None]
      ; did_change_watched_files: DynamicRegistration.t option
            [@key "didChangeWatchedFiles"]
            [@default None]
      ; symbol: DynamicRegistration.t option
            [@key "symbol"]
            [@default None]
      ; execute_command: DynamicRegistration.t option
            [@key "executeCommand"]
            [@default None]
      ; workspace_folders: bool option
            [@key "workspaceFolders"]
            [@default None]
      ; configuration: bool option
            [@key "configuration"]
            [@default None]
      }
    (* strict is false: Nuclide LSP sends nonstandard fields *)
    [@@deriving yojson { strict = false }]
  end

  module CompletionItemKind = struct
    type t =
      { value_set: int list option
            [@key "valueSet"]
            [@default None]
      }
    [@@deriving yojson]
  end

  module TextDocumentClientCapabilities = struct
    type synchronization =
      { dynamic_registration: bool option
            [@key "dynamicRegistration"]
            [@default None]
      ; will_save: bool option
            [@key "willSave"]
            [@default None]
      ; will_save_wait_until: bool option
            [@key "willSaveWaitUntil"]
            [@default None]
      ; did_save: bool option
            [@key "didSave"]
            [@default None]
      }
    [@@deriving yojson]

    type completion_item =
      { snippet_support: bool option
            [@key "snippetSupport"]
            [@default None]
      ; commit_characters_support: bool option
            [@key "commitCharactersSupport"]
            [@default None]
      }
    [@@deriving yojson { strict = false }]

    type completion =
      { dynamic_registration: bool option
            [@key "dynamicRegistration"]
            [@default None]
      ; completion_item: completion_item option
            [@key "completionItem"]
            [@default None]
      ; completion_item_kind: CompletionItemKind.t option
            [@key "completionItemKind"]
            [@default None]
      ; context_support: bool option
            [@key "contextSupport"]
            [@default None]
      }
    [@@deriving yojson]

    type t =
      { synchronization: synchronization option
            [@key "synchronization"]
            [@default None]
      ; completion: completion option
            [@key "completion"]
            [@default None]
      ; hover: DynamicRegistration.t option
            [@key "hover"]
            [@default None]
      ; signature_help: DynamicRegistration.t option
            [@key "signatureHelp"]
            [@default None]
      ; references: DynamicRegistration.t option
            [@key "references"]
            [@default None]
      ; document_highlight: DynamicRegistration.t option
            [@default None]
            [@key "documentHighlight"]
      ; document_symbol: DynamicRegistration.t option
            [@key "documentSymbol"]
            [@default None]
      ; formatting: DynamicRegistration.t option
            [@key "formatting"]
            [@default None]
      ; range_formatting: DynamicRegistration.t option
            [@key "rangeFormatting"]
            [@default None]
      ; on_type_formatting: DynamicRegistration.t option
            [@key "onTypeFormatting"]
            [@default None]
      ; definition: DynamicRegistration.t option
            [@key "definition"]
            [@default None]
      ; code_action: DynamicRegistration.t option
            [@key "codeAction"]
            [@default None]
      ; code_lens: DynamicRegistration.t option
            [@key "codeLens"]
            [@default None]
      ; document_link: DynamicRegistration.t option
            [@key "documentLink"]
            [@default None]
      ; rename: DynamicRegistration.t option
            [@key "rename"]
            [@default None]
      ; type_definition: DynamicRegistration.t option
            [@key "typeDefinition"]
            [@default None]
      ; implementation: DynamicRegistration.t option
            [@key "implementation"]
            [@default None]
      ; color_provider: DynamicRegistration.t option
            [@key "colorProvider"]
            [@default None]
      }
    [@@deriving yojson]
  end

  module WindowClientCapabilities = struct
    type t =
      { progress: DynamicRegistration.t option
            [@key "progress"]
            [@default None]
      ; action_required: DynamicRegistration.t option
            [@key "actionRequired"]
            [@default None]
      ; status: DynamicRegistration.t option
            [@key "status"]
            [@default None]
      }
    [@@deriving yojson]
  end

  module type S = sig
    type t =
      { workspace: WorkspaceClientCapabilities.t option
      ; text_document: TextDocumentClientCapabilities.t option
      ; experimental: experimental option
      ; window: WindowClientCapabilities.t option
      }
    and experimental
    [@@deriving yojson]
  end

  module Make (AnyExperimental: Any): S
    with type experimental = AnyExperimental.t = struct
    type t =
      { workspace: WorkspaceClientCapabilities.t option
            [@key "workspace"]
            [@default None]
      ; text_document: TextDocumentClientCapabilities.t option
            [@key "textDocument"]
            [@default None]
      ; experimental: experimental option
            [@key "experimental"]
            [@default None]
      ; window: WindowClientCapabilities.t option
            [@key "window"]
            [@default None]
      }
    and experimental = AnyExperimental.t
    [@@deriving yojson]
  end
end

(** {2 Requests} *)

(** {3 Initialize Request *)
module InitializeRequest = struct

  module InitializeParams = struct
    module ClientCapabilities = ClientCapabilities.Make(Experimental)

    module TraceSetting = struct
      type t =
        | Off
        | Messages
        | Verbose

      let of_yojson = function
        | `String "off" -> Ok Off
        | `String "messages" -> Ok Messages
        | `String "verbose" -> Ok Verbose
        | json ->
          Error ("JSON trace setting unsupported: " ^ Yojson.Safe.to_string json)

      let to_yojson = function
        | Off -> `String "off"
        | Messages -> `String "messages"
        | Verbose -> `String "verbose"
    end

    (* Example: [rootPath] is not required so we set a @default attribute for
       yojson deriving. This means if [rootPath] is not in the JSON, it maps to
       None. [rootUri] is required, so it does not have an @default
       attribute. But, it may be null, so it remains an option type that maps to
       None if the JSON value is `Null.*)
    type t =
      { process_id: int option
            [@default None]
            [@key "processId"]
      ; root_path: string option
            [@default None]
            [@key "rootPath"]
      ; root_uri: DocumentUri.t option
            [@key "rootUri"]
      ; initialization_options: Void.t option
            [@default None]
            [@key "initializationOptions"]
      ; capabilities: ClientCapabilities.t
            [@key "capabilities"]
      ; trace: TraceSetting.t option
            [@default None]
            [@key "trace"]
      ; workspaceFolders: (WorkspaceFolder.t list) option
            [@default None]
            [@key "workspaceFolders"]
      }
    [@@deriving yojson]
  end

  include RequestMessage.Make
      (struct let name = "initialize" end)
      (InitializeParams)
end

module ShutdownRequest = RequestMessage.Make
    (struct let name = "shutdown" end)
    (Void)

module GotoDefinitionRequest = RequestMessage.Make
    (struct let name = "textDocument/definition" end)
    (TextDocumentPositionParams)

module HoverRequest = RequestMessage.Make
    (struct let name = "textDocument/hover" end)
    (TextDocumentPositionParams)

module CodeActionRequest = RequestMessage.Make
    (struct let name = "textDocument/codeAction" end)
    (CodeActionParams)

module DidCloseTextDocument =
  NotificationMessage.Make
    (struct let name = "textDocument/didClose" end)
    (DidCloseTextDocumentParams)

module DidSaveTextDocument =
  NotificationMessage.Make
    (struct let name = "textDocument/didSave" end)
    (DidSaveTextDocumentParams)

module DidOpenTextDocument = NotificationMessage.Make
    (struct let name = "textDocument/didOpen" end)
    (DidOpenTextDocumentParams)

module ShowMessage = NotificationMessage.Make
    (struct let name = "window/showMessage" end)
    (ShowMessageParams)

(** {2 Responses} *)

(** {3 Initialize Response} *)

module InitializeResponse = struct
  (* A Null type module is used for the experimental Any type possible in
     ServerCapabilities *)
  module InitializeResult = struct
    module ServerCapabilities = ServerCapabilities.Make(Null)
    type t = { capabilities: ServerCapabilities.t }
    [@@deriving yojson]
  end
  (* The data field for InitializeResult consists of a retry value *)
  module InitializeError = ResponseError.Make(struct
      type t = { retry: bool }
      [@@deriving yojson]
    end)

  include ResponseMessage.Make (InitializeResult) (InitializeError)
end


module ShutdownResponse = struct
  module ShutdownResult = Null
  module ShutdownError = ResponseError.Make(Null)

  include ResponseMessage.Make (ShutdownResult) (ShutdownError)
end


module TextDocumentDefinitionResponse = struct
  module DefinitionResult = struct
    type t = Location.t list
    [@@deriving yojson]
  end

  module DefinitionError = ResponseError.Make(Null)

  include ResponseMessage.Make (DefinitionResult) (DefinitionError)
end


module HoverResponse = struct
  module HoverResult = struct
    type t = {
      contents: string;
      range: Range.t option;
    }
    [@@deriving yojson]
  end

  module HoverError = ResponseError.Make(Null)

  include ResponseMessage.Make (HoverResult) (HoverError)
end


(** {2 Notifications} *)

(** {3 PublishDiagnostics Notification} *)
module PublishDiagnosticsParams = struct
  type t =
    { uri: DocumentUri.t
          [@key "uri"]
    ; diagnostics: Diagnostic.t list
          [@key "diagnostics"]
    }
  [@@deriving yojson]
end

module PublishDiagnostics =
  NotificationMessage.Make
    (struct let name = "textDocument/publishDiagnostics" end)
    (PublishDiagnosticsParams)

(** {3 DidChangeTextDocument Notification} *)
module DidChangeTextDocumentParams = struct
  module TextDocumentContentChangeEvent = struct
    type t =
      { range: Range.t
            [@key "range"]
      ; range_length: int option
            [@key "rangeLength"]
            [@default None]
      ; text: string
            [@key "text"]
      }
    [@@deriving yojson]
  end

  type t =
    { textDocument: VersionedTextDocumentIdentifier.t
    ; contentChanges: TextDocumentContentChangeEvent.t list
    }
  [@@deriving yojson]
end

module DidChangeTextDocument =
  NotificationMessage.Make
    (struct let name = "textDocument/didChange" end)
    (DidChangeTextDocumentParams)


(** Namespaces *)

(** ErrorCodes possible in responses *)
module ErrorCodes = struct
  type t =
    | ParseError
    | InvalidRequest
    | MethodNotFound
    | InvalidParams
    | InternalError
    | ServerErrorStart
    | ServerErrorEnd
    | ServerNotInitialized
    | UnknownErrorCode
    | RequestCancelled

  let to_yojson : t -> Yojson.Safe.json = function
    | ParseError -> `Int (-32700)
    | InvalidRequest -> `Int (-32600)
    | MethodNotFound -> `Int (-32601)
    | InvalidParams -> `Int (-32602)
    | InternalError -> `Int (-32603)
    | ServerErrorStart -> `Int (-32099)
    | ServerErrorEnd -> `Int (-32000)
    | ServerNotInitialized -> `Int (-32002)
    | UnknownErrorCode -> `Int (-32001)
    | RequestCancelled -> `Int (-32800)
end
