module RestApi where
import Http
import Task
import Effects exposing (Effects)
import Json.Decode as Json exposing ((:=))
import Snippet.Base exposing (Snippet, initializeSnippet)
import Snippet exposing (stringToSnippetType)


-- EFFECTS --

fetchSnippetList : Effects (Result Http.Error (List Snippet))
fetchSnippetList =
  Http.get snippetListDecoder "/server/index.json"
    |> Task.toResult
    |> Effects.task


-- DECODERS --

snippetListDecoder : Json.Decoder (List Snippet)
snippetListDecoder =
  let toSnippet content kind = initializeSnippet content (stringToSnippetType kind)
      snippet =
        Json.object2 toSnippet
          ("content" := Json.string)
          ("kind" := Json.string)
  in  "snippets" := Json.list snippet
