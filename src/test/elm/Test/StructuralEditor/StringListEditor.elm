module Test.StructuralEditor.StringListEditor where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import Html.Attributes as Attributes
import Effects exposing (Never)
import StartApp exposing (App)
import Component
import StructuralEditor.ListEditor as ListEditor
import StructuralEditor.EditorKind as EditorKind
import StructuralEditor.Separator as Separator

main : Signal Html
main =
  app.html

port tasks : Signal (Task Never ())
port tasks =
  app.tasks

type alias Element =
  String

type alias Model =
  ListEditor.Model Element

type alias Action =
  ListEditor.Action Element

app : App Model
app =
  Component.start
    {
      init =
        ListEditor.init EditorKind.string context actionMailbox.address,
      update =
        ListEditor.update actionMailbox.address,
      view address model =
        ListEditor.view address model |> wrap,
      inputs =
        inputs
    }

context : ListEditor.Context
context =
  {
    url =
      "https://thsoft.firebaseio.com/DUX/test/StringListEditor",
    separator =
      Separator.line
  }

actionMailbox : Mailbox Action
actionMailbox =
  Signal.mailbox ListEditor.None

inputs : List (Signal Action)
inputs =
  [actionMailbox.signal]

wrap : Html -> Html
wrap html =
  Html.div
    [
      Attributes.style [
        ("font-family", fontFamily ++ ", Monaco, Consolas, 'Courier New', Courier")
      ]
    ]
    [
      Html.node "link"
        [
          Attributes.rel "stylesheet",
          Attributes.type' "text/css",
          Attributes.href ("http://fonts.googleapis.com/css?family=" ++ fontFamily)
        ]
        [],
      Html.node "script"
        []
        [Html.text script],
      html
    ]

fontFamily : String
fontFamily =
  "Inconsolata"

script : String
script = """
var observer = new MutationObserver(function(mutations) {
  mutations.forEach(function(mutation) {
    handleAutofocus(mutation.addedNodes);
  });
});
var target = document.querySelector('body > div');
var config = { childList: true, subtree: true };
observer.observe(target, config);

function handleAutofocus(nodeList) {
  for (var i = 0; i < nodeList.length; i++) {
    var node = nodeList[i];
    if (node instanceof Element && node.hasAttribute('data-autofocus')) {
      node.focus();
      break;
    } else {
      handleAutofocus(node.childNodes);
    }
  }
}
"""
