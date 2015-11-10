module Test.StructuralEditor.StringListEditor where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import Html.Attributes as Attributes
import ElmFire exposing (Location)
import Component exposing (Output)
import StructuralEditor.ListEditor as ListEditor
import StructuralEditor.EditorKind as EditorKind
import StructuralEditor.Separator as Separator

main : Signal Html
main =
  output.html

port tasks : Signal (Task () ())
port tasks =
  output.tasks

type alias Element =
  String

type alias Model =
  ListEditor.Model Element

output : Output Model
output =
  Component.start
    {
      init =
        ListEditor.init EditorKind.string context,
      update =
        ListEditor.update,
      view address model =
        ListEditor.view address model |> wrap,
      inputs =
        []
    }

context : ListEditor.Context
context =
  {
    location =
      "https://thsoft.firebaseio.com/DUX/test/StringListEditor" |> ElmFire.fromUrl,
    separator =
      Separator.line
  }

wrap : Html -> Html
wrap html =
  Html.div
    [
      Attributes.style [
        ("font-family", fontFamily ++ ", Helvetica, sans-serif"),
        ("font-size", "90%")
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
  "Open Sans"

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
