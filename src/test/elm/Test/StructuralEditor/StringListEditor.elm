module Test.StructuralEditor.StringListEditor where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import Html.Attributes as Attributes
import ElmFire exposing (Location)
import Component exposing (Output)
import StructuralEditor.ListEditor as ListEditor exposing (ListEditor)
import StructuralEditor.ValueEditor as ValueEditor exposing (ValueEditor)
import StructuralEditor.Separators as Separators
import StructuralEditor.ValueEditorContexts as ValueEditorContexts

main : Signal Html
main =
  output.html |> Signal.map wrap

port tasks : Signal (Task () ())
port tasks =
  output.tasks

type alias Model =
  ListEditor Element

type alias Element =
  ValueEditor.Model String

output : Output Model
output =
  Component.start
    {
      init =
        ListEditor.init context location,
      update =
        ListEditor.update context,
      view =
        ListEditor.view context,
      inputs =
        []
    }

location : Location
location =
  "https://thsoft.firebaseio.com/DUX/test/StringListEditor" |> ElmFire.fromUrl

context : ListEditor.Context Element ValueEditor.Action
context =
  {
    initialItem =
      ValueEditor.initialModel,
    itemUpdateContext =
      ValueEditor.updateContext ValueEditorContexts.string,
    separator =
      Separators.line,
    itemViewContext =
      ValueEditor.viewContext ValueEditorContexts.string
  }

wrap : Html -> Html
wrap html =
  Html.div
    [
      Attributes.style [
        ("font-family", fontFamily ++ ", Helvetica, sans-serif"),
        ("font-size", "90%"),
        ("padding", "0.36em")
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
