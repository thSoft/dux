module Test.StructuralEditor.ListEditor where

import Signal exposing (Mailbox)
import Task exposing (Task)
import Html exposing (Html)
import Html.Attributes as Attributes
import ElmFire exposing (Location)
import Component exposing (Output)
import StructuralEditor.ListEditor as ListEditor exposing (ListEditor)
import StructuralEditor.ValueEditor as ValueEditor exposing (ValueEditor)
import StructuralEditor.Separators as Separators
import Test.StructuralEditor.ChoiceEditor as ChoiceEditor

main : Signal Html
main =
  output.html |> Signal.map wrap

port tasks : Signal (Task () ())
port tasks =
  output.tasks

type alias Model =
  ListEditor Element

type alias Element =
  ValueEditor.Model ChoiceEditor.Element

output : Output Model
output =
  Component.start
    {
      init =
        ListEditor.init context location,
      update =
        ListEditor.update context,
      view =
        ListEditor.view True context,
      inputs =
        []
    }

location : Location
location =
  "https://thsoft.firebaseio.com/DUX/test/ListEditor" |> ElmFire.fromUrl

context : ListEditor.Context Element ValueEditor.Action
context =
  {
    initialItem =
      ValueEditor.initialModel,
    itemUpdateContext =
      ValueEditor.updateContext ChoiceEditor.context,
    separator =
      Separators.line,
    itemViewContext =
      ValueEditor.viewContext ChoiceEditor.context
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
var addedNodesObserver = new MutationObserver(function(mutations) {
  mutations.forEach(function(mutation) {
    handleChildrenAdded(mutation.addedNodes);
  });
});
var subtreeChangedConfig = { childList: true, subtree: true };
var target = document.querySelector('body > div');
addedNodesObserver.observe(target, subtreeChangedConfig);

function handleChildrenAdded(nodeList) {
  for (var i = 0; i < nodeList.length; i++) {
    var node = nodeList[i];
    handleAttributesChanged(node);
    targetObserver.observe(node, attributesChangedConfig); // XXX necessary because of the following behavior of virtual-dom:
    // inserting NodeToFocus before OtherNode happens by mutating OtherNode into NodeToFocus and inserting a new OtherNode after it
    handleChildrenAdded(node.childNodes);
  }
}

var targetObserver = new MutationObserver(function(mutations) {
  mutations.forEach(function(mutation) {
    handleAttributesChanged(mutation.target);
  });
});
var attributesChangedConfig = { attributes: true };

function handleAttributesChanged(node) {
  if (node instanceof Element && node.hasAttribute('data-autofocus')) {
    node.focus();
  }
}
"""
