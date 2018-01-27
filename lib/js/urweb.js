// VENDORING MORPHDOM: https://github.com/patrick-steele-idem/morphdom
var morphdom = (function(){
  var range; // Create a range object for efficently rendering strings to elements.
  var NS_XHTML = 'http://www.w3.org/1999/xhtml';

  var doc = typeof document === 'undefined' ? undefined : document;

  var testEl = doc ?
      doc.body || doc.createElement('div') :
      {};

  // Fixes <https://github.com/patrick-steele-idem/morphdom/issues/32>
  // (IE7+ support) <=IE7 does not support el.hasAttribute(name)
  var actualHasAttributeNS;

  if (testEl.hasAttributeNS) {
      actualHasAttributeNS = function(el, namespaceURI, name) {
          return el.hasAttributeNS(namespaceURI, name);
      };
  } else if (testEl.hasAttribute) {
      actualHasAttributeNS = function(el, namespaceURI, name) {
          return el.hasAttribute(name);
      };
  } else {
      actualHasAttributeNS = function(el, namespaceURI, name) {
          return el.getAttributeNode(namespaceURI, name) != null;
      };
  }

  var hasAttributeNS = actualHasAttributeNS;


  function toElement(str) {
      if (!range && doc.createRange) {
          range = doc.createRange();
          range.selectNode(doc.body);
      }

      var fragment;
      if (range && range.createContextualFragment) {
          fragment = range.createContextualFragment(str);
      } else {
          fragment = doc.createElement('body');
          fragment.innerHTML = str;
      }
      return fragment.childNodes[0];
  }

  /**
  * Returns true if two node's names are the same.
  *
  * NOTE: We don't bother checking `namespaceURI` because you will never find two HTML elements with the same
  *       nodeName and different namespace URIs.
  *
  * @param {Element} a
  * @param {Element} b The target element
  * @return {boolean}
  */
  function compareNodeNames(fromEl, toEl) {
      var fromNodeName = fromEl.nodeName;
      var toNodeName = toEl.nodeName;

      if (fromNodeName === toNodeName) {
          return true;
      }

      if (toEl.actualize &&
          fromNodeName.charCodeAt(0) < 91 && /* from tag name is upper case */
          toNodeName.charCodeAt(0) > 90 /* target tag name is lower case */) {
          // If the target element is a virtual DOM node then we may need to normalize the tag name
          // before comparing. Normal HTML elements that are in the "http://www.w3.org/1999/xhtml"
          // are converted to upper case
          return fromNodeName === toNodeName.toUpperCase();
      } else {
          return false;
      }
  }

  /**
  * Create an element, optionally with a known namespace URI.
  *
  * @param {string} name the element name, e.g. 'div' or 'svg'
  * @param {string} [namespaceURI] the element's namespace URI, i.e. the value of
  * its `xmlns` attribute or its inferred namespace.
  *
  * @return {Element}
  */
  function createElementNS(name, namespaceURI) {
      return !namespaceURI || namespaceURI === NS_XHTML ?
          doc.createElement(name) :
          doc.createElementNS(namespaceURI, name);
  }

  /**
  * Copies the children of one DOM element to another DOM element
  */
  function moveChildren(fromEl, toEl) {
      var curChild = fromEl.firstChild;
      while (curChild) {
          var nextChild = curChild.nextSibling;
          toEl.appendChild(curChild);
          curChild = nextChild;
      }
      return toEl;
  }

  function morphAttrs(fromNode, toNode) {
      var attrs = toNode.attributes;
      var i;
      var attr;
      var attrName;
      var attrNamespaceURI;
      var attrValue;
      var fromValue;

      for (i = attrs.length - 1; i >= 0; --i) {
          attr = attrs[i];
          attrName = attr.name;
          attrNamespaceURI = attr.namespaceURI;
          attrValue = attr.value;

          if (attrNamespaceURI) {
              attrName = attr.localName || attrName;
              fromValue = fromNode.getAttributeNS(attrNamespaceURI, attrName);

              if (fromValue !== attrValue) {
                  fromNode.setAttributeNS(attrNamespaceURI, attrName, attrValue);
              }
          } else {
              fromValue = fromNode.getAttribute(attrName);

              if (fromValue !== attrValue) {
                  fromNode.setAttribute(attrName, attrValue);
              }
          }
      }

      // Remove any extra attributes found on the original DOM element that
      // weren't found on the target element.
      attrs = fromNode.attributes;

      for (i = attrs.length - 1; i >= 0; --i) {
          attr = attrs[i];
          if (attr.specified !== false) {
              attrName = attr.name;
              attrNamespaceURI = attr.namespaceURI;

              if (attrNamespaceURI) {
                  attrName = attr.localName || attrName;

                  if (!hasAttributeNS(toNode, attrNamespaceURI, attrName)) {
                      fromNode.removeAttributeNS(attrNamespaceURI, attrName);
                  }
              } else {
                  if (!hasAttributeNS(toNode, null, attrName)) {
                      fromNode.removeAttribute(attrName);
                  }
              }
          }
      }
  }

  function syncBooleanAttrProp(fromEl, toEl, name) {
      if (fromEl[name] !== toEl[name]) {
          fromEl[name] = toEl[name];
          if (fromEl[name]) {
              fromEl.setAttribute(name, '');
          } else {
              fromEl.removeAttribute(name, '');
          }
      }
  }

  var specialElHandlers = {
      /**
      * Needed for IE. Apparently IE doesn't think that "selected" is an
      * attribute when reading over the attributes using selectEl.attributes
      */
      OPTION: function(fromEl, toEl) {
          syncBooleanAttrProp(fromEl, toEl, 'selected');
      },
      /**
      * The "value" attribute is special for the <input> element since it sets
      * the initial value. Changing the "value" attribute without changing the
      * "value" property will have no effect since it is only used to the set the
      * initial value.  Similar for the "checked" attribute, and "disabled".
      */
      INPUT: function(fromEl, toEl) {
          syncBooleanAttrProp(fromEl, toEl, 'checked');
          syncBooleanAttrProp(fromEl, toEl, 'disabled');

          if (fromEl.value !== toEl.value) {
              fromEl.value = toEl.value;
          }

          if (!hasAttributeNS(toEl, null, 'value')) {
              fromEl.removeAttribute('value');
          }
      },

      TEXTAREA: function(fromEl, toEl) {
          var newValue = toEl.value;
          if (fromEl.value !== newValue) {
              fromEl.value = newValue;
          }

          var firstChild = fromEl.firstChild;
          if (firstChild) {
              // Needed for IE. Apparently IE sets the placeholder as the
              // node value and vise versa. This ignores an empty update.
              var oldValue = firstChild.nodeValue;

              if (oldValue == newValue || (!newValue && oldValue == fromEl.placeholder)) {
                  return;
              }

              firstChild.nodeValue = newValue;
          }
      },
      SELECT: function(fromEl, toEl) {
          if (!hasAttributeNS(toEl, null, 'multiple')) {
              var selectedIndex = -1;
              var i = 0;
              var curChild = toEl.firstChild;
              while(curChild) {
                  var nodeName = curChild.nodeName;
                  if (nodeName && nodeName.toUpperCase() === 'OPTION') {
                      if (hasAttributeNS(curChild, null, 'selected')) {
                          selectedIndex = i;
                          break;
                      }
                      i++;
                  }
                  curChild = curChild.nextSibling;
              }

              fromEl.selectedIndex = i;
          }
      }
  };

  var ELEMENT_NODE = 1;
  var TEXT_NODE = 3;
  var COMMENT_NODE = 8;

  function noop() {}

  function defaultGetNodeKey(node) {
      return node.id;
  }

  function morphdomFactory(morphAttrs) {

      return function morphdom(fromNode, toNode, options) {
          if (!options) {
              options = {};
          }

          if (typeof toNode === 'string') {
              if (fromNode.nodeName === '#document' || fromNode.nodeName === 'HTML') {
                  var toNodeHtml = toNode;
                  toNode = doc.createElement('html');
                  toNode.innerHTML = toNodeHtml;
              } else {
                  toNode = toElement(toNode);
              }
          }

          var getNodeKey = options.getNodeKey || defaultGetNodeKey;
          var onBeforeNodeAdded = options.onBeforeNodeAdded || noop;
          var onNodeAdded = options.onNodeAdded || noop;
          var onBeforeElUpdated = options.onBeforeElUpdated || noop;
          var onElUpdated = options.onElUpdated || noop;
          var onBeforeNodeDiscarded = options.onBeforeNodeDiscarded || noop;
          var onNodeDiscarded = options.onNodeDiscarded || noop;
          var onBeforeElChildrenUpdated = options.onBeforeElChildrenUpdated || noop;
          var childrenOnly = options.childrenOnly === true;
          var skipChildren = options.skipChildren || 0;
          var skipChildrenEnd = options.skipChildrenEnd || 0;

          // This object is used as a lookup to quickly find all keyed elements in the original DOM tree.
          var fromNodesLookup = {};
          var keyedRemovalList;

          function addKeyedRemoval(key) {
              if (keyedRemovalList) {
                  keyedRemovalList.push(key);
              } else {
                  keyedRemovalList = [key];
              }
          }

          function walkDiscardedChildNodes(node, skipKeyedNodes) {
              if (node.nodeType === ELEMENT_NODE) {
                  var curChild = node.firstChild;
                  while (curChild) {

                      var key = undefined;

                      if (skipKeyedNodes && (key = getNodeKey(curChild))) {
                          // If we are skipping keyed nodes then we add the key
                          // to a list so that it can be handled at the very end.
                          addKeyedRemoval(key);
                      } else {
                          // Only report the node as discarded if it is not keyed. We do this because
                          // at the end we loop through all keyed elements that were unmatched
                          // and then discard them in one final pass.
                          onNodeDiscarded(curChild);
                          if (curChild.firstChild) {
                              walkDiscardedChildNodes(curChild, skipKeyedNodes);
                          }
                      }

                      curChild = curChild.nextSibling;
                  }
              }
          }

          /**
          * Removes a DOM node out of the original DOM
          *
          * @param  {Node} node The node to remove
          * @param  {Node} parentNode The nodes parent
          * @param  {Boolean} skipKeyedNodes If true then elements with keys will be skipped and not discarded.
          * @return {undefined}
          */
          function removeNode(node, parentNode, skipKeyedNodes) {
              if (onBeforeNodeDiscarded(node) === false) {
                  return;
              }

              if (parentNode) {
                  parentNode.removeChild(node);
              }

              onNodeDiscarded(node);
              walkDiscardedChildNodes(node, skipKeyedNodes);
          }

          // // TreeWalker implementation is no faster, but keeping this around in case this changes in the future
          // function indexTree(root) {
          //     var treeWalker = document.createTreeWalker(
          //         root,
          //         NodeFilter.SHOW_ELEMENT);
          //
          //     var el;
          //     while((el = treeWalker.nextNode())) {
          //         var key = getNodeKey(el);
          //         if (key) {
          //             fromNodesLookup[key] = el;
          //         }
          //     }
          // }

          // // NodeIterator implementation is no faster, but keeping this around in case this changes in the future
          //
          // function indexTree(node) {
          //     var nodeIterator = document.createNodeIterator(node, NodeFilter.SHOW_ELEMENT);
          //     var el;
          //     while((el = nodeIterator.nextNode())) {
          //         var key = getNodeKey(el);
          //         if (key) {
          //             fromNodesLookup[key] = el;
          //         }
          //     }
          // }

          function indexTree(node, skipChildrenI, skipChildrenEndI) {
              var skipChildren = skipChildrenI || 0;
              var skipChildrenEnd = skipChildrenEndI || 0;

              if (node.nodeType === ELEMENT_NODE) {
                  var curChild = node.childNodes[skipChildren];
                  var endingChild = node.childNodes[node.childNodes.length - skipChildrenEnd];
                  var i = 0;
                  while (curChild && curChild !== endingChild) {
                      var key = getNodeKey(curChild);
                      if (key) {
                          fromNodesLookup[key] = curChild;
                      }

                      // Walk recursively
                      indexTree(curChild);

                      curChild = curChild.nextSibling;
                  }
              }
          }

          indexTree(fromNode, skipChildren, skipChildrenEnd);

          function handleNodeAdded(el) {
              onNodeAdded(el);

              var curChild = el.firstChild;
              while (curChild) {
                  var nextSibling = curChild.nextSibling;

                  var key = getNodeKey(curChild);
                  if (key) {
                      var unmatchedFromEl = fromNodesLookup[key];
                      if (unmatchedFromEl && compareNodeNames(curChild, unmatchedFromEl)) {
                          curChild.parentNode.replaceChild(unmatchedFromEl, curChild);
                          morphEl(unmatchedFromEl, curChild);
                      }
                  }

                  handleNodeAdded(curChild);
                  curChild = nextSibling;
              }
          }

          function morphEl(fromEl, toEl, childrenOnly, skipChildrenI, skipChildrenEndI) {
              var toElKey = getNodeKey(toEl);
              var curFromNodeKey;
              var skipChildren = skipChildrenI || 0;
              var skipChildrenEnd = skipChildrenEndI || 0;

              var endingFromNodeChild = fromEl.childNodes[fromEl.childNodes.length - skipChildrenEnd];

              if (toElKey) {
                  // If an element with an ID is being morphed then it is will be in the final
                  // DOM so clear it out of the saved elements collection
                  delete fromNodesLookup[toElKey];
              }

              if (toNode.isSameNode && toNode.isSameNode(fromNode)) {
                  return;
              }

              if (!childrenOnly) {
                  if (onBeforeElUpdated(fromEl, toEl) === false) {
                      return;
                  }

                  morphAttrs(fromEl, toEl);
                  onElUpdated(fromEl);

                  if (onBeforeElChildrenUpdated(fromEl, toEl) === false) {
                      return;
                  }
              }

              if (fromEl.nodeName !== 'TEXTAREA') {
                  var curToNodeChild = toEl.firstChild;
                  var curFromNodeChild = fromEl.childNodes[skipChildren];
                  var curToNodeKey;

                  var fromNextSibling;
                  var toNextSibling;
                  var matchingFromEl;

                  outer: while (curToNodeChild) {
                      toNextSibling = curToNodeChild.nextSibling;
                      curToNodeKey = getNodeKey(curToNodeChild);

                      while (curFromNodeChild && curFromNodeChild !== endingFromNodeChild) {
                          fromNextSibling = curFromNodeChild.nextSibling;

                          if (curToNodeChild.isSameNode && curToNodeChild.isSameNode(curFromNodeChild)) {
                              curToNodeChild = toNextSibling;
                              curFromNodeChild = fromNextSibling;
                              continue outer;
                          }

                          curFromNodeKey = getNodeKey(curFromNodeChild);

                          var curFromNodeType = curFromNodeChild.nodeType;

                          var isCompatible = undefined;

                          if (curFromNodeType === curToNodeChild.nodeType) {
                              if (curFromNodeType === ELEMENT_NODE) {
                                  // Both nodes being compared are Element nodes

                                  if (curToNodeKey) {
                                      // The target node has a key so we want to match it up with the correct element
                                      // in the original DOM tree
                                      if (curToNodeKey !== curFromNodeKey) {
                                          // The current element in the original DOM tree does not have a matching key so
                                          // let's check our lookup to see if there is a matching element in the original
                                          // DOM tree
                                          if ((matchingFromEl = fromNodesLookup[curToNodeKey])) {
                                              if (curFromNodeChild.nextSibling === matchingFromEl) {
                                                  // Special case for single element removals. To avoid removing the original
                                                  // DOM node out of the tree (since that can break CSS transitions, etc.),
                                                  // we will instead discard the current node and wait until the next
                                                  // iteration to properly match up the keyed target element with its matching
                                                  // element in the original tree
                                                  isCompatible = false;
                                              } else {
                                                  // We found a matching keyed element somewhere in the original DOM tree.
                                                  // Let's moving the original DOM node into the current position and morph
                                                  // it.

                                                  // NOTE: We use insertBefore instead of replaceChild because we want to go through
                                                  // the `removeNode()` function for the node that is being discarded so that
                                                  // all lifecycle hooks are correctly invoked
                                                  fromEl.insertBefore(matchingFromEl, curFromNodeChild);

                                                  fromNextSibling = curFromNodeChild.nextSibling;

                                                  if (curFromNodeKey) {
                                                      // Since the node is keyed it might be matched up later so we defer
                                                      // the actual removal to later
                                                      addKeyedRemoval(curFromNodeKey);
                                                  } else {
                                                      // NOTE: we skip nested keyed nodes from being removed since there is
                                                      //       still a chance they will be matched up later
                                                      removeNode(curFromNodeChild, fromEl, true /* skip keyed nodes */);
                                                  }

                                                  curFromNodeChild = matchingFromEl;
                                              }
                                          } else {
                                              // The nodes are not compatible since the "to" node has a key and there
                                              // is no matching keyed node in the source tree
                                              isCompatible = false;
                                          }
                                      }
                                  } else if (curFromNodeKey) {
                                      // The original has a key
                                      isCompatible = false;
                                  }

                                  isCompatible = isCompatible !== false && compareNodeNames(curFromNodeChild, curToNodeChild);
                                  if (isCompatible) {
                                      // We found compatible DOM elements so transform
                                      // the current "from" node to match the current
                                      // target DOM node.
                                      morphEl(curFromNodeChild, curToNodeChild);
                                  }

                              } else if (curFromNodeType === TEXT_NODE || curFromNodeType == COMMENT_NODE) {
                                  // Both nodes being compared are Text or Comment nodes
                                  isCompatible = true;
                                  // Simply update nodeValue on the original node to
                                  // change the text value
                                  if (curFromNodeChild.nodeValue !== curToNodeChild.nodeValue) {
                                      curFromNodeChild.nodeValue = curToNodeChild.nodeValue;
                                  }

                              }
                          }

                          if (isCompatible) {
                              // Advance both the "to" child and the "from" child since we found a match
                              curToNodeChild = toNextSibling;
                              curFromNodeChild = fromNextSibling;
                              continue outer;
                          }

                          // No compatible match so remove the old node from the DOM and continue trying to find a
                          // match in the original DOM. However, we only do this if the from node is not keyed
                          // since it is possible that a keyed node might match up with a node somewhere else in the
                          // target tree and we don't want to discard it just yet since it still might find a
                          // home in the final DOM tree. After everything is done we will remove any keyed nodes
                          // that didn't find a home
                          if (curFromNodeKey) {
                              // Since the node is keyed it might be matched up later so we defer
                              // the actual removal to later
                              addKeyedRemoval(curFromNodeKey);
                          } else {
                              // NOTE: we skip nested keyed nodes from being removed since there is
                              //       still a chance they will be matched up later
                              removeNode(curFromNodeChild, fromEl, true /* skip keyed nodes */);
                          }

                          curFromNodeChild = fromNextSibling;
                      }

                      // If we got this far then we did not find a candidate match for
                      // our "to node" and we exhausted all of the children "from"
                      // nodes. Therefore, we will just append the current "to" node
                      // to the end
                      if (curToNodeKey && (matchingFromEl = fromNodesLookup[curToNodeKey]) && compareNodeNames(matchingFromEl, curToNodeChild)) {
                          if (skipChildrenEnd && endingFromNodeChild){
                            fromEl.insertBefore(matchingFromEl, endingFromNodeChild);
                          } else {
                            fromEl.appendChild(matchingFromEl);
                          }
                          morphEl(matchingFromEl, curToNodeChild);
                      } else {
                          var onBeforeNodeAddedResult = onBeforeNodeAdded(curToNodeChild);
                          if (onBeforeNodeAddedResult !== false) {
                              if (onBeforeNodeAddedResult) {
                                  curToNodeChild = onBeforeNodeAddedResult;
                              }

                              if (curToNodeChild.actualize) {
                                  curToNodeChild = curToNodeChild.actualize(fromEl.ownerDocument || doc);
                              }
                              if (skipChildrenEnd && endingFromNodeChild){
                                fromEl.insertBefore(curToNodeChild, endingFromNodeChild);
                              } else {
                                fromEl.appendChild(curToNodeChild);
                              }
                              handleNodeAdded(curToNodeChild);
                          }
                      }

                      curToNodeChild = toNextSibling;
                      curFromNodeChild = fromNextSibling;
                  }

                  // We have processed all of the "to nodes". If curFromNodeChild is
                  // non-null then we still have some from nodes left over that need
                  // to be removed
                  while (curFromNodeChild && curFromNodeChild !== endingFromNodeChild) {
                      fromNextSibling = curFromNodeChild.nextSibling;
                      if ((curFromNodeKey = getNodeKey(curFromNodeChild))) {
                          // Since the node is keyed it might be matched up later so we defer
                          // the actual removal to later
                          addKeyedRemoval(curFromNodeKey);
                      } else {
                          // NOTE: we skip nested keyed nodes from being removed since there is
                          //       still a chance they will be matched up later
                          removeNode(curFromNodeChild, fromEl, true /* skip keyed nodes */);
                      }
                      curFromNodeChild = fromNextSibling;
                  }
              }

              var specialElHandler = specialElHandlers[fromEl.nodeName];
              if (specialElHandler) {
                  specialElHandler(fromEl, toEl);
              }
          } // END: morphEl(...)

          var morphedNode = fromNode;
          var morphedNodeType = morphedNode.nodeType;
          var toNodeType = toNode.nodeType;

          if (!childrenOnly) {
              // Handle the case where we are given two DOM nodes that are not
              // compatible (e.g. <div> --> <span> or <div> --> TEXT)
              if (morphedNodeType === ELEMENT_NODE) {
                  if (toNodeType === ELEMENT_NODE) {
                      if (!compareNodeNames(fromNode, toNode)) {
                          onNodeDiscarded(fromNode);
                          morphedNode = moveChildren(fromNode, createElementNS(toNode.nodeName, toNode.namespaceURI));
                      }
                  } else {
                      // Going from an element node to a text node
                      morphedNode = toNode;
                  }
              } else if (morphedNodeType === TEXT_NODE || morphedNodeType === COMMENT_NODE) { // Text or comment node
                  if (toNodeType === morphedNodeType) {
                      if (morphedNode.nodeValue !== toNode.nodeValue) {
                          morphedNode.nodeValue = toNode.nodeValue;
                      }

                      return morphedNode;
                  } else {
                      // Text node to something else
                      morphedNode = toNode;
                  }
              }
          }

          if (morphedNode === toNode) {
              // The "to node" was not compatible with the "from node" so we had to
              // toss out the "from node" and use the "to node"
              onNodeDiscarded(fromNode);
          } else {
              morphEl(morphedNode, toNode, childrenOnly, skipChildren, skipChildrenEnd);

              // We now need to loop over any keyed nodes that might need to be
              // removed. We only do the removal if we know that the keyed node
              // never found a match. When a keyed node is matched up we remove
              // it out of fromNodesLookup and we use fromNodesLookup to determine
              // if a keyed node has been matched up or not
              if (keyedRemovalList) {
                  for (var i=0, len=keyedRemovalList.length; i<len; i++) {
                      var elToRemove = fromNodesLookup[keyedRemovalList[i]];
                      if (elToRemove) {
                          removeNode(elToRemove, elToRemove.parentNode, false);
                      }
                  }
              }
          }

          if (!childrenOnly && morphedNode !== fromNode && fromNode.parentNode) {
              if (morphedNode.actualize) {
                  morphedNode = morphedNode.actualize(fromNode.ownerDocument || doc);
              }
              // If we had to swap out the from node with a new node because the old
              // node was not compatible with the target node then we need to
              // replace the old DOM node in the original DOM tree. This is only
              // possible if the original DOM node was part of a DOM tree which
              // we know is the case if it has a parent node.
              fromNode.parentNode.replaceChild(morphedNode, fromNode);
          }

          return morphedNode;
      };
  }

  return morphdomFactory(morphAttrs);
}());

// Detect browser quirks that we should be aware of.

function needsDynPrefix() {
    var span = document.createElement("span");
    span.innerHTML = "<script>alert('test');</script>";
    var scripts = span.getElementsByTagName("script");
    return scripts.length == 0;
}

var dynPrefix = needsDynPrefix() ? "<span style=\"display:none\">A</span>" : "";

// Function versions of operators

function not(x) { return !x; }
function neg(x) { return -x; }

function eq(x, y) { return x == y; }
function plus(x, y) { return x + y; }
function minus(x, y) { return x - y; }
function times(x, y) { return x * y; }
function div(x, y) { return x / y; }
function divInt(x, y) { if (y == 0) er("Division by zero"); var n = x / y; return n < 0 ? Math.ceil(n) : Math.floor(n); }
function mod(x, y) { return x % y; }
function modInt(x, y) { if (y == 0) er("Division by zero"); var n = x % y; return n < 0 ? Math.ceil(n) : Math.floor(n); }
function lt(x, y) { return x < y; }
function le(x, y) { return x <= y; }

// Characters

function isLower(c) { return c >= 'a' && c <= 'z'; }
function isUpper(c) { return c >= 'A' && c <= 'Z'; }
function isAlpha(c) { return isLower(c) || isUpper(c); }
function isDigit(c) { return c >= '0' && c <= '9'; }
function isAlnum(c) { return isAlpha(c) || isDigit(c); }
function isBlank(c) { return c == ' ' || c == '\t'; }
function isSpace(c) { return isBlank(c) || c == '\r' || c == '\n'; }
function isXdigit(c) { return isDigit(c) || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'); }
function ord(c) { return c.charCodeAt(0); }
function isPrint(c) { return ord(c) > 31 && ord(c) < 127; }
function toLower(c) { return c.toLowerCase(); }
function toUpper(c) { return c.toUpperCase(); }

// Lists

function cons(v, ls) {
    return { next : ls, data : v };
}
function rev(ls) {
    var acc = null;
    for (; ls; ls = ls.next)
       acc = cons(ls.data, acc);
    return acc;
}
function concat(ls1, ls2) {
    var acc = ls2;
    ls1 = rev(ls1);
    for (; ls1; ls1 = ls1.next)
        acc = cons(ls1.data, acc);
    return acc;
}
function member(x, ls) {
    for (; ls; ls = ls.next)
        if (ls.data == x)
            return true;
    return false;
}
function remove(x, ls) {
    var acc = null;

    for (; ls; ls = ls.next)
        if (ls.data == x)
            return concat(acc, ls.next);
        else
            acc = cons(ls.data, acc);

    return ls;
}
function union(ls1, ls2) {
    var acc = ls2;

    for (; ls1; ls1 = ls1.next)
        if (!member(ls1.data, ls2))
            acc = cons(ls1.data, acc);

    return acc;
}
function length(ls) {
    var acc = 0;

    for (; ls; ls = ls.next)
        ++acc;

    return acc;
}


// Floats

function float(n) {
    return n;
}

function trunc(n) {
    return ~~n;
}

function ceil(n) {
    return Math.ceil(n);
}

function round(n) {
    return Math.round(n);
}

function pow(n, m) {
    return Math.pow(n, m);
}

function sqrt(n){
    return Math.sqrt(n);
}

function sin(n){
    return Math.sin(n);
}

function cos(n){
    return Math.cos(n);
}

function log(n){
    return Math.log(n);
}

function exp(n){
    return Math.exp(n);
}

function asin(n){
    return Math.asin(n);
}
function acos(n){
    return Math.acos(n);
}

function atan(n){
    return Math.atan(n);
}

function atan2(n, m){
    return Math.atan2(n, m);
}

function floor(n){
    return Math.floor(n);
}

function abs(n){
    return Math.abs(n);
}

// Time, represented as counts of microseconds since the epoch

var time_format = "%c";

function showTime(tm) {
    return strftime(time_format, tm);
}

function showTimeHtml(tm) {
    return eh(showTime(tm));
}

function now() {
    return (new Date()).getTime() * 1000;
}

function diffInSeconds(tm1, tm2) {
    return Math.round((tm2 - tm1) / 1000000);
}

function diffInMilliseconds(tm1, tm2) {
    return Math.round((tm2 - tm1) / 1000);
}

function toSeconds(tm) {
    return Math.round(tm / 1000000);
}

function toMilliseconds(tm) {
    return Math.round(tm / 1000);
}

function fromMilliseconds(tm) {
    return tm * 1000;
}

function addSeconds(tm, n) {
    return tm + n * 1000000;
}

function stringToTime_error(string) {
    var t = Date.parse(string);
    if (isNaN(t))
        er("Invalid date string: " + string);
    else
        return t * 1000;
}

function stringToTime(string) {
    try {
        var t = Date.parse(string);
        if (isNaN(t))
            return null;
        else
            return t * 1000;
    } catch (e) {
        return null;
    }
}

/*
strftime() implementation from:
YUI 3.4.1 (build 4118)
Copyright 2011 Yahoo! Inc. All rights reserved.
Licensed under the BSD License.
http://yuilibrary.com/license/
*/

var xPad=function (x, pad, r)
{
    if(typeof r === "undefined")
    {
	r=10;
    }
    pad = pad.toString();
    for( ; parseInt(x, 10)<r && r>1; r/=10) {
	x = pad + x;
    }
    return x.toString();
};

var YDateEn = {
    a: ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"],
    A: ["Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"],
    b: ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"],
    B: ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"],
    c: "%a %d %b %Y %T %Z",
    p: ["AM", "PM"],
    P: ["am", "pm"],
    r: "%I:%M:%S %p",
    x: "%d/%m/%y",
    X: "%T"
};

var Dt = {
    formats: {
	a: function (d, l) { return l.a[d.getDay()]; },
	A: function (d, l) { return l.A[d.getDay()]; },
	b: function (d, l) { return l.b[d.getMonth()]; },
	B: function (d, l) { return l.B[d.getMonth()]; },
	C: function (d) { return xPad(parseInt(d.getFullYear()/100, 10), 0); },
	d: ["getDate", "0"],
	e: ["getDate", " "],
	g: function (d) { return xPad(parseInt(Dt.formats.G(d)%100, 10), 0); },
	G: function (d) {
	    var y = d.getFullYear();
	    var V = parseInt(Dt.formats.V(d), 10);
	    var W = parseInt(Dt.formats.W(d), 10);

	    if(W > V) {
		y++;
	    } else if(W===0 && V>=52) {
		y--;
	    }

	    return y;
	},
	H: ["getHours", "0"],
	I: function (d) { var I=d.getHours()%12; return xPad(I===0?12:I, 0); },
	j: function (d) {
	    var gmd_1 = new Date("" + d.getFullYear() + "/1/1 GMT");
	    var gmdate = new Date("" + d.getFullYear() + "/" + (d.getMonth()+1) + "/" + d.getDate() + " GMT");
	    var ms = gmdate - gmd_1;
	    var doy = parseInt(ms/60000/60/24, 10)+1;
	    return xPad(doy, 0, 100);
	},
	k: ["getHours", " "],
	l: function (d) { var I=d.getHours()%12; return xPad(I===0?12:I, " "); },
	m: function (d) { return xPad(d.getMonth()+1, 0); },
	M: ["getMinutes", "0"],
	p: function (d, l) { return l.p[d.getHours() >= 12 ? 1 : 0 ]; },
	P: function (d, l) { return l.P[d.getHours() >= 12 ? 1 : 0 ]; },
	s: function (d, l) { return parseInt(d.getTime()/1000, 10); },
	S: ["getSeconds", "0"],
	u: function (d) { var dow = d.getDay(); return dow===0?7:dow; },
	U: function (d) {
	    var doy = parseInt(Dt.formats.j(d), 10);
	    var rdow = 6-d.getDay();
	    var woy = parseInt((doy+rdow)/7, 10);
	    return xPad(woy, 0);
	},
	V: function (d) {
	    var woy = parseInt(Dt.formats.W(d), 10);
	    var dow1_1 = (new Date("" + d.getFullYear() + "/1/1")).getDay();
	    var idow = woy + (dow1_1 > 4 || dow1_1 <= 1 ? 0 : 1);
	    if(idow === 53 && (new Date("" + d.getFullYear() + "/12/31")).getDay() < 4)
	    {
		idow = 1;
	    }
	    else if(idow === 0)
	    {
		idow = Dt.formats.V(new Date("" + (d.getFullYear()-1) + "/12/31"));
	    }

	    return xPad(idow, 0);
	},
	w: "getDay",
	W: function (d) {
	    var doy = parseInt(Dt.formats.j(d), 10);
	    var rdow = 7-Dt.formats.u(d);
	    var woy = parseInt((doy+rdow)/7, 10);
	    return xPad(woy, 0, 10);
	},
	y: function (d) { return xPad(d.getFullYear()%100, 0); },
	Y: "getFullYear",
	z: function (d) {
	    var o = d.getTimezoneOffset();
	    var H = xPad(parseInt(Math.abs(o/60), 10), 0);
	    var M = xPad(Math.abs(o%60), 0);
	    return (o>0?"-":"+") + H + M;
	},
	Z: function (d) {
	    var tz = d.toString().replace(/^.*:\d\d( GMT[+-]\d+)? \(?([A-Za-z ]+)\)?\d*$/, "$2").replace(/[a-z ]/g, "");
	    if(tz.length > 4) {
		tz = Dt.formats.z(d);
	    }
	    return tz;
	},
	"%": function (d) { return "%"; }
    },

    aggregates: {
	c: "locale",
	D: "%m/%d/%y",
	F: "%Y-%m-%d",
	h: "%b",
	n: "\n",
	r: "%I:%M:%S %p",
	R: "%H:%M",
	t: "\t",
	T: "%H:%M:%S",
	x: "locale",
	X: "locale"
    },

    format : function (oDate, format) {
	var replace_aggs = function (m0, m1) {
	    var f = Dt.aggregates[m1];
	    return (f === "locale" ? YDateEn[m1] : f);
	};

	var replace_formats = function (m0, m1) {
	    var f = Dt.formats[m1];
	    switch(typeof f) {
	    case "string":
		return oDate[f]();
	    case "function":
		return f.call(oDate, oDate, YDateEn);
	    case "array":
            case "object":
		if(typeof(f[0]) === "string")
		    return xPad(oDate[f[0]](), f[1]);
	    default:
		return m1;
	    }
	};

	while(format.match(/%[cDFhnrRtTxX]/)) {
	    format = format.replace(/%([cDFhnrRtTxX])/g, replace_aggs);
	}

	var str = format.replace(/%([aAbBCdegGHIjklmMpPsSuUVwWyYzZ%])/g, replace_formats);

	replace_aggs = replace_formats = undefined;

	return str;
    }
};

// End of YUI code

function strftime(fmt, thisTime)
{
    var thisDate = new Date();
    thisDate.setTime(Math.floor(thisTime / 1000));
    return Dt.format(thisDate, fmt);
};

function fromDatetime(year, month, date, hour, minute, second) {
  return (new Date(year, month, date, hour, minute, second)).getTime() * 1000;
};

function datetimeYear(t) {
  return (new Date(t / 1000)).getYear() + 1900;
};

function datetimeMonth(t) {
  return (new Date(t / 1000)).getMonth();
};

function datetimeDay(t) {
  return (new Date(t / 1000)).getDate();
};

function datetimeHour(t) {
  return (new Date(t / 1000)).getHours();
};

function datetimeMinute(t) {
  return (new Date(t / 1000)).getMinutes();
};

function datetimeSecond(t) {
  return (new Date(t / 1000)).getSeconds();
};

function datetimeDayOfWeek(t) {
  return (new Date(t / 1000)).getDay();
};


// Error handling

function uw_debug(msg) {
    try {
        console.debug(msg);
    } catch (e) {
        alert("DEBUG: " + msg);
    }

    return 0;
}

function whine(msg) {
    alert(msg);
    throw msg;
}

function pf(loc) {
    throw ("Pattern match failure (" + loc + ")");
}

var lameDuck = false;

function runHandlers(kind, ls, arg) {
    if (!lameDuck) {
        if (ls == null)
            alert(kind + ": " + arg);
        for (; ls; ls = ls.next)
            try {
                exec({c:"a", f:{c:"a", f:ls.data, x:{c:"c", v:arg}}, x:{c:"c", v:null}});
            } catch (v) { }
    }
}

var errorHandlers = null;

function flift0(v) {
    return {c:"c", v:v};
}

function onError(f) {
    errorHandlers = cons(flift0(f), errorHandlers);
}

function er(s) {
    runHandlers("Error", errorHandlers, s);
    throw {uw_error: s};
}

var failHandlers = null;

function onFail(f) {
    failHandlers = cons(flift0(f), failHandlers);
}

function doExn(v) {
    if (v == null || v.uw_error == null) {
        var s = (v == null ? "null" : v.message ? v.message : v.toString());
        if (v != null && v.fileName && v.lineNumber)
            s += " (" + v.fileName + ":" + v.lineNumber + ")";
        runHandlers("Fail", failHandlers, s);
    }
}

var disconnectHandlers = null;

function flift(f) {
    return {c: "c", v:{env:cons(f,null), body:{c:"v", n:1}}};
}

function onDisconnect(f) {
    disconnectHandlers = cons(flift(f), disconnectHandlers);
}

function discon() {
    runHandlers("Disconnect", disconnectHandlers, null);
}

var connectHandlers = null;

function onConnectFail(f) {
    connectHandlers = cons(flift(f), connectHandlers);
}

function conn(msg) {
    var rx = /(.*)<body>((.|\n|\r)*)<\/body>(.*)/g;
    var arr = rx.exec(msg);
    msg = (arr && arr.length >= 3) ? arr[2] : msg;
    runHandlers("RPC failure", connectHandlers, msg);
}

var serverHandlers = null;

function onServerError(f) {
    serverHandlers = cons(flift0(f), serverHandlers);
}

function servErr(s) {
    window.setTimeout(function () { runHandlers("Server", serverHandlers, s); }, 0);
}

// Key and mouse events

var uw_event = null;

function uw_getEvent() {
    return window.event ? window.event : uw_event;
}

function firstGood(x, y) {
    if (x == undefined || x == 0)
        return y;
    else
        return x;
}

function uw_mouseEvent() {
    var ev = uw_getEvent();

    return {_ScreenX : firstGood(ev.screenX, 0),
            _ScreenY : firstGood(ev.screenY, 0),
            _ClientX : firstGood(ev.clientX, 0),
            _ClientY : firstGood(ev.clientY, 0),
            _CtrlKey : firstGood(ev.ctrlKey, false),
            _ShiftKey : firstGood(ev.shiftKey, false),
            _AltKey : firstGood(ev.altKey, false),
            _MetaKey : firstGood(ev.metaKey, false),
            _Button : ev.button == 2 ? "Right" : ev.button == 1 ? "Middle" : "Left"};
}

function uw_keyEvent() {
    var ev = uw_getEvent();

    return {_KeyCode : firstGood(ev.keyCode, ev.which),
            _CtrlKey : firstGood(ev.ctrlKey, false),
            _ShiftKey : firstGood(ev.shiftKey, false),
            _AltKey : firstGood(ev.altKey, false),
            _MetaKey : firstGood(ev.metaKey, false)};
}



// Document events

function uw_handler(name, f) {
    var old = document[name];
    if (old == undefined)
        document[name] = function(event) { uw_event = event; execF(execF(f, uw_mouseEvent())); };
    else
        document[name] = function(event) { uw_event = event; old(); execF(execF(f, uw_mouseEvent())); };
}

function uw_onClick(f) {
    uw_handler("onclick", f);
}

function uw_onContextmenu(f) {
    uw_handler("oncontextmenu", f);
}

function uw_onDblclick(f) {
    uw_handler("ondblclick", f);
}

function uw_onMousedown(f) {
    uw_handler("onmousedown", f);
}

function uw_onMouseenter(f) {
    uw_handler("onmouseenter", f);
}

function uw_onMouseleave(f) {
    uw_handler("onmouseleave", f);
}

function uw_onMousemove(f) {
    uw_handler("onmousemove", f);
}

function uw_onMouseout(f) {
    uw_handler("onmouseout", f);
}

function uw_onMouseover(f) {
    uw_handler("onmouseover", f);
}

function uw_onMouseup(f) {
    uw_handler("onmouseup", f);
}

function uw_keyHandler(name, f) {
    var old = document[name];
    if (old == undefined)
        document[name] = function(event) { uw_event = event; execF(execF(f, uw_keyEvent())); };
    else
        document[name] = function(event) { uw_event = event; old(); execF(execF(f, uw_keyEvent())); };
}

function uw_onKeydown(f) {
    uw_keyHandler("onkeydown", f);
}

function uw_onKeypress(f) {
    uw_keyHandler("onkeypress", f);
}

function uw_onKeyup(f) {
    uw_keyHandler("onkeyup", f);
}

// Cancelling of further event processing

function uw_preventDefault() {
    var e = window.event ? window.event : uw_event;
    e.returnValue = false;
    if (e.preventDefault) e.preventDefault();
}

function uw_stopPropagation() {
    var e = window.event ? window.event : uw_event;
    e.cancelBubble = true;
    if (e.stopPropagation) e.stopPropagation();
}

// Embedding closures in XML strings

function cs(f) {
    return {closure: f};
}

function isWeird(v) {
    return v.closure != null || v.cat1 != null;
}

function cat(s1, s2) {
    if (isWeird(s1) || isWeird(s2))
        return {cat1: s1, cat2: s2};
    else
        return s1 + s2;
}

var closures = [];
var freeClosures = null;

function newClosure(f) {
    var n;
    if (freeClosures == null) {
        n = closures.length;
    } else {
        n = freeClosures.data;
        freeClosures = freeClosures.next;
    }
    closures[n] = f;
    return n;
}

function freeClosure(n) {
    closures[n] = null;
    freeClosures = cons(n, freeClosures);
}

function cr(n) {
    return closures[n];
}

function flattenAcc(a, cls, trs) {
    while (trs) {
        var tr = trs.data;
        trs = trs.next;

        if (tr.cat1 != null) {
            trs = cons(tr.cat1, cons(tr.cat2, trs));
        } else if (tr.closure != null) {
            var cl = newClosure(tr.closure);
            cls.v = cons(cl, cls.v);
            a.push("cr(", cl.toString(), ")");
        } else
            a.push(tr);
    }
}

function flatten(cls, tr) {
    var a = [];
    flattenAcc(a, cls, cons(tr, null));
    return a.join("");
}

function flattenLocal(s) {
    var cls = {v : null};
    var r = flatten(cls, s);
    for (cl = cls.v; cl != null; cl = cl.next)
        freeClosure(cl.data);
    return r;
}


// Dynamic tree management

function populate(node) {
    if (node.dead) return;

    var s = node.signal;
    var oldSources = node.sources;
    try {
        var sr = execF(s, null);
        var newSources = sr._sources;

        for (var sp = oldSources; sp; sp = sp.next)
            if (!member(sp.data, newSources))
                sp.data.dyns = remove(node, sp.data.dyns);

        for (var sp = newSources; sp; sp = sp.next)
            if (!member(sp.data, oldSources))
                sp.data.dyns = cons(node, sp.data.dyns);

        node.sources = newSources;
        node.recreate(sr._data);
    } catch (v) {
        doExn(v);
    }
}

function sc(v) {
    return {data : v, dyns : null};
}
function sv(s, v) {
    if (s.data != v) {
        s.data = v;

        for (var ls = s.dyns; ls; ls = ls.next)
            populate(ls.data);
    }
}
function sg(s) {
    return s.data;
}

function ss(s) {
    return {env:cons(s, null), body:{c:"r", l:
            cons({n:"sources", v:{c:"c", v:cons(s, null)}},
                 cons({n:"data", v:{c:"f", f:sg, a:cons({c:"v", n:1}, null)}}, null))}};
}
function sr(v) {
    return {env:null, body:{c:"c", v:{_sources : null, _data : v}}};
}
function sb(x,y) {
    return {env:cons(y,cons(x,null)),
            body:{c:"=",
                e1:{c:"a", f:{c:"v", n:2}, x:{c:"c", v:null}},
                e2:{c:"=",
                    e1:{c:"a",
                        f:{c:"a", f:{c:"v", n:2}, x:{c:".", r:{c:"v", n:0}, f:"data"}},
                        x:{c:"c", v:null}},
                    e2:{c:"r", l:cons(
                                      {n:"sources", v:{c:"f", f:union, a:cons({c:".", r:{c:"v", n:1}, f:"sources"},
                                                                              cons({c:".", r:{c:"v", n:0}, f:"sources"}, null))}},
                                      cons({n:"data", v:{c:".", r:{c:"v", n:0}, f:"data"}}, null))}}}};
}
function scur(s) {
    return execF(s, null)._data;
}

function lastParent() {
    var pos = document.body;

    while (pos.lastChild && pos.lastChild.nodeType == 1)
        pos = pos.lastChild;

    pos = pos.parentNode;

    return pos;
}

var thisScript = null;

function addNode(node) {
    if (thisScript) {
        if (thisScript.parentNode)
            thisScript.parentNode.replaceChild(node, thisScript);
    } else
        lastParent().appendChild(node);
}

function runScripts(node) {
    if (node.tagName == "SCRIPT") {
        var savedScript = thisScript;
        thisScript = node;

        try {
            eval(thisScript.text);
        } catch (v) {
            doExn(v);
        }
        if (thisScript.parentNode)
            thisScript.parentNode.removeChild(thisScript);

        thisScript = savedScript;
    } else if (node.getElementsByTagName) {
        var savedScript = thisScript;

        var scripts = node.getElementsByTagName("script"), scriptsCopy = [];
        var len = scripts.length;
        for (var i = 0; i < len; ++i)
            scriptsCopy[i] = scripts[i];
        for (var i = 0; i < len; ++i) {
            thisScript = scriptsCopy[i];

            try {
                eval(thisScript.text);
            } catch (v) {
                doExn(v);
            }
            if (thisScript.parentNode)
                thisScript.parentNode.removeChild(thisScript);
        }

        thisScript = savedScript;
    }
}


// Dynamic tree entry points

function killScript(scr) {
    scr.dead = true;
    for (var ls = scr.sources; ls; ls = ls.next)
        ls.data.dyns = remove(scr, ls.data.dyns);
    for (var ls = scr.closures; ls; ls = ls.next)
        freeClosure(ls.data);
}

// Sometimes we wind up with tables that contain <script>s outside the single <tbody>.
// To avoid dealing with that case, we normalize by moving <script>s into <tbody>.
function normalizeTable(table) {
    var orig = table;

    var script, next;

    while (table && table.tagName != "TABLE")
        table = table.parentNode;

    for (var tbody = table.firstChild; tbody; tbody = tbody.nextSibling) {
        if (tbody.tagName == "TBODY") {
            var firstChild = tbody.firstChild;

            for (script = table.firstChild; script && script != tbody; script = next) {
                next = script.nextSibling;

                if (script.tagName === "SCRIPT") {
                    if (firstChild)
                        tbody.insertBefore(script, firstChild);
                    else
                        tbody.appendChild(script);
                }
            }

            return;
        }
    }

    var tbody = document.createElement("tbody");
    for (script = table.firstChild; script; script = next) {
        next = script.nextSibling;

        tbody.appendChild(script);
    }
    table.appendChild(tbody);
}

var suspendScripts = false;

function dyn(pnode, s) {
    if (suspendScripts)
        return;

    // Marker script
    var x = document.createElement("script");
    x.dead = false;
    x.signal = s;
    x.sources = null;
    x.closures = null;

    var firstChild = null;

    x.recreate = function(v) {
        for (var ls = x.closures; ls; ls = ls.next)
            freeClosure(ls.data);

        var next;
        for (var child = firstChild; child && child != x; child = next) {
            next = child.nextSibling;

            killScript(child);
            if (child.getElementsByTagName) {
                var arr = child.getElementsByTagName("script");
                for (var i = 0; i < arr.length; ++i)
                    killScript(arr[i]);
            }
        }

        var cls = {v : null};
        var html = flatten(cls, v);
        if (pnode != "table" && pnode != "tr")
            html = dynPrefix + html;
        x.closures = cls.v;
        
        if (!x.parentNode){return; }

        if (pnode == "table" || pnode == "tr") {
            if (pnode == "table")
                normalizeTable(x.parentNode);

            var parent = x.parentNode;

            if (!firstChild){
                firstChild = document.createElement("script");
                parent.insertBefore(firstChild, x);
            }

            var dummy = document.createElement(parent.tagName);
            suspendScripts = true;
            try {
                dummy.innerHTML = html;
            } catch (e) {
                suspendScripts = false;
                throw e;
            }

            var skipChildren = Array.prototype.indexOf.call(firstChild.parentNode.childNodes, firstChild) + 1; // + 1 because we want to skip the "firstChild" marker script
            var skipChildrenEnd = x.parentNode.childNodes.length - Array.prototype.indexOf.call(x.parentNode.childNodes, x); // No -1 because we want to skip the "x" marker script

            morphdom(parent, dummy, {childrenOnly: true, skipChildren: skipChildren, skipChildrenEnd: skipChildrenEnd});

            suspendScripts = false;
            runScripts(dummy);
        } else {
            suspendScripts = true;
            var dummy;
            try {
              if (!firstChild){
                  firstChild = document.createElement("span");
                  firstChild.innerHTML = html;
                  if (x.parentNode)
                      x.parentNode.insertBefore(firstChild, x);
              } else {
                dummy = document.createElement("span");
                dummy.innerHTML = html;
                morphdom(firstChild, dummy, {childrenOnly: true});
              }
            } catch (e) {
                suspendScripts = false;
                throw e;
            }
            suspendScripts = false;
            runScripts(firstChild);
        }
    };

    addNode(x);
    populate(x);
}

function setInnerHTML(node, html) {
    var x;

    if (node.previousSibling && node.previousSibling.closures != undefined) {
        x = node.previousSibling;

        for (var ls = x.closures; ls; ls = ls.next)
            freeClosure(ls.data);

        if (node.getElementsByTagName) {
            var arr = node.getElementsByTagName("script");
            for (var i = 0; i < arr.length; ++i)
                killScript(arr[i]);
        }
    } else {
        x = document.createElement("script");
        x.dead = false;
        x.sources = null;

        if (node.parentNode)
            node.parentNode.insertBefore(x, node);
        else
            whine("setInnerHTML: node is not already in the DOM tree");
    }

    var cls = {v : null};
    var html = flatten(cls, html);
    x.closures = cls.v;
    suspendScripts = true;
    node.innerHTML = html;
    suspendScripts = false;
    runScripts(node);
}

var maySuspend = true;

function active(s) {
    if (suspendScripts)
        return;

    var ms = maySuspend;
    maySuspend = false;
    try {
        var html = execF(s);
    } catch (e) {
        maySuspend = ms;
        throw e;
    }
    maySuspend = ms;
    if (html != "") {
        var span = document.createElement("span");
        addNode(span);
        setInnerHTML(span, html);
    }
}

function input(x, s, recreate, type, name) {
    if (name) x.name = name;
    if (type) x.type = type;
    addNode(x);

    var sc = document.createElement("script");
    sc.dead = false;
    sc.signal = ss(s);
    sc.sources = null;
    sc.recreate = recreate(x);

    if (x.parentNode)
        x.parentNode.insertBefore(sc, x);

    populate(sc);

    return x;
}

function inpt(type, s, name) {
    if (suspendScripts)
        return;

    var x = input(document.createElement("input"), s,
                  function(x) { return function(v) { if (x.value != v) x.value = v; }; }, type, name);
    x.value = s.data;
    x.onkeyup = x.oninput = x.onchange = x.onpropertychange = function() { sv(s, x.value) };

    return x;
}
function inpt_float(type, s, name) {
    if (suspendScripts)
        return;

    var filterFloat = function(value) {
	if (/^(\-|\+)?([0-9]+(\.[0-9]+)?|Infinity)$/
	    .test(value))
	    return Number(value);
	return null;
    }
    var x = input(document.createElement("input"), s, function(x) { return function(v) { if (x.value != v) x.value = v; }; }, type, name);
    x.value = s.data;
    x.onkeyup = x.oninput = x.onchange = x.onpropertychange = function() { sv(s, filterFloat(x.value)) };

    return x;
}


function inp(s, name) {
    return inpt("text", s, name);
}

function password(s, name) {
    return inpt("password", s, name);
}

function email(s, name) {
    return inpt("email", s, name);
}

function search(s, name) {
    return inpt("search", s, name);
}

function url(s, name) {
    return inpt("url", s, name);
}

function tel(s, name) {
    return inpt("tel", s, name);
}

function color(s, name) {
    return inpt("color", s, name);
}

function number(s, name) {
    return inpt_float("number", s, name);
}

function range(s, name) {
    return inpt_float("range", s, name);
}

function date(s, name) {
    return inpt("date", s, name);
}

function datetime(s, name) {
    return inpt("datetime", s, name);
}

function datetime_local(s, name) {
    return inpt("datetime-local", s, name);
}

function month(s, name) {
    return inpt("month", s, name);
}

function week(s, name) {
    return inpt("week", s, name);
}

function time(s, name) {
    return inpt("time", s, name);
}


function selectValue(x) {
    if (x.options.length == 0)
        return "";
    else
        return x.options[x.selectedIndex].value;
}

function setSelectValue(x, v) {
  for (var i = 0; i < x.options.length; ++i) {
      if(x.options[i].value == v) {
          x.selectedIndex = i;
          return;
      }
  }
}

function sel(s, content) {
    if (suspendScripts)
        return;

    var dummy = document.createElement("span");
    dummy.innerHTML = "<select>" + content + "</select>";
    var x = input(dummy.firstChild, s, function(x) { return function(v) { if (selectValue(x) != v) setSelectValue(x, v); }; });

    for (var i = 0; i < x.options.length; ++i) {
        if (x.options[i].value == "")
            x.options[i].value = x.options[i].text;
        else
            x.options[i].value = x.options[i].value.substring(1);
    }

    setSelectValue(x, s.data);
    if (selectValue(x) != s.data)
        sv(s, selectValue(x));
    x.onchange = function() { sv(s, selectValue(x)) };

    return x;
}

function chk(s) {
    if (suspendScripts)
        return;

    var x = input(document.createElement("input"), s,
                  function(x) { return function(v) { if (x.checked != v) x.checked = v; }; }, "checkbox");
    x.defaultChecked = x.checked = s.data;
    x.onclick = x.onkeyup = x.oninput = x.onchange = x.onpropertychange = function() { sv(s, x.checked) };

    return x;
}

function tbx(s) {
    if (suspendScripts)
        return;

    var x = input(document.createElement("textarea"), s,
                  function(x) { return function(v) { if (x.value != v) x.value = v; }; });
    x.innerHTML = s.data;
    x.onkeyup = x.oninput = x.onchange = x.onpropertychange = function() { sv(s, x.value) };

    return x;
}

function dynClass(pnode, html, s_class, s_style) {
    if (suspendScripts)
        return;

    var htmlCls = {v : null};
    html = flatten(htmlCls, html);
    htmlCls = htmlCls.v;

    var dummy = document.createElement(pnode);
    suspendScripts = true;
    dummy.innerHTML = html;
    suspendScripts = false;
    var html = dummy.firstChild;
    dummy.removeChild(html);
    if (pnode == "table" && html.tagName == "TBODY") {
        html = html.firstChild;
    }

    var x = null;
    var y = null;

    if (s_class) {
        x = document.createElement("script");
        x.dead = false;
        x.signal = s_class;
        x.sources = null;
        x.closures = htmlCls;

        x.recreate = function(v) {
            for (var ls = x.closures; ls != htmlCls; ls = ls.next)
                freeClosure(ls.data);

            var cls = {v : null};
            html.className = flatten(cls, v);
	    x.closures = concat(cls.v, htmlCls);
        }

        populate(x);
    }

    if (s_style) {
        var htmlCls2 = s_class ? null : htmlCls;
        y = document.createElement("script");
        y.dead = false;
        y.signal = s_style;
        y.sources = null;
        y.closures = htmlCls2;

        y.recreate = function(v) {
            for (var ls = y.closures; ls != htmlCls2; ls = ls.next)
                freeClosure(ls.data);

            var cls = {v : null};
            html.style.cssText = flatten(cls, v);
	    y.closures = concat(cls.v, htmlCls2);
        }

        populate(y);
    }

    addNode(html);
    runScripts(html);

    if (x)
        html.appendChild(x);
    if (y)
        html.appendChild(y);
}

function bodyDynClass(s_class, s_style) {
    if (suspendScripts)
        return;

    var htmlCls = null;

    if (s_class) {
        var x = document.createElement("script");
        x.dead = false;
        x.signal = s_class;
        x.sources = null;
        x.closures = htmlCls;

        x.recreate = function(v) {
            for (var ls = x.closures; ls != htmlCls; ls = ls.next)
                freeClosure(ls.data);

            var cls = {v : null};
            document.body.className = flatten(cls, v);
            console.log("className to + " + document.body.className);
	    x.closures = concat(cls.v, htmlCls);
        }

        document.body.appendChild(x);
        populate(x);
    }

    if (s_style) {
        var htmlCls2 = s_class ? null : htmlCls;
        var y = document.createElement("script");
        y.dead = false;
        y.signal = s_style;
        y.sources = null;
        y.closures = htmlCls2;

        y.recreate = function(v) {
            for (var ls = y.closures; ls != htmlCls2; ls = ls.next)
                freeClosure(ls.data);

            var cls = {v : null};
            document.body.style.cssText = flatten(cls, v);
            console.log("style to + " + document.body.style.cssText);
	    y.closures = concat(cls.v, htmlCls2);
        }

        document.body.appendChild(y);
        populate(y);
    }
}

function addOnChange(x, f) {
    var old = x.onchange;
    if (old == null)
        x.onchange = f;
    else
        x.onchange = function() { old(); f(); };
}

function addOnInput(x, f) {
    var old = x.oninput;
    if (old == null)
        x.oninput = f;
    else
        x.oninput = function() { old(); f(); };
}

function addOnKeyUp(x, f) {
    var old = x.onkeyup;
    if (old == null)
        x.onkeyup = f;
    else
        x.onkeyup = function(x) { old(x); f(x); };
}


// Basic string operations

function eh(x) {
    if (x == null)
        return "NULL";
    else
        return x.split("&").join("&amp;").split("<").join("&lt;").split(">").join("&gt;");
}

function ts(x) { return x.toString() }
function bs(b) { return (b ? "True" : "False") }
function s2b(s) { return s == "True" ? true : s == "False" ? false : null; }
function s2be(s) { return s == "True" ? true : s == "False" ? false : er("Illegal Boolean " ^ s); }

function id(x) { return x; }
function sub(s, i) { return s.charAt(i); }
function suf(s, i) { return s.substring(i); }
function slen(s) { return s.length; }
function sidx(s, ch) {
    var r = s.indexOf(ch);
    if (r == -1)
        return null;
    else
        return r;
}
function ssidx(h, n) {
    var r = h.indexOf(n);
    if (r == -1)
        return null;
    else
        return r;
}
function sspn(s, chs) {
    for (var i = 0; i < s.length; ++i)
        if (chs.indexOf(s.charAt(i)) != -1)
            return i;

    return s.length;
}
function schr(s, ch) {
    var r = s.indexOf(ch);
    if (r == -1)
        return null;
    else
        return s.substring(r);
}
function ssub(s, start, len) {
    return s.substring(start, start+len);
}
function strlenGe(s, len) {
    return s.length >= len;
}

function trimZeroes(s) {
    for (var i = 0; i < s.length; ++i)
        if (s.charAt(i) != '0') {
            if (i > 0)
                return s.substring(i);
            else
                return s;
        }

    if (s.length == 0)
        return s;
    else
        return "0";
}

function pi(s) {
    var st = trimZeroes(s);
    var r = parseInt(st);
    if (r.toString() == st)
        return r;
    else
        er("Can't parse int: " + s);
}

function pfl(s) {
    var r = parseFloat(s);
    if (r.toString() == s)
        return r;
    else
        er("Can't parse float: " + s);
}

function pio(s) {
    var st = trimZeroes(s);
    var r = parseInt(st);
    if (r.toString() == st)
        return r;
    else
        return null;
}

function pflo(s) {
    var r = parseFloat(s);
    if (r.toString() == s)
        return r;
    else
        return null;
}

function parseSource(s1, s2) {
    return eval("s" + s1 + "_" + s2);
}

function uf(s) {
    if (s.length == 0)
        return "_";
    s = s.replace(/\./g, ".2E");
    return (s.charAt(0) == '_' ? "_" : "") + encodeURIComponent(s).replace(/%/g, ".");
}

function uu(s) {
    if (s.length > 0 && s.charAt(0) == '_') {
        s = s.substring(1);
    } else if (s.length >= 3 && (s.charAt(0) == '%' || s.charAt(0) == '.')
               && s.charAt(1) == '5' && (s.charAt(2) == 'f' || s.charAt(2) == 'F'))
        s = s.substring(3);
    s = s.replace(/\+/g, " ");
    s = s.replace(/\./g, "%");
    return decodeURIComponent(s);
}

function atr(s) {
    return s.replace(/\"/g, "&quot;").replace(/&/g, "&amp;")
}

function ub(b) {
    return b ? "1" : "0";
}

function uul(getToken, getData) {
    var tok = getToken();
    if (tok == "Nil") {
        return null;
    } else if (tok == "Cons") {
        var d = getData();
        var l = uul(getToken, getData);
        return {_1:d, _2:l};
    } else
        whine("Can't unmarshal list (" + tok + ")");
}

function strcmp(str1, str2) {
    return ((str1 == str2) ? 0 : ((str1 > str2) ? 1 : -1));
}

function chr(n) {
    return String.fromCharCode(n);
}

function htmlifySpecialChar(ch) {
    return "&#" + ch.charCodeAt(0) + ";";
}


// Remote calls

var client_id = null;
var client_pass = 0;
var url_prefix = "/";
var timeout = 60;
var isPost = false;

function getXHR(uri)
{
    try {
        return new XMLHttpRequest();
    } catch (e) {
        try {
            return new ActiveXObject("Msxml2.XMLHTTP");
        } catch (e) {
            try {
                return new ActiveXObject("Microsoft.XMLHTTP");
            } catch (e) {
                whine("Your browser doesn't seem to support AJAX.");
            }
        }
    }
}

var sig = null;

var unloading = false, inFlight = null;

function unload() {
    for (; inFlight; inFlight = inFlight.next) {
        inFlight.data.abort();
    }
}

function requestUri(xhr, uri, needsSig, isRpc) {
    var extraData = null;

    if (isRpc && uri.length > 2000) {
        extraData = uri.substring(2000);
        uri = uri.substring(0, 2000);
    }

    xhr.open("POST", uri, !unloading);
    xhr.setRequestHeader("Content-type", "text/plain");

    if (client_id != null) {
        xhr.setRequestHeader("UrWeb-Client", client_id.toString());
        xhr.setRequestHeader("UrWeb-Pass", client_pass.toString());
    }

    if (needsSig) {
        if (sig == null)
            whine("Missing cookie signature!");

        xhr.setRequestHeader("UrWeb-Sig", sig);
    }

    inFlight = cons(xhr, inFlight);
    xhr.send(extraData);
}

function xhrFinished(xhr) {
    xhr.abort();
    inFlight = remove(xhr, inFlight);
}

function unurlify(parse, s) {
    return parse(s);
}

function redirect(s) {
    window.location = s;
}

function makeSome(isN, v) {
    if (isN)
        return {v: v};
    else
        return v;
}

function rc(prefix, uri, parse, k, needsSig, isN) {
    if (!maySuspend)
        er("May not 'rpc' in main thread of 'code' for <active>");

    uri = cat(prefix, uri);
    uri = flattenLocal(uri);
    var xhr = getXHR();

    xhr.onreadystatechange = function() {
        if (xhr.readyState == 4) {
            var isok = false;

            try {
                if (xhr.status == 200)
                    isok = true;
            } catch (e) { }

            if (isok) {
                var lines = xhr.responseText.split("\n");
                if (lines.length != 2) {
                    if (isN == null)
                        whine("Bad RPC response lines");
                    else
                        k(null);
                } else {
                    eval(lines[0]);

                    try {
                        var v = parse(lines[1]);
                        try {
                            k(makeSome(isN, v));
                        } catch (v) {
                            doExn(v);
                        }
                    } catch (v) {
                        k(null);
                    }
                }
            } else {
                if (isN == null)
                    conn(xhr.responseText);
                else
                    k(null);
            }

            xhrFinished(xhr);
        }
    };

    requestUri(xhr, uri, needsSig, true);
}

function path_join(s1, s2) {
    if (s1.length > 0 && s1.charAt(s1.length-1) == '/')
        return s1 + s2;
    else
        return s1 + "/" + s2;
}

var channels = [];

function newQueue() {
    return { front : null, back : null };
}
function enqueue(q, v) {
    if (q.front == null) {
        q.front = cons(v, null);
        q.back = q.front;
    } else {
        var node = cons(v, null);
        q.back.next = node;
        q.back = node;
    }
}
function dequeue(q) {
    if (q.front == null)
        return null;
    else {
        var r = q.front.data;
        q.front = q.front.next;
        if (q.front == null)
            q.back = null;
        return r;
    }
}

function newChannel() {
    return { msgs : newQueue(), listeners : newQueue() };
}

function listener() {
    var uri = path_join(url_prefix, ".msgs");
    var xhr = getXHR();
    var tid, orsc, onTimeout, lastTick;

    var connect = function () {
        xhr.onreadystatechange = orsc;
        lastTick = new Date().getTime();
        tid = window.setTimeout(onTimeout, timeout * 500);
        requestUri(xhr, uri, false, false);
    }

    orsc = function() {
        if (xhr.readyState == 4) {
            window.clearTimeout(tid);

            var isok = false;

            try {
                if (xhr.status == 200)
                    isok = true;
            } catch (e) { }

            if (isok) {
                var text = xhr.responseText;
                if (text == "")
                    return;
                var lines = text.split("\n");

                if (lines.length == 1 && lines[0] == "R") {
                    lameDuck = true;

                    if (isPost)
                        history.back();
                    else
                        location.reload();

                    return;
                }

                if (lines.length < 2) {
                    discon();
                    return;
                }

                var messageReader = function(i) {
                    if (i+1 >= lines.length) {
                        xhrFinished(xhr);
                        connect();
                    }
                    else {
                        var chn = lines[i];
                        var msg = lines[i+1];

                        if (chn == "E") {
                            eval(msg);
                            window.setTimeout(function() { messageReader(i+2); }, 0);
                        } else {
                            if (chn < 0)
                                whine("Out-of-bounds channel in message from remote server");

                            var ch;

                            if (chn >= channels.length || channels[chn] == null) {
                                ch = newChannel();
                                channels[chn] = ch;
                            } else
                                ch = channels[chn];

                            var listener = dequeue(ch.listeners);
                            if (listener == null) {
                                enqueue(ch.msgs, msg);
                            } else {
                                try {
                                    listener(msg);
                                } catch (v) {
                                    doExn(v);
                                }
                            }

                            messageReader(i+2);
                        }
                    }
                }

                messageReader(0);
            }
            else {
                try {
                    if (xhr.status != 0)
                        servErr("Error querying remote server for messages: " + xhr.status);
                } catch (e) { }
            }
        }
    };

    onTimeout = function() {
        var thisTick = new Date().getTime();
        xhrFinished(xhr);

        if (thisTick - lastTick > timeout * 1000) {
            if (confirm("The session for this page has expired.  Please choose \"OK\" to reload.")) {
                if (isPost)
                    history.back();
                else
                    location.reload();
            }
        } else {
            connect();
        }
    };

    connect();
}

function rv(chn, parse, k) {
    if (!maySuspend)
        er("May not 'recv' in main thread of 'code' for <active>");

    if (chn == null)
        er("Client-side code tried to recv() from a channel belonging to a different page view.");

    if (chn < 0)
        whine("Out-of-bounds channel receive");

    var ch;

    if (chn >= channels.length || channels[chn] == null) {
        ch = newChannel();
        channels[chn] = ch;
    } else
        ch = channels[chn];

    var msg = dequeue(ch.msgs);
    if (msg == null) {
        enqueue(ch.listeners, function(msg) { k(parse(msg)); });
    } else {
        try {
            k(parse(msg));
        } catch (v) {
            doExn(v);
        }
    }
}

function sl(ms, k) {
    if (!maySuspend)
        er("May not 'sleep' in main thread of 'code' for <active>");

    window.setTimeout(function() { k(null); }, ms);
}

function sp(e) {
    window.setTimeout(function() { execF(e); }, 0);
}


// The Ur interpreter

var urfuncs = [];

function lookup(env, n) {
    while (env != null) {
        if (n == 0)
            return env.data;
        else {
            --n;
            env = env.next;
        }
    }

    whine("Out-of-bounds Ur variable reference");
}

function execP(env, p, v) {
    switch (p.c) {
    case "v":
        return cons(v, env);
    case "c":
        if (v == p.v)
            return env;
        else
            return false;
    case "s":
        if (v == null)
            return false;
        else
            return execP(env, p.p, p.n ? v.v : v);
    case "1":
        if (v.n != p.n)
            return false;
        else
            return execP(env, p.p, v.v);
    case "r":
        for (var fs = p.l; fs != null; fs = fs.next) {
            env = execP(env, fs.data.p, v["_" + fs.data.n]);
            if (env == false)
                return false;
        }
        return env;
    default:
        whine("Unknown Ur pattern kind " + p.c);
    }
}

function exec0(env, e) {
    return exec1(env, null, e);
}

function exec1(env, stack, e) {
    var stack, usedK = false;

    var saveEnv = function() {
        if (stack.next != null && stack.next.data.c != "<")
            stack = cons({c: "<", env: env}, stack.next);
        else
            stack = stack.next;
    };

    while (true) {
        switch (e.c) {
        case "c":
            var v = e.v;
            if (stack == null)
                return v;
            var fr = stack.data;

            switch (fr.c) {
            case "s":
                e = {c: "c", v: {v: v}};
                stack = stack.next;
                break;
            case "1":
                e = {c: "c", v: {n: fr.n, v: v}};
                stack = stack.next;
                break;
            case "f":
                fr.args[fr.pos++] = v;
                if (fr.a == null) {
                    var res;
                    stack = stack.next;

                    if (fr.f.apply)
                        res = fr.f.apply(null, fr.args);
                    else if (fr.args.length == 0)
                        res = fr.f();
                    else if (fr.args.length == 1)
                        res = fr.f(fr.args[0]);
                    else if (fr.args.length == 2)
                        res = fr.f(fr.args[0], fr.args[1]);
                    else if (fr.args.length == 3)
                        res = fr.f(fr.args[0], fr.args[1], fr.args[2]);
                    else if (fr.args.length == 4)
                        res = fr.f(fr.args[0], fr.args[1], fr.args[2], fr.args[3]);
                    else if (fr.args.length == 5)
                        res = fr.f(fr.args[0], fr.args[1], fr.args[2], fr.args[3], fr.args[4]);
                    else
                        whine("Native function has " + fr.args.length + " args, but there is no special case for that count.");

                    e = {c: "c", v: res};
                    if (usedK) return null;
                } else {
                    e = fr.a.data;
                    fr.a = fr.a.next;
                }
                break;
            case "a1":
                e = fr.x;
                stack = cons({c: "a2", f: v}, stack.next);
                break;
            case "a2":
                if (fr.f == null)
                    whine("Ur: applying null function");
                else if (fr.f.body) {
                    saveEnv();
                    env = cons(v, fr.f.env);
                    e = fr.f.body;
                } else {
                    e = {c: "c", v: fr.f(v)};
                    stack = stack.next;
                }
                break;
            case "<":
                env = fr.env;
                stack = stack.next;
                break;
            case "r":
                fr.fs["_" + fr.n] = v;
                if (fr.l == null) {
                    e = {c: "c", v: fr.fs};
                    stack = stack.next;
                } else {
                    fr.n = fr.l.data.n;
                    e = fr.l.data.v;
                    fr.l = fr.l.next;
                }
                break;
            case ".":
                e = {c: "c", v: v["_" + fr.f]};
                stack = stack.next;
                break;
            case ";":
                e = fr.e2;
                stack = stack.next;
                break;
            case "=":
                saveEnv();
                env = cons(v, env);
                e = fr.e2;
                break;
            case "m":
                var ps;
                for (ps = fr.p; ps != null; ps = ps.next) {
                    var r = execP(env, ps.data.p, v);
                    if (r != false) {
                        saveEnv();
                        env = r;
                        e = ps.data.b;
                        break;
                    }
                }
                if (ps == null)
                    whine("Match failure in Ur interpretation");
                break;
            default:
                whine("Unknown Ur continuation kind " + fr.c);
            }

            break;
        case "v":
            e = {c: "c", v: lookup(env, e.n)};
            break;
        case "n":
            var idx = e.n;
            e = urfuncs[idx];
            if (e.c == "t")
                e = urfuncs[idx] = eval("(" + e.f + ")");
            break;
        case "s":
            stack = cons({c: "s"}, stack);
            e = e.v;
            break;
        case "1":
            stack = cons({c: "1", n: e.n}, stack);
            e = e.v;
            break;
        case "f":
            if (e.a == null)
                e = {c: "c", v: e.f()};
            else {
                var args = [];
                stack = cons({c: "f", f: e.f, args: args, pos: 0, a: e.a.next}, stack);
                if (!e.a.data.c) alert("[2] fr.f = " + e.f + "; 0 = " + e.a.data);
                e = e.a.data;
            }
            break;
        case "l":
            e = {c: "c", v: {env: env, body: e.b}};
            break;
        case "a":
            stack = cons({c: "a1", x: e.x}, stack);
            e = e.f;
            break;
        case "r":
            if (e.l == null)
                whine("Empty Ur record in interpretation");
            var fs = {};
            stack = cons({c: "r", n: e.l.data.n, fs: fs, l: e.l.next}, stack);
            e = e.l.data.v;
            break;
        case ".":
            stack = cons({c: ".", f: e.f}, stack);
            e = e.r;
            break;
        case ";":
            stack = cons({c: ";", e2: e.e2}, stack);
            e = e.e1;
            break;
        case "=":
            stack = cons({c: "=", e2: e.e2}, stack);
            e = e.e1;
            break;
        case "m":
            stack = cons({c: "m", p: e.p}, stack);
            e = e.e;
            break;
        case "e":
            e = {c: "c", v: cs({c: "wc", env: env, body: e.e})};
            break;
        case "wc":
            env = e.env;
            e = e.body;
            break;
        case "K":
            { var savedStack = stack.next, savedEnv = env;
                e = {c: "c", v: function(v) { return exec1(savedEnv, savedStack, {c: "c", v: v}); } };}
            usedK = true;
            break;
        default:
            whine("Unknown Ur expression kind " + e.c);
        }
    }
}

function execD(e) {
    return exec0(null, e);
}

function exec(e) {
    var r = exec0(null, e);

    if (r != null && r.body != null)
        return function(v) { return exec0(cons(v, r.env), r.body); };
    else
        return r;
}

function execF(f, x) {
    return exec0(cons(x, f.env), f.body);
}


// Wrappers

function confrm(s) {
    return confirm(s) ? true : false;
}


// URL blessing

var urlRules = null;

function checkUrl(s) {
    for (var r = urlRules; r; r = r.next) {
        var ru = r.data;
        if (ru.prefix ? s.indexOf(ru.pattern) == 0 : s == ru.pattern)
            return ru.allow ? s : null;
    }

    return null;
}

function bless(s) {
    u = checkUrl(s);
    if (u == null)
        er("Disallowed URL: " + s);
    return u;
}


// Attribute name blessing

function blessData(s) {
    for (var i = 0; i < s.length; ++i) {
        var c = s[i];
        if (!isAlnum(c) && c != '-' && c != '_')
            er("Disallowed character in data-* attribute name");
    }

    return s;
}


// CSS validation

function atom(s) {
    for (var i = 0; i < s.length; ++i) {
        var c = s[i];
        if (!isAlnum(c) && c != '+' && c != '-' && c != '.' && c != '%' && c != '#')
            er("Disallowed character in CSS atom");
    }

    return s;
}

function css_url(s) {
    for (var i = 0; i < s.length; ++i) {
        var c = s[i];
        if (!isAlnum(c) && c != ':' && c != '/' && c != '.' && c != '_' && c != '+'
            && c != '-' && c != '%' && c != '?' && c != '&' && c != '=' && c != '#')
            er("Disallowed character in CSS URL");
    }

    return s;
}

function property(s) {
    if (s.length <= 0)
        er("Empty CSS property");

    if (!isLower(s[0]) && s[0] != '_')
        er("Bad initial character in CSS property");

    for (var i = 0; i < s.length; ++i) {
        var c = s[i];
        if (!isLower(c) && !isDigit(c) && c != '_' && c != '-')
            er("Disallowed character in CSS property");
    }

    return s;
}


// ID generation

var nextId = 0;

function fresh() {
    return "uw" + (--nextId);
}

function giveFocus(id) {
    var node = document.getElementById(id);

    if (node)
        node.focus();
    else
        er("Tried to give focus to ID not used in document: " + id);
}


// App-specific code
