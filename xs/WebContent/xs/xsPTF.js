/**
 * Copyright 2013 Andrew Conway. All rights reserved.
 * 
 * 
 * Pseudo text field - a text field with error underlines.
 * This is hard to do with the normal TextBox, so the idea here
 * is to use a contentEditable div. After each change the contents will
 * be normalized to get rid of stray formatting, and text with errors
 * will have it inside a span with the appropriate message.
 * 
 * This incidentally allows us to have multiline text fields that automatically
 * grow to use the space they need.
 * 
 * This can be used freestanding instead of part of xs. It can be distributed under the MIT license. It does not have any
 * dependencies other than xs.css. It does not use jquery.
 */
var xsPTF = {


	/** True if the oninput event handler has been shown to work */
	inputWorks : false,

	/** Function to be called by input events on a pseudo text field */
	input :	function(event) {
	  xsPTF.inputWorks=true;
	  xsPTF.validate(event.target);
    },

    /** If you don't have a working oninput event handler for contentEditable=true divs (IE), then call this from lots of other events instead. */
    inputSurrogate : function(event) {
    	if (!xsPTF.inputWorks) xsPTF.validate(event.target);
    },

  		
  /** Produce a string that can be used as innerHTML to display the original string (escaping html characters) */
    escapeText : function(s) { 
	  return s.replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;");
    },		
    escapeTextIncludingDoubleQuotes : function(s) { 
  	  return s.replace(/&/g,"&amp;").replace(/</g,"&lt;").replace(/>/g,"&gt;").replace(/\"/g,"&#34;");
    },		
      
    
    emptyErrorList : [],
    /** errorListByID.ffff is the error list for field with associated gui field having id "ffff" */
   // errorListByID : new Object,
    /* callbackOnChangeByID.ffff is a callback function for field with associated gui field having id "ffff" 
    callbackOnChangeByID : new Object,*/
    
    /** Given a node, get a list of error messages associated with it */
    getErrorList : function(target) {
    	/*
    	var id = target.id;
    	if (id) {
    		var res = this.errorListByID[id];
    		if (res) return res;
    	}*/
    	var list = target.errorList;
    	if (list) return list;
    	return this.emptyErrorList;
    },
    
    isPTF : function(target) {
    	return target && (/(^| )xsPseudoTextField( |$)/.test(target.className));
    },
    
    isPTFErrorTooltip : function(target) {
    	return target && ((/(^| )xsSpanErrorGridTooltip( |$)/.test(target.className)) || (/(^| )xsSpanErrorTooltip( |$)/.test(target.className)));
    },
    
    
    /** Call to assign a set of errors to a particular id */
    setErrorList : function(id,errorList) {
    	/* this.errorListByID[id]=errorList; */
    	// check to see if this is a pseudo-text field, in which case you will need to revalidate to display the error highlighting properly in it.
    	var tf = document.getElementById(id);
    	if (tf) {
        	tf.errorList = errorList;
        	if (this.isPTF(tf)) {
        		this.validate(tf);
        	}
    	}
    	// check to see if there is a UI that the error list can be displayed in.
    	var icon = document.getElementById(id+"_erroricon");
    	if (icon) {
			icon.className="xsErrorIcon xsErrorIcon"+this.worstSeverity(errorList);
        	var tt = document.getElementById(id+"_errortooltip");
        	if (tt) {
        		tt.innerHTML = this.errorListAsHTML(errorList);
        	}
    	}
    },
    
    /** Called to disassociate error lists from UI values. Should be done when old UIs get discarded. Actually, doesn't work as there may be multiple editing sessions in one window. */
    /*
    unregisterErrorLists : function() {
    	errorListByID = new Object;
    },*/
    
    /** Called when the node target gets changed by the user to some new text.
     * This checks the data-onInput field, which should be the name of a function that will be called, given the target and the new text result. 
     * If that is not set, it checks data-onInputObj, which if set will be an object on which ptfInput(target,newText) will be called. */
    callbackOnChange : function(target,newText) {
    	var fns = target.getAttribute("data-onInput");
    	if (fns) {
    		//console.log("fns= "+fns);
    		var fn = eval(fns);
    		//console.log(fn);
    		fn(target,newText);
    	} else {
    		var objs = target.getAttribute("data-onInputObj");
    		if (objs) {
    			var obj = eval(objs);
    			obj.ptfInput(target,newText);
    		}
    	}
    	/*
    	var id = target.id;
    	if (id) {
    		var res = this.callbackOnChangeByID[id];
    		if (res) return res(target,newText);
    	} */   	
    },
    
    /** Register a function that gets called back when the pseudo text field with given id has input changed by the user. The function should take two arguments - the first is the element, the second is the new text. 
    registerCallbackOnChange : function(id,fn) {
    	this.callbackOnChangeByID[id]=fn;
    },*/

    /** Utility class that is helpful in determining the cursor position */
    GetCursorPosition : function (container,offset) {
      this.container = container;
      this.offset = offset;
      this.res = -1; // not found yet
      this.checkPos = function(parentNode,numChildNodesProcessed,textSoFar) {
        if (parentNode==this.container && numChildNodesProcessed==this.offset) this.res=textSoFar.length;
      };
      this.checkTextNode= function(textNode,textSoFar) {
        if (textNode==this.container) this.res=textSoFar.length+offset;
      };
    },

    /** Return an integer describing the severity of the worst (lowest 0 = error, 1=warning, 2 = info, 1000=blank) error */
    worstSeverity : function(errorList) {
    	if (errorList.length>0) {
            var severities = errorList.map(function(e) { return e.severity;});
            severities.sort();
            return severities[0];
    	} else return 1000;
    },
    
    /** Return some HTML (a series of DIVs), one per error, suitable for a tooltip for those errors. */
    errorListAsHTML : function(errorList) {
		var res = "";
		for (var i=0;i<errorList.length;i++) {
			var e = errorList[i];
			res+="<div class='xsErrorDescription"+e.severity+"'>"+e.text+"</div>"; // The text is actually HTML.
		}
        return res;    	
    },

    isFirefox : (navigator.appName=="Netscape"),
    
    /** Validate a pseudo text field. This should be called whenever the input changes, or whenever the error list changes. It
     * (1) Reads in the junk in the div (copied from elsewhere, or inserted by browser) and converts it to a simple string. Store that in target.data-text
     * (2) If the text has changed (since the prior stored in target.data-text), call a callback (registered via registerCallbackOnChange above).
     * (3) Get an error list (registered via setErrorList above) 
     * (4) Convert the text into HTML that fits inside the div, replacing newlines by <br/> (needed for IE), and inserting appropriate error spans for errors in the list.
     * (5) Restore the cursor to where it was at the start of all of this.
     * 
     * The first argument is the node being processed.
     * The second (optional) argument can give it some extra commands:
     *  if it has a field "overrideText" then the value of said field will override the previous text generated in step (1). This is used when you are setting the value programatically. The callback in step 2 will not be called.
     *  if if has a field "selectAll" then everything will be selected
     */
    validate: function(target,options) {
       var contents = this.getPTFContents(target);
       // finished step (1), start step (2)
       if (options && !(options.overrideText===undefined)) {
    	   contents.text=options.overrideText || "";
       } else if (target.getAttribute("data-text")!=contents.text) this.callbackOnChange(target,contents.text);
       if (options && options.selectAll) {
    	   contents.startSelection=0;
    	   contents.endSelection=contents.text.length;
       }
       target.setAttribute("data-text",contents.text);
       // finished step (2), start step (3)
       var errorList = this.getErrorList(target);
       // finished step (3), start step (4)
       var resHTML = this.PTFinnerHTMLOfText(contents.text,errorList,xs.grid.getGridFromAncestor(target));
       target.innerHTML = resHTML;
       // finished step (4), start step (5)
       if (window.getSelection && document.createRange && contents.startSelection != -1 && contents.endSelection!= -1) {
    	   xsPTF.setPTFSelection(target,contents.startSelection,contents.endSelection);
      };
    },
    
    
    getPTFContents : function(target) {
   	   //console.log(target.innerHTML);
    	var suppressNL = target.getAttribute("data-xsSuppressNL")=="true";
    	var savedRange=null;
    	if(window.getSelection && window.getSelection().rangeCount > 0) {//FF,Chrome,Opera,Safari,IE9+
          savedRange = window.getSelection().getRangeAt(0).cloneRange();
        }
        var startSelection = null;
        var endSelection = null;
        if (savedRange!=null) {
          startSelection = new xsPTF.GetCursorPosition(savedRange.startContainer,savedRange.startOffset);
          endSelection = new xsPTF.GetCursorPosition(savedRange.endContainer,savedRange.endOffset);
        } else {
          startSelection = new xsPTF.GetCursorPosition(null,0);
          endSelection = new xsPTF.GetCursorPosition(null,0);
        }
        var resText = "";
        var hadDiv = false;
        var procNode = function(parentNode) {
          //console.log(parentNode);
          var children = parentNode.childNodes;
          for (var i=0;i<children.length;i++) {
            startSelection.checkPos(parentNode,i,resText);
            endSelection.checkPos(parentNode,i,resText);
            var node = children[i];
            //console.log(node);
            var nodeName = node.nodeName;
            if (nodeName=="BR") {
              if (node.className!="xsDummyFinalBR" && !suppressNL) resText+="\n";
            } else if (xsPTF.isPTFErrorTooltip(node)) {
       	   // ignore it - it is an error message.
            } else if (nodeName=="DIV" || nodeName=="P" || nodeName=="H1"||nodeName=="H2"||nodeName=="H3") {
               if ((resText.length>0 || hadDiv) && !suppressNL) resText+="\n";
               hadDiv=true;
               procNode(node);
            } else if (node.nodeType==3) { // text node
               startSelection.checkTextNode(node,resText);
               endSelection.checkTextNode(node,resText);
               // When you press enter at the end of a line, IE9 inserts <p>&nbsp;</p>
               if (node.textContent!="\xa0") {
            	   var toAdd = node.textContent.replace(/\xa0/g," ");
            	   if (suppressNL) toAdd=toAdd.replace(/\n/g,"");
            	   resText+=toAdd;
               }
            } else {
               procNode(node);
            };  
          }   
          startSelection.checkPos(parentNode,children.length,resText);
          endSelection.checkPos(parentNode,children.length,resText);
        };
        procNode(target);
        //console.log("start Range = "+startSelection.res+ " end range="+endSelection.res);
        return { text : resText, startSelection : startSelection.res, endSelection : endSelection.res };    	
    },
    
    setPTFSelection : function(target,startSelectionPosition,endSelectionPosition) {
 	   //console.log("trying to set selection");
        var sel = window.getSelection();
        var range = document.createRange();
        var sofar = 0;
        var procTextNode = function (node) {
           var len = node.textContent.length;
           var off1 = startSelectionPosition-sofar;
           if (off1>0 && off1<=len) range.setStart(node,off1);
           var off2 = endSelectionPosition-sofar;
           if (off2>0 && off2<=len) range.setEnd(node,off2);
           sofar+=len;
        };
        var procSingleNode = function (parentNode) {
          var newChildren = parentNode.childNodes;
          var checkHere = function(count) {
             //console.log("sofar="+sofar+"  count="+count+" startSelection="+startSelection.res+"parentNode="+parentNode);
             if (sofar==startSelectionPosition) range.setStart(parentNode,count);
             if (sofar==endSelectionPosition) range.setEnd(parentNode,count);
          };
          for (var i=0;i<newChildren.length;i++) {
            checkHere(i);
            var cn = newChildren[i];
            if (cn.nodeType==3) procTextNode(cn);
            else if (cn.nodeName=="SPAN") procSingleNode(cn);
            else if (cn.nodeName=="BR") sofar+=1;
            else if (cn.nodeName=="DIV") {
              if (xsPTF.isPTFErrorTooltip(cn)) {} // do nothing
              else {
         	   procSingleNode(cn);
         	   sofar+=1;
              }
            } else cn.shouldNotGetHere; // throw exception
          }
          checkHere(newChildren.length);
        };
        procSingleNode(target);
        sel.removeAllRanges();
        //console.log("Setting range "+range.startContainer+":"+range.startOffset+" to "+range.endContainer+":"+range.endOffset);
        sel.addRange(range);
    	
    },
    
    PTFinnerHTMLOfText : function(resText,errorList,isInGrid) {
        // now we need to convert the text into HTML that can be displayed. 
        //  1. We need to break up spans that have errors
        //  2. to make a final newline display on IE, we need to replace newlines by <br/> tags.
        //  3. we need to escape HTML text.
        var splitStartLocations = errorList.map(function(e) { return e.from; });
        var splitEndLocations = errorList.map(function(e) { return e.to; });
        var splitLocations = splitStartLocations.concat(splitEndLocations,[resText.length]);
        if (!this.isFirefox) { // split by newlines
     	   var newlinepos=0;
     	   while ((newlinepos=resText.indexOf("\n",newlinepos))!= -1) {
     		   splitLocations.push(newlinepos);
     		   splitLocations.push(newlinepos+1);
     		   // console.log("newlinepos="+newlinepos);
     		   newlinepos++;
     	   }
        }
        splitLocations.sort(function(a,b){return a-b;});  // locations to split. May have duplicates and excess -1s.
        var lastSplit = 0;
        var resLine = "";
        var resHTML = "";
        //console.log("Split locations = "+splitLocations);
        var endLine = function() { resHTML+="<div>"+resLine+"</div>"; resLine=""; };
        for (var i=0;i<splitLocations.length;i++) {
          var newSplit = splitLocations[i];
          if (newSplit>lastSplit && lastSplit<resText.length) {
            var splitText = this.escapeText(resText.slice(lastSplit,newSplit));
            if ((!this.isFirefox) && splitText=="\n") endLine();
            else {
              if (this.isFirefox) splitText = splitText.replace(/\n/g,"<br/>");
              var splitErrors = errorList.filter(function(e) { return e.from<=lastSplit && e.to>=newSplit;});
              //console.log("Split distance "+lastSplit+" to "+newSplit+" text "+splitText);
              //console.log("Errors "+splitErrors+" length "+splitErrors.length);
              if (splitErrors.length!=0) {
            	  var errorOpenDiv = isInGrid?"<div class='xsSpanErrorGridTooltip'>":"<div class='xsSpanErrorTooltip'>"; 
            	  var mouseCommands = isInGrid?" onmouseover='xs.grid.SlickGridCellMouseOver(event)' onmouseout='xs.grid.SlickGridCellMouseOut(event)'":"";	  
            	  splitText="<span class='xsSpanError xsSpanError"+this.worstSeverity(splitErrors)+"'"+mouseCommands+">"+errorOpenDiv+this.errorListAsHTML(splitErrors)+"</div>"+splitText+"</span>";
              }
              if (this.isFirefox) resHTML+=splitText;
              else resLine+=splitText;
            }
            lastSplit=newSplit;        	   
          };
        }
        if (this.isFirefox) resHTML+="<br class='xsDummyFinalBR'/>";
        else endLine();
        return resHTML;
    },
    
};

xsPTF.GetCursorPosition.prototype.checkPos = function(parentNode,numChildNodesProcessed,textSoFar) {
    if (parentNode==this.container && numChildNodesProcessed==this.offset) this.res=textSoFar.length;
  };
  xsPTF.GetCursorPosition.prototype.checkTextNode= function(textNode,textSoFar) {
    if (textNode==this.container) this.res==textSoFar.length+offset;
  };






