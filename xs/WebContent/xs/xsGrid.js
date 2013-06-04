/**
 * Copyright 2013 Andrew Conway. All rights reserved.
 *
 * Code to use SlickGrid with xs.
 * Should be referenced after xsedit.js and after jquery.
 */

xs.grid = {

    majorDataChange : function (elem,restoreEditorData) {
		var slickgrid = elem.dataxs_sg;
		if (slickgrid) {
			var saved = xs.saveSlickGridEditor(slickgrid);
			slickgrid.setData(xs.grid.getDataModel(elem)); 
			slickgrid.updateRowCount();
			slickgrid.render();
			xs.restoreSlickGridEditor(slickgrid, saved,restoreEditorData);
		};    	
    },
    
    getDataModel : function(elem) {
    	return {
			getLength : function () { return elem.dataxs_rows.length; },
			getItem : function (index) { return elem.dataxs_rows[index]; },
			getItemMetadata : function(index) { return elem.dataxs_rowmetadata&&elem.dataxs_rowmetadata.length>index?elem.dataxs_rowmetadata[index]:null;}
		};
    },
    
    checkForOverflow : function(id) {
    	var tf = document.getElementById(id);
    	if (tf) {
    		//console.log("For "+id);
    		//console.log("scrollHeight = "+tf.scrollHeight+"  scrollWidth = "+tf.scrollWidth);
    		//console.log("clientHeight = "+tf.clientHeight+"  clientWidth = "+tf.clientWidth);
    		if (tf.parentNode && tf.parentNode.parentNode) {
    			var container = tf.parentNode.parentNode;
    			//console.log("Container");
        		//console.log("scrollHeight = "+container.scrollHeight+"  scrollWidth = "+container.scrollWidth);
        		//console.log("clientHeight = "+container.clientHeight+"  clientWidth = "+container.clientWidth);
        		if (container.scrollHeight>container.clientHeight || container.scrollWidth>container.clientWidth) { // overflow
        			$(tf).addClass("xsOverflowed");
        		}
    		}
    	}
    },
    
    /** Get, if it exists, the most recent ancestor of elem that is of class xsTableHolder  */
    getGridFromAncestor : function(elem) {
        var grid = elem;
        while (grid && ! /(^| )xsTableHolder( |$)/.test(grid.className)) grid=grid.parentNode;
        return grid;    	
    },

    //
    // Tooltips (for errors and normal tooltips)
    // These are complicated by the fact that the cells and tables have overflow:hidden, which means the tooltips get trimmed.
    // To get around this we make the tooltips absolutely positioned outside the table.
    //
    
    /** tooltipType=1 for normal tooltip and 2 for error  */
    insertSlickGridTooltip : function(elem,tooltipType) {
       //console.log("insertSlickGridTooltip Tooltip type "+tooltipType);
       var grid = xs.grid.getGridFromAncestor(elem);
       if (!grid) return;
       var o1 = $(elem).offset();
       var o2 = $(grid).offset();
       var offx = o1.left-o2.left+8;
       var offy = o1.top-o2.top+14;
       //console.log("offx = "+offx+" offy="+offy);
       //console.log(elem);
       if (elem.childNodes && elem.childNodes[0] && grid.childNodes && grid.childNodes[tooltipType]) {
    	   var html = elem.childNodes[0].innerHTML;
    	   var dest = grid.childNodes[tooltipType];
    	   dest.innerHTML=html;
    	   dest.setAttribute("data-tooltipsource",elem.getAttribute("data-tooltipFor"));
    	   var newStyle = (html=="")? "":("display:block; left:"+offx+"px; top:"+offy+"px;");  
    	   dest.setAttribute("style",newStyle);
       }
    },

    /** tooltipType=1 for normal tooltip and 2 for error */
    removeSlickGridTooltip : function(element,tooltipType) {
    	var requireID = element.getAttribute("data-tooltipFor");
    	//console.log("Remove slick Grid tooltip "+element+" type="+tooltipType);
        var grid = xs.grid.getGridFromAncestor(element);
        //console.log(grid);
        if (grid && grid.childNodes && grid.childNodes[tooltipType]) {
     	   var dest = grid.childNodes[tooltipType];
     	   //console.log("expectedid="+dest.getAttribute("data-tooltipsource")+"   actual="+requireID);
     	   //console.log(requireID);
     	   if ((!requireID) || (requireID=="") || dest.getAttribute("data-tooltipsource")==requireID) {
     		   //console.log("Removing");
         	   dest.removeAttribute("data-tooltipsource");
         	   dest.innerHTML="";
         	   dest.setAttribute("style","");     		   
     	   }
        }    	
    },
    
    /** Get, if it exists, the most recent ancestor of elem that is of class xsGridTooltipHolder  */
    getGridTooltipHolderFromAncestor : function(elem) {
        var grid = elem;
        while (grid && ! /(^| )xsGridTooltipHolder( |$)/.test(grid.className)) grid=grid.parentNode;
        return grid;    	
    },

    SlickGridCellMouseOverPlainTooltip : function(event) {
    	var elem = xs.grid.getGridTooltipHolderFromAncestor(event.target);
    	//console.log("Over");
    	//console.log(event.target);
    	//console.log("Parent");
    	//console.log(elem);
    	if (!elem) return
        xs.grid.insertSlickGridTooltip(elem,1);
    },
    
    SlickGridCellMouseOutPlainTooltip : function(event) {
    	var elem = xs.grid.getGridTooltipHolderFromAncestor(event.target);
    	//console.log("Out");
    	//console.log(event.target);
    	//console.log("Parent");
    	//console.log(elem);
    	if (!elem) return
        xs.grid.removeSlickGridTooltip(elem,1);
    },
    
    SlickGridCellMouseOutError : function(event) {
        //console.log("Mouse out for "+event.target);	
        //console.log(event.target);
        var elem = event.target;
        xs.grid.removeSlickGridTooltip(elem,2);
     },

     SlickGridCellMouseOverError : function(event) {
         var elem = event.target;
         xs.grid.insertSlickGridTooltip(elem,2);
      },
      
      startEditingCell : function(gridID,cellID) {
     	 //console.log("startEditingCell("+gridID+","+cellID+")");
     	 var gridelem = document.getElementById(gridID+"_ui");
     	 if (gridelem && gridelem.childNodes) {
     		 var tooltip = gridelem.childNodes[1];
     		 if (tooltip) {
     			var currentlyShowing = tooltip.getAttribute("data-tooltipsource");
//     			console.log("currently showing "+currentlyShowing);
     			if (currentlyShowing==cellID) { // suppress current tooltip
     				tooltip.setAttribute("style","");
     			}
     		 }
     		 var tooltiperror = gridelem.childNodes[2];
     		 if (tooltiperror) {
     			var currentlyShowing = tooltiperror.getAttribute("data-tooltipsource");
//     			console.log("currently showing error "+currentlyShowing);
     			if (currentlyShowing==cellID) { // suppress current tooltip
     				tooltiperror.setAttribute("style","");
     			}
     		 }
     	 }
      },
      endEditingCell : function(gridID,cellID) {
//     	 console.log("endEditingCell("+gridID+","+cellID+")");
      },
      

    
     surroundWithErrorCheck : function(id,base,mainTableElem) {
 		if (mainTableElem && mainTableElem.xsCellErrorLists && mainTableElem.xsCellErrorLists[id]) {
 			  setTimeout(function() {xsPTF.setErrorList(id,mainTableElem.xsCellErrorLists[id]);},1); // needs to be done after creation
 		}
    	var tooltipContents = "";
    	if (mainTableElem && mainTableElem.xsTooltips && mainTableElem.xsTooltips[id]) tooltipContents=mainTableElem.xsTooltips[id];
    	// onmouseenter='xs.grid.SlickGridCellMouseOver(event,2)' onmouseout='xs.grid.SlickGridCellMouseOutPlainTooltip(event,1)'
    	var withTooltip = "<div class='xsGridTooltipHolder' data-tooltipFor='"+id+"'><div id='"+id+"_tooltip' class='xsTooltip'>"+tooltipContents+"</div>"+base+"</div>"; 
    	var errorHolder = "<div class='xsErrorIconHolder'><div data-tooltipFor='"+id+"' id='"+id+"_erroricon' class='xsErrorIcon xsErrorIcon1000' onmouseover='xs.grid.SlickGridCellMouseOverError(event)' onmouseout='xs.grid.SlickGridCellMouseOutError(event)'><div id='"+id+"_errortooltip' style='display:none;'></div></div></div>";
     	return "<div class='xsErrorLabeled'>"+errorHolder+withTooltip+"</div>";    	 
     },
     
    
    SlickGridPTFFormatter : function(row, cell, value, columnDef, dataContext) {
    	//console.log(columnDef);
    	var id = columnDef.mainID+"_grid_R"+row+"C"+columnDef.id;
    	var text = value || "" ;
    	var errorlist = [];
    	//console.log("Slick formatter for "+text);
	    var elem = document.getElementById(columnDef.mainID+"_ui");
		setTimeout(function() {xs.grid.checkForOverflow(id);},1); // needs to be done after creation.
		var base="<div id='"+id+"' class='xsPseudoTextField' onmouseover='xs.grid.SlickGridCellMouseOverPlainTooltip(event)' onmouseout='xs.grid.SlickGridCellMouseOutPlainTooltip(event)'>"+xsPTF.PTFinnerHTMLOfText(text,errorlist,true,id)+"</div>";
		return xs.grid.surroundWithErrorCheck(id,base,elem);
      },
    
      
    SlickGridPTFEditor : function(args) {
        var gridpos = args.grid.getActiveCell();
        var $input;
        var defaultValue;
        var scope = this;
    	var id= args.column.mainID+"_grid_R"+gridpos.row+"C"+args.column.id;;
    	var target;

        var $wrapper;

        this.init = function () {
          //console.log(gridpos);
          //console.log(gridpos.row);
          //console.log(id);
          var $container = $("body");
          //console.log("Container width ="+$(args.container).css("width"));
          // <div class="xsErrorIconHolder"></div>
          $wrapper = $("<div class='xsErrorLabeled xsEditorAbsolutelyPositioned'><div class='xsErrorIconHolder'><div class='xsErrorIcon xsErrorIcon1000' id='"+id+"_erroricon'><div id='"+id+"_errortooltip'></div></div></div></div>")
              .appendTo($container);

          $input = $("<div id='"+id+"' class='xsPseudoTextField' data-ontablepasteobj='"+args.grid.xsPTFonInputObj+"' data-xsSuppressNL='"+ (!args.column.multiline)+"' data-text='' xs-data-gridRow='"+gridpos.row+"' xs-data-gridCol='"+args.column.id+"' data-onInputObj='"+args.grid.xsPTFonInputObj+"' contenteditable='true' spellcheck='false' oninput='xsPTF.input(event)' onkeyup='xsPTF.inputSurrogate(event)' onblur='xsPTF.inputSurrogate(event)' onpaste='xsPTF.inputSurrogate(event)' oncut='xsPTF.inputSurrogate(event)'  onmouseover='xs.grid.SlickGridCellMouseOverPlainTooltip(event)' onmouseout='xs.grid.SlickGridCellMouseOutPlainTooltip(event)'></div>")
              .appendTo($wrapper);
          target = document.getElementById(id);
          var elem = document.getElementById(args.column.mainID+"_ui");
  		  if (elem && elem.xsCellErrorLists && elem.xsCellErrorLists[id]) {
  		    xsPTF.setErrorList(id,elem.xsCellErrorLists[id]);
  	      }
          xs.grid.removeSlickGridTooltip(args.container);
          $(target).css("min-width",$(args.container).css("width"));
          $(target).css("min-height",$(args.container).css("height"));
          $input.bind("keydown", xs.grid.handleKeyDown({
        	canUseEnter: (args.column.multiline==true),
          	save:function() { args.commitChanges(); },
          	cancel:function() {xsPTF.validate(target,{overrideText:defaultValue}); xsPTF.callbackOnChange(target,defaultValue); args.cancelChanges();  },
          	grid:args.grid
          }));

          scope.position(args.position);
          $input.focus().select();
          var cellclasses = args.grid.getCellCssStyles("xsTotallyIllegal");
          if (cellclasses) {
        	  var cellclassesrow = cellclasses[gridpos.row];
        	  if (cellclassesrow) {
        		  var cellclassescell = cellclassesrow[args.column.id];
        		  if (cellclassescell) $(target).addClass("xsTotallyIllegal");
        	  }
          }
          xs.grid.startEditingCell(args.column.mainID,id);
        };
        
        this.getTarget = function () { return target; };

        /* There is a bug in SlickGrid where if the cell is only partly visible, it is hidden - even if 99% visible. Workaround - don't have hide and show functions.
        this.hide = function () {
          $wrapper.hide();
        };

        this.show = function () {
          $wrapper.show();
        };
*/
        this.position = function (position) {
          $wrapper
              .css("top", position.top+1)
              .css("left", position.left+1);
        };

        this.destroy = function () {
        	//console.log("Destroy "+id);
          $wrapper.remove();
          xs.grid.endEditingCell(args.column.mainID,id); 
        };

        this.focus = function () {
        	//console.log("Focus "+id);
          $input.focus();
        };

        this.getValue = function () {
          var value = target.getAttribute("data-text") || "";
          //console.log("getValue "+id+" ="+value);
          return value;
        };

        this.setValue = function (val) {
            //console.log("setValue "+id+" to "+val);
        	xsPTF.validate(target,{overrideText:val});
        };

        this.loadValue = function (item) {
          defaultValue = item[args.column.field] || "";
          //console.log("loadValue "+id+" to "+defaultValue);
       	  xsPTF.validate(target,{overrideText:defaultValue,selectAll:true});
          //$input[0].defaultValue = defaultValue;
          $input.select();
        };

        this.serializeValue = function () {
            var value = target.getAttribute("data-text") || "";
           // console.log("serializeValue "+id+" ="+value);
            return value;
        };

        this.applyValue = function (item, state) {
         // console.log("applyValue "+id+" to "+state);
          item[args.column.field] = state;
        };

        this.isValueChanged = function () {
          var value = target.getAttribute("data-text") || "";
          var res= (!(value == "" && defaultValue == null)) && (value != defaultValue);
          //console.log("isValueChanged "+id+" ="+res);
          return res;
        };

        this.validate = function () {
         // console.log("validate");
          if (args.column.validator) {
            var validationResults = args.column.validator(target.getAttribute("data-text"));
            if (!validationResults.valid) {
              return validationResults;
            }
          }

          return {
            valid: true,
            msg: null
          };
        };

        this.init();
    	
    },

    handleKeyDown : function(args) {
      return function (e) {
    	//console.log(e.which);
        if (e.which == $.ui.keyCode.ENTER && (e.ctrlKey || !args.canUseEnter) && !args.preventDoubleNav) {
            e.preventDefault();
          args.save();
        } else if ((e.which == $.ui.keyCode.ESCAPE) && args.cancel) {
          e.preventDefault();
          args.cancel();
        } else if (e.which == $.ui.keyCode.TAB && e.shiftKey && !args.preventDoubleNav) {
          e.preventDefault();
          args.grid.navigatePrev();
        } else if (e.which == $.ui.keyCode.SPACE && args.onspace) {
            e.preventDefault();
            args.onspace();
        } else if (e.which == $.ui.keyCode.TAB && !args.preventDoubleNav) {
          e.preventDefault();
          args.grid.navigateNext();
        }
      };
    },

    isTrueString : function(string) {
    	return string && string.toUpperCase && string.toUpperCase()=="TRUE";
    },
    
    clickSlickGridBoolean : function(elem) {
    	//console.log("Invert");
    	var row = elem.getAttribute("data-gridRow");
    	var colfield = elem.getAttribute("data-gridColField");
    	var isChecked = elem.innerHTML=="✗";
    	elem.innerHTML=isChecked?"✓":"✗";
    	var text = isChecked?"true":"false";
        elem.setAttribute("class","xsGridCheckbox"+isChecked);
        var gridholder = xs.grid.getGridFromAncestor(elem);
        var slickgrid = gridholder.dataxs_sg;
        var rows = slickgrid.getData().getItem(row);
        if (rows) rows[colfield]=text;
    	slickgrid.xsChangeGrid(row,colfield,text);
    	
    },
    
    SlickGridBooleanFormatter : function(row, cell, value, columnDef, dataContext) {
      	var id = columnDef.mainID+"_grid_R"+row+"C"+columnDef.id;
      	var isChecked = xs.grid.isTrueString(value);
      	//console.log("isChecked = "+isChecked);
      	var base = "<span id='"+id+"_ui' class='xsGridCheckbox"+isChecked+"' onclick='xs.grid.clickSlickGridBoolean(event.target)' data-gridRow='"+row+"' data-gridColField='"+columnDef.field+"' tabindex='0' role='button' onmouseover='xs.grid.SlickGridCellMouseOverPlainTooltip(event)' onmouseout='xs.grid.SlickGridCellMouseOutPlainTooltip(event)'>"+(isChecked?"✓":"✗")+"</span>";
    	//console.log("Slick formatter for "+text);
	    var elem = document.getElementById(columnDef.mainID+"_ui");
		return xs.grid.surroundWithErrorCheck(id,base,elem);
     },
     
     SlickGridBooleanEditor : function(args) {
         var gridpos = args.grid.getActiveCell();
         var datarow = args.grid.getData().getItem(gridpos.row);
       	 var id = args.column.mainID+"_grid_R"+gridpos.row+"C"+args.column.id;
       	 var getElem = function() { return document.getElementById(id+"_ui"); };
       	 var defaultValue = (datarow&&(xs.grid.isTrueString(datarow[args.column.field])))?"true":"false";
         var $input=$(xs.grid.SlickGridBooleanFormatter(gridpos.row,gridpos.cell,defaultValue,args.column,null));
         var getValue = function() {
             return (getElem().innerHTML=="✓")?"true":"false";
         };
         
         this.init = function () {
            $input.bind("keydown", xs.grid.handleKeyDown({
            	canUseEnter:false,
            	save:function() { args.commitChanges(); },
            	cancel:function() { args.cancelChanges(); },
            	grid:args.grid,
            	preventDoubleNav:true,
            	onspace:function() { xs.grid.clickSlickGridBoolean(getElem());}
            }));
             $input.appendTo(args.container);
              xs.grid.removeSlickGridTooltip(args.container);
             $(getElem()).focus();
             //$input.focus().select();
             xs.grid.startEditingCell(args.column.mainID,id);
         };

         this.save = function () {  args.commitChanges();  };
         this.cancel = function () {  args.cancelChanges();  };

         this.destroy = function () {  $input.remove(); xs.grid.endEditingCell(args.column.mainID,id); };
         this.focus = function () { $input.focus(); };

         this.loadValue = function (item) {
            getElem().innerHTML=xs.grid.isTrueString(item[args.column.field])?"✓":"✗";
         };

           this.serializeValue = getValue;

           this.applyValue = function (item, state) {
             item[args.column.field] = state;
           };

           this.isValueChanged = function () {
             return (getValue() != defaultValue);
           };

         
         this.validate = function () {
           return {
             valid: true,
             msg: null
           };
         };

         this.init();

     },
     
     SlickGridChoiceFormatter : function(row, cell, value, columnDef, dataContext) {
       	var id = columnDef.mainID+"_grid_R"+row+"C"+columnDef.id;
     	var choices = columnDef.choices;
       	var base = "";
       	for (var i=0;i<choices.length;i++) {
       		var c = choices[i];
       		if (c.original==value) base=xsPTF.escapeText(c.localized); 
       	}
      	base = "<span onmouseover='xs.grid.SlickGridCellMouseOverPlainTooltip(event)' onmouseout='xs.grid.SlickGridCellMouseOutPlainTooltip(event)'>"+base+"</span>";
     	//console.log("Slick formatter for "+text);
 	    var elem = document.getElementById(columnDef.mainID+"_ui");
 		return xs.grid.surroundWithErrorCheck(id,base,elem);
      },
      

     
     choiceEditorChange : function(elem) {
     	var row = elem.getAttribute("data-gridRow");
    	var colfield = elem.getAttribute("data-gridColField");
    	var ind = elem.selectedIndex;
    	var options = elem.options;
    	if (ind>=0 && ind<options.length) {
        	var text = options[ind].value;
            var gridholder = xs.grid.getGridFromAncestor(elem);
            var slickgrid = gridholder.dataxs_sg;
            var rows = slickgrid.getData().getItem(row);
            if (rows) rows[colfield]=text;
        	slickgrid.xsChangeGrid(row,colfield,text);
    	}

     },
     
     SlickGridChoiceEditor : function(args) {
         var gridpos = args.grid.getActiveCell();
         //console.log(args.grid.getData());
         var datarow = args.grid.getData().getItem(gridpos.row);
       	 var id = args.column.mainID+"_grid_R"+gridpos.row+"C"+args.column.id;
       	 var getElem = function() { return document.getElementById(id+"_ui"); };
       	 var choices = args.column.choices;
       	 //console.log(datarow);
       	 //console.log(args.column.field);
       	 var defaultValue = datarow&&(datarow[args.column.field]);
       	 //console.log("choice defaultValue = "+defaultValue);
       	 var inputBase = "<select id='"+id+"_ui' data-gridRow='"+gridpos.row+"' data-gridColField='"+args.column.field+"' onchange='xs.grid.choiceEditorChange(event.target)'>";
       	 for (var i=0;i<choices.length;i++) {
       		 var c=choices[i];
       		 inputBase+='<option value="'+xsPTF.escapeTextIncludingDoubleQuotes(c.original)+'"';   
       		 if (c.original==defaultValue) inputBase+=' selected="selected"';
       		 inputBase+='>'+xsPTF.escapeText(c.localized)+'</option>';
       	 }
       	 inputBase+="</select>";
         var $input=$(xs.grid.surroundWithErrorCheck(id,inputBase,null));

         var getValue = function() {
        	 var ind = getElem().selectedIndex;
        	 if (ind>=0 && ind<choices.length) return choices[ind].original;
        	 else return "";
         };
         
         this.init = function () {
            $input.bind("keydown", xs.grid.handleKeyDown({
            	canUseEnter:true,
            	save:function() { args.commitChanges(); },
            	cancel:function() {datarow[args.column.field]=defaultValue; args.grid.xsChangeGrid(gridpos.row,args.column.field,defaultValue); args.cancelChanges();  },
            	grid:args.grid,
            	preventDoubleNav:true
            }));
             $input.appendTo(args.container);
             var elem = document.getElementById(args.column.mainID+"_ui");
   		     if (elem && elem.xsCellErrorLists && elem.xsCellErrorLists[id]) {
   		        xsPTF.setErrorList(id,elem.xsCellErrorLists[id]);
   	         }
             xs.grid.removeSlickGridTooltip(args.container);

             $input.focus();
             //$input.focus().select();
             xs.grid.startEditingCell(args.column.mainID,id);
         };


         this.destroy = function () {  $input.remove(); xs.grid.endEditingCell(args.column.mainID,id); };
         this.focus = function () { $input.focus(); };

         this.loadValue = function (item) {
            // already done in constructor
         };

           this.serializeValue = getValue;

           this.applyValue = function (item, state) {
             item[args.column.field] = state;
           };

           this.isValueChanged = function () {
             return (getValue() != defaultValue);
           };

         
         this.validate = function () {
           return {
             valid: true,
             msg: null
           };
         };

         this.init();

     }

};




