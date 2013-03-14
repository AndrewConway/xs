/**
 * Copyright 2013 Andrew Conway. All rights reserved.
 *
 * Code to use SlickGrid with xs.
 * Should be referenced after xsedit.js and after jquery.
 */

xs.grid = {

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
    
    SlickGridCellMouseOver : function(event) {
       //console.log("Mouse over for "+event.target);	
       // find the grid
       var elem = event.target;
       var grid = xs.grid.getGridFromAncestor(elem);
       if (!grid) return;
       var o1 = $(elem).offset();
       var o2 = $(grid).offset();
       var offx = o1.left-o2.left+8;
       var offy = o1.top-o2.top+12;
       //console.log("offx = "+offx+" offy="+offy);
       //console.log(elem);
       if (elem.childNodes && elem.childNodes[0] && grid.childNodes && grid.childNodes[1]) {
    	   var html = elem.childNodes[0].innerHTML;
    	   var dest = grid.childNodes[1];
    	   dest.innerHTML=html;
    	   dest.setAttribute("data-tooltipsource",elem.id);
    	   dest.setAttribute("style","display:block; color:red; left:"+offx+"px; top:"+offy+"px;");
       }
    },

    removeSlickGridTooltip : function(element,requireID) {
    	//console.log("Remove slick Grid tooltip "+element);
        var grid = xs.grid.getGridFromAncestor(element);
        if (grid && grid.childNodes && grid.childNodes[1]) {
     	   var dest = grid.childNodes[1];
     	   if ((!requireID) || dest.getAttribute("data-tooltipsource")==requireID) {
         	   dest.removeAttribute("data-tooltipsource");
         	   dest.innerHTML="";
         	   dest.setAttribute("style","");     		   
     	   }
        }    	
    },
    
    SlickGridCellMouseOut : function(event) {
        //console.log("Mouse out for "+event.target);	
        var elem = event.target;
        xs.grid.removeSlickGridTooltip(elem,elem.id);
     },
    
     surroundWithErrorCheck : function(id,base) {
     	return "<div class='xsErrorLabeled'><div class='xsErrorIconHolder'><div id='"+id+"_erroricon' class='xsErrorIcon xsErrorIcon1000' onmouseover='xs.grid.SlickGridCellMouseOver(event)' onmouseout='xs.grid.SlickGridCellMouseOut(event)'><div id='"+id+"_errortooltip' style='display:none;'></div></div></div>"+base+"</div>";    	 
     },
    
    SlickGridPTFFormatter : function(row, cell, value, columnDef, dataContext) {
    	var id = columnDef.mainID+"_grid_R"+row+"C"+cell;
    	var text = value || "" ;
    	var errorlist = [];
    	//console.log("Slick formatter for "+text);
	    var elem = document.getElementById(columnDef.mainID+"_ui");
		if (elem && elem.xsCellErrorLists && elem.xsCellErrorLists[id]) {
		  setTimeout(function() {xsPTF.setErrorList(id,elem.xsCellErrorLists[id]);},1); // needs to be done after creation
	    }
		setTimeout(function() {xs.grid.checkForOverflow(id);},1); // needs to be done after creation.
		var base="<div id='"+id+"' class='xsPseudoTextField'>"+xsPTF.PTFinnerHTMLOfText(text,errorlist,true)+"</div>";
		return xs.grid.surroundWithErrorCheck(id,base);
      },
    
      
    SlickGridPTFEditor : function(args) {
        var $input;
        var defaultValue;
        var scope = this;
    	var id;
    	var target;

        var $wrapper;

        this.init = function () {
          var gridpos = args.grid.getActiveCell();
          //console.log(gridpos);
          //console.log(gridpos.row);
      	  id = args.column.mainID+"_grid_R"+gridpos.row+"C"+gridpos.cell;
          // id = args.grid.xsPTFid+"_R"+gridpos.row+"C"+gridpos.cell+"_grid_ui";
          //console.log(id);
          var $container = $("body");
          //console.log("Container width ="+$(args.container).css("width"));
          // <div class="xsErrorIconHolder"></div>
          $wrapper = $("<div class='xsErrorLabeled xsEditorAbsolutelyPositioned'><div class='xsErrorIconHolder'><div class='xsErrorIcon xsErrorIcon1000' id='"+id+"_erroricon'><div id='"+id+"_errortooltip'></div></div></div></div>")
              .appendTo($container);

          $input = $("<div id='"+id+"' class='xsPseudoTextField' data-xsSuppressNL='"+ (!args.column.multiline)+"' data-text='' xs-data-gridRow='"+gridpos.row+"' xs-data-gridCol='"+args.column.id+"' data-onInputObj='"+args.grid.xsPTFonInputObj+"' contenteditable='true' spellcheck='false' oninput='xsPTF.input(event)' onkeyup='xsPTF.inputSurrogate(event)' onblur='xsPTF.inputSurrogate(event)' onpaste='xsPTF.inputSurrogate(event)' oncut='xsPTF.inputSurrogate(event)' ></div>")
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
          var res= value && (!(value == "" && defaultValue == null)) && (value != defaultValue);
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
        var rows = slickgrid.getData();
        if (rows.length>row) rows[row][colfield]=text;
    	slickgrid.xsChangeGrid(row,colfield,text);
    	
    },
    
    SlickGridBooleanFormatter : function(row, cell, value, columnDef, dataContext) {
      	var id = columnDef.mainID+"_grid_R"+row+"C"+cell;
      	var isChecked = value=="true";
      	//console.log("isChecked = "+isChecked);
      	var base = "<span id='"+id+"_ui' class='xsGridCheckbox"+isChecked+"' onclick='xs.grid.clickSlickGridBoolean(event.target)' data-gridRow='"+row+"' data-gridColField='"+columnDef.field+"' tabindex='0' role='button'>"+(isChecked?"✓":"✗")+"</span>";
    	//console.log("Slick formatter for "+text);
	    var elem = document.getElementById(columnDef.mainID+"_ui");
		if (elem && elem.xsCellErrorLists && elem.xsCellErrorLists[id]) {
		  setTimeout(function() {xsPTF.setErrorList(id,elem.xsCellErrorLists[id]);},1); // needs to be done after creation
	    }
		return xs.grid.surroundWithErrorCheck(id,base);
     },
     
     SlickGridBooleanEditor : function(args) {
         var gridpos = args.grid.getActiveCell();
         var datarow = args.grid.getData()[gridpos.row];
       	 var id = args.column.mainID+"_grid_R"+gridpos.row+"C"+gridpos.cell;
       	 var getElem = function() { return document.getElementById(id+"_ui"); };
       	 var defaultValue = (datarow&&(datarow[args.column.field]=="true"))?"true":"false";
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
             
         };

         this.save = function () {  args.commitChanges();  };
         this.cancel = function () {  args.cancelChanges();  };

         this.destroy = function () {  $input.remove(); };
         this.focus = function () { $input.focus(); };

         this.loadValue = function (item) {
            getElem().innerHTML=item[args.column.field]=="true"?"✓":"✗";
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
       	var id = columnDef.mainID+"_grid_R"+row+"C"+cell;
     	var choices = columnDef.choices;
       	var base = "";
       	for (var i=0;i<choices.length;i++) {
       		var c = choices[i];
       		if (c.original==value) base=xsPTF.escapeText(c.localized); 
       	}
     	//console.log("Slick formatter for "+text);
 	    var elem = document.getElementById(columnDef.mainID+"_ui");
 		if (elem && elem.xsCellErrorLists && elem.xsCellErrorLists[id]) {
 		  setTimeout(function() {xsPTF.setErrorList(id,elem.xsCellErrorLists[id]);},1); // needs to be done after creation
 	    }
 		return xs.grid.surroundWithErrorCheck(id,base);
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
            var rows = slickgrid.getData();
            if (rows.length>row) rows[row][colfield]=text;
        	slickgrid.xsChangeGrid(row,colfield,text);
    	}

     },
     
     SlickGridChoiceEditor : function(args) {
         var gridpos = args.grid.getActiveCell();
         var datarow = args.grid.getData()[gridpos.row];
       	 var id = args.column.mainID+"_grid_R"+gridpos.row+"C"+gridpos.cell;
       	 var getElem = function() { return document.getElementById(id+"_ui"); };
       	 var choices = args.column.choices;
       	 var defaultValue = datarow&&(datarow[args.column.field]);
       	 var inputBase = "<select id='"+id+"_ui' data-gridRow='"+gridpos.row+"' data-gridColField='"+args.column.field+"' onchange='xs.grid.choiceEditorChange(event.target)'>";
       	 for (var i=0;i<choices.length;i++) {
       		 var c=choices[i];
       		 inputBase+='<option value="'+xsPTF.escapeTextIncludingDoubleQuotes(c.original)+'"';   
       		 if (c.original==defaultValue) inputBase+=' selected="selected"';
       		 inputBase+='>'+xsPTF.escapeText(c.localized)+'</option>';
       	 }
       	 inputBase+="</select>";
         var $input=$(xs.grid.surroundWithErrorCheck(id,inputBase));

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
             
         };


         this.destroy = function () {  $input.remove(); };
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




