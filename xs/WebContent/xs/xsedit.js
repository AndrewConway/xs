 
function xsProcessClientMessageFromServer(json,session) {
  if (json!=null) {
	if (json.commands) {
		json.commands.map(function(c) { xsProcessClientMessageFromServer(c,session);});
	}
	if (json.cmd) {
		if (json.cmd=="SetAtt") {
			var elem = document.getElementById(json.args[0]);
            if (elem) elem.setAttribute(json.args[1],json.args[2]);
		} else if (json.cmd=="RemoveAtt") {
			var elem = document.getElementById(json.args[0]);
            if (elem) elem.removeAttribute(json.args[1]);
		} else if (json.cmd=="SetHTML") {
			var elem = document.getElementById(json.args[0]);
            if (elem) elem.innerHTML=json.args[1];
		} else if (json.cmd=="SetValue") {
			var elem = document.getElementById(json.args[0]);
            if (elem) {
            	if (xsPTF.isPTF(elem)) xsPTF.validate(elem,{overrideText:json.args[1]});
            	else elem.value=json.args[1];
            }
		} else if (json.cmd=="SetChecked") {
			var elem = document.getElementById(json.args[0]);
            if (elem) elem.checked= (json.args[1]=="true");
		} else if (json.cmd=="SetCurrentlyEditing") {
			xs[json.args[0]].currentlyEditing=json.args[1];
		} else if (json.cmd=="Remove") {
			$(json.args[0]).remove();
		} else if (json.cmd=="AddAfter") {
			$(json.args[0]).after(json.args[1]);
		} else if (json.cmd=="AddAtStart") {
			$(json.args[0]).prepend(json.args[1]);
		} else if (json.cmd=="AddAtEnd") {
			$(json.args[0]).append(json.args[1]);
		} else if (json.cmd=="AddClass") {
			$(json.args[0]).addClass(json.args[1]);
		} else if (json.cmd=="RemoveClass") {
			$(json.args[0]).removeClass(json.args[1]);
		} else if (json.cmd=="GotoURL") {
			window.location.href=json.args[0];
		} else if (json.cmd=="GotoURLNewTab") {
			window.open(json.args[0],'_blank');
		} else if (json.cmd=="message") {
			alert(json.args[0]);
		} else if (json.cmd=="ToolbarStatus") {
			var elem = document.getElementById(json.args[0]);
			elem.disabled = json.args[1]=='false';
			elem.innerHTML=json.args[2];
		} else if (json.cmd=="Run") {
			console.log("Got to Run with "+json.args[0]);
			try { eval(json.args[0]);} catch(err) { console.log(err);}
		} else if (json.cmd=="Show") {
			$(json.args[0]).show();
		} else if (json.cmd=="Hide") {
			$(json.args[0]).hide();
		} else if (json.cmd=="Enable") {
			$(json.args[0]).removeAttr('disabled');
		} else if (json.cmd=="Disable") {
			$(json.args[0]).attr('disabled', 'disabled');
		} else if (json.cmd=="LostSession") { 
			session.errorInServerConnection("LostSession");
		} else if (json.cmd=="SetGridTooltip") {
		    var id = json.args[0];	
		    var html = json.args[1];
			var elem = document.getElementById(id+"_tooltip");
            if (elem) elem.innerHTML=html;
            var gridelem = document.getElementById(json.args[2]);
            if (gridelem) {
				  if (!gridelem.xsTooltips) gridelem.xsTooltips={};
				  gridelem.xsTooltips[id]=html;
				  // see if tooltip is currently being displayed.
				  if (gridelem.childNodes && gridelem.childNodes[1]) {
					  var tooltip = gridelem.childNodes[1];
					  var currentID = tooltip.getAttribute("data-tooltipsource");
					  if (currentID==id) {
						  tooltip.innerHTML=html;
					  }
				  }
            }
		} else if (json.cmd=="Errors") {
			xsPTF.setErrorList(json.id,json.errors);
			if (json.gridID) {
			  var elem = document.getElementById(json.gridID+"_ui");
			  if (elem) {
				  if (!elem.xsCellErrorLists) elem.xsCellErrorLists={};
				  elem.xsCellErrorLists[json.id]=json.errors;
				  // Should really check to see if the error is currently being displayed...
			  }
			}
		} else if (json.cmd=="SetRows") {
			var elem = document.getElementById(json.id+"_ui");
			elem.dataxs_rows = json.rows;
			xs.grid.majorDataChange(elem,false);
		} else if (json.cmd=="SetGridRowMetadata") {
			var elem = document.getElementById(json.id+"_ui");
			elem.dataxs_rowmetadata = json.rows;
			xs.grid.majorDataChange(elem,true);
		} else if (json.cmd=="SetRow") {
			var elem = document.getElementById(json.id+"_ui");
			elem.dataxs_rows[json.num] = json.row;
			var slickgrid = elem.dataxs_sg;
			if (slickgrid) {
				var saved = xs.saveSlickGridEditor(slickgrid);
			    slickgrid.invalidateRow(json.num);
				slickgrid.render(); 
				xs.restoreSlickGridEditor(slickgrid, saved,false);
			}
		} else if (json.cmd=="GridSetCellCssStyles") {
			var elem = document.getElementById(json.id+"_ui");
			var slickgrid = elem.dataxs_sg;
			if (slickgrid) {
				var saved = xs.saveSlickGridEditor(slickgrid);
				var oldstyles = slickgrid.getCellCssStyles(json.key) || {};
				slickgrid.setCellCssStyles(json.key,json.what);
				for (var row=0;row<slickgrid.getDataLength();row++) {
					if ((json.what[row] || oldstyles[row]) && json.what[row]!=oldstyles[row])
				        slickgrid.invalidateRow(row);
				}			    
				slickgrid.render(); 
				xs.restoreSlickGridEditor(slickgrid, saved,true);
			}			
		} else if (json.cmd=="StartGrid") {
			var id = json.args[0]+"_ui";
			var elem = document.getElementById(id);
			var dialogid = id+"_export";
			$(elem).append("<div id='"+dialogid+"' title='Copy to other application (like Excel)'><p>Select and copy the table below, being the currently selected rows. To paste data from a spreadsheet, close this, click to edit the start cell, then paste.</p> <table class='xsPasteTable' id='"+dialogid+"_table'></table></div>");
			$("#"+dialogid).dialog({modal:true, autoOpen:false,width:$(window).width()*3/4});
			var cols = eval(json.args[1]);
			var options = eval(json.args[3]); 
			var slickgrid = new Slick.Grid("#"+json.args[0]+"_grid",xs.grid.getDataModel(elem),cols,options);
			slickgrid.xsPTFonInputObj = json.args[2];
			slickgrid.xsPTFid = json.args[0];
			elem.dataxs_sg=slickgrid;
		    slickgrid.setSelectionModel(new Slick.RowSelectionModel());
            slickgrid.onAddNewRow.subscribe(function (e, args) {
		      var item = args.item;
		      var data = elem.dataxs_rows;
		      slickgrid.invalidateRow(data.length);
		      data.push(item);
		      slickgrid.updateRowCount();
		      slickgrid.render();
		      var field = args.column.field;
           	  session.sendToServer({cmd:"NewRowOnGrid",args:[json.args[0],field,item[field],session.currentlyEditing]});
		    });
            slickgrid.onCellChange.subscribe(function (e,args) {
            	var columnField = slickgrid.getColumns()[args.cell].field;
            	var newValue = args.item[columnField];
            	session.sendToServer({cmd:"ChangeGrid",args:[json.args[0],args.row.toString(),columnField,newValue,session.currentlyEditing]});
            	//console.log(args.row.toString());
            	//console.log(columnField);
                //console.log(args); 
            });
            slickgrid.xsChangeGrid = function(row,columnField,newValue) { // for booleans.
            	session.sendToServer({cmd:"ChangeGrid",args:[json.args[0],row.toString(),columnField,newValue,session.currentlyEditing]});            	
            };
            var moveRowsPlugin = new Slick.RowMoveManager({
                cancelEditOnDrag: true
            });

            moveRowsPlugin.onMoveRows.subscribe(function (e, args) {
            	eval(slickgrid.xsPTFonInputObj).gridDnD(slickgrid.xsPTFid,args.rows,args.insertBefore);
            	slickgrid.setSelectedRows([]);
            	//console.log("onMoveRows");
            	//console.log(args.rows);
            	//console.log(args.insertBefore);
            });

            slickgrid.registerPlugin(moveRowsPlugin);

            slickgrid.onDragInit.subscribe(function (e, dd) {
              // prevent the grid from cancelling drag'n'drop by default
              e.stopImmediatePropagation();
            });

            xs.resizeSlickGrid(elem);
            slickgrid.render();
            /* Set up grid context menu */

		} else if (json.cmd=="StopGrid") {
			var elem = document.getElementById(json.args[0]+"_ui");
			var slickgrid = elem.dataxs_sg;
			if (slickgrid) {
				slickgrid.destroy();
			}
		} else if (json.cmd=="ProgressBar") {
			var id = json.args[0]+"_ui";
			var progid = id+"_progressLine";
			var statusid = id+"_status";
			var statusBar = id+"_progress";
			var statusPercent = id+"_progressPercent";
			if (!document.getElementById(progid)) {
				$(document.getElementById(id).parentNode).append("<div class='xsProgressStatus' id='"+progid+"'>Status : <span id='"+statusid+"'></span></div>");
			}
			var statusel = document.getElementById(statusid);
			var cmd = json.args[1];
			if (cmd=="Start") {
				statusel.className="xsStatusWorking";
 			    statusel.innerHTML = "<progress value='0' max='100' id='"+statusBar+"'></progress> <span id='"+statusPercent+"'>0%</span> <button onclick='xs.S"+session.id+".cancelJob(\""+json.args[0]+"\");'>Cancel</button>";
			} else if (cmd=="Progress") {
				var percent = Math.round(parseFloat(json.args[2])*100.0);
				var p1 = document.getElementById(statusBar);
				if (p1) p1.value=percent;
				var p2 = document.getElementById(statusPercent);
				if (p2) p2.textContent=""+percent+"%";
			} else if (cmd=="FinishedOK") {
				statusel.innerHTML="Finished : "+json.args[2]; 
				statusel.className="xsStatusGood";
			} else if (cmd=="FinishedError") {
				statusel.innerHTML="Failed : "+json.args[2]; 
				statusel.className="xsStatusError";
			}
		} else if (json.cmd=="ShowCustomPopup") {
			var id = json.args[0];
			var okfunction = eval("["+json.args[1]+"][0]"); // needs [...][0] otherwise treats function() {...} as error
			var goodfunction = eval("["+json.args[2]+"][0]");
			$("#"+id+"_popup").dialog({
				autoOpen : true,
			    modal : true,
			    buttons : {
			    	"OK" : function() {
			    		try {
				    		var message = okfunction();
				    		if (message) alert(message);
				    		else {
				    			var result = goodfunction();
				    			session.popupSetField(id,result);
					        	$(this).dialog("close");			    			
				    		};			    			
			    		} catch(err) {
			    			console.log("There was an error trying to execute popup functions "+err.message);
			    			console.log(err);
			    		}
			    	},
			        Cancel: function() {
			        	$(this).dialog("close");
			        }
			    },
			    close : function() {
			    	session.closedPopup(id);
			    }
			});
			// $("#"+id+"_popup").dialog("open"); 
		} else if (json.cmd=="DisposeCustomPopup") {
			var id = json.args[0];
			var okfunction = eval(json.args[1]);
			var goodfunction = eval(json.args[2]);
			$("#"+id+"_popup").dialog("close");
		} else alert("Unknown client message "+json.cmd);
	};
  };
}



var xs = {
	
  resizeAllSlickGrids : function () {
	$(".xsTableHolder").each(function () { xs.resizeSlickGrid(this);});  
  },
  
  /** 
   * Resizing the grid is more complex than one might think. The width has to be specified explitly (slick grid requirement). 
   * Making it bigger is not too bad - if the area is expanded, the table is made wider, and extra slop goes into the wide cell because of the CSS width=99%. You get the width of the div it is in, and resize to that size (although it creates a scrollbar if there is not a 1-2 pixel slop - see to do below)
   *
   * Making it smaller is harder - the td it is in will not shrink as it contains a wide grid. We can't make the table layout algorithm ignore its with
   * without also ignoring its height... So look at parents and adjust.
   */
  resizeSlickGrid : function(tableHolderElem) {
	//console.log("width = "+tableHolderElem.clientWidth);  
	//console.log("width = "+tableHolderElem.offsetWidth);  
	var td = xs.getParentTD(tableHolderElem);
//	console.log(td);
	if (!td) return;
	var tr = td.parentNode;
//	console.log(tr);
	if (!tr) return;
	//console.log("tr.width = "+tr.clientWidth);  
	//console.log("tr.width = "+tr.offsetWidth);  
	var pdiv = xs.getParentDiv(tr);
	if (!pdiv) return;
	//console.log("pdiv.width = "+pdiv.clientWidth);  
	//console.log("pdiv.width = "+pdiv.offsetWidth);  
	var toowide = tr.offsetWidth-pdiv.clientWidth+4;  // TODO get this pixel perfect. The +4 works on most size clients, with a little slop
	var w=tableHolderElem.clientWidth-2; // TODO get this pixel perfect. Again there is a little slop here
	if (toowide>0) w=w-toowide;
	if (w<100) w=100;
	if (tableHolderElem.dataCurrentWidth == w) return;
	tableHolderElem.dataCurrentWidth = w;
	var slickgrid = tableHolderElem.dataxs_sg;
    $(tableHolderElem).children('.xsTableGrid').css({'width':""+w+'px'});
    slickgrid.resizeCanvas();
  },
  
  saveSlickGridEditor : function(grid) {
	var target = (grid.getCellEditor()&&grid.getCellEditor().getTarget)?grid.getCellEditor().getTarget():null;
	return {
		active : grid.getActiveCell(),
		editor : grid.getCellEditor(),
		contents : target?xsPTF.getPTFContents(target):null
	};  
  },
  
  restoreSlickGridEditor : function(grid,saved,restoreText) {
	  //console.log("restore slick grid "+saved);
	  if (saved.active) {
		  //console.log(saved.active);
		  grid.setActiveCell(saved.active.row,saved.active.cell);
		  if (saved.editor) {
			  //console.log("Reenabling editor");
			  //console.log(saved);
			  grid.editActiveCell();
			  if (saved.contents) {
				  //console.log("saved.contents = "+saved.contents);
	 		      var target = grid.getCellEditor()?grid.getCellEditor().getTarget():null;
                  if (target) {
                	  //console.log("restoreText : "+restoreText);
        			  if (restoreText) grid.getCellEditor().setValue(saved.contents.text);
                      xsPTF.setPTFSelection(target,saved.contents.startSelection,saved.contents.endSelection);                	  
                  }				  
			  }
		  }
	  }
  },
  
  // get the first ancestor of DOM element elem that is a DIV (or null if none).		
  getParentDiv : function(elem) {
	  if (elem==null) return null;
	  if (elem.nodeType==1 && elem.nodeName=="DIV") return elem;
	  if (elem.parentNode) return xs.getParentDiv(elem.parentNode);
	  return null;
  },

  // get the first ancestor of DOM element elem that is a TD (or null if none).		
  getParentTD : function(elem) {
	  if (elem==null) return null;
	  if (elem.nodeType==1 && elem.nodeName=="TD") return elem;
	  if (elem.parentNode) return xs.getParentTD(elem.parentNode);
	  return null;
  },
  
  
  Session : function(sessionid) {
	  this.sessionClosed = false;
	  this.id = sessionid;
	  this.sentMessageCount = 0;
	  this.currentlyEditing = "";
	  /** Actually do the sending of a command. This is a low level interface - you would normally use this.sendToServer */
	  var sendCmd = function(cmd,isSynchronous) {
			$.ajax({
				cache:false,
				url : xs.userURL+"?sub=message&xsSessionID="+sessionid+"&count="+cmd.index,
				data : JSON.stringify(cmd.message),
				contentType: "application/json; charset=utf-8",
				processData : false,
				dataType : "json",
				async : !isSynchronous,
				error : function () { considerResending(1000,cmd.index);},
				success : function (acks) { if (acks.cmd=="ACK" && acks.args && acks.args.length==3) gotAcks(acks.args[0],acks.args[1],acks.args[2]); },
				type: "POST"
			});
		  
	  };
	  var sendBuffer = [];
	  var gotAcks = function(numberAcked,nextExpected,largestReceived) {
		  //console.log("Before acks "+sendBuffer.length);
		  sendBuffer = sendBuffer.filter(function(cmd) {
			var n = cmd.index;
			if (n==numberAcked) return false; // just received
			if (n<nextExpected) return false; // received in the past
			return true;
		  });
		  if (sendBuffer.length>0) {
			  var head = sendBuffer[0];
			  if (head.index < numberAcked) considerResending(500,head.index);
		  }
		  //console.log("After acks "+sendBuffer.length);
	  };
	  /** If there has not been an ACK for the given sequeuence index after delay ms from now, then resend it. */
      var considerResending = function(delay,index) {
    	  setTimeout(function(){
    		  for (var i=0;i<sendBuffer.length;i++) {
    			  var cmd = sendBuffer[i];
    			  if (cmd.index == index) {
    				  console.log("Resending packet "+index);
    				  sendCmd(cmd);
    			  }
    		  }
    	  },delay);
      };	  
	  
	  this.sendToServer = function (message,isSynchronous) {
		  var cmd = { index : this.sentMessageCount, "message" : message };
		  this.sentMessageCount++;
		  sendBuffer.push(cmd);
		  sendCmd(cmd,isSynchronous);
		  considerResending(10000,cmd.index);
	  };
	  /** Called by a client action - eg clicking on a "new X" action */
	  this.action = function(id) {
		  var elem = document.getElementById(id+"_ui");
		  if (elem.getAttribute("disabled")=="disabled") return;
		  this.sendToServer({cmd:"Action",args:[id,this.currentlyEditing]});
	  };
	  /** Called by a client clicking on the opener for a tree */
	  this.treeOpen = function(id) {
		  var elem = document.getElementById(id+"_opener");
		  // the code below is not necessary - the server will send corresponding commands - but it will speed up perceived response on a slow connection.
		  var isNowOpen = elem.textContent!="▼";
		  if (isNowOpen) {
			elem.textContent="▼";
			$("#"+id+"_subs").show();  
		  } else {
			elem.textContent="►";
			$("#"+id+"_subs").hide();  
		  }
		  // end unnecessary code
		  this.sendToServer({cmd:"TreeOpen",args:[id,""+isNowOpen]});		  
      }; 
      /** Get the id of a tree given the id of an element inside it. Removes xsTreeNode from start and _xxxx from end */
      this.treeID = function(id) {
    	  return id.slice(10,id.indexOf("_"));
      };
	  /** Called by a client clicking on the main part of a tree node */
	  this.treeSelect = function(event,id) {
		  var treeID = "xsTree"+this.treeID(id);
		  var added = document.getElementById(treeID).getAttribute("data-multiselect")=="true" && (event.shiftKey||event.ctrlKey);
		  if (!added) $("div#"+this.treeID(id)+" .xsSelected").removeClass("xsSelected"); // redundant, but anticipate server command for faster response.
		  $("#"+id+"_all > span:nth-child(2)").addClass("xsSelected"); // redundant, but anticipate server command for faster response.
		  this.sendToServer({cmd:"TreeSelect",args:[id,""+added]});
	  }; 
	  /** Called when a text field changes */
	  this.change = function(id) { 
		  var elem = document.getElementById(id+"_ui");
		  var value = elem.value;
		  this.sendToServer({cmd:"Change",args:[id,value,this.currentlyEditing]});
	  };
	  /** Called when a pseudo text field changes */
	  this.ptfInput = function(target,newtext) {
		  var id = target.id.slice(0,-3); // remove _ui
		  this.sendToServer({cmd:"Change",args:[id,newtext,this.currentlyEditing,target.getAttribute("xs-data-gridRow")||"",target.getAttribute("xs-data-gridCol")||""]});
	  };
	  /** Called when a pseudo text field changes by a pasted table */
	  this.ptfGridPaste = function(target,table) {
		  var tosend = [target.id,this.currentlyEditing,target.getAttribute("xs-data-gridRow")||"",target.getAttribute("xs-data-gridCol")||""];
		  for (var i=0;i<table.length;i++) {
			  var row = table[i];
			  tosend.push(""+row.length);
			  for (var j=0;j<row.length;j++) tosend.push(row[j]);
		  }
		  this.sendToServer({cmd:"PasteTable",args:tosend});		  
	  };
	  /** Called when a check box field changes */
	  this.changeCB = function(id) { 
		  var elem = document.getElementById(id+"_ui");
		  var value = elem.checked;
		  this.sendToServer({cmd:"ChangeCB",args:[id,value,this.currentlyEditing]});
	  };
	  /** Called when the user presses a key in some text field */
	  this.keyPress = function(id) { // TODO don't send back every key press if haven't got response from server already
		  var elem = document.getElementById(id+"_ui");
		  var value = elem.value;
		  this.sendToServer({cmd:"KeyPress",args:[id,value,this.currentlyEditing]});
	  };
	  
	  /** Called when a drag and drop is done on the grid (with id id) dragging rows rows (array of int indices) to insert before row index insertBefore */ 
	  this.gridDnD = function(id,rows,insertBefore) {
		  //console.log("Grid DnD "+rows+" "+insertBefore);
		  this.sendToServer({cmd:"GridDnD",args:[id,rows.toString(),insertBefore.toString(),this.currentlyEditing]});  
	  };

	  /** Called when a click is done on a toolbar button */
	  this.toolbar = function(id) {
		  this.sendToServer({cmd:"Toolbar",args:[id]});
	  };
	  
	  this.cancelJob = function(id) {
		  this.sendToServer({cmd:"CancelJob",args:[id,this.currentlyEditing]});
	  };
	  
	  this.initiatePopup = function(id) {
		  this.sendToServer({cmd:"InitiatePopup",args:[id,this.currentlyEditing]});
	  };
	  this.closedPopup = function(id) {
		  this.sendToServer({cmd:"ClosedPopup",args:[id,this.currentlyEditing]});
	  };
	  this.popupSetField = function(id,value) {
		  this.sendToServer({cmd:"PopupSetField",args:[id,this.currentlyEditing,value]});
	  };
	  //
	  // stuff for the tree - should be documented better.
	  //
	  
	  this.current_dragging_div = null;
	  this.getParentDivNotBeingDragged = function(elem) {
		  var div = xs.getParentDiv(elem);
		  if (div) {
			  if (div!=this.current_dragging_div && this.current_dragging_div!=null) return div; // don't drag onto self, or non-local
		  }
		  return null;
	  };
	  
	  /** For the tree nodes */
	  this.dragStart = function(ev) {
		  ev.target.style.opacity = '0.4';
		  ev.dataTransfer.setData("text/plain",ev.target.textContent);
		  this.current_dragging_div = ev.target;
		  return true;
	  };
	  
	  /** For the tree nodes */
	  this.dragEnd = function(ev) {
		  ev.target.style.opacity = '1.0';
		  this.current_dragging_div = null;
	  };
	  
	  this.dragOverAtTop = "false";

	  /** For the tree nodes */
	  this.dragOver = function(ev) {
		  var div = this.getParentDivNotBeingDragged(ev.target);
		  if (div) {
		    if (ev.preventDefault) ev.preventDefault(); 
		    var y = ev.clientY ;
		    var rect = div.getBoundingClientRect();
		    if (y-rect.top<4) {
		       $(div).addClass("xs-dragover-top");
		       this.dragOverAtTop = "true";
		    } else {
		       $(div).removeClass("xs-dragover-top");
		       this.dragOverAtTop = "false";
		    }
		    return false;
		  }
		  return true;
	  },

	  /** For the tree nodes */
	  this.dragEnter = function(ev) {
	    var div = this.getParentDivNotBeingDragged(ev.target);
	    if (div) {
	      $(div).toggleClass('xs-dragover');
	    }
	  },

	  /** For the tree nodes */
	  this.dragLeave = function(ev) {
	    var div = this.getParentDivNotBeingDragged(ev.target);
	    if (div) {
	      $(div).toggleClass('xs-dragover');
	    }
	  },
		

	  /** For the tree nodes */
	  
	  this.drop = function(ev) {
		var div = this.getParentDivNotBeingDragged(ev.target);
		if (div) {
	      $(div).removeClass('xs-dragover');
		  ev.preventDefault();
		  ev.stopPropagation();
		  if (this.current_dragging_div!=null) {
		    this.sendToServer({cmd:"TreeDragLocal",args:[this.current_dragging_div.id.slice(0,-4),div.id.slice(0,-4),this.dragOverAtTop]});			    	
		  } else {
			  // TODO non local drag
		  }
		} 
		return false;	
	  };

	  /** Get the currently selected nodes as an array of ids. Note that multiple selection is not currently implemented so this will perforce be a single element array */
	  this.getSelectedTreeNodes = function(baseID) {
		  var res = [];
		  $("div#"+baseID+" .xsSelected").each(function() {
			  var idfull = this.id; // need to remove _selectable from the end (11 chars)
			  res.push(idfull.substring(0,idfull.length-11)); 
		  });
		  return res;
	  };
	  
	  this.treeContextMenu = function(subtype,treeID) {
		  console.log(treeID);
		  xsthis.sendToServer({cmd:"TreeContextMenu",args:[treeID,subtype,xsthis.getSelectedTreeNodes(treeID).toString()]});
	  };
	  
	  this.gridContextMenu = function(subtype,tableid,selectedrows) {
		  xsthis.sendToServer({cmd:"TableContextMenu",args:[tableid,subtype,selectedrows.toString(),xsthis.currentlyEditing]});
	  };
	  
	  var xsthis = this;
		
	  this.imageFromBlob = function(id,elem,blob) {
		  console.log(blob);
		  var reader = new FileReader();
		  reader.onload = function (event) {
			elem.src = event.target.result;
			xsthis.sendToServer({cmd:"Change",args:[id,event.target.result,xsthis.currentlyEditing]});
		  };
		  reader.readAsDataURL(blob);
	  };
	  
	  /** Load url as a blob, and then call callback with the blob as an argument */
	  this.loadURLAsBlob = function(url,callback) {
		  var blobxhr = new XMLHttpRequest();
		  blobxhr.open("GET",url, true);
		  blobxhr.responseType = "blob"; // requires IE 10
		  blobxhr.addEventListener("load", function () {
            if (blobxhr.status === 200) { callback(blobxhr.response); }
		  }, false);
		  blobxhr.send();
	  };
	  
	  this.imageDrop = function(id,ev) {
		  ev.preventDefault();
		  var target = document.getElementById(id+"_image");
		  var file = ev.dataTransfer.files && ev.dataTransfer.files[0];
		  if (file && file!=null) {
			  this.imageFromBlob(id,target,file);
		  } else {
			  console.log(ev.dataTransfer);
			  var url =  ev.dataTransfer.getData("text/uri-list")|| ev.dataTransfer.getData("url");
			  console.log(url);
			  if (url) {
				  if (target.getAttribute("data-ResolveNetworkReferences")=="true" && !url.indexOf("data:")==0) {
					  this.loadURLAsBlob(url,function(blob) { xsthis.imageFromBlob(id,target,blob);});					  
				  } else {
					  target.src=url;
					  xsthis.sendToServer({cmd:"Change",args:[id,url,xsthis.currentlyEditing]});					  
				  }
				  //console.log(url);
			  } else {
				  // could try to resolve other things
				  console.log(ev.dataTransfer);
			  }
		  }
		  return false;
	  };
	  
	  this.imageDrag = function(id,ev) { // should check data types.
		  ev.stopPropagation();
		  ev.preventDefault();
		  ev.dataTransfer.dropEffect = 'copy';
		  try { ev.dataTransfer.effectAllowed='copy'; } catch (err) {} // causes exception on IE9
	  };
	  
	  window.addEventListener("beforeunload", function( event ) {
		  if (!xsthis.sessionClosed) {
			  xsthis.sessionClosed = true;
			  $("body").hide();
			  xsthis.sendToServer({cmd:"CloseConnection",args:[]},true); // send synchronously
			  //alert("Sent");			  
		  }
      });


	  this.errorInServerConnection = function(cause) {
		  if (!xsthis.sessionClosed) { 
			  if (xsthis.sentMessageCount>0) alert("reloading from server"); 
			  else $("body").hide();
			  location.reload(true); 
		  }
	  };
	  this.cometCall = function() {
		  $.ajax({
			  cache : false,
			  complete : function (xhr,status) {
				  if (status=="success" || status=="timeout") { if (!xsthis.sessionClosed) xsthis.cometCall(); }
				  else { xsthis.errorInServerConnection("Connection fail"); }
				  //else if (status=="notmodified")
				  //"success", "notmodified", "error", "timeout", "abort", or "parsererror"				  
			  },
		      data : { sub:"comet",xsSessionID:sessionid },
			  dataType : "json",
			  success : function (data, textStatus) {
				  xsProcessClientMessageFromServer(data,xsthis);
				},
			  timeout : 20000,
			  type : "POST",
			  
			  url : xs.cometURL
				  // url is current by default
		  });
		  
	  };
	  $(document).ready(function(){
		  xsthis.cometCall();
      });
	  
	  
  }, // end session definition
  
  userURL : "",
  cometURL : ""

};

/* Set up tree context menu */
$(function() {
	$.contextMenu({
		selector: '.xsEditTree',
		callback: function(key,options) {
			var objdesc = this[0].getAttribute("data-oninputobj");
			var obj = eval(objdesc);
			obj.treeContextMenu(key,this[0].id);
		},
		items: {
			cut : { name:"Cut", icon:"cut" },
			copy : { name:"Copy", icon:"copy" },
		    paste : { name:"Paste", icon:"paste" },
		    erase : { name:"Delete", icon:"delete" }
		}
	});
});
/* Set up grid context menu */
$(function() {
	$.contextMenu({
		selector: '.xsTableHolder',
		callback: function(key,options) {
			//console.log(this);
			var tableid = this[0].getAttribute("id");
			var dialogid = tableid+"_export";
			tableid=tableid.substring(0,tableid.length-3); // remove _ui
			var slickgrid = this[0].dataxs_sg;
			var obj = eval(slickgrid.xsPTFonInputObj);
			var selected = slickgrid.getSelectedRows();
			if ((!selected) || selected.length==0) {
				alert("Select a row or rows first.");
			} else if (key=="copyExport") {
				var cols = slickgrid.getColumns();
				console.log(cols);
				var data = slickgrid.getData();
				console.log(data);
				var table = document.getElementById(dialogid+"_table");
				var contents = "";
				selected=selected.sort();
				console.log(selected);
				console.log(selected.sort());
				for (var i=0;i<selected.length;i++) {
					var row = data.getItem(selected[i]);
					contents+="<tr>";
					for (var j=0;j<cols.length;j++) {
						contents+="<td>"+xsPTF.escapeText(row[cols[j].id]||"").replace(/\n/g,"<br/>")+"</td>";
					}
					contents+="</tr>";
				}
				table.innerHTML = contents;
				$("#"+dialogid).dialog("open");
				if (document.body.createTextRange) { //ms
				  var range = document.body.createTextRange();
				  range.moveToElementText(table);
				  range.select();
				} else if (window.getSelection) { //all others
				  var selection = window.getSelection();        
				  var range = document.createRange();
				  range.selectNodeContents(table);
				  selection.removeAllRanges();
				  selection.addRange(range);
				}
			} else obj.gridContextMenu(key,tableid,selected);
		},
		items: {
			cut : { name:"Cut", icon:"cut" },
			copy : { name:"Copy", icon:"copy" },
			copyExport : { name:"Copy to other application", icon:"copy" },
		    paste : { name:"Paste", icon:"paste" },
		    erase : { name:"Delete", icon:"delete" }
		}
	});
});

setInterval(xs.resizeAllSlickGrids,200); // onresize doesn't work for divs.
