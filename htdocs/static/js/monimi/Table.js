dojo.provide("monimi.Table");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");

// our declared class
dojo.declare("monimi.Table", [dijit._Widget, dijit._Templated],
    // class properties:
    {

    templatePath: dojo.moduleUrl("monimi","Table.html"),
	items: null,
	names: null,
	titles: null,
	body: null,
	selectFirstRow: false,
	
	postCreate: function() {
		if(this.titles != null) {
			var row = this.head.insertRow(-1);
			for(var i=0; i<this.titles.length; i++){
				var cell  = row.insertCell(-1);
				cell.appendChild(dojo.doc.createTextNode(this.titles[i]));
				if(this.names != null) {
					dojo.addClass(cell, this.names[i]);
				}
			}
		}
		this.createRow = this.names == null ? this.createRowFromString : this.createRowFromObject;
		for(var i=0; i<this.items.length; i++){
			var row = this.createRow(this.items[i]);
			this.connectEvents(this.items[i], row);
		}
		dojo.query("tr:nth-child(odd)", this.body).addClass("oddRow");
		dojo.query("tr:nth-child(even)", this.body).removeClass("oddRow");
		if(this.selectFirstRow) {
			dojo.query("tr:first-child", this.tableBody).addClass("selected");
		}
	},

	connectEvents: function(item, row) {
		dojo.connect(row, "onclick", this, function() {this.onClick(item, row)});
		dojo.connect(row, "onmouseover", this, dojo.partial(dojo.addClass, row, 'rowHover'));
		dojo.connect(row, "onmouseout",  this, dojo.partial(dojo.removeClass, row, 'rowHover'));
	},

	createRowFromString: function(item) {
		var row = this.body.insertRow(-1);
		var cell  = row.insertCell(-1);
		cell.appendChild(dojo.doc.createTextNode(item));
		return row;
	},
	
	createRowFromObject: function(item) {
		var row = this.body.insertRow(-1);
		for(var i=0; i<this.names.length; i++){
			var cell  = row.insertCell(-1);
			if(typeof(this.names[i]) == 'function') {
				var colspan = this.names[i](cell, item);
				if(colspan) {
					i = i + colspan;
				}
			} else {
				cell.appendChild(dojo.doc.createTextNode(item[this.names[i]]));
				dojo.addClass(cell, this.names[i]);	
			}
		}
		return row;
	},
		
	onClick: function(item, row) {
		//dojo.toggleClass(row, 'rowSelected');
	}
});
