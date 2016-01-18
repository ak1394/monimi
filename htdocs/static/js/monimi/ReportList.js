dojo.provide("monimi.ReportList");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");

// our declared class
dojo.declare("monimi.ReportList", [dijit._Widget, dijit._Templated],
    // class properties:
    {

  	templatePath: dojo.moduleUrl("monimi", "ReportList.html"),	
	widgetsInTemplate: true,
	tableBody: null,
	calendar: null,

	postCreate: function() {
		dojo.forEach(this.monitors,
			function(monitor) {
				this.addReportRow(monitor);
			},
		    this);
		dojo.query("tr:nth-child(odd)", this.tableBody).addClass("oddRow");
		dojo.query("tr:nth-child(even)", this.tableBody).removeClass("oddRow");
		dojo.query("tr:first-child", this.tableBody).addClass("selected");
	},
	
	addReportRow: function(monitor) {
		var row = this.tableBody.insertRow(-1); // insert new row after the caption
		
		var cell  = row.insertCell(-1);
		var nameDiv = dojo.doc.createElement('div');
		nameDiv.appendChild(dojo.doc.createTextNode(monitor.name));
		nameDiv.className = "name";
		
		cell.appendChild(nameDiv);
		
		dojo.connect(row, "onclick", this, function(){ this.onMonitorNameSelected(monitor.name, row); });
		dojo.connect(row, "onmouseover", this, function() { if(!dojo.hasClass(row, 'selected')) { dojo.toggleClass(row, 'hover');}});
		dojo.connect(row, "onmouseout", this, function() { if(!dojo.hasClass(row, 'selected')) { dojo.toggleClass(row, 'hover');}});
	},

	onMonitorNameSelected: function(monitorName, tr) {
		dojo.query("tr", this.tableBody).removeClass("selected");
		dojo.removeClass(tr, "hover");
		dojo.toggleClass(tr, 'selected');
	}

});
