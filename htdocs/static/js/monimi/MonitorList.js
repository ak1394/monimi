dojo.provide("monimi.MonitorList");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");

dojo.declare("monimi.MonitorList", [dijit._Widget, dijit._Templated],
    {
  	templatePath: dojo.moduleUrl("monimi", "MonitorList.html"),	
	widgetsInTemplate: true,
	tableBody: null,
	
	postCreate: function() {
		dojo.forEach(this.monitors,
			function(monitor) {
				this.addMonitorRow(monitor);
			},
		    this);
		dojo.query("tr:nth-child(odd)", this.tableBody).addClass("oddRow");
		dojo.query("tr:nth-child(even)", this.tableBody).removeClass("oddRow");
		dojo.query("tr:first-child", this.tableBody).addClass("selected");
	},

	refresh: function(monitors) {
		for(var i=0; i<monitors.length; i++) {
			var statusDiv = dojo.byId('status-' + monitors[i].name);
			statusDiv.className = this.getMonitorStatus(monitors[i])
		} 
	},

	getMonitorStatus: function(monitor) {
		// status: paused -> unknown -> up -> down
		var status = monitor.user_state == 'U_ST_PAUSED' ? "status paused"
			: monitor.state == 'ST_ADDED' ? "status unknown"
				: monitor.state == 'ST_UP' ? "status up"
					:"status down";
		return status;
	},
	
	addMonitorRow: function(monitor) {
		var row = this.tableBody.insertRow(-1); // insert new row after the caption
		
		var cell  = row.insertCell(-1);
		var nameDiv = dojo.doc.createElement('div');
		nameDiv.appendChild(dojo.doc.createTextNode(monitor.name));
		nameDiv.className = "name";
		
		cell.appendChild(nameDiv);
		
		var statusDiv = dojo.doc.createElement('div');
		statusDiv.id = "status-" + monitor.name;
		statusDiv.appendChild(dojo.doc.createTextNode(' '));

		statusDiv.className = this.getMonitorStatus(monitor);

		cell.appendChild(statusDiv);
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
