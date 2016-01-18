dojo.provide("monimi.ReportView");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");

dojo.require("monimi.Table");

// our declared class
dojo.declare("monimi.ReportView", [dijit._Widget, dijit._Templated],
    // class properties:
    {

  	templatePath: dojo.moduleUrl("monimi", "ReportView.html"),	
	widgetsInTemplate: false,
	tableBody: null,
	monitor: null,

	postCreate: function() {
		return monimiRPC.reportList({monitor:this.monitor.name}).addCallbacks(
			dojo.hitch(this, this.displayReport),
			dojo.hitch(this, 'onRpcError')
		);	
	},
	
	displayReport: function(reports) {
		var items_daily = [];
		dojo.forEach(reports.daily, function(item) {
			var date = dojo.date.stamp.fromISOString(item);
			var formatted = dojo.date.locale.format(date, {selector:'date'}); 
			items_daily.push({date:date, formatted: formatted});
		});
		this.daily = new monimi.Table({names:['formatted'], items: items_daily});
		this.dailyDiv.appendChild(this.daily.domNode);
		dojo.connect(this.daily, 'onClick', this, 'displayDaily');

		var items_monthly = [];
		dojo.forEach(reports.monthly, function(item) {
			var date = dojo.date.stamp.fromISOString(item);
			var formatted = dojo.date.locale.format(date, {selector:'date', datePattern:'y, MMMM'}); 
			items_monthly.push({date:date, formatted: formatted});
		});
		
		this.monthly = new monimi.Table({names:['formatted'], items: items_monthly});
		this.monthlyDiv.appendChild(this.monthly.domNode);
		dojo.connect(this.monthly, 'onClick', this, 'displayMonthly');

		this.yearly = new monimi.Table({items: reports.yearly});
		this.yearlyDiv.appendChild(this.yearly.domNode);
		dojo.connect(this.yearly, 'onClick', this, 'displayDaily');
	},
	
	displayDaily: function(date) {
	},
	
	displayMonthly: function(date) {
		
	},

	displayYearly: function(date) {
		
	},

	onRpcError: function(error) {
		alert("rpc error: " + error);	
	}
});
