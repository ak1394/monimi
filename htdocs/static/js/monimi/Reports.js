dojo.provide("monimi.Reports");
dojo.require("dojo.date.stamp");
dojo.require("dojo.back");
dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.layout.TabContainer");
dojo.require("dijit.layout.ContentPane");
dojo.require("dijit.TitlePane");

dojo.require("monimi.ReportList");
dojo.require("monimi.ReportView");
dojo.require("monimi.report.UniversalView");
dojo.require("monimi.ErrorDialog");


// our declared class
dojo.declare("monimi.Reports", [dijit._Widget, dijit._Templated],
    // class properties:
    {
  	templatePath: dojo.moduleUrl("monimi", "Reports.html"),	
	widgetsInTemplate: true,
	currentMonitorName: null,

	postCreate: function() {
		dojo.back.setInitialState({
			back: dojo.hitch(this, function() {
				this.report.style.display = 'none';
				this.reportSelector.style.display = 'block';
				if (this.displayedReport) {
					this.displayedReport.destroyRecursive();
				}
			})
		});
		dojo.connect(this.linkBack1, 'onclick', dojo.back.goBack);		
		dojo.connect(this.linkBack2, 'onclick', dojo.back.goBack);		
		return monimiRPC.monitorList().addCallbacks(
			dojo.hitch(this, this.onReports),
			dojo.hitch(this, 'onRpcError')
		);	
	},
	
	onReports: function(monitors) {
		this.reportList = new monimi.ReportList({monitors: monitors});
		this.reportListDiv.appendChild(this.reportList.domNode);
		this.monitors = monitors;
		dojo.connect(this.reportList, 'onMonitorNameSelected', this, 'showReportView');
		if(monitors.length > 0) {
			this.showReportView(monitors[0].name);
		}
		this.domNode.style.display = 'block';
	},	

	showReportView: function(monitorName) {
		if(this.reportView) {
			this.reportView.destroyRecursive();
		}
		dojo.forEach(this.monitors, function(monitor) {
			if(monitor.name == monitorName) {
				this.currentMonitor = monitor;	
				this.reportView = new monimi.ReportView({monitor: monitor});
				this.reportViewDiv.appendChild(this.reportView.domNode);
				dojo.connect(this.reportView, 'displayDaily', this, 'showDay');
				dojo.connect(this.reportView, 'displayMonthly', this, 'showMonth');
				dojo.connect(this.reportView, 'displayYearly', this, 'showYear');

			}
		}, this);
	},
	
	showDay: function(item) {
		dojo.back.addToHistory({changeUrl:'day'});		
		var view = new monimi.report.UniversalView({monitor: this.currentMonitor, date: item.formatted});
		this.displayedReport = view;
		this.reportSelector.style.display = 'none';
		this.reportDiv.appendChild(view.domNode);
		this.report.style.display = 'block';		
		monimiRPC.reportDaily({monitor:this.currentMonitor.name, date_for: dojo.date.stamp.toISOString(item.date)}).addCallbacks(
			dojo.hitch(view, view.setData),
			dojo.hitch(this, 'onRpcError'));
	},
	
	showMonth: function(item) {
		dojo.back.addToHistory({changeUrl:'month'});
		var view = new monimi.report.UniversalView({monitor: this.currentMonitor, date: item.formatted});
		this.displayedReport = view;
		this.reportSelector.style.display = 'none';
		this.reportDiv.appendChild(view.domNode);
		this.report.style.display = 'block';		
		monimiRPC.reportMonthly({monitor:this.currentMonitor.name, date_for: dojo.date.stamp.toISOString(item.date)}).addCallbacks(
			dojo.hitch(view, view.setData),
			dojo.hitch(this, 'onRpcError'));
	},

	showYear: function(item) {
		// TODO
	},
	
	onRpcError: function(error) {
		var dialog = new monimi.ErrorDialog({
			title: "RPC Error occured",
			message: "Error: " + error.message
			});
        dialog.show();
	}
});
