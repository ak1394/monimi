dojo.provide("monimi.Home");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.layout.ContentPane");
dojo.require("dijit.form.Button");

dojo.require("monimi.HomeController");
dojo.require("monimi.MonitorList");
dojo.require("monimi.MonitorView");
dojo.require("monimi.monitor.AddDialog");

// our declared class
dojo.declare("monimi.Home", [dijit._Widget, dijit._Templated],
    // class properties:
    {
  	templatePath: dojo.moduleUrl("monimi", "Home.html"),	
	widgetsInTemplate: true,
	monitorViewPane: null,

	postCreate: function() {
		this.controller = new monimi.HomeController(this);
		dojo.connect(this.addMonitorButton, "onClick", this, function() {
			new monimi.monitor.AddDialog({controller:this.controller}).show();
		});
	},
	
	displayMonitors: function() {
		// monitors
		var monitors = this.controller.monitors;
		if(monitors.length > 0) {
			this.monitorList = new monimi.MonitorList({controller: this.controller, monitors: monitors});
			this.monitorListDiv.appendChild(this.monitorList.domNode);
			this.showMonitor(monitors[0].name);
			// when onMonitorSelected is called, call showMonitor too with the same arguments
			dojo.connect(this.monitorList, 'onMonitorNameSelected', this, 'showMonitor');
		} else {
			this.monitorViewDiv.innerHTML = '<div class="centered-message">Please add checks.</div>';
		}
	},

	displayLimits: function() {
		this.addMonitorButton.setLabel("Add check (" + this.controller.monitorsLeft + " left)");
		this.addMonitorButton.setAttribute('disabled', this.controller.monitorsLeft <= 0);
		if(this.controller.limits.sms_credits <= this.controller.limits.sms_credis_warn) {
			// warn credit too low
			this.notificationMessage.innerHTML = "<div>You've got " + this.controller.limits.sms_credits
					 + ' SMS credits left. Please consider an <a href="/profile">upgrade</a>.</div>';
			this.notificationPanel.style.display = 'block';
		}
	},
	
	refreshMonitors: function() {
		this.monitorList.refresh(this.controller.monitors);
		this.monitorView.refresh();
	},

	clearMonitors: function() {
		if(this.monitorList) {
			this.monitorList.destroy();
			this.monitorView.destroy();
			this.monitorList = null;
			this.monitorView = null;
		} else {
			this.monitorViewDiv.removeChild(this.monitorViewDiv.firstChild);			
		}
	},
	
	showMonitor: function(monitorName) {
		var selectedTab = 0;
		if(this.monitorView) {
			selectedTab = this.monitorView.getSelectedTab();
			this.monitorView.destroy();
		}
		this.monitorView = new monimi.MonitorView({controller: this.controller, monitorName:monitorName, initiallySelectedTab:selectedTab});
		this.monitorViewDiv.appendChild(this.monitorView.domNode);
		this.monitorView.tabContainer.resize();
		
	}
});
