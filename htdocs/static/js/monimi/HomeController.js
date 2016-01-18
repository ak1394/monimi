dojo.provide("monimi.HomeController");

dojo.require("dojo.date.stamp");
dojo.require("dojo.DeferredList");
dojo.require("monimi.JsonService");
dojo.require("monimi.ErrorDialog");
dojo.require("monimi.util");

dojo.declare("monimi.HomeController", null,
    {
	defaultRefreshTimeout: 60000,
	monitorsLeft: 0,
	monitors: [],
	
	constructor: function(view) {
		this.view = view;
		this.rpc = new monimi.JsonService(window.location.protocol + "//" + window.location.host + "/controllers/smd");
        (new dojo.DeferredList([this.loadMonitors(), this.loadMonitorLimits()])).addCallbacks(
			dojo.hitch(this, function() {
				this.view.domNode.style.display = 'block';
            	this.view.displayLimits();
            	this.view.displayMonitors(this.monitors);
				setTimeout(dojo.hitch(this, function() {
                    if(this.view.monitorView) {
                        this.view.monitorView.tabContainer.resize();
                    }
                  }), 500);
				}),
			dojo.hitch(this, this.onRpcError));
	},

	refreshMonitors: function() {
		this.loadMonitors().addCallback(this.view, this.view.refreshMonitors);
	},

	reloadMonitors: function() {
		clearTimeout(this.refreshTimeoutId);
        (new dojo.DeferredList([this.loadMonitors(), this.loadMonitorLimits()])).addCallbacks(
			dojo.hitch(this, function() {
				this.view.clearMonitors();
            	this.view.displayLimits();
            	this.view.displayMonitors();
				}),
			dojo.hitch(this, this.onRpcError));
	},

	loadMonitors: function() {
		return this.rpc.monitorList().addCallback(
			dojo.hitch(this, function(response) {
				this.monitors = response;
				dojo.forEach(this.monitors, function(monitor) {
					monitor.checked = dojo.date.stamp.fromISOString(monitor.checked);
					monitor.next_check = dojo.date.stamp.fromISOString(monitor.next_check);
					monitor.up_down_since = dojo.date.stamp.fromISOString(monitor.up_down_since);
					monitor.displayStatus = monitor.user_state == 'U_ST_PAUSED' ? "Paused"
						: monitor.state == 'ST_ADDED' ? "Unknown"
							: monitor.state == 'ST_UP' ? "Up"
								:"Down";
				});
				if(this.monitors.length > 0) {
					this.setRefreshTimeout(); 
				}
			})
		);		
	},

	loadMonitorLimits: function() {
		return this.rpc.monitorLimits().addCallbacks(
			dojo.hitch(this, function(response) {
				this.monitorsLeft = response.max_monitors - this.monitors.length;
				this.limits = response;
			}),
			dojo.hitch(this, 'onRpcError')
		);		
	},

	getMonitor: function(name) {
		return dojo.filter(this.monitors, function(monitor){ return monitor.name == name; }).pop();
	},
	
	addHttpMonitor: function(fields) {
        var params = monimi.util.form_params(fields);
        return this.rpc.monitorAddHttp(params).addCallbacks(dojo.hitch(this, this.reloadMonitors),
				dojo.hitch(this, this.onFormError, 'Failed to add check', fields));
	},

	addHttpsMonitor: function(fields) {
        var params = monimi.util.form_params(fields);
        return this.rpc.monitorAddHttps(params).addCallbacks(dojo.hitch(this, this.reloadMonitors),
				dojo.hitch(this, this.onFormError, 'Failed to add check', fields));
	},

	addPingMonitor: function(fields) {
        var params = monimi.util.form_params(fields);
        return this.rpc.monitorAddPing(params).addCallbacks(dojo.hitch(this, this.reloadMonitors),
				dojo.hitch(this, this.onFormError, 'Failed to add check', fields));
	},

	addDnsMonitor: function(fields) {
        var params = monimi.util.form_params(fields);
        return this.rpc.monitorAddDns(params).addCallbacks(dojo.hitch(this, this.reloadMonitors),
				dojo.hitch(this, this.onFormError, 'Failed to add check', fields));
	},
	
	pauseMonitor: function(monitor) {
		if(monitor.user_state == 'U_ST_PAUSED') {
			var action = this.rpc.monitorResume({name: monitor.name});
		} else {
			var action = this.rpc.monitorPause({name: monitor.name});
		}
		action.addCallbacks(
			dojo.hitch(this, 'reloadMonitors'),
			dojo.hitch(this, 'onRpcError')
		);
	},

	setMonitorBeat: function(monitor, beat) {
		var action = this.rpc.monitorSetBeat({name: monitor.name, beat:beat});
		action.addCallbacks(
			dojo.hitch(this, 'reloadMonitors'),
			dojo.hitch(this, 'onRpcError')
		);
	},
	
	removeMonitor: function(monitor) {
		this.rpc.monitorRemove({name: monitor.name}).addCallbacks(
			dojo.hitch(this, 'reloadMonitors'),
			dojo.hitch(this, 'onRpcError')
		);
	},

	setRefreshTimeout: function() {
		// find closest next_check time and schedule timeout accordingly
		var refreshTimeout = this.defaultRefreshTimeout;
		var currentDate = new Date();
		dojo.forEach(this.monitors, function(monitor){
			if(monitor.next_check != null) {
				var candidateTimeout = monitor.next_check.valueOf() - currentDate.valueOf();
				refreshTimeout = candidateTimeout > 0 ? candidateTimeout : refreshTimeout;
			} 
		}, this);
		refreshTimeout =  refreshTimeout > this.defaultRefreshTimeout ? refreshTimeout : this.defaultRefreshTimeout; 
		console.log("refresh timeout set to", refreshTimeout);
		this.refreshTimeoutId = setTimeout(dojo.hitch(this, 'refreshMonitors'), refreshTimeout);
	},

	onFormError: function(title, fields, error) {
		if (error.message == 'validation_failed') {
			var dialog = new monimi.ErrorDialog({
				title: title,
				message: monimi.util.form_format_error(fields, error.errorObject.detail)
			});
		} else {
			var dialog = new monimi.ErrorDialog({
				title: title,
				message: "Error: " + error.message
			});
		}
        dialog.show();
	},
	
	onRpcError: function(error) {
		var dialog = new monimi.ErrorDialog({
			title: "RPC Error occured",
			message: "Error: " + error.message
			});
        dialog.show();
	}
});
