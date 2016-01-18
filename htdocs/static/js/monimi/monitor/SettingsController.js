dojo.provide("monimi.monitor.SettingsController");

dojo.require("dojo.date.stamp");
dojo.require("monimi.util");

dojo.declare("monimi.monitor.SettingsController", null,
    {
	constructor: function(view, monitor, parent_controller) {
		this.view = view;
		this.parent_controller = parent_controller;
		this.monitor = monitor;
		this.rpc = parent_controller.rpc;
		this.loadSettings().addCallback(dojo.hitch(this.view, this.view.displaySettings));
	},

	loadSettings: function() {
		return this.rpc.monitorListSettings({name:this.monitor.name}).addCallbacks(
			dojo.hitch(this, this.onSettings),
			dojo.hitch(this, 'onRpcError')
		);		
	},

	onSettings: function(settings) {
		this.settings = settings;
		return this.settings;
	},

	submitChanges: function(fields) {
        var params = monimi.util.form_params(fields);
		params.name = this.monitor.name;
		
		var full_refresh = this.monitor.name != params.new_name; 
		
		this.view.busyApplyButton();

        return this.rpc.monitorSetSettings(params).addCallbacks(dojo.hitch(this, function() {
					if(full_refresh) {
						this.parent_controller.reloadMonitors();
					} else {
						this.view.disableApplyButton();	
					}
				}),
				dojo.hitch(this, function(error) {
					this.view.disableApplyButton();
					this.view.enableApplyButton();
					
					var title = 'Failed to modify check';
					// show dialog
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
				}));
	},

	onRpcError: function(error) {
		this.parent_controller.onRpcError("notifications:" + error);
	}
});
