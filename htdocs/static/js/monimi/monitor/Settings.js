dojo.provide("monimi.monitor.Settings");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.layout.StackContainer");
dojo.require("dijit.layout.ContentPane");
dojo.require("dijit.form.ComboBox");
dojo.require("monimi.monitor.SettingsController");
dojo.require("dojox.form.BusyButton");
dojo.require("dijit.form.ValidationTextBox");

// our declared class
dojo.declare("monimi.monitor.Settings", [dijit._Widget, dijit._Templated],
    // class properties:
    {

  	templatePath: dojo.moduleUrl("monimi.monitor", "Settings.html"),	
	widgetsInTemplate: true,
	monitor: null,
	controller: null,

	postMixInProperties: function() {
	},
	
    postCreate: function() {
		this.fields = {
			new_name: {name: 'Name', textbox: this.checkName},
			beat: {name: 'Check every', value: dojo.hitch(this, function() { return this.beatSelect.value })}
		};
		
		this.controller = new monimi.monitor.SettingsController(this, this.monitor, this.parent_controller);
		dojo.connect(this.applyButton, "onClick", dojo.hitch(this.controller, 'submitChanges', this.fields));
	},

	enableApplyButton: function() {
		this.applyButton.attr('disabled', false);
	},

	disableApplyButton: function() {
		this.applyButton.cancel();
		this.applyButton.attr('disabled', true);
	},
	
	busyApplyButton: function() {
		this.applyButton.makeBusy();
	},
	
	displaySettings: function(notifications) {
		this.checkName.attr('value', this.controller.monitor.name);
		var beat = [1,2,3,4,5,10,15,20,25,30,40,50,60];
		dojo.forEach(beat, function(v) {
			if(this.parent_controller.limits.min_beat <= v) {
				var min = v == 1 ? " minute" : " minutes";
				this.beatSelect[this.beatSelect.length] = new Option(v + min, v, this.controller.settings.beat == v);
			}
		}, this);

		this.connect(this.beatSelect, 'onchange', this.enableApplyButton);
		this.connect(this.checkName, 'onFocus', this.enableApplyButton);		
	}
});
