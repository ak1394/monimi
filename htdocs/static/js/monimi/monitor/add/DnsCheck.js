dojo.provide("monimi.monitor.add.DnsCheck");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.form.Form");
dojo.require("dijit.form.Button");
dojo.require("dijit.layout.StackContainer");
dojo.require("dijit.layout.ContentPane");
dojo.require("dijit.form.ValidationTextBox");
dojo.require("dojox.form.DropDownSelect");
dojo.require("dojox.form.BusyButton");

dojo.declare("monimi.monitor.add.DnsCheck", [dijit._Widget, dijit._Templated],
    {

  	templatePath: dojo.moduleUrl("monimi.monitor.add", "DnsCheck.html"),	
	widgetsInTemplate: true,
	monitor: null,
	controller: null,
	dialog: null,

    postCreate: function() {
		// form fields
		this.fields = {
			name: {name: 'Name', textbox: this.checkName},
			hostname: {name: 'URL', textbox: this.checkHostname},
			beat: {name: 'Check every', widget: this.beatSelect}
			}

		this.fields_advanced = {
			name: {name: 'Name', textbox: this.checkName},
			hostname: {name: 'URL', textbox: this.checkHostname},
			beat: {name: 'Check every', widget: this.beatSelect},
			ns: {name: 'Nameserver', widget: this.nsSelect},
			qtype: {name: 'Query type', widget: this.qtypeSelect}
			}
		// set beat
		dojo.forEach([1,2,3,4,5,10,15,20,25,30,40,50,60], function(v) {
			if(this.controller.limits.min_beat <= v) {
				var min = v == 1 ? " minute" : " minutes";
				this.beatSelect.addOption({value:v, label:v + min});
			}
		}, this);
		dojo.connect(this.okButton, 'onClick', this, 'addMonitor');
		dojo.connect(this.okButton2, 'onClick', this, 'addMonitorAdvanced');
		dojo.connect(this.cancelButton, 'onClick', this.dialog, 'hide');
		dojo.connect(this.cancelButton2, 'onClick', this.dialog, 'hide');
		dojo.connect(this.advancedButton, "onClick", this, 'advancedSettings');
		dojo.connect(this.backButton, "onClick", this.stackContainer, 'back');
		dojo.connect(this.formOne, "onValidStateChange", this, function() {
			if(this.formOne.isValid()) {
				this.okButton.attr('disabled', false);
				this.advancedButton.attr('disabled', false);
			} else {
				this.okButton.attr('disabled', true);
				this.advancedButton.attr('disabled', true);
			}
		});
	},

	advancedSettings: function() {
		var name = this.checkHostname.attr('value');
		this.advancedButton.makeBusy();
		this.controller.rpc.monitorGetNameservers({name: name}).addCallbacks(
			dojo.hitch(this, function(result) {
				this.nsSelect.removeOption(this.nsSelect.getOptions());
				this.nsSelect.addOption({value:"any-ns", label:"Any Nameserver"});
				dojo.forEach(result.nameservers, function(ns) {
					this.nsSelect.addOption({value:ns, label:ns});
				}, this);
				this.advancedButton.cancel();
				this.stackContainer.forward();			
			}),
			dojo.hitch(this, function(error) {
				this.advancedButton.cancel();
				this.controller.onRpcError(error);
			})
		);
	},
		
	addMonitor: function() {
		this.dialog.hide();
		this.controller.addDnsMonitor(this.fields);
	},
	
	addMonitorAdvanced: function() {
		this.dialog.hide();
		this.controller.addDnsMonitor(this.fields_advanced);
	}
	
});
