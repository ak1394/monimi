dojo.provide("monimi.monitor.add.HttpCheck");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.form.Form");
dojo.require("dijit.form.Button");
dojo.require("dijit.layout.StackContainer");
dojo.require("dijit.layout.ContentPane");
dojo.require("dijit.form.ValidationTextBox");
dojo.require("dojox.form.DropDownSelect");

// our declared class
dojo.declare("monimi.monitor.add.HttpCheck", [dijit._Widget, dijit._Templated],
    // class properties:
    {

  	templatePath: dojo.moduleUrl("monimi.monitor.add", "HttpCheck.html"),	
	widgetsInTemplate: true,
	monitor: null,
	controller: null,

    postCreate: function() {
		// form fields
		this.fields = {
			name: {name: 'Name', textbox: this.checkName},
			url: {name: 'URL', textbox: this.checkUrl},
			beat: {name: 'Check every', widget: this.beatSelect}
			}
		// set beat
		dojo.forEach([1,2,3,4,5,10,15,20,25,30,40,50,60], function(v) {
			if(this.controller.limits.min_beat <= v) {
				var min = v == 1 ? " minute" : " minutes";
				this.beatSelect.addOption({value:v, label:v + min});
			}
		}, this);
		dojo.connect(this.okButton, 'onClick', this, 'addMonitor');
		dojo.connect(this.cancelButton, 'onClick', this.dialog, 'hide');
		dojo.connect(this.form, "onValidStateChange", this, function() {
			if(this.form.isValid()) {
				this.okButton.attr('disabled', false);
			} else {
				this.okButton.attr('disabled', true);
			}
		});
	},
	
	addMonitor: function() {
		this.dialog.hide();
		this.controller.addHttpMonitor(this.fields);
	}
});
