dojo.provide("monimi.monitor.Remove");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.form.Button");
dojo.require("dijit.layout.StackContainer");
dojo.require("dijit.layout.ContentPane");
dojo.require("dojox.form.BusyButton");

// our declared class
dojo.declare("monimi.monitor.Remove", [dijit._Widget, dijit._Templated],
    // class properties:
    {

  	templatePath: dojo.moduleUrl("monimi.monitor", "Remove.html"),	
	widgetsInTemplate: true,
	monitor: null,
	controller: null,

    postCreate: function() {
		dojo.connect(this.removeButton, "onClick", this.stackContainer, 'forward');
		dojo.connect(this.cancelButton, "onClick", this.stackContainer, 'back');
		dojo.connect(this.confirmButton, "onClick", this, function() {
			this.confirmButton.makeBusy();
			this.controller.removeMonitor(this.monitor);
		});
	}
});
