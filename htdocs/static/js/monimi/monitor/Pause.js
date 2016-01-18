dojo.provide("monimi.monitor.Pause");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.form.Button");
dojo.require("dijit.layout.StackContainer");
dojo.require("dijit.layout.ContentPane");

// our declared class
dojo.declare("monimi.monitor.Pause", [dijit._Widget, dijit._Templated],
    // class properties:
    {

  	templatePath: dojo.moduleUrl("monimi.monitor", "Pause.html"),	
	widgetsInTemplate: true,
	monitor: null,
	controller: null,

	postMixInProperties: function() {
		this.toggleTo = this.monitor.user_state == 'U_ST_NORMAL' ? "Pause" : "Resume";
		this.toggleToMessage = this.monitor.user_state == 'U_ST_NORMAL' ? "You are about to pause this check, are you sure?"
			: "You are about to resume this check, are you sure?";
	},
	
    postCreate: function() {
		dojo.connect(this.pauseButton, "onClick", this.stackContainer, 'forward');
		dojo.connect(this.cancelButton, "onClick", this.stackContainer, 'back');
		dojo.connect(this.confirmButton, "onClick", this, function() {
			this.controller.pauseMonitor(this.monitor);
		});
	}
});
