dojo.provide("monimi.monitor.QuietPeriodDialog");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.form.Button");
dojo.require("dijit.form.TimeTextBox");
dojo.require("dijit.Dialog");

// our declared class
dojo.declare("monimi.monitor.QuietPeriodDialog", [dijit._Widget,  dijit._Templated],
    // class properties:
    {
	title: "Quiet Period",
  	templatePath: dojo.moduleUrl("monimi.monitor", "QuietPeriodDialog.html"),	
	widgetsInTemplate: true,
	controller: null,
	notification: null,
	notification_id: null,
	
	applyQuietPeriod: function() {
		var tstart = this.timeStart.getValue();
		var tend = this.timeEnd.getValue();
		this.close();
		this.controller.setQuietPeriod(this.notification_id, tstart, tend);
	},

	disableQuietPeriod: function() {
		this.close();
		this.controller.setQuietPeriod(this.notification_id, null, null);
	},
		
	close: function() {
		this.dialog.destroy();
		this.destroyRecursive();	
	},

	show: function() {
		this.dialog = new dijit.Dialog({title: this.title});
		this.dialog.setContent(this.domNode);
		// update values in the dialog
		this.timeStart.setValue(this.notification.qp_start);
		this.timeEnd.setValue(this.notification.qp_end);
		this.dialog.show();
		dojo.connect(this.buttonOk, 'onClick', this, 'applyQuietPeriod');
		dojo.connect(this.buttonCancel, 'onClick', this, 'close');
		dojo.connect(this.buttonDisable, 'onClick', this, 'disableQuietPeriod');
		dojo.connect(this.dialog.closeButtonNode, 'onclick', this, 'close');
	}
});