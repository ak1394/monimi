dojo.provide("monimi.monitor.AddDialog");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.form.Button");
dojo.require("dijit.Dialog");
dojo.require("dijit.form.ValidationTextBox");
dojo.require("monimi.monitor.add.HttpCheck");
dojo.require("monimi.monitor.add.HttpsCheck");
dojo.require("monimi.monitor.add.PingCheck");
dojo.require("monimi.monitor.add.DnsCheck");

// our declared class
dojo.declare("monimi.monitor.AddDialog", [dijit._Widget, dijit._Templated],
    // class properties:
    {
	title: "Add Check",
  	templatePath: dojo.moduleUrl("monimi.monitor", "AddDialog.html"),	
	widgetsInTemplate: true,
	controller: null,

	show: function(){
		this.dialog = new dijit.Dialog({
			title: this.title,
			parseOnLoad: false
		});
		this.dialog.setContent(this.domNode);
		this.dialog.show();
		
		this.tabContainer.addChild(new monimi.monitor.add.HttpCheck({title: 'HTTP', controller: this.controller, dialog:this.dialog}));
		this.tabContainer.addChild(new monimi.monitor.add.HttpsCheck({title: 'HTTPS', controller: this.controller, dialog:this.dialog}));
		this.tabContainer.addChild(new monimi.monitor.add.PingCheck({title: 'Ping', controller: this.controller, dialog:this.dialog}));
		this.tabContainer.addChild(new monimi.monitor.add.DnsCheck({title: 'DNS', controller: this.controller, dialog:this.dialog}));
		this.tabContainer.resize();
	}
});
