dojo.provide("monimi.TimezoneSelector");
dojo.require("dojo.date.stamp");
dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.form.FilteringSelect");

// our declared class
dojo.declare("monimi.TimezoneSelector", [dijit._Widget, dijit._Templated],
    // class properties:
    {
  	templatePath: dojo.moduleUrl("monimi", "TimezoneSelector.html"),	
	widgetsInTemplate: true,

	postCreate: function() {
	},
	
	getZone: function () {
		var value = this.select.getValue();
		if(!value) {
			value = 'UTC';
		}
		return value;
	},
	
	setZone: function(zone) {
		if(!zone) {
			this.select.setValue('UTC');		
		} else {
			this.select.setValue(zone);	
		}
		
	}
});
