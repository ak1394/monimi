dojo.provide("monimi.profile.PaymentButton");

dojo.require("dijit._Widget");
dojo.require("dijit._Templated");
dojo.require("dijit.form.Button");

// our declared class
dojo.declare("monimi.profile.PaymentButton", [dijit._Widget, dijit._Templated],
    // class properties:
    {

  	templatePath: dojo.moduleUrl("monimi.profile", "PaymentButton.html"),	
	widgetsInTemplate: true,

	postCreate: function() {
		this.form.setAttribute('action', this.action);
		for(var name in this.fields) {
			var input = dojo.doc.createElement('input');
			input.setAttribute('type', 'hidden');
			input.setAttribute('name', name);
			input.setAttribute('value', this.fields[name]);
			this.form.appendChild(input);
			this.payButton.setLabel(this.name);
			//this.payButton.innerHTML = this.name;
		}
	}
});
